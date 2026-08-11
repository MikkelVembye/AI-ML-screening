library(dplyr)
library(tidyr)
library(purrr)
library(furrr)
library(tibble)
library(reticulate)
library(progressr)
library(simhelpers)

# Show a progress bar.
if (!isTRUE(getOption("knitr.in.progress"))) {
  progressr::handlers(global = TRUE)
}
progressr::handlers("txtprogressbar")

source("priority_screening/priority_functions.r")

#-------------------------------------------------------------------------------------------------
# Step 1 (Table 4.1: Generate/Analyze/Summarize): build one test pile, analyse it once, summarise
# many analyses - combined below into a single driver with simhelpers::bundle_sim().
# See https://meghapsimatrix.github.io/simhelpers/reference/bundle_sim.html
#-------------------------------------------------------------------------------------------------

# f_generate for bundle_sim(): dat_full unchanged if pool_size and relevant_pct are both NA,
# otherwise resampled to that size/relevant split
f_generate <- function(dat_full, # the full dataset to resample from
                       pool_size = NA, # how many papers to include in the test pile (NA = use the full dataset)
                       relevant_pct = NA # what share of the pile should be genuinely relevant (NA = use the full dataset's natural mix)
  ) {

  if (is.na(pool_size) && is.na(relevant_pct)) return(dat_full)

  relevant_pool   <- dat_full |> filter(included_final == 1)
  irrelevant_pool <- dat_full |> filter(included_final == 0)

  # Fall back to the full dataset's size/mix for whichever wasn't specified.
  if (is.na(pool_size))    pool_size    <- nrow(dat_full)
  if (is.na(relevant_pct)) relevant_pct <- nrow(relevant_pool) / nrow(dat_full)

  n_relevant   <- round(pool_size * relevant_pct)
  n_irrelevant <- pool_size - n_relevant

  # Not enough records in the original dataset to satisfy the requested split.
  if (n_relevant > nrow(relevant_pool) || n_irrelevant > nrow(irrelevant_pool)) {
    stop(
      "f_generate(): requested pool_size/relevant_pct needs ", n_relevant,
      " relevant and ", n_irrelevant, " irrelevant records, but dat_full only has ",
      nrow(relevant_pool), " relevant and ", nrow(irrelevant_pool), " irrelevant records available."
    )
  }

  bind_rows(
    relevant_pool   |> slice_sample(n = n_relevant),
    irrelevant_pool |> slice_sample(n = n_irrelevant)
  )
}

# f_analyze for bundle_sim(): runs the method once and records where the last target, seed, and
# AI-missed studies landed in the ranked list. Reads python_dir from the calling environment.
f_analyze <- function(dat, # the test pile to screen, from f_generate()
                      model = "all-MiniLM-L6-v2", # sentence-transformers model to use for embedding
                      relevant_col = c("human_code", "decision_binary"), # column(s) marking a record as relevant
                      c_target = 0.95, # target recall for the priority screening method
                      R_c = 0.95, # target reliability for the priority screening method
                      alpha = 1, # glmnet mixing parameter (1 = LASSO, 0 = Ridge, between = Elastic Net)
                      RandomForrest = FALSE, # use a random forest classifier instead of glmnet
                      ai_miss_pct = 0, # share of finally included studies artificially flipped to AI-missed
                      seed_pct = 1 # share of finally included studies used as seed studies
  ) {

  tryCatch({
    res <- run_priority_screening(
      data          = dat,
      model         = model,
      python_dir    = python_dir,
      relevant_col  = relevant_col,
      c_target      = c_target,
      R_c           = R_c,
      alpha         = alpha,
      RandomForrest = RandomForrest,
      ai_miss_pct   = ai_miss_pct,
      seed_pct      = seed_pct,
      seed          = sample.int(.Machine$integer.max, 1)
    )

    pl <- res$priority_list
    n_list <- nrow(pl)

    # Row number of the last target, last held-out seed, and last AI-missed study in the ranked list
    last_target_row    <- max(pl$row_number[pl$is_target])
    last_s20_row       <- if (any(pl$is_final_inc20)) max(pl$row_number[pl$is_final_inc20]) else NA_integer_
    last_ai_missed_row <- if (any(pl$is_ai_missed))   max(pl$row_number[pl$is_ai_missed])   else NA_integer_

    # Row number of the last human-included and last AI-included record in the ranked list.
    # human_code/decision_binary are required columns for every project, so these are safe to
    # compute unconditionally.
    is_human   <- as.numeric(pl$human_code) == 1
    is_ai      <- as.numeric(pl$decision_binary) == 1
    last_human_row <- if (any(is_human, na.rm = TRUE)) max(pl$row_number[is_human]) else NA_integer_
    last_ai_row    <- if (any(is_ai, na.rm = TRUE))    max(pl$row_number[is_ai])    else NA_integer_

    # Row number of the last finally-included ("benchmark") study in the ranked list. included_final
    # is a required column for every project, so this is safe to compute unconditionally.
    is_benchmark       <- as.numeric(pl$included_final) == 1
    last_benchmark_row <- if (any(is_benchmark, na.rm = TRUE)) max(pl$row_number[is_benchmark]) else NA_integer_

    list(result = tibble(
      k_min                  = res$k_min,
      n_priority_list        = n_list,
      last_target_row        = last_target_row,
      last_s20_row           = last_s20_row,
      last_ai_missed_row     = last_ai_missed_row,
      last_human_row         = last_human_row,
      last_ai_row            = last_ai_row,
      last_benchmark_row     = last_benchmark_row,
      last_target_pct        = last_target_row / n_list * 100,
      last_s20_pct           = last_s20_row / n_list * 100,
      last_ai_missed_pct     = last_ai_missed_row / n_list * 100,
      last_human_pct         = last_human_row / n_list * 100,
      last_ai_pct            = last_ai_row / n_list * 100,
      last_benchmark_pct     = last_benchmark_row / n_list * 100,
      workload_saved_pct     = (1 - last_target_row / n_list) * 100,
      run_time_sec           = res$run_time_sec # timed inside run_priority_screening() itself
    ), error = NULL)
  }, error = function(e) list(result = NULL, error = e))
}

#-------------------------------------------------------------------------------------------------
# Step 3: summarise the repeated tests into overall results.
#
# last_target_row/pct is the stopping point; last_s20_row/pct checks whether the held-out seed
# studies keep up with it; last_ai_missed_row/pct checks whether studies the AI got wrong still
# surface early. Mean + standard error of each across repeats.
#-------------------------------------------------------------------------------------------------

# f_summarize for bundle_sim(): takes the list of repeats for one design and boils it down to one
# summary row, dropping any failed repeats.
f_summarize <- function(rep_list # list of f_analyze() results (list(result=, error=)) for one design
  ) {

  # Failed repeats are dropped, counted and reported
  n_failed <- sum(purrr::map_lgl(rep_list, ~ !is.null(.x$error)))
  error_message <- NA_character_
  if (n_failed > 0) {
    error_msgs <- rep_list |> purrr::map("error") |> purrr::compact() |>
      purrr::map_chr(conditionMessage) |> unique()
    error_message <- paste(error_msgs, collapse = " | ")
    warning(
      n_failed, " out of ", length(rep_list), " runs failed for this design and were skipped: ",
      error_message
    )
  }

  rep_results <- rep_list |> purrr::map("result") |> purrr::compact() |> purrr::list_rbind()

  # Every repeat failed - one row of NAs instead of zero rows.
  if (nrow(rep_results) == 0) {
    return(tibble(
      n_failed                = n_failed,
      error_message           = error_message,
      mean_k_min              = NA_real_,
      mean_last_target_row    = NA_real_,
      se_last_target_row      = NA_real_,
      mean_last_s20_row       = NA_real_,
      se_last_s20_row         = NA_real_,
      mean_last_ai_missed_row = NA_real_,
      se_last_ai_missed_row   = NA_real_,
      mean_last_human_row     = NA_real_,
      se_last_human_row       = NA_real_,
      mean_last_ai_row        = NA_real_,
      se_last_ai_row          = NA_real_,
      mean_last_benchmark_row = NA_real_,
      se_last_benchmark_row   = NA_real_,
      mean_last_target_pct    = NA_real_,
      se_last_target_pct      = NA_real_,
      mean_last_s20_pct       = NA_real_,
      mean_last_ai_missed_pct = NA_real_,
      mean_last_human_pct     = NA_real_,
      mean_last_ai_pct        = NA_real_,
      mean_last_benchmark_pct = NA_real_,
      mean_workload_saved_pct = NA_real_,
      mean_run_time_sec       = NA_real_,
      se_run_time_sec         = NA_real_
    ))
  }

  rep_results |>
    summarise(
      n_failed                = n_failed,
      error_message           = error_message,
      mean_k_min              = mean(k_min),
      mean_last_target_row    = mean(last_target_row, na.rm = TRUE),
      se_last_target_row      = sd(last_target_row, na.rm = TRUE) / sqrt(n()),
      mean_last_s20_row       = mean(last_s20_row, na.rm = TRUE),
      se_last_s20_row         = sd(last_s20_row, na.rm = TRUE) / sqrt(n()),
      mean_last_ai_missed_row = mean(last_ai_missed_row, na.rm = TRUE),
      se_last_ai_missed_row   = sd(last_ai_missed_row, na.rm = TRUE) / sqrt(n()),
      mean_last_human_row     = mean(last_human_row, na.rm = TRUE),
      se_last_human_row       = sd(last_human_row, na.rm = TRUE) / sqrt(n()),
      mean_last_ai_row        = mean(last_ai_row, na.rm = TRUE),
      se_last_ai_row          = sd(last_ai_row, na.rm = TRUE) / sqrt(n()),
      mean_last_benchmark_row = mean(last_benchmark_row, na.rm = TRUE),
      se_last_benchmark_row   = sd(last_benchmark_row, na.rm = TRUE) / sqrt(n()),
      mean_last_target_pct    = mean(last_target_pct, na.rm = TRUE),
      se_last_target_pct      = sd(last_target_pct, na.rm = TRUE) / sqrt(n()),
      mean_last_s20_pct       = mean(last_s20_pct, na.rm = TRUE),
      mean_last_ai_missed_pct = mean(last_ai_missed_pct, na.rm = TRUE),
      mean_last_human_pct     = mean(last_human_pct, na.rm = TRUE),
      mean_last_ai_pct        = mean(last_ai_pct, na.rm = TRUE),
      mean_last_benchmark_pct = mean(last_benchmark_pct, na.rm = TRUE),
      mean_workload_saved_pct = mean(workload_saved_pct, na.rm = TRUE),
      mean_run_time_sec       = mean(run_time_sec, na.rm = TRUE),
      se_run_time_sec         = sd(run_time_sec, na.rm = TRUE) / sqrt(n())
    )
}

# bundle data-generation, data-analysis, and performance summary functions
sim_driver <- simhelpers::bundle_sim(
  f_generate         = f_generate, # generates one test pile for a design
  f_analyze          = f_analyze, # runs the method once on that pile
  f_summarize        = f_summarize, # summarises the repeats for one design
  reps_name          = "iterations", # name of the argument for the number of replications
  summarize_opt_name = "summarize", # name of the argument for where to apply f_summarize to the simulation results
  row_bind_reps      = FALSE # whether to combine the simulation results into a data.frame. This is set to FALSE as this is already done in f_summarize()
)

#-------------------------------------------------------------------------------------------------
# Step 4: run every combination of settings being tested, repeating and summarising each one.
#-------------------------------------------------------------------------------------------------

# Returns: a tibble with one row per design combination in design_factors, its original columns
# plus the summary columns from f_summarize(). Parallelizes across design rows, not across
# repeats within a design.

run_sim <- function(iterations, # how many times to repeat the test for each design
                    design_factors, # tibble with one row per combination of settings to test
                    base_seed = 1, # starting random seed
                    workers = max(1, parallel::detectCores() - 1) # number of parallel workers
  ) {
  future::plan(future::multisession, workers = workers)
  on.exit(future::plan(future::sequential), add = TRUE)

  # Start up message with how many designs and repeats are about to be run, and when it started.
  n_designs  <- nrow(design_factors)
  start_time <- Sys.time()
  message(
    "Starting simulation: ", n_designs, " design", if (n_designs != 1) "s" else "",
    " x ", iterations, " repeats each, at ", format(start_time, "%H:%M:%S")
  )

  # Run the simulation using the simhelpers evaluate_by_row(). design_factors' columns are matched
  # by name to sim_driver's arguments, which has no "..." to catch stray columns - so design_id is
  # attached afterward instead of being a design_factors column itself. evaluate_by_row() preserves
  # row order, so row_number() lines results back up with the design that produced them.
  results <- progressr::with_progress({
    evaluate_by_row(
      params       = design_factors, # the tibble of design factors to iterate over
      sim_function = sim_driver, # the function that runs one design's repeats and summarizes them
      iterations   = iterations, # how many times to repeat each design
      seed         = base_seed, # starting random seed for the whole simulation
      summarize    = TRUE, # apply f_summarize() to each design's repeats, not just return the raw list
      .progress    = TRUE, # show a progress tick each time a design row finishes
      # Globals/packages each worker needs, listed explicitly since automatic detection misses some.
      .options     = furrr::furrr_options(
        seed     = TRUE, # give each parallel worker its own reproducible random stream
        globals  = c("sim_driver", "f_generate", "f_analyze",
                    "f_summarize", "run_priority_screening", "python_dir"),
        packages = c("dplyr", "tibble", "glmnet", "ranger", "reticulate", "AIscreenR")
      )
    )
  }) |> mutate(design_id = row_number(), .before = 1)

  elapsed <- difftime(Sys.time(), start_time)
  message("Simulation finished in ", round(as.numeric(elapsed), 2), " ", units(elapsed))

  results
}

# Returns:
#   dat_full                - design_parameters: the full dataset each repeat resampled from (carried through as a list-column)
#   model                   - design_parameters: which sentence-transformers model was used
#   pool_size               - design_parameters: how many papers were in the test pile (NA = the real pile, unresampled)
#   relevant_pct            - design_parameters: what share of the pile was genuinely relevant (NA = the real pile's natural mix)
#   c_target                - focal_parameters: the nominal recall the method promises
#   R_c                     - focal_parameters: the nominal reliability the method promises
#   alpha                   - design_parameters: LASSO/Ridge/Elastic Net mixing parameter (ignored when RandomForrest = TRUE)
#   RandomForrest           - design_parameters: whether the classifier was a random forest instead of glmnet
#   ai_miss_pct             - auxiliary_parameters: how poor the AI screener was made to be
#   seed_pct                - auxiliary_parameters: how much prior knowledge reviewers were given
#   design_id               - which row of design_factors this result came from
#   n_failed                - how many of this design's repeats errored out and were dropped
#   error_message           - the failed repeats' error text
#   mean_k_min              - average target-set size k_min across repeats
#   mean_last_target_row    - average row position of the last target study
#   se_last_target_row      - standard error of that average
#   mean_last_s20_row       - average row position of the last held-out seed study
#   se_last_s20_row         - standard error of that average
#   mean_last_ai_missed_row - average row position of the last artificially AI-missed study
#   se_last_ai_missed_row   - standard error of that average
#   mean_last_human_row     - average row position of the last human_code == 1 record
#   se_last_human_row       - standard error of that average
#   mean_last_ai_row        - average row position of the last decision_binary == 1 record
#   se_last_ai_row          - standard error of that average
#   mean_last_benchmark_row - average row position of the last finally-included (included_final == 1) record (only different from mean_last_s20_row if seed_pct < 1 i.e., whenever not every finally-included study gets routed into the seed pool)
#   se_last_benchmark_row   - standard error of that average
#   mean_last_target_pct    - mean_last_target_row expressed as a % of the priority list's length
#   se_last_target_pct      - standard error of that percentage
#   mean_last_s20_pct       - mean_last_s20_row expressed as a % of the priority list's length
#   mean_last_ai_missed_pct - mean_last_ai_missed_row expressed as a % of the priority list's length
#   mean_last_human_pct     - mean_last_human_row expressed as a % of the priority list's length
#   mean_last_ai_pct        - mean_last_ai_row expressed as a % of the priority list's length
#   mean_last_benchmark_pct - mean_last_benchmark_row expressed as a % of the priority list's length
#   mean_workload_saved_pct - average % of the pile that did not need to be screened
#   mean_run_time_sec       - average wall-clock time, in seconds, that one repeat of this design took
#   se_run_time_sec         - standard error of that average
