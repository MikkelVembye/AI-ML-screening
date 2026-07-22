# Simulating the priority screening function multiple times
library(dplyr)
library(tidyr)
library(purrr)
library(furrr)
library(tibble)
library(reticulate)
library(progressr)
library(simhelpers)

# Show a progress bar
progressr::handlers(global = TRUE)
progressr::handlers("txtprogressbar")

# Bring in run_priority_screening()
source("friends/priority_functions.r")

python_dir <- "C:/Users/B375477/AppData/Local/miniconda3/envs/positron-python/python.exe"

sentence_transformers <- import("sentence_transformers")

friends_data <- readRDS("friends/data/friends_cleaned.rds")

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
      " relevant and ", n_irrelevant, " irrelevant records, but friends_data only has ",
      nrow(relevant_pool), " relevant and ", nrow(irrelevant_pool), " irrelevant records available."
    )
  }

  bind_rows(
    relevant_pool   |> slice_sample(n = n_relevant),
    irrelevant_pool |> slice_sample(n = n_irrelevant)
  )
}

# f_analyze for bundle_sim(): runs the method once and records where the last target, seed, and
# AI-missed studies landed in the ranked list.
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

    list(result = tibble(
      k_min                  = res$k_min,
      n_priority_list        = n_list,
      last_target_row        = last_target_row,
      last_s20_row           = last_s20_row,
      last_ai_missed_row     = last_ai_missed_row,
      last_target_pct        = last_target_row / n_list * 100,
      last_s20_pct           = last_s20_row / n_list * 100,
      last_ai_missed_pct     = last_ai_missed_row / n_list * 100,
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

  # Failed repeats are dropped, counted and reported.
  n_failed <- sum(purrr::map_lgl(rep_list, ~ !is.null(.x$error)))
  if (n_failed > 0) {
    error_msgs <- rep_list |> purrr::map("error") |> purrr::compact() |>
      purrr::map_chr(conditionMessage) |> unique()
    warning(
      n_failed, " out of ", length(rep_list), " runs failed for this design and were skipped: ",
      paste(error_msgs, collapse = " | ")
    )
  }

  rep_results <- rep_list |> purrr::map("result") |> purrr::compact() |> purrr::list_rbind()

  # Every repeat failed - one row of NAs instead of zero rows.
  if (nrow(rep_results) == 0) {
    return(tibble(
      n_failed                = n_failed,
      mean_k_min              = NA_real_,
      mean_last_target_row    = NA_real_,
      se_last_target_row      = NA_real_,
      mean_last_s20_row       = NA_real_,
      se_last_s20_row         = NA_real_,
      mean_last_ai_missed_row = NA_real_,
      se_last_ai_missed_row   = NA_real_,
      mean_last_target_pct    = NA_real_,
      se_last_target_pct      = NA_real_,
      mean_last_s20_pct       = NA_real_,
      mean_last_ai_missed_pct = NA_real_,
      mean_workload_saved_pct = NA_real_,
      mean_run_time_sec       = NA_real_,
      se_run_time_sec         = NA_real_
    ))
  }

  rep_results |>
    summarise(
      n_failed                = n_failed,
      mean_k_min              = mean(k_min),
      mean_last_target_row    = mean(last_target_row, na.rm = TRUE),
      se_last_target_row      = sd(last_target_row, na.rm = TRUE) / sqrt(n()),
      mean_last_s20_row       = mean(last_s20_row, na.rm = TRUE),
      se_last_s20_row         = sd(last_s20_row, na.rm = TRUE) / sqrt(n()),
      mean_last_ai_missed_row = mean(last_ai_missed_row, na.rm = TRUE),
      se_last_ai_missed_row   = sd(last_ai_missed_row, na.rm = TRUE) / sqrt(n()),
      mean_last_target_pct    = mean(last_target_pct, na.rm = TRUE),
      se_last_target_pct      = sd(last_target_pct, na.rm = TRUE) / sqrt(n()),
      mean_last_s20_pct       = mean(last_s20_pct, na.rm = TRUE),
      mean_last_ai_missed_pct = mean(last_ai_missed_pct, na.rm = TRUE),
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
#   mean_k_min              - average target-set size k_min across repeats
#   mean_last_target_row    - average row position of the last target study
#   se_last_target_row      - standard error of that average
#   mean_last_s20_row       - average row position of the last held-out seed study
#   se_last_s20_row         - standard error of that average
#   mean_last_ai_missed_row - average row position of the last artificially AI-missed study
#   se_last_ai_missed_row   - standard error of that average
#   mean_last_target_pct    - mean_last_target_row expressed as a % of the priority list's length
#   se_last_target_pct      - standard error of that percentage
#   mean_last_s20_pct       - mean_last_s20_row expressed as a % of the priority list's length
#   mean_last_ai_missed_pct - mean_last_ai_missed_row expressed as a % of the priority list's length
#   mean_workload_saved_pct - average % of the pile that did not need to be screened
#   mean_run_time_sec       - average wall-clock time, in seconds, that one repeat of this design took
#   se_run_time_sec         - standard error of that average

#-------------------------------------------------------------------------------------------------
# Quick check before running anything expensive: one design, two repeats.
#-------------------------------------------------------------------------------------------------
one_design <- tibble(
  # design_parameters
  dat_full = list(friends_data),
  model = "all-MiniLM-L6-v2", relevant_col = list(c("human_code", "decision_binary")),
  pool_size = NA_real_, relevant_pct = NA_real_, alpha = 1, RandomForrest = FALSE,
  # focal_parameters
  c_target = 0.90, R_c = c(0.75, 0.90),
  # auxiliary_parameters
  ai_miss_pct = 0, seed_pct = 1
)
# one_design has 2 rows (R_c sweeps two values, recycling everything else) - use just the first row.
one_row <- one_design[1, ]
set.seed(1) # seeds f_generate()'s ambient RNG state; f_analyze() still draws its own seed for run_priority_screening()
one_dat <- f_generate(dat_full = one_row$dat_full[[1]], pool_size = one_row$pool_size, relevant_pct = one_row$relevant_pct)
f_analyze(
  dat = one_dat, model = one_row$model, relevant_col = one_row$relevant_col[[1]],
  c_target = one_row$c_target, R_c = one_row$R_c, alpha = one_row$alpha,
  RandomForrest = one_row$RandomForrest, ai_miss_pct = one_row$ai_miss_pct,
  seed_pct = one_row$seed_pct
)   # should return one row of results

smoke_test <- run_sim(iterations = 2, design_factors = one_design)
smoke_test |> glimpse()

#-------------------------------------------------------------------------------------------------
# The full simulation - two separate experiments.
#   - main_design: real pile size, varies AI quality and reviewer prior knowledge.
#   - n_l_design: fixed AI quality/prior knowledge, varies pile size and relevant-paper rarity.
#-------------------------------------------------------------------------------------------------

main_design <- tidyr::expand_grid(
  # design_parameters
  dat_full             = list(friends_data),
  model                = "all-MiniLM-L6-v2",
  relevant_col         = list(c("human_code", "decision_binary")),
  pool_size            = NA_real_,   # NA = use the real pile as it is
  relevant_pct         = NA_real_,   # NA = keep the real, natural mix of relevant and irrelevant papers
  alpha                = 1,
  RandomForrest        = c(FALSE, TRUE),
  # focal_parameters
  c_target             = c(0.90, 0.95),
  R_c                  = c(0.90, 0.95),
  # auxiliary_parameters
  ai_miss_pct          = c(0, 0.3),  # how poor the AI is: 0 = normal, higher values = worse
  seed_pct             = c(0.1, 1)   # how much reviewers already know: low = starting mostly from scratch, 1 = mostly already known
)

n_l_design <- tidyr::expand_grid(
  # design_parameters
  dat_full             = list(friends_data),
  model                = "all-MiniLM-L6-v2",
  relevant_col         = list(c("human_code", "decision_binary")),
  pool_size            = c(500, 1000, 2500),
  relevant_pct         = c(0.01, 0.05),
  alpha                = 1,
  RandomForrest        = FALSE,
  # focal_parameters
  c_target             = 0.95,
  R_c                  = 0.95,
  # auxiliary_parameters
  ai_miss_pct           = 0,
  seed_pct             = 1
)

sim_results     <- run_sim(iterations = 1, design_factors = main_design)
sim_results_n_l <- run_sim(iterations = 5, design_factors = n_l_design)

saveRDS(sim_results, "friends/data/simulation_results.rds")
saveRDS(sim_results_n_l, "friends/data/simulation_results_n_l.rds")
