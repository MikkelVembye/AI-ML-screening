
#-------------------------------------------------------------------------------------------
# Generate prioritized data with target studies, following our suggested screening algorithm. 
#--------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------
# Corpus embeddings
#--------------------------------------------------------------------------
# Embeddings are deterministic i.e. doesnt vary across iterations, so they are precomputed once per (dataset, model) and reused by every
# design row and every iteration. https://medium.com/@karamvirhapal/encoding-vs-embedding-models-both-output-numbers-different-stories-5c85eced1801

embedding_dir <- "simulation/embeddings"

# Function to embed the corpus using a specified model and save the embeddings to a file
embed_corpus <- function(data, model, python_dir, dir = embedding_dir) {

  data_name <- attr(data, "data_name")
  path <- file.path(dir, paste0(data_name, "_", model, ".rds"))

  # Every record in the corpus is embedded, so any subset an iteration draws can be looked up
  # by eppi_id. Ids must be unique.
  ids <- as.character(data[["eppi_id"]])
  stopifnot(!anyNA(ids), !anyDuplicated(ids))

  # Use reticulate to call Python and load the sentence-transformers library
  # This is not per worker, but per (dataset, model) combination.
  reticulate::use_python(python_dir, required = TRUE)
  sentence_transformers <- reticulate::import("sentence_transformers")
  embed_model <- sentence_transformers$SentenceTransformer(model)
  # Embed the corpus by concatenating the title and abstract for each record
  embeddings <- embed_model$encode(paste(data$title, data$abstract))
  rownames(embeddings) <- ids
  # ranger's x/y matrix interface requires named columns to recognize covariates
  colnames(embeddings) <- paste0("V", seq_len(ncol(embeddings)))

  # Save the embeddings to a file, creating the directory if it doesn't exist
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(embeddings, path)

  invisible(path)
}

# Function to load embeddings from a file, using a cache to avoid reloading if already loaded.
# We use local here to create a closure that holds the cache environment, so each worker has its own cache.
# Meaning that each worker will load the embeddings once per (dataset, model) combination, and reuse them for all iterations.
load_embeddings <- local({

  cache <- new.env(parent = emptyenv())

  function(data_name, model, dir = embedding_dir) {

    # Use a unique key for the cache based on dataset name and model
    key <- paste0(data_name, "_", model)
    # If the embeddings for this (dataset, model) combination are already in the cache, return them
    if (identical(cache$key, key)) return(cache$value)

    # If not, load the embeddings from the file and store them in the cache
    path <- file.path(dir, paste0(data_name, "_", model, ".rds"))

    # Drop the previous matrix before reading the next one so the two never coexist
    cache$key   <- NULL
    cache$value <- NULL
    # Force garbage collection to free memory before loading the new embeddings
    gc(verbose = FALSE)

    # Read the embeddings from the file and store them in the cache
    cache$value <- readRDS(path)
    cache$key   <- key
    cache$value
  }
})

generate_prioritized_data <-
    function(
      data, # data frame containing the full AI-screened dataset; must include a binary "included_final" column (1 = finally included, 0 = not)
      model, # name of the sentence-transformers model; embeddings are loaded automatically for this (data, model) pair, see load_embeddings()
      n_irrelevant_test_records = 200, # number of irrelevant records to sample for testing the model's performance
      included_var = "human_and_ai_in",
      c_target      = 0.95, # target recall for the priority screening process
      R_c           = 0.95, # target specificity for the priority screening process
      alpha         = 0, # regularization parameter for the logistic regression model (1 for LASSO, 0 for Ridge, between 0 and 1 for Elastic Net). If alpha = 2 it uses Random Forest instead of logistic regression.
      ai_miss_pct   = 0, # percentage of finally included studies to artificially flip to AI-missed (0 for no artificial flipping, 1 for all finally included studies flipped)
      seed_pct      = 1, # percentage of the finally included studies to extract as the "seed studies" pool used below; the remainder are folded back into the candidate pool as ordinary records, findable only through the normal AH+/A- screening process
      seed_train_pct = 0.5, # percentage of the seed studies to use for training the model; the remainder are held out for validation
      embed_dir     = embedding_dir, # where load_embeddings() looks for the precomputed matrix; only needs overriding in tests or when workers see a different working directory
      seed          = NULL
  ) { # random seed for reproducibility

  total_records <- nrow(data)
  data_name <- attr(data, "data_name")

  # Embeddings are a deterministic function of (data, model) so they
  # are looked up here
  embeddings <- load_embeddings(data_name, model, dir = embed_dir)

  run_start_time <- Sys.time()

  # One seed governs every random draw below; nothing downstream re-seeds
  if (!is.null(seed)) set.seed(seed)

  # Split off the finally included studies (included_final == 1) from the rest of the candidate pool
  final_inc_studies <- data |> dplyr::filter(included_final == 1)
  data              <- data |> dplyr::filter(included_final == 0)
  
  # Draw the irrelevant test set only from records both AI and human excluded
  irrelevant_pool_idx <- which(data[["human_code"]] == 0 & data[["decision_binary"]] == 0)
  irrelevant_test_study_idx <- sample(irrelevant_pool_idx, size = n_irrelevant_test_records)
  irrelevant_test_study <- data[irrelevant_test_study_idx, , drop = FALSE]

  data <- data[-irrelevant_test_study_idx, , drop = FALSE]
      
  # Artificially flip decision_binary to 0 for a share of ALL the finally included studies
  n_flip <- round(ai_miss_pct * nrow(final_inc_studies))

  if (n_flip > 0L) {
    flip_idx <- sample.int(nrow(final_inc_studies), size = n_flip)
    final_inc_studies[["decision_binary"]][flip_idx] <- 0L
    ai_missed <- final_inc_studies[flip_idx, , drop = FALSE]
    caught_final_inc <- final_inc_studies[-flip_idx, , drop = FALSE]
  } else {
    flip_idx <- integer(0)
    ai_missed <- final_inc_studies[FALSE, , drop = FALSE]
    caught_final_inc <- final_inc_studies
  }

  # Extract seed studies as a percentage of the finally included studies
  n_seed <- max(2L, as.integer(round(seed_pct * nrow(caught_final_inc))))

  seed_idx <- sample.int(nrow(caught_final_inc), size = n_seed)
  known_seed_studies <- caught_final_inc[seed_idx, , drop = FALSE]
  non_seed_final_inc <- caught_final_inc[-seed_idx, , drop = FALSE]
  
  # Candidate pool P = data, with the non-seed finally included studies folded back in as ordinary records
  data <- 
    data |> 
    dplyr::bind_rows(non_seed_final_inc) 

  # Step 6: Let 𝐀+ denote records classified as potentially eligible by 𝒜, and let 𝐀− denote records not classified as potentially eligible by 𝒜.
  a_minus <- dplyr::bind_rows(data |> dplyr::filter(.data[["decision_binary"]] == 0), ai_missed)

  # Step 7: Define 𝐀𝐇+ as all non-seed records included both by 𝒜 and humans up to this point.
  ah_plus <- data |> dplyr::filter(.data[[included_var]] == 1)
      
  # Steps 8-14: target set T, sampled with replacement from AH+ until k_min relevant records are
  # found (Hou & Tipton)
  target <- AIscreenR::sample_references(
    data = data,
    relevant_col = included_var,
    c_target = c_target,
    R_c = R_c,
    id_col = "eppi_id",
    seed = NULL # Set seed to null to allow for variance across simulations
  )
  target_ids <- target$target_ids

  # Step 15: Randomly split the correctly-caught seed studies into a training set 𝐒t and a
  # held-out validation set 𝐒v.
  St_idx <- sample.int(nrow(known_seed_studies), size = round(seed_train_pct * nrow(known_seed_studies)))
  St <- known_seed_studies[St_idx, ]
  Sv <- known_seed_studies[-St_idx, ]

  # Step 16: Define the included training set as: 𝐈 = 𝐒t%∪(𝐀𝐇+\ 𝐓)
  I_set <- dplyr::bind_rows(St, ah_plus |> dplyr::filter(!.data[["eppi_id"]] %in% target_ids))

  # Step 17: Randomly sample an irrelevant training set 𝐄    
  E_set <- AIscreenR::sample_references(
    data = irrelevant_test_study, n = nrow(I_set), id_col = "eppi_id", with_replacement = TRUE,
    seed = NULL # As for the target set: draw from the stream set at the top of this function
  )

  # Embeddings are precomputed over the whole corpus, so every record an iteration can draw is
  # already present; rows are pulled by eppi_id. Duplicated ids (E_set is sampled with
  # replacement) resolve to the same row, as before.
  emb_rows <- function(ids) {
    idx <- match(as.character(ids), rownames(embeddings))
    embeddings[idx, , drop = FALSE]
  }

  # Step 18: Train ℳ using the included training set 𝐈 and the irrelevant training set 𝐄.
  train_ids <- c(I_set[["eppi_id"]], E_set[["eppi_id"]])
  x_train   <- emb_rows(train_ids)
  y_train   <- c(rep(1L, nrow(I_set)), rep(0L, nrow(E_set)))

  if (alpha == 2) {
    fit <- ranger::ranger(x = x_train, y = factor(y_train, levels = c(0, 1)), probability = TRUE)
  } else {
    fit <- glmnet::cv.glmnet(x = x_train, y = y_train, family = "binomial", alpha = alpha)
  }

  # Step 19: Define the priority-screening set as: 𝐏∗ = 𝐀−∪ 𝐓 ∪ 𝐒v%
  ## We need to remove the E studies from a_minus here.    
      
  #a_minus <- a_minus |> dplyr::filter(!.data[["eppi_id"]] %in% E_set[["eppi_id"]])
      
  P_star <- dplyr::bind_rows(a_minus, target$target_set, Sv) |>
    dplyr::distinct(.data[["eppi_id"]], .keep_all = TRUE)

  # Step 20: Use ℳ to rank all records in 𝐏∗.
  x_pstar <- emb_rows(P_star[["eppi_id"]])
  
  if (alpha == 2) {
    P_star$priority_score <- predict(fit, data = x_pstar)$predictions[, "1"]
  } else {
    P_star$priority_score <- as.numeric(predict(fit, newx = x_pstar, s = "lambda.min", type = "response"))
  }
  
  P_star <- P_star[order(-P_star$priority_score), ]   
  P_star$row_number     <- seq_len(nrow(P_star))
        
  #P_star$is_target      <- P_star[["eppi_id"]] %in% target_ids
  #P_star$is_seed <- P_star[["eppi_id"]] %in% Sv[["eppi_id"]]
  #P_star$is_ai_missed   <- P_star[["eppi_id"]] %in% ai_missed[["eppi_id"]]

  # Get the run time in seconds
  run_time_sec <- as.numeric(difftime(Sys.time(), run_start_time, units = "secs"))
       
  attr(P_star, "total_records") <- total_records
  
  attr(P_star, "info_dat") <- tibble::tibble(
      data_name = data_name,
      model = model, 
      c_target = c_target,
      R_c = R_c,
      alpha = alpha,
      seed_pct = seed_pct,
      seed_train_pct = seed_train_pct,
      included_var = included_var,
      ai_miss_pct = ai_miss_pct,
      run_time_sec = run_time_sec
  ) |> 
    dplyr::mutate(
      train_model = dplyr::case_when(
        alpha == 2 ~ "Random Forest",
        alpha == 1 ~ "LASSO",
        alpha == 0 ~ "Ridge",
        alpha > 0 & alpha < 1 ~ "Elastic Net",
        .default = "Check model"
      )
    ) |> 
    dplyr::relocate(train_model, .after = model)
  
      
  P_star |> 
    dplyr::mutate(
      is_target = dplyr::if_else(eppi_id %in% target_ids, 1L, 0L),
      is_seed = dplyr::if_else(eppi_id %in% Sv[["eppi_id"]], 1L, 0L),
      is_ai_missed = dplyr::if_else(eppi_id %in% ai_missed[["eppi_id"]], 1L, 0L)
    )
      

}

# ## Test
# # # Load data with the "included_final" column indicating whether each record is a finally included study (1) or not (0)
# friends_data <- readRDS("friends/data/friends_cleaned.rds")
# #
# ## python_dir <- "C:/Users/B199526/AppData/Local/miniconda3/envs/positron-python/python.exe"
# #python_dir <- "C:/Users/B199526/AppData/Local/miniconda3/envs/positron-python/python.exe"
# #
# ## Seed the whole run once, here. not inside generate_prioritized_data(). Every call below then
# ## draws fresh from this one reproducible stream, so re-running this script end-to-end reproduces
# ## the same sequence of results. Repeated calls still differ from each other.
# set.seed(13082026)
# #
# # # Test (remove #)
# #friends_data <- readRDS("friends/data/friends_cleaned.rds")
# ##python_dir <- "C:/Users/B199526/AppData/Local/miniconda3/envs/positron-python/python.exe"
# ##
# python_dir <- "C:/Users/B375477/AppData/Local/miniconda3/envs/positron-python/python.exe"

# # Build embeddings
# embed_corpus(friends_data, "all-MiniLM-L6-v2", python_dir, dir = "simulation/embeddings")

# debugonce(generate_prioritized_data)
# tictoc::tic()
# data_test_small <- generate_prioritized_data(
#   data          = friends_data,
#   model         = "all-MiniLM-L6-v2",
#   included_var  = "human_and_ai_in",
#   c_target      = 0.90,
#   R_c           = 0.95,
#   alpha         = 0,
#   seed_pct      = 0.5,
#   ai_miss_pct   = 0,
#   seed          = NULL,
#   embed_dir = "simulation/embeddings"
# ) |>
#   suppressWarnings()
# tictoc::toc()

# # Build embeddings for the second model, if not already present.
# embed_corpus(friends_data, "all-mpnet-base-v2", python_dir, dir = "simulation/embeddings")

# tictoc::tic()
# data_test_large <- generate_prioritized_data(
#   data          = friends_data,
#   model         = "all-mpnet-base-v2",
#   included_var  = "human_and_ai_in",
#   c_target      = 0.90,
#   R_c           = 0.95,
#   alpha         = 0,
#   seed_pct      = 0.5,
#   ai_miss_pct   = 0,
#   seed          = NULL,
#   embed_dir = "simulation/embeddings"
# ) |>
#   suppressWarnings()
# tictoc::toc()


#--------------------------------------------------------------------------
# Estimation functions
#--------------------------------------------------------------------------

#last_target_row <- max(result_data$row_number[result_data$is_target == 1]) 
#last_target_row
#last_seed_row   <- max(result_data$row_number[result_data$is_seed == 1])
#last_ai_missed_row <- max(result_data$row_number[result_data$is_ai_missed == 1])

estimate_f <- function(data) {

  last_target_row <- max(
    data$row_number[data$is_target == 1],
    na.rm = TRUE
  )

  last_seed_row <- max(
    data$row_number[data$is_seed == 1],
    na.rm = TRUE
  )

  last_ai_missed_row <- if (
    any(data$is_ai_missed == 1, na.rm = TRUE)
  ) {
    max(
      data$row_number[data$is_ai_missed == 1],
      na.rm = TRUE
    )
  } else {
    NA_integer_
  }

  total_records <- attr(data, "total_records")

  data |>
    dplyr::summarise(
      workload_saved =
        (dplyr::n() - last_target_row) / total_records,

      pct_needed_to_find_target =
        last_target_row / dplyr::n(),

      n_ai_missed_after_target =
        sum(
          is_ai_missed == 1 &
            row_number > last_target_row,
          na.rm = TRUE
        ),

      n_ai_missed_after_seed =
        sum(
          is_ai_missed == 1 &
            row_number > last_seed_row,
          na.rm = TRUE
        ),

      any_ai_missed_after_target =
        dplyr::if_else(
          is.na(last_ai_missed_row),
          NA_integer_,
          as.integer(last_ai_missed_row > last_target_row)
        ),

      any_ai_missed_after_seed =
        dplyr::if_else(
          is.na(last_ai_missed_row),
          NA_integer_,
          as.integer(last_ai_missed_row > last_seed_row)
        ),

      any_seed_missed_after_target =
        dplyr::if_else(
          last_seed_row > last_target_row,
          1L,
          0L
        )
    ) |>
    dplyr::bind_cols(attr(data, "info_dat")) |>
    dplyr::relocate(
      workload_saved:any_seed_missed_after_target,
      .after = run_time_sec
    )
}

#debugonce(estimate_f)
#data_test_small |> estimate_f() |> View()
##
#set.seed(13082026)
#
#result_list <- 
#  purrr::map(1:2, \(i) {
#  generate_prioritized_data(
#    data          = friends_data,
#    #model         = "all-MiniLM-L6-v2",
#    #python_dir    = python_dir,
#    included_var = "human_and_ai_in",
#    c_target      = 0.90,
#    R_c           = 0.95,
#    alpha         = 0,
#    seed_pct      = 0.2,
#    seed_train_pct = 0.5,
#    ai_miss_pct   = 0,
#    seed          = NULL 
#  ) |> 
#  suppressWarnings() |> 
#  estimate_f() 
#}) |> 
#  purrr::list_rbind(names_to = "id")

#--------------------------------------------------------------------------
# Performance assessment
#--------------------------------------------------------------------------

assess_performance <- function(results) {
  
  #require(dplyr)
  
  results  |> 
    dplyr::summarise(
      n_sim = dplyr::n(),
      cnvg = mean(!is.na(workload_saved)),
      wl_mean = mean(workload_saved, na.rm = TRUE),
      wl_se = sd(workload_saved, na.rm = TRUE) / sqrt(n_sim),
      need_see_mean = mean(pct_needed_to_find_target, na.rm = TRUE),
      need_see_se = sd(pct_needed_to_find_target, na.rm = TRUE) / sqrt(n_sim),
      missed_after_target_pct = mean(any_ai_missed_after_target, na.rm = TRUE),
      # add var
      missed_after_seed_pct = mean(any_ai_missed_after_seed, na.rm = TRUE),
      times_seed_after_target_pct = mean(any_seed_missed_after_target, na.rm = TRUE),
      mean_n_ai_missed_after_target = mean(n_ai_missed_after_target, na.rm = TRUE),
      var_n_ai_missed_after_target = var(n_ai_missed_after_target, na.rm = TRUE),
      mean_n_ai_missed_after_seed = mean(n_ai_missed_after_seed, na.rm = TRUE),
      var_n_ai_missed_after_seed = var(n_ai_missed_after_seed, na.rm = TRUE),
      .by = data_name:ai_miss_pct 
    ) 
  
}

#assess_performance(result_list) |> View() 


#--------------------------------------------------------------------------
# Simulation driver
#--------------------------------------------------------------------------

run_sim <- 
  function(
   iterations,
   data,
   model,
   included_var,
   c_target,      
   R_c,          
   alpha,        
   seed_pct,       
   ai_miss_pct,
   seed,
   embed_dir = "simulation/embeddings"
  ) {

    #require(dplyr)
    #require(purrr)

    # Iteration j always gets seed + j, so raising `iterations` adds replicates without changing
    # the existing ones
    iteration_seeds <- seed + seq_len(iterations)

    iteration_results <- purrr::map(seq_len(iterations), function(i) {
      iteration_seed <- iteration_seeds[[i]]

      tryCatch(
        {
          generate_prioritized_data(
            data          = data,
            model         = model,
            embed_dir     = embed_dir,
            included_var  = included_var,
            c_target      = c_target,
            R_c           = R_c,
            alpha         = alpha,
            seed_pct      = seed_pct,
            ai_miss_pct   = ai_miss_pct,
            seed          = iteration_seed
          ) |>
            estimate_f() |>
            dplyr::mutate(
              iteration = i,
              iteration_seed = iteration_seed,
              status = "success",
              error_message = NA_character_,
              .before = 1
            )
        },
        error = function(e) {
          tibble::tibble(
            iteration = i,
            iteration_seed = iteration_seed,
            status = "failed",
            error_message = conditionMessage(e)
          )
        }
      )
    })

    failed_results <- iteration_results |>
      purrr::keep(~ identical(.x$status[[1]], "failed"))

    successful_results <- iteration_results |>
      purrr::keep(~ identical(.x$status[[1]], "success")) |>
      purrr::list_rbind()

    if (nrow(successful_results) == 0L) {
      return(tibble::tibble(
        data_name = if (is.null(attr(data, "data_name"))) NA_character_ else attr(data, "data_name"),
        model = model,
        n_sim = iterations,
        n_successful = 0L,
        cnvg = 0,
        n_failed = length(failed_results),
        failed_iterations = paste(purrr::map_int(failed_results, "iteration"), collapse = ","),
        error_messages = paste(purrr::map_chr(failed_results, "error_message"), collapse = " | ")
      ))
    }

    summary <- assess_performance(successful_results)

    summary |>
      dplyr::mutate(
        n_sim = iterations,
        n_successful = nrow(successful_results),
        cnvg = nrow(successful_results) / iterations,
        n_failed = length(failed_results),
        failed_iterations = if (length(failed_results) == 0L) {
          NA_character_
        } else {
          paste(purrr::map_int(failed_results, "iteration"), collapse = ",")
        },
        error_messages = if (length(failed_results) == 0L) {
          NA_character_
        } else {
          paste(purrr::map_chr(failed_results, "error_message"), collapse = " | ")
        },
        .after = n_sim
      )
    
  }

#set.seed(13082026)
#
#tictoc::tic()
#sim_res <- 
#  run_sim(
#    iterations    = 10,
#    data          = friends_data,
#    model         = "all-MiniLM-L6-v2",
#    included_var  = "human_and_ai_in",
#    c_target      = 0.90,
#    R_c           = 0.95,
#    alpha         = 0,
#    seed_pct      = 0.2,
#    ai_miss_pct   = 0L,
#    seed          = NULL
#)
#tictoc::toc()
#sim_res$wl_mean
#sim_res$wl_se
