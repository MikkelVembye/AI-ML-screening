
#-------------------------------------------------------------------------------------------
# Generate prioritized data with target studies, following our suggested screening algorithm. 
#--------------------------------------------------------------------------------------------

generate_prioritized_data <- 
    function(
      data, # data frame containing the full AI-screened dataset; must include a binary "included_final" column (1 = finally included, 0 = not)
      model, # name of the sentence-transformers model to use for embedding
      included_var = "human_and_ai_in",
      python_dir, # path to the Python executable with sentence-transformers installed; set up independently inside this function so it works no matter which process (including parallel workers) calls it
      c_target      = 0.95, # target recall for the priority screening process
      R_c           = 0.95, # target specificity for the priority screening process
      alpha         = 0, # regularization parameter for the logistic regression model (1 for LASSO, 0 for Ridge, between 0 and 1 for Elastic Net). If alpha = 2 it uses Random Forest instead of logistic regression.
      ai_miss_pct   = 0, # percentage of finally included studies to artificially flip to AI-missed (0 for no artificial flipping, 1 for all finally included studies flipped)
      seed_pct      = 1, # percentage of the finally included studies to extract as the "seed studies" pool used below; the remainder are folded back into the candidate pool as ordinary records, findable only through the normal AH+/A- screening process
      seed_train_pct = 0.8, # percentage of the seed studies to use for training the model; the remainder are held out for validation
      seed          = 123
  ) { # random seed for reproducibility

  total_records <- nrow(data)
      
  run_start_time <- Sys.time()

  if (!is.null(seed)) set.seed(seed)

  # We need to set up python individually for each worker when running in parallel
  reticulate::use_python(python_dir, required = TRUE)
  sentence_transformers <- reticulate::import("sentence_transformers")
  embed_model <- sentence_transformers$SentenceTransformer(model)
     
 
  # Split off the finally included studies (included_final == 1) from the rest of the candidate pool
  final_inc_studies <- data |> dplyr::filter(included_final == 1)
  data              <- data |> dplyr::filter(included_final == 0)

  # Artificially flip decision_binary to 0 for a share of ALL the finally included studies
  # Number of records to flip
  n_flip   <- round(ai_miss_pct * nrow(final_inc_studies))
  # Randomly sample indices of records to flip
  flip_idx <- sample.int(nrow(final_inc_studies), size = n_flip)
  # Flip the decision_binary for the sampled records
  final_inc_studies[["decision_binary"]][flip_idx] <- 0
  # Create a separate data frame for the artificially-missed records
  ai_missed <- final_inc_studies[flip_idx, ]
  # Create a separate data frame for the remaining (correctly-caught) finally included studies
  caught_final_inc <- if (n_flip > 0) final_inc_studies[-flip_idx, ] else final_inc_studies

  # Extract seed studies as a percentage of the finally included studies
  n_seed   <- round(seed_pct * nrow(caught_final_inc))
  seed_idx <- sample.int(nrow(caught_final_inc), size = n_seed)
  caught_seed_studies <- caught_final_inc[seed_idx, ]
  # Put the remaining, non-seed finally included studies back into the candidate pool as ordinary records
  non_seed_final_inc <- if (n_seed > 0) caught_final_inc[-seed_idx, ] else caught_final_inc

  # Candidate pool P = data, with the non-seed finally included studies folded back in as ordinary records
  data <- 
    data |> 
    dplyr::bind_rows(non_seed_final_inc) 

  # Step 6: Let 𝐀+ denote records classified as potentially eligible by 𝒜, and let 𝐀− denote records not classified as potentially eligible by 𝒜.
  a_minus <- dplyr::bind_rows(data |> dplyr::filter(.data[["decision_binary"]] == 0), ai_missed)

  # Step 7: Define 𝐀𝐇+ as all non-seed records included both by 𝒜 and humans up to this point.
  ah_plus <- data |> dplyr::filter(.data[[included_var]] == 1)
      
  ## Used to construct E 
  ah_minus <- data |> dplyr::filter(.data[[included_var]] == 0)

  # Embed every record that could possibly end up in P_star. Target sampling (below) draws from the
  # full `data` pool using the `relevant_col` that is passed in
  all_records <- dplyr::bind_rows(data, ai_missed, caught_seed_studies) |>
    dplyr::distinct(.data[["eppi_id"]], .keep_all = TRUE)

  embeddings <- embed_model$encode(paste(all_records$title, all_records$abstract))
  rownames(embeddings) <- all_records[["eppi_id"]]
  # ranger's x/y matrix interface requires named columns to recognize covariates
  colnames(embeddings) <- paste0("V", seq_len(ncol(embeddings)))

  # Steps 8-14: target set T, sampled with replacement from AH+ until k_min relevant records are
  # found (Hou & Tipton)
  target <- AIscreenR::sample_references(
    data = data,
    relevant_col = included_var,
    c_target = c_target,
    R_c = R_c,
    id_col = "eppi_id",
    seed = seed
  )
  target_ids <- target$target_ids

  # Step 15: Randomly split the correctly-caught seed studies into a training set 𝐒t% and a
  # held-out validation set 𝐒20%.
  St_idx <- sample.int(nrow(caught_seed_studies), size = round(seed_train_pct * nrow(caught_seed_studies)))
  St <- caught_seed_studies[St_idx, ]
  Sv <- caught_seed_studies[-St_idx, ]

  # Step 16: Define the included training set as: 𝐈 = 𝐒t%∪(𝐀𝐇+\ 𝐓)
  I_set <- dplyr::bind_rows(St, ah_plus |> dplyr::filter(!.data[["eppi_id"]] %in% target_ids))

  # Step 17: Randomly sample an irrelevant training set 𝐄    
  E_set <- AIscreenR::sample_references(
    data = ah_minus, n = nrow(I_set), id_col = "eppi_id", with_replacement = TRUE, seed = seed
  )

  # Step 18: Train ℳ using the included training set 𝐈 and the irrelevant training set 𝐄.
  train_ids <- c(I_set[["eppi_id"]], E_set[["eppi_id"]])
  x_train   <- embeddings[match(train_ids, rownames(embeddings)), , drop = FALSE]
  y_train   <- c(rep(1L, nrow(I_set)), rep(0L, nrow(E_set)))

  if (alpha == 2) {
    fit <- ranger::ranger(x = x_train, y = factor(y_train, levels = c(0, 1)), probability = TRUE)
  } else {
    fit <- glmnet::cv.glmnet(x = x_train, y = y_train, family = "binomial", alpha = alpha)
  }

  # Step 19: Define the priority-screening set as: 𝐏∗ = 𝐀−∪ 𝐓 ∪ 𝐒v%
  P_star <- dplyr::bind_rows(a_minus, target$target_set, Sv) |>
    dplyr::distinct(.data[["eppi_id"]], .keep_all = TRUE)

  # Step 20: Use ℳ to rank all records in 𝐏∗.
  x_pstar <- embeddings[match(P_star[["eppi_id"]], rownames(embeddings)), , drop = FALSE]
  P_star$priority_score <- if (alpha == 2) {
    predict(fit, data = x_pstar)$predictions[, "1"]
  } else {
    as.numeric(predict(fit, newx = x_pstar, s = "lambda.min", type = "response"))
  }
  
  P_star <- P_star[order(-P_star$priority_score), ]   
  P_star$row_number     <- seq_len(nrow(P_star))
        
  #P_star$is_target      <- P_star[["eppi_id"]] %in% target_ids
  #P_star$is_final_inc20 <- P_star[["eppi_id"]] %in% Sv[["eppi_id"]]
  #P_star$is_ai_missed   <- P_star[["eppi_id"]] %in% ai_missed[["eppi_id"]]

  # Get the run time in seconds
  run_time_sec <- as.numeric(difftime(Sys.time(), run_start_time, units = "secs"))
     
  attr(P_star, "run_time_sec") <- run_time_sec    
  attr(P_star, "total_records") <- total_records
  attr(P_star, "run_time_sec") <- run_time_sec
      
  P_star |> 
    dplyr::mutate(
      is_target = dplyr::if_else(eppi_id %in% target_ids, 1L, 0L),
      is_final_inc20 = dplyr::if_else(eppi_id %in% Sv[["eppi_id"]], 1L, 0L),
      is_ai_missed = dplyr::if_else(eppi_id %in% ai_missed[["eppi_id"]], 1L, 0L)
    )
      
#  list(
#    priority_list = P_star,
#    target_ids    = target_ids,
#    s20_ids       = S20[["eppi_id"]],
#    ai_missed_ids = ai_missed[["eppi_id"]],
#    k_min         = target$k,
#    seed          = seed,
#    c_target      = c_target,
#    R_c           = R_c,
#    alpha         = alpha,
#    seed_pct      = seed_pct,
#    RandomForrest = RandomForrest,
#    ai_miss_pct   = ai_miss_pct,
#    run_time_sec  = run_time_sec
#  )
      
    }


## Test
# # Load data with the "included_final" column indicating whether each record is a finally included study (1) or not (0)
friends_data <- readRDS("friends/data/friends_cleaned.rds")
#
set.seed(123)
#

# python_dir <- "C:/Users/B199526/AppData/Local/miniconda3/envs/positron-python/python.exe"
python_dir <- "C:/Users/B375477/AppData/Local/miniconda3/envs/positron-python/python.exe"

# Seed the whole run once, here. not inside generate_prioritized_data(). Every call below then
# draws fresh from this one reproducible stream, so re-running this script end-to-end reproduces
# the same sequence of results. Repeated calls still differ from each other.
set.seed(123)

result_data <-
  generate_prioritized_data(
    data          = friends_data,
    model         = "all-MiniLM-L6-v2",
    python_dir    = python_dir,
    included_var = "human_and_ai_in",
    c_target      = 0.90,
    R_c           = 0.95,
    alpha         = 0,
    seed_pct      = 0.2,
    ai_miss_pct   = 0.2,
    seed          = NULL # We need to set correct seeds 
) |> 
  suppressWarnings()
#
## # Find the last row number of the target studies in the priority list
#last_target_row <- max(result_data$row_number[result_data$is_target == 1])
#last_target_row 
#last_seed_row   <- max(result_data$row_number[result_data$is_final_inc20 == 1])
#last_seed_row


#--------------------------------------------------------------------------
# Estimation functions
#--------------------------------------------------------------------------

#last_target_row <- max(result_data$row_number[result_data$is_target == 1]) 
#last_target_row
#last_seed_row   <- max(result_data$row_number[result_data$is_final_inc20 == 1])
#last_ai_missed_row <- max(result_data$row_number[result_data$is_ai_missed == 1])

estimate_f <- function(data) {

  last_target_row <- max(data$row_number[data$is_target == 1]) 
  last_seed_row   <- max(data$row_number[data$is_final_inc20 == 1])
  last_ai_missed_row <- max(data$row_number[data$is_ai_missed == 1])
  total_records <- attr(data, "total_records")

  data |>
    dplyr::summarise(
      workload_saved = (dplyr::n() - last_target_row) / total_records,

      n_ai_missed_after_target = sum(
        is_ai_missed == 1 & row_number > last_target_row,
        na.rm = TRUE
      ),

      n_ai_missed_after_seed = sum(
        is_ai_missed == 1 & row_number > last_seed_row,
        na.rm = TRUE
      ),

      ai_any_missed_target = dplyr::if_else(last_ai_missed_row > last_target_row, 1, 0),
      ai_any_missed_seed = dplyr::if_else(last_ai_missed_row > last_seed_row, 1, 0),
      
    ) 
}

## Test
estimate_f(result_data)

#--------------------------------------------------------------------------
# Performance assessment
#--------------------------------------------------------------------------


#--------------------------------------------------------------------------
# Simulation driver
#--------------------------------------------------------------------------

