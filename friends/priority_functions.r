library(dplyr)
library(readr)
library(stringr)
remotes::install_github("MikkelVembye/AIscreenR", build_vignettes = TRUE, force = TRUE)
library(AIscreenR)
library(tidyverse)
library(CiteSource)
library(glmnet)
library(reticulate)

# Steps 1-2: Deduplicate all candidate records in 𝐏 and remove records without abstracts for manual human screening
# Done in data_manipulation.r

# Step 3: Construct a test set V consisting of all finally included studies and 1,000 randomly sampled records from P.
# This step is already done.

# Step 4: Develop and validate the AI screening model A on V until all finally included studies are identified with satisfactory specificity.
# This step is already done.

# Step 5: Apply A to all remaining records in P.
# This step is already done.

run_priority_screening <- function(data, # data frame containing the full AI-screened dataset; must include a binary "included_final" column (1 = finally included, 0 = not)
                                    model, # name of the sentence-transformers model to use for embedding
                                    relevant_col  = c("human_code", "decision_binary"), # Names of the relevant columns used for sampling T
                                    c_target      = 0.95, # target recall for the priority screening process
                                    R_c           = 0.95, # target specificity for the priority screening process
                                    alpha         = 1, # regularization parameter for the logistic regression model (1 for LASSO, 0 for Ridge, between 0 and 1 for Elastic Net)
                                    ai_miss_pct   = 0, # percentage of finally included studies to artificially flip to AI-missed (0 for no artificial flipping, 1 for all finally included studies flipped)
                                    seed_pct      = 1, # percentage of the finally included studies to extract as the "seed studies" pool used below; the remainder are folded back into the candidate pool as ordinary records, findable only through the normal AH+/A- screening process
                                    seed          = 123) { # random seed for reproducibility

  set.seed(seed)

  embed_model <- sentence_transformers$SentenceTransformer(model)

  # Split off the finally included studies (included_final == 1) from the rest of the candidate pool
  final_inc_studies <- data |> filter(included_final == 1)
  data              <- data |> filter(included_final == 0)

  # Extract seed studies as a percentage of the finally included studies
  n_seed   <- round(seed_pct * nrow(final_inc_studies))
  seed_idx <- sample.int(nrow(final_inc_studies), size = n_seed)
  seed_studies       <- final_inc_studies[seed_idx, ]
  # Put the remaining finally included studies back into the candidate pool as ordinary records
  non_seed_final_inc <- if (n_seed > 0) final_inc_studies[-seed_idx, ] else final_inc_studies

  # artificially flip decision_binary to 0 for a share of the seed studies.
  # Number of records to flip
  n_flip   <- round(ai_miss_pct * nrow(seed_studies))
  # Randomly sample indices of records to flip
  flip_idx <- sample.int(nrow(seed_studies), size = n_flip)
  # Flip the decision_binary for the sampled records
  seed_studies[["decision_binary"]][flip_idx] <- 0
  # Create a separate data frame for the artificially-missed records
  ai_missed <- seed_studies[flip_idx, ]
  # Create a separate data frame for the remaining seed studies (the ones not artificially flipped to
  # AI-missed above)
  caught_seed_studies <- if (n_flip > 0) seed_studies[-flip_idx, ] else seed_studies

  # Candidate pool P = data, with the non-seed finally included studies folded back in as ordinary
  # (unflipped) candidate-pool records
  data <- data |> bind_rows(non_seed_final_inc)

  # Step 6: Let 𝐀+ denote records classified as potentially eligible by 𝒜, and let 𝐀− denote records not classified as potentially eligible by 𝒜.
  a_minus <- bind_rows(data |> filter(.data[["decision_binary"]] == 0), ai_missed)

  # Step 7: Define 𝐀𝐇+ as all non-seed records included both by 𝒜 and humans up to this point.
  ah_plus <- data |> filter(.data[["decision_binary"]] == 1, .data[["human_code"]] == 1)

  # Embed every record needed for this run (AH+, A-, all finally included studies) with the chosen model
  all_records <- bind_rows(ah_plus, a_minus, caught_seed_studies) |>
    distinct(.data[["eppi_id"]], .keep_all = TRUE)

  embeddings <- embed_model$encode(paste(all_records$title, all_records$abstract))
  rownames(embeddings) <- all_records[["eppi_id"]]

  # Steps 8-14: target set T, sampled with replacement from AH+ until k_min relevant records are
  # found (Hou & Tipton)
  target <- sample_references(
    data = data,
    relevant_col = relevant_col,
    c_target = c_target,
    R_c = R_c,
    id_col = "eppi_id",
    seed = seed
  )
  target_ids <- target$target_ids

  # Step 15: Randomly split the correctly-caught seed studies into a training set 𝐒80% and a
  # held-out validation set 𝐒20%.
  s80_idx <- sample.int(nrow(caught_seed_studies), size = round(0.8 * nrow(caught_seed_studies)))
  S80 <- caught_seed_studies[s80_idx, ]
  S20 <- caught_seed_studies[-s80_idx, ]

  # Step 16: Define the included training set as: 𝐈 = 𝐒80%∪(𝐀𝐇+\ 𝐓)
  I_set <- bind_rows(S80, ah_plus |> filter(!.data[["eppi_id"]] %in% target_ids))

  # Step 17: Randomly sample an irrelevant training set 𝐄
  E_set <- sample_references(
    data = a_minus, n = nrow(I_set), id_col = "eppi_id", with_replacement = TRUE, seed = seed
  )

  # Step 18: Train ℳ using the included training set 𝐈 and the irrelevant training set 𝐄.
  train_ids <- c(I_set[["eppi_id"]], E_set[["eppi_id"]])
  x_train   <- embeddings[match(train_ids, rownames(embeddings)), , drop = FALSE]
  y_train   <- c(rep(1L, nrow(I_set)), rep(0L, nrow(E_set)))

  fit <- cv.glmnet(x = x_train, y = y_train, family = "binomial", alpha = alpha)

  # Step 19: Define the priority-screening set as: 𝐏∗ = 𝐀−∪ 𝐓 ∪ 𝐒20%
  P_star <- bind_rows(a_minus, target$target_set, S20) |>
    distinct(.data[["eppi_id"]], .keep_all = TRUE)

  # Step 20: Use ℳ to rank all records in 𝐏∗.
  x_pstar <- embeddings[match(P_star[["eppi_id"]], rownames(embeddings)), , drop = FALSE]
  P_star$priority_score <- as.numeric(predict(fit, newx = x_pstar, s = "lambda.min", type = "response"))
  P_star <- P_star[order(-P_star$priority_score), ]

  P_star |>
    mutate(
      row_number      = row_number(),
      is_target       = .data[["eppi_id"]] %in% target_ids,
      is_final_inc20  = .data[["eppi_id"]] %in% S20[["eppi_id"]],
      is_ai_missed    = .data[["eppi_id"]] %in% ai_missed[["eppi_id"]],
      # Position of the last (worst-ranked) target study (stopping point when following hou and tipton)
      last_target_row = max(row_number[is_target]),
      # % of the held-out finally included studies (S20%) that would already have been screened by
      # the time you reach that stopping point, i.e. before the last target study is found
      pct_final_inc20_before_target =
        sum(is_final_inc20 & row_number <= last_target_row) / sum(is_final_inc20) * 100,
      # % of the artificially AI-missed finally included studies (decision_binary flipped 1 -> 0)
      # that would already have been screened before the last target study is found
      pct_ai_missed_before_target =
        if (any(is_ai_missed)) {
          sum(is_ai_missed & row_number <= last_target_row) / sum(is_ai_missed) * 100
        } else {
          NA_real_
        },
      seed           = seed,
      c_target       = c_target,
      R_c            = R_c,
      alpha          = alpha,
      ai_miss_pct    = ai_miss_pct,
      k_min          = target$k
    )
}

#------------------------------------------------------------------------
# Example usage of the run_priority_screening function
#------------------------------------------------------------------------
python_dir <- "C:/Users/B375477/AppData/Local/miniconda3/envs/positron-python/python.exe"

  use_python(
    python_dir,
    required = TRUE
  )

  py_config()

  sentence_transformers <- import("sentence_transformers")

# Load data with the "included_final" column indicating whether each record is a finally included study (1) or not (0)
friends_data <- readRDS("friends/data/friends_cleaned.rds")


result <- run_priority_screening(
                                    data          = friends_data,
                                    model         = "all-MiniLM-L6-v2",
                                    relevant_col  = c("human_code", "decision_binary"),
                                    c_target      = 0.90,
                                    R_c           = 0.90,
                                    alpha         = 1,
                                    ai_miss_pct   = 0,
                                    seed          = 123
)

analyzed_res <-result |>
  summarise(
    n_priority_list                = n(),
    last_target_row                = max(row_number[is_target]),
    last_final_inc20_row           = max(row_number[is_final_inc20]),
    last_ai_missed_row             = if (any(is_ai_missed)) max(row_number[is_ai_missed]) else NA_integer_,
    pct_final_inc20_before_target  = first(pct_final_inc20_before_target),
    pct_ai_missed_before_target    = first(pct_ai_missed_before_target)
  ) |>
  mutate(
    last_target_pct      = last_target_row / n_priority_list * 100,
    last_final_inc20_pct = last_final_inc20_row / n_priority_list * 100,
    last_ai_missed_pct   = last_ai_missed_row / n_priority_list * 100
  )

last_target_row <- max(result$row_number[result$is_target])

total_final_inc <- sum(result$is_final_inc20 | result$is_ai_missed)
total_human_inc <- sum(as.numeric(result$human_code) == 1, na.rm = TRUE)
total_ai_inc    <- sum(as.numeric(result$decision_binary) == 1, na.rm = TRUE)

recall_curve <- result |>
  arrange(row_number) |>
  mutate(
    final_inc     = is_final_inc20 | is_ai_missed,
    human_inc     = as.numeric(human_code) == 1,
    ai_inc        = as.numeric(decision_binary) == 1,
    cum_final_inc = cumsum(final_inc) / total_final_inc * 100,
    cum_human_inc = cumsum(human_inc) / total_human_inc * 100,
    cum_ai_inc    = cumsum(ai_inc) / total_ai_inc * 100
  ) |>
  select(row_number, cum_final_inc, cum_human_inc, cum_ai_inc) |>
  pivot_longer(
    cols = starts_with("cum_"),
    names_to = "group",
    values_to = "recall"
  ) |>
  mutate(
    group = recode(
      group,
      cum_final_inc = "Finally included",
      cum_human_inc = "Human included",
      cum_ai_inc    = "AI included"
    )
  )

ggplot(recall_curve, aes(x = row_number, y = recall, color = group)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = last_target_row, linetype = "dashed", color = "black") +
  annotate(
    "text", x = last_target_row, y = 10,
    label = "Stopping point\n(last target study)",
    hjust = -0.05, size = 3.5, color = "black"
  ) +
  labs(
    x = "Number of studies screened (priority list position)",
    y = "Cumulative recall (%)",
    color = NULL,
    title = "Cumulative recall by priority-list position"
  ) +
  theme_minimal()

missed_inc <- result |>
  filter((is_final_inc20 | is_ai_missed) & row_number > last_target_row) |>
  arrange(row_number) |>
  select(eppi_id, title, abstract, row_number, is_final_inc20, is_ai_missed) |>
  as.data.frame()

sample_references(
  friends_data,
  relevant_col = c("human_code", "decision_binary"),
  c_target = 0.90,
  R_c = 0.90,
  message = TRUE,
  id_col = "eppi_id",
)
