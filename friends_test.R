library(glmnet)
library(AIscreenR)
library(reticulate)
library(tidyverse)

use_python(
  "C:/Users/B199526/AppData/Local/miniconda3/envs/positron-python/python.exe",
  required = TRUE
)

py_config()
py_install("sentence-transformers", envname = "positron-python")

sentence_transformers <- import("sentence_transformers")
model <- sentence_transformers$SentenceTransformer("all-MiniLM-L6-v2")

# Load FRIENDS dataset 
dat <- readRDS("friends_full_screened_dat.rds")

set.seed(01062026)

records <- 
  dat |> 
  filter(decision_binary == 1) |> 
  sample_references(40) |> 
  bind_rows(
    dat |> 
      filter(decision_binary == 0) |> 
      sample_references(40)
  ) |> 
  slice_sample(prop = 1) |> 
  mutate(
    id = row_number()
  )

train_ids <- records$eppi_id

dat_unseen <- dat |> 
  filter(!eppi_id %in% train_ids)

dat_all_ordered <- 
  bind_rows(records, dat_unseen)


texts <- paste(dat_all_ordered$title, dat_all_ordered$abstract)

embeddings <- model$encode(texts)

#embeddings_r <- reticulate::py_to_r(embeddings)
dim(embeddings)  # check (should be 80 x 384)

# create training / unseen sets (R is 1-based)
x_train <- as.matrix(embeddings[1:80, , drop = FALSE])
y_train <- as.numeric(dat_all_ordered$decision_binary[1:80])
x_unseen <- as.matrix(embeddings[81:nrow(embeddings), , drop = FALSE])


fit <- cv.glmnet(x = x_train, y = y_train, family = "binomial", alpha = 1)

# predict probabilities
preds <- predict(fit, newx = x_unseen, s = "lambda.min", type = "response")

dat_all_ordered$priority_score <- NA_real_
dat_all_ordered$priority_score[81:nrow(dat_all_ordered)] <- as.numeric(preds)

# Rank unscreened records by priority
priority_list <- dat_all_ordered[81:nrow(dat_all_ordered), ]
priority_list <- priority_list[order(-priority_list$priority_score), ]

priority_list

final_included_ids <- 
  read_ris_to_dataframe("friends_final_included.ris") |> 
  mutate(
    across(c(author, title, abstract), ~ na_if(., ""))
  ) |> 
  filter_out(is.na(abstract)) |> 
  pull(eppi_id)


priority_list <- 
  priority_list |> 
  mutate(
    review_included = if_else(eppi_id %in% final_included_ids, 1, 0),
    row_number = row_number()
  )


dat_low_percent <- priority_list[-c(1:round(0.12 * nrow(priority_list))),]

priority_list |> 
  filter(review_included == 1) |> 
  slice_max(row_number, n = 1) |> 
  pull(row_number)/nrow(priority_list)


stopping_rule_list <- 
  priority_list |>
  arrange(row_number) |>
  mutate(
    # Mark runs of consecutive irrelevant (0) studies
    is_irrelevant = decision_binary == 0,
    run_id = cumsum(is_irrelevant != lag(is_irrelevant, default = first(is_irrelevant)))
  ) |>
  group_by(run_id) |>
  summarise(
    is_irrelevant_run = first(is_irrelevant),
    run_length        = n(),
    start_row         = min(row_number),
    end_row           = max(row_number)
  ) |>
  filter(is_irrelevant_run) |>
  mutate(pct_of_total = run_length / nrow(priority_list) * 100) |>
  select(-is_irrelevant_run) |>
  arrange(desc(run_length))

stopping_rule_list



total_relevant <- sum(priority_list$review_included == 1, na.rm = TRUE)

priority_list |>
  arrange(row_number) |>
  mutate(
    cumulative_relevant = cumsum(review_included == 1),
    recall = cumulative_relevant / total_relevant * 100
  ) |>
  ggplot(aes(x = row_number, y = recall)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = c(95, 100), linetype = "dashed", color = "tomato", alpha = 0.7) +
  annotate("text", x = max(priority_list$row_number) * 0.6, y = 96.5,
           label = "95% recall", color = "tomato", size = 3.5) +
  annotate("text", x = max(priority_list$row_number) * 0.6, y = 101.5,
           label = "100% recall", color = "tomato", size = 3.5) +
  labs(x = "Position in priority list (screening cutoff)",
       y = "Cumulative recall (%)",
       title = "Cumulative recall curve (all finally included refs found)") +
  theme_minimal()

total_relevant_AI_included <- sum(priority_list$decision_binary == 1, na.rm = TRUE)

priority_list |>
  arrange(row_number) |>
  mutate(
    cumulative_relevant = cumsum(decision_binary == 1),
    recall = cumulative_relevant / total_relevant_AI_included * 100
  ) |>
  ggplot(aes(x = row_number, y = recall)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = c(95, 100), linetype = "dashed", color = "tomato", alpha = 0.7) +
  annotate("text", x = max(priority_list$row_number) * 0.6, y = 96.5,
           label = "95% recall", color = "tomato", size = 3.5) +
  annotate("text", x = max(priority_list$row_number) * 0.6, y = 101.5,
           label = "100% recall", color = "tomato", size = 3.5) +
  labs(x = "Position in priority list (screening cutoff)",
       y = "Cumulative recall (%)",
       title = "Cumulative recall curve (all AI included refs found)") +
  theme_minimal()

total_relevant_human_included <- sum(priority_list$human_code == 1, na.rm = TRUE)

priority_list |>
  arrange(row_number) |>
  mutate(
    cumulative_relevant = cumsum(human_code == 1),
    recall = cumulative_relevant / total_relevant_human_included * 100
  ) |>
  ggplot(aes(x = row_number, y = recall)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = c(95, 100), linetype = "dashed", color = "tomato", alpha = 0.7) +
  annotate("text", x = max(priority_list$row_number) * 0.6, y = 96.5,
           label = "95% recall", color = "tomato", size = 3.5) +
  annotate("text", x = max(priority_list$row_number) * 0.6, y = 101.5,
           label = "100% recall", color = "tomato", size = 3.5) +
  labs(x = "Position in priority list (screening cutoff)",
       y = "Cumulative recall (%)",
       title = "Cumulative recall curve (all human included refs found)") +
  theme_minimal()

#-----------------------------------------------------------------------------------------

# Using all records for training 
set.seed(01062026)

records_all <- 
  dat |> 
  filter(decision_binary == 1) |> 
  bind_rows(
    dat |> 
      filter(decision_binary == 0) |> 
      sample_references(100)
  ) |> 
  slice_sample(prop = 1) |> 
  mutate(
    id = row_number()
  )

train_ids_all <- records_all$eppi_id

dat_unseen_wo_all <- dat |> 
  filter(!eppi_id %in% train_ids_all)

dat_all_ordered_all <- 
  bind_rows(records_all, dat_unseen_wo_all)


texts_all <- paste(dat_all_ordered_all$title, dat_all_ordered_all$abstract)

embeddings_all <- model$encode(texts_all)

#embeddings_r <- reticulate::py_to_r(embeddings)
dim(embeddings_all) 

# create training / unseen sets (R is 1-based)
x_train_all <- as.matrix(embeddings_all[1:219, , drop = FALSE])
y_train_all <- as.numeric(dat_all_ordered_all$decision_binary[1:219])
x_unseen_all <- as.matrix(embeddings_all[220:nrow(embeddings_all), , drop = FALSE])


fit_all <- cv.glmnet(x = x_train_all, y = y_train_all, family = "binomial", alpha = 1)

# predict probabilities
preds_all <- predict(fit_all, newx = x_unseen_all, s = "lambda.min", type = "response")

dat_all_ordered_all$priority_score <- NA_real_
dat_all_ordered_all$priority_score[220:nrow(dat_all_ordered_all)] <- as.numeric(preds_all)

# Rank unscreened records by priority
priority_list_all <- dat_all_ordered_all[220:nrow(dat_all_ordered_all), ]
priority_list_all <- priority_list_all[order(-priority_list_all$priority_score), ]

priority_list_all <- 
  priority_list_all |> 
  mutate(
    review_included = if_else(eppi_id %in% final_included_ids, 1, 0),
    row_number = row_number()
  )

total_relevant_human <- sum(priority_list_all$human_code == 1, na.rm = TRUE)

priority_list_all |>
  arrange(row_number) |>
  mutate(
    cumulative_relevant = cumsum(human_code == 1),
    recall = cumulative_relevant / total_relevant_human * 100
  ) |>
  ggplot(aes(x = row_number, y = recall)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = c(95, 100), linetype = "dashed", color = "tomato", alpha = 0.7) +
  annotate("text", x = max(priority_list$row_number) * 0.6, y = 96.5,
           label = "95% recall", color = "tomato", size = 3.5) +
  annotate("text", x = max(priority_list$row_number) * 0.6, y = 101.5,
           label = "100% recall", color = "tomato", size = 3.5) +
  labs(x = "Position in priority list (screening cutoff)",
       y = "Cumulative recall (%)",
       title = "Cumulative recall curve") +
  theme_minimal()
