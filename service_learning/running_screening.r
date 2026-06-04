# Running the screening process for the systematic review on Service Learning interventions

# Load necessary libraries
library(dplyr)
library(readr)
library(stringr)
library(AIscreenR)
library(future)
library(glmnet)
library(reticulate)
library(tidyverse)

prompt <- "We are screening titles and abstracts of studies for a systematic review about the effects of service learning interventions on 
academic success, neither employed, nor in education or training (NEET) status post compulsory school, personal and social skills, 
and risk behaviour of students in primary and secondary education. INCLUDE if ALL are true:

A) The study focuses on service learning  

B) The participants are children in grades kindergarten to 12 (or the equivalent in European countries) 

C) Is the report/article a quantitative evaluation study 
"

# Load the RIS file with human annotations
# human_code for title and abstract screening: 1 for included, 0 for excluded
# included_full for full text screening: 1 for included, 0 for excluded.
#ris_path <- "service_learning/data/combined_studies.ris"
#filges2022_full_data <- read_ris_to_dataframe(ris_path)
#
#saveRDS(filges2022_full_data, "service_learning/data/filges2022_full_data.rds")

filges2022_full_data <- 
  readRDS("service_learning/data/filges2022_full_data.rds") |>
  slice(1, .by = eppi_id) |> # Remove duplicates, keeping the first occurrence of each eppi_id
  select(eppi_id, title, abstract, human_code, included_full) |> 
  filter_out(abstract == "") # Remove rows with missing abstracts

# To conduct the screening we will start by evaluating the prompt on a small sample of studies. 
# We will use 150 studies for the first round of screening. 100 with human code 0 and 50 with human code 1.
# We need at least 2 of the 50 studies with human code 1 to have an included_full code of 1. These are studies that we know should be included in the final report.

set.seed(04062026) # For reproducibility

# Sample 48 studies with human_code == 1, and 2 with included_full == 1, and 100 studies with human_code == 0
sample_included <- filges2022_full_data |>
  filter(human_code == 1) |>
  sample_n(48)

sample_included_full <- filges2022_full_data |>
  filter(included_full == 1) |>
  sample_n(5)

sample_excluded <- filges2022_full_data |>
  filter(human_code == 0) |>
  sample_n(100)

screening_sample <- 
  bind_rows(sample_included, sample_included_full, sample_excluded) |> 
  slice(1, .by = eppi_id) # Remove duplicates, keeping the first occurrence of each eppi_id

# Run the screening
plan(multisession)
results_sample <-
    AIscreenR::tabscreen_gpt.tools(
        data = screening_sample,
        prompt = prompt,
        studyid = eppi_id,
        title = title,
        abstract = abstract,
        model = "gpt-5.1",
        decision_description = FALSE,
        overinclusive = TRUE,
    )
plan(sequential)

#save(results_sample, file = "service_learning/screening_results_sample_with_descriptions.rdata")

# Check the results
AIscreenR::screen_analyzer(results_sample)

#save(results_sample, file = "service_learning/screening_test_results.rdata")
load("service_learning/screening_test_results.rdata")
test_ids <- results_sample$answer_data$eppi_id

#report(
#  results_sample$answer_data,
#  studyid = eppi_id,
#  title = title,
#  abstract = abstract,
#  gpt_answer = detailed_description,
#  human_code = human_code,
#  final_decision_gpt_num = decision_binary,
#  file = "SL-included_descriptions2",
#  format = "docx",
#  document_title = "Service Learning Screening Results - Test2"
#)

results_sample$answer_data |> filter(included_full == 1 & decision_binary == 0) 

# Print false negatives
answer_data <- results_sample$answer_data
false_negatives <- answer_data |>
  filter(decision_binary == 0 & human_code == 1)

for (i in 1:nrow(false_negatives)) {
  cat("Abstract:", false_negatives$abstract[i], "\n")
  cat("Decision Binary:", false_negatives$decision_binary[i], "\n")
  cat("Human Code:", false_negatives$human_code[i], "\n")
  cat("------\n")
}

# Print false positives
false_positives <- answer_data |>
  filter(decision_binary == 1 & human_code == 0)

for (i in 1:nrow(false_positives)) {
  cat("Abstract:", false_positives$abstract[i], "\n")
  cat("Decision Binary:", false_positives$decision_binary[i], "\n")
  cat("Human Code:", false_positives$human_code[i], "\n")
  cat("------\n")
}


#---------------------------------------------------------------------------------------------------------
# FULL SCREENING

plan(multisession)
results_all <-
    AIscreenR::tabscreen_gpt.tools(
        data = filges2022_full_data, # For testing, we will only screen the first 10 and the last 5 studies. We will screen all studies in the next step.
        prompt = prompt,
        studyid = eppi_id,
        title = title,
        abstract = abstract,
        model = c("gpt-4o-mini", "gpt-5.1"),
        decision_description = FALSE,
        overinclusive = TRUE,
        force = TRUE
    )
plan(sequential)

save(results_all, file = "service_learning/screening_results_all.RData")

# Check the results
AIscreenR::screen_analyzer(results_all)

results_all$answer_data |> filter(model == "gpt-5.1") |> filter(included_full == 1 & decision_binary == 0) |> View()

#---------------------------------------------------------------------------------------------------------
# PRIORITY SCREENING

use_python(
  "C:/Users/B199526/AppData/Local/miniconda3/envs/positron-python/python.exe",
  required = TRUE
)

py_config()
#py_install("sentence-transformers", envname = "positron-python")

sentence_transformers <- import("sentence_transformers")
model <- sentence_transformers$SentenceTransformer("all-MiniLM-L6-v2")

load("service_learning/screening_results_all.RData")

filges2022_full_data_screened <- 
  results_all$answer_data |> 
  filter(model == "gpt-5.1") |> 
  mutate(
    across(human_code:included_full, as.numeric)
  ) 

set.seed(04062026)

filges2022_full_data_screened_wo_test <- 
  filges2022_full_data_screened |> 
  filter(!eppi_id %in% test_ids)

test_data <- 
  filges2022_full_data_screened |> 
  filter(eppi_id %in% test_ids)

records <- 
  filges2022_full_data_screened_wo_test |> 
  filter(decision_binary == 1) |>
  bind_rows(
    filges2022_full_data_screened_wo_test |> 
      filter(decision_binary == 0) |> 
      sample_references(100)
  ) |> 
  slice_sample(prop = 1) |>
  mutate(
    id = row_number()
  ) 


train_ids <- records$eppi_id

dat_unseen <- 
  filges2022_full_data_screened_wo_test |> 
  filter(!eppi_id %in% train_ids) |> 
  bind_rows(test_data) 

dat_all_ordered <- 
  bind_rows(records, dat_unseen)


texts <- paste(dat_all_ordered$title, dat_all_ordered$abstract)

embeddings <- model$encode(texts)

#embeddings_r <- reticulate::py_to_r(embeddings)
dim(embeddings)  

# create training / unseen sets (R is 1-based)
x_train <- as.matrix(embeddings[1:nrow(records), , drop = FALSE])
y_train <- as.numeric(dat_all_ordered$decision_binary[1:nrow(records)])
x_unseen <- as.matrix(embeddings[(nrow(records) + 1):nrow(embeddings), , drop = FALSE])


fit <- cv.glmnet(x = x_train, y = y_train, family = "binomial", alpha = 1)

# predict probabilities
preds <- predict(fit, newx = x_unseen, s = "lambda.min", type = "response")

dat_all_ordered$priority_score <- NA_real_
dat_all_ordered$priority_score[(nrow(records) + 1):nrow(dat_all_ordered)] <- as.numeric(preds)

# Rank unscreened records by priority
priority_list <- dat_all_ordered[(nrow(records) + 1):nrow(dat_all_ordered), ]
priority_list <- priority_list[order(-priority_list$priority_score), ]

priority_list <- 
  priority_list |> 
  mutate(
    row_number = row_number()
  )

priority_list

priority_list |> filter(included_full == 1) |> pull(row_number)
1395/5816

stopping_rule1_list <- 
  priority_list |>
  arrange(row_number) |>
  mutate(
    # Mark runs of consecutive irrelevant (0) studies
    is_irrelevant = included_full == 0,
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

stopping_rule1_list

stopping_rule2_list <- 
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

stopping_rule2_list

total_relevant <- sum(priority_list$included_full == 1, na.rm = TRUE)

priority_list |>
  arrange(row_number) |>
  mutate(
    cumulative_relevant = cumsum(included_full == 1),
    recall = cumulative_relevant / total_relevant * 100,
    perc_of_total = row_number / nrow(priority_list) * 100
  ) |>
  ggplot(aes(x = perc_of_total, y = recall)) +
  geom_hline(yintercept = c(95), linetype = "dashed", color = "black", alpha = 0.7) +
  geom_line(color = "steelblue") +
  annotate("text", x = 50, y = 96.5,
           label = "95% recall", color = "black", size = 3.5) +
  annotate("text", x = 50, y = 101.5,
           label = "100% recall", color = "black", size = 3.5) +
  labs(x = "Percent needed to be screened (%)",
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

