library(dplyr)
library(AIscreenR)
library(future)

#--------------------------------------------------------------------------
# Prompt: exactly the one used in the first run
#--------------------------------------------------------------------------
old <- readRDS("friends/data/full_screening_w_humans_FRIENDS_2.rds")
prompt <- old$answer_data$prompt[1]

#--------------------------------------------------------------------------
# Data: the cleaned data already holds the abstracts that were found online
#--------------------------------------------------------------------------
friends_data <- readRDS("friends/data/friends_FRIENDS_2_cleaned.rds")

# All records without an abstract are removed before screening, including finally included studies
no_abstract <- is.na(friends_data$abstract) |
  trimws(friends_data$abstract) == "" |
  friends_data$abstract == "No information"

to_remove <- no_abstract
removed_ids <- friends_data$eppi_id[to_remove]
included_final_ids <- friends_data$eppi_id[no_abstract & friends_data$included_final == 1] # finally included, removed too

cat("Records in data:                               ", nrow(friends_data), "\n")
cat("Records without abstract (all removed):        ", sum(to_remove), "\n")
cat("  of which finally included:                   ", length(included_final_ids), "\n")

screening_data <- friends_data[!to_remove, ] |>
  mutate(human_code = as.numeric(human_code)) |>
  select(-prompt, -model, -iterations, -req_per_min, -question, -reasoning_effort, -verbosity, -topp, -decision_gpt, -decision_binary, -prompt_tokens, -completion_tokens, -submodel, -run_time, -run_date, -n)

cat("Records to screen:                             ", nrow(screening_data), "\n")

#--------------------------------------------------------------------------
# Screening
#--------------------------------------------------------------------------
plan(multisession)

res_all <- AIscreenR::tabscreen_gpt(
  data = screening_data,
  prompt = prompt,
  studyid = eppi_id,
  title = title,
  abstract = abstract,
  model = "gpt-6-luna",
  reps = 1,
  reasoning_effort = "medium",
  verbosity = "low",
  decision_description = FALSE
)

plan(sequential)

AIscreenR::screen_analyzer(res_all)

# Confusion matrix: how many records were included/excluded by the AI vs. the human decision
table(res_all$answer_data$decision_binary, res_all$answer_data$human_code)

table(res_all$answer_data$decision_binary, res_all$answer_data$included_final)

false_negatives <- res_all$answer_data |>
  filter(decision_binary == 0 & human_code == 1)

# Print decision_desciption and abstract of the false negatives (records that were finally included by the human but excluded by the AI)
if (nrow(false_negatives) > 0) {
  cat("False negatives (AI excluded, human included):\n")
  for (i in seq_len(nrow(false_negatives))) {
    cat("eppi_id:", false_negatives$eppi_id[i], "\n")
    cat("detailed_description:", false_negatives$detailed_description[i], "\n")
    cat("Abstract:", false_negatives$abstract[i], "\n\n")
    cat("Finally included:", false_negatives$included_final[i], "\n\n")
    cat("--------------------------------------------------\n\n")
  }
} else {
  cat("No false negatives (AI excluded, human included)\n")
}

#--------------------------------------------------------------------------
# Save (new files, the FRIENDS_2 files are not overwritten)
#--------------------------------------------------------------------------
saveRDS(res_all, "friends/data/screening_FRIENDS_3_ai.rds")
saveRDS(removed_ids, "friends/data/screening_FRIENDS_3_removed_no_abstract_ids.rds")

#--------------------------------------------------------------------------
# Check that new data is sorted the same way as the old data (FRIENDS_2)
#--------------------------------------------------------------------------
# print titles of the first 10 records in the new data
cat("First 10 titles in the new data (FRIENDS_3):\n")
print(res_all$answer_data$title[1:10])

# print titles of the first 10 records in the old data (FRIENDS_2)
cat("First 10 titles in the old data (FRIENDS_2):\n")
print(friends_data$title[1:10])

# print titles of the last 10 records in the new data
cat("Last 10 titles in the new data (FRIENDS_3):\n")
print(tail(res_all$answer_data$title, 10))

# print titles of the last 10 records in the old data
cat("Last 10 titles in the old data (FRIENDS_2):\n")
print(tail(friends_data$title, 10))

#--------------------------------------------------------------------------
# Prepare the data for the simulation
#--------------------------------------------------------------------------
# answer_data already holds the human columns, included_final and the new AI decisions for every screened record.
friends_data_new <- res_all$answer_data |>
  mutate(
    human_code      = as.numeric(human_code),
    decision_binary = as.numeric(decision_binary),
    included_final  = as.integer(included_final),
    human_and_ai_in = as.integer(decision_binary == 1 & human_code == 1) # included by both the AI and the humans
  )

# Checks: unique ids, no missing decisions, every record has a row in an existing embedding file
stopifnot(
  !anyDuplicated(friends_data_new$eppi_id),
  !anyNA(friends_data_new$decision_binary),
  !anyNA(friends_data_new$human_code),
  !anyNA(friends_data_new$included_final)
)
emb_file <- "simulation/embeddings/friends_data_tfidf.rds"
if (file.exists(emb_file)) {
  stopifnot(all(as.character(friends_data_new$eppi_id) %in% rownames(readRDS(emb_file))))
}

cat("Prepared data:", nrow(friends_data_new), "records |",
    "AI included:", sum(friends_data_new$decision_binary == 1),
    "| human included:", sum(friends_data_new$human_code == 1),
    "| human and AI:", sum(friends_data_new$human_and_ai_in == 1),
    "| finally included:", sum(friends_data_new$included_final == 1), "\n")

# The simulation finds the embedding files through this attribute ("friends_data" = the name the existing files were built under)
attr(friends_data_new, "data_name") <- "friends_data"
saveRDS(friends_data_new, "friends/data/friends_FRIENDS_3_cleaned.rds")
