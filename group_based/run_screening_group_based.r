library(dplyr)
library(AIscreenR)
library(future)

#--------------------------------------------------------------------------
# Prompt
#--------------------------------------------------------------------------
# Based on the Campbell protocol in the group_based folder (Dalgaard et al., 2022)
prompt <- ""


#--------------------------------------------------------------------------
# Data
#--------------------------------------------------------------------------
# Duplicates and studies without abstract are already removed in "combine data.R"
raw <- readRDS("group_based/data/group_based_data.rds")

# id is the eppi_id (the studies added by hand have a fake one, made in "combine data.R")
screening_data <- raw |>
  transmute(
    id             = eppi_id,
    title          = title,
    abstract       = abstract,
    author         = author,
    year           = as.integer(year),
    human_code     = as.numeric(human_code),
    included_final = as.integer(included_final)
  )

stopifnot(!anyDuplicated(screening_data$id), !anyNA(screening_data$id), !anyNA(screening_data$human_code), !anyNA(screening_data$included_final))

cat("Records to screen:                             ", nrow(screening_data), "\n")
cat("  human included:                              ", sum(screening_data$human_code == 1), "\n")
cat("  finally included:                            ", sum(screening_data$included_final == 1), "\n")


#--------------------------------------------------------------------------
# Screening
#--------------------------------------------------------------------------
plan(multisession)

res_all <- AIscreenR::tabscreen_gpt(
  data = screening_data,
  prompt = prompt,
  studyid = id,
  title = title,
  abstract = abstract,
  model = "gpt-6-luna",
  reps = 1,
  reasoning_effort = "high",
  verbosity = "low",
  confidence = TRUE,
  overinclusive = FALSE,
  decision_description = FALSE
)

plan(sequential)

AIscreenR::screen_analyzer(res_all)

table(AI = res_all$answer_data$decision_binary, human = res_all$answer_data$human_code)

table(AI = res_all$answer_data$decision_binary, final = res_all$answer_data$included_final)

# False negatives: included by humans but excluded by AI
false_negatives <- res_all$answer_data |>
  filter(human_code == 1 & decision_binary == 0) |>
  select(id, title, abstract, human_code, decision_binary, included_final)

cat("False negatives (human in, AI out):", nrow(false_negatives),
    "| of which finally included:", sum(false_negatives$included_final == 1), "\n")

# False positives: excluded by humans but included by AI
false_positives <- res_all$answer_data |>
  filter(human_code == 0 & decision_binary == 1) |>
  select(id, title, abstract, human_code, decision_binary, included_final)

for (i in seq_len(nrow(false_positives))) {
  cat("ID:", false_positives$id[i], "\n")
  cat("Title:", false_positives$title[i], "\n")
  # Print only the first 300 characters of the abstract, to avoid cluttering the console with long abstracts
  cat("Abstract:", substr(false_positives$abstract[i], 1, 300), ifelse(nchar(false_positives$abstract[i]) > 300, "...", ""), "\n")
  cat("Human code:", false_positives$human_code[i], "| AI code:", false_positives$decision_binary[i], "| Finally included:", false_positives$included_final[i], "\n\n")
}

saveRDS(res_all, "group_based/data/screening_group_based_ai.rds")


#--------------------------------------------------------------------------
# Prepare the data for the simulation
#--------------------------------------------------------------------------
group_based_data <- res_all$answer_data |>
  mutate(
    id              = as.character(id),
    human_code      = as.numeric(human_code),
    decision_binary = as.numeric(decision_binary),
    included_final  = as.integer(included_final),
    human_and_ai_in = as.integer(decision_binary == 1 & human_code == 1) # included by both the AI and the humans
  )

stopifnot(!anyDuplicated(group_based_data$id), !anyNA(group_based_data$decision_binary))

cat("Prepared data:", nrow(group_based_data), "records |",
    "AI included:", sum(group_based_data$decision_binary == 1),
    "| human included:", sum(group_based_data$human_code == 1),
    "| human and AI:", sum(group_based_data$human_and_ai_in == 1),
    "| finally included:", sum(group_based_data$included_final == 1), "\n")

attr(group_based_data, "data_name") <- "group_based_data"
saveRDS(group_based_data, "group_based/data/group_based_cleaned.rds")
