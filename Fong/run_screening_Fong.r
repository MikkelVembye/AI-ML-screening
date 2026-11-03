library(dplyr)
library(AIscreenR)
library(future)
library(CiteSource)

#--------------------------------------------------------------------------
# Prompt
#--------------------------------------------------------------------------
prompt <- ""

#--------------------------------------------------------------------------
# Data
#--------------------------------------------------------------------------
raw <- read.csv("Fong/data/07_Fong_data.csv", stringsAsFactors = FALSE)

fong_raw <- raw |>
  transmute(
    id             = as.character(key),
    title          = title,
    abstract       = abstract,
    author         = authors,
    year           = year,
    human_code     = as.numeric(label_included),
    included_final = as.integer(included_final)
  )

unique_citations <- CiteSource::dedup_citations(fong_raw, manual = TRUE)$unique

# If a record has included_final = 1, then human_code must be 1 (included). If not, set human_code to 1.
unique_citations <- unique_citations |> mutate(human_code = ifelse(included_final == 1, 1, human_code))

stopifnot(!anyDuplicated(unique_citations$id), !anyNA(unique_citations$human_code), !anyNA(unique_citations$included_final))

# All records without an abstract are removed before screening, including finally included studies
no_abstract <- is.na(unique_citations$abstract) | trimws(unique_citations$abstract) == "" | unique_citations$abstract == "No information"
removed_ids <- unique_citations$id[no_abstract]
included_final_ids <- unique_citations$id[no_abstract & unique_citations$included_final == 1] # finally included, removed too

cat("Records in data:                               ", nrow(fong_raw), "\n")
cat("Records without abstract (all removed):        ", sum(no_abstract), "\n")
cat("  of which finally included:                   ", length(included_final_ids), "\n")

screening_data <- unique_citations[!no_abstract, ]
cat("Records to screen:                             ", nrow(screening_data), "\n")

#--------------------------------------------------------------------------
# Small test set
#--------------------------------------------------------------------------
# 50 human in + 50 human out (none of them finally included) + all finally included studies
set.seed(123)
test_data <- bind_rows(
  screening_data |> filter(included_final == 1),
  screening_data |> filter(included_final == 0, human_code == 1) |> slice_sample(n = 50),
  screening_data |> filter(included_final == 0, human_code == 0) |> slice_sample(n = 50)
)
table(human_code = test_data$human_code, included_final = test_data$included_final)

plan(multisession)

res_test <- AIscreenR::tabscreen_gpt(
  data = test_data,
  prompt = prompt,
  studyid = id,
  title = title,
  abstract = abstract,
  model = "gpt-6-luna",
  reps = 1,
  reasoning_effort = "medium",
  verbosity = "low",
  decision_description = TRUE
)

plan(sequential)

AIscreenR::screen_analyzer(res_test)
table(AI = res_test$answer_data$decision_binary, human = res_test$answer_data$human_code)
table(AI = res_test$answer_data$decision_binary, final = res_test$answer_data$included_final)

# Finally included studies the AI excluded, with its explanation
res_test$answer_data |>
  filter(decision_binary == 0, included_final == 1) |>
  select(id, title, detailed_description)

# Human excluded studies the AI included, with its explanation
res_test$answer_data |>
  filter(decision_binary == 1, human_code == 0) |>
  select(id, title, detailed_description)

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
  reasoning_effort = "medium",
  verbosity = "low",
  decision_description = FALSE
)

plan(sequential)

AIscreenR::screen_analyzer(res_all)

table(res_all$answer_data$decision_binary, res_all$answer_data$human_code)
table(res_all$answer_data$decision_binary, res_all$answer_data$included_final)

saveRDS(res_all, "Fong/data/screening_Fong_ai.rds")

#--------------------------------------------------------------------------
# Prepare the data for the simulation
#--------------------------------------------------------------------------
fong_data <- res_all$answer_data |>
  mutate(
    id              = as.character(id),
    human_code      = as.numeric(human_code),
    decision_binary = as.numeric(decision_binary),
    included_final  = as.integer(included_final),
    human_and_ai_in = as.integer(decision_binary == 1 & human_code == 1) # included by both the AI and the humans
  )

cat("Prepared data:", nrow(fong_data), "records |",
    "AI included:", sum(fong_data$decision_binary == 1),
    "| human included:", sum(fong_data$human_code == 1),
    "| human and AI:", sum(fong_data$human_and_ai_in == 1),
    "| finally included:", sum(fong_data$included_final == 1), "\n")

attr(fong_data, "data_name") <- "fong_data"
saveRDS(fong_data, "Fong/data/fong_cleaned.rds")
