# Load data and manipulate format for further analysis
library(dplyr)
library(readr)
library(stringr)
library(tidyverse)
library(CiteSource)

set.seed(123)

# Read data from friends\data\friends_screening_full.RData
load("friends/data/friends_screening_full.RData")
friends_data <- result_obj$answer_data

# Step 1: Deduplicate all candidate records in P.
friends_data <- CiteSource::dedup_citations(friends_data) |>
  # Drop "duplicate_id"
  select(-duplicate_id)

# Step 2: Remove records without abstracts for manual human screening.
friends_data <- friends_data |>
  mutate(year = as.integer(year))

# Combine the final included studies with the AI-screened dataset.
final_inc_studies <- AIscreenR::read_ris_to_dataframe("friends_final_included.ris")|>
  # if abstract is empty, NA, or "No information", remove the record
  mutate(abstract = ifelse(str_trim(abstract) == "" | is.na(abstract) | str_detect(abstract, "No information"), NA, abstract)) |>
  filter(!is.na(abstract))

final_inc_studies <- final_inc_studies |>
  select(-any_of(c("decision_binary", "human_code"))) |>
  left_join(
    friends_data |> select(eppi_id, decision_binary, human_code, title_fd = title, abstract_fd = abstract),
    by = "eppi_id"
  ) |>
  mutate(
    title    = coalesce(title_fd, title),
    abstract = coalesce(abstract_fd, abstract)
  ) |>
  select(-title_fd, -abstract_fd)
final_inc_studies |> filter(is.na(abstract) | abstract == "") |> select(eppi_id, title)

# Artificially add any finally included studies that were not screened by AI 
# (decision_binary = NA) and assign them a decision_binary = 1 and human_code = 1, 
# so they are treated as relevant records in the priority screening process.
studies_missing_from_friends_data <- final_inc_studies |>
  filter(is.na(decision_binary)) |>
  mutate(decision_binary = "1", human_code = "1")

final_inc_studies <- final_inc_studies |>
  filter(!is.na(decision_binary)) |>
  bind_rows(studies_missing_from_friends_data)

friends_data <- friends_data |>
  bind_rows(studies_missing_from_friends_data |> filter(!eppi_id %in% friends_data$eppi_id)) |>
  mutate(included_final = if_else(eppi_id %in% final_inc_studies$eppi_id, 1, 0))

# Add a column to indicate whether the record is a included_final study or not
friends_data <- 
  friends_data |>
  mutate(
    included_final = as.integer(eppi_id %in% final_inc_studies$eppi_id),
    human_and_ai_in = as.integer(decision_binary == 1L & human_code == 1L),
    across(
      c(decision_binary, human_code, eppi_id, promptid, prompt_tokens, topp,
        n, completion_tokens, run_time, req_per_min, iterations),
      as.numeric
    )
  ) |> 
  select(-c(issue:source))

attr(friends_data, "data_name") <- "friends"

saveRDS(friends_data, "friends/data/friends_cleaned.rds")
