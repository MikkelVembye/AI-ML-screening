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
  filter(!is.na(abstract))

saveRDS(friends_data, "friends/data/friends_cleaned.rds")