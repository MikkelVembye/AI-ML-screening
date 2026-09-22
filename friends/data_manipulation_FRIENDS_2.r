# Prepare the FRIENDS_2 screening data (friends/data/full_screening_w_humans_FRIENDS_2.rds) for the
# priority-screening simulation, mirroring the role data_manipulation.r plays for the original dataset.
library(dplyr)
library(AIscreenR)
library(CiteSource)
library(stringr)

raw <- readRDS("friends/data/full_screening_w_humans_FRIENDS_2.rds")

# `answer_data` carries a stray `title`/`title.y` (NA except for finally-included rows) and
# `finally_included` from an earlier join attempt; `title.x` is the real title/abstract-stage title.
friends_data <- raw$answer_data |>
  select(-any_of(c("title", "title.y", "finally_included"))) |>
  rename(title = title.x, human_code = final_human_decision)

# Final-included studies list (post Barrett-2006 removal, see friends_final_included.ris).
final_inc <- AIscreenR::read_ris_to_dataframe("friends_final_included.ris")

final_inc_matched <- final_inc |>
  select(eppi_id, title_fd = title, abstract_fd = abstract) |>
  left_join(
    friends_data |> select(eppi_id, decision_binary, human_code, title, abstract),
    by = "eppi_id"
  ) |>
  mutate(
    title    = coalesce(title, title_fd),
    abstract = coalesce(abstract, abstract_fd)
  ) |>
  select(-title_fd, -abstract_fd)

# Artificially add any finally included studies that were not screened by AI (decision_binary = NA)
# and assign them decision_binary = 1 and human_code = 1, so they are treated as relevant records
# in the priority screening process.
studies_missing_from_friends_data <- final_inc_matched |>
  filter(is.na(decision_binary)) |>
  mutate(decision_binary = 1, human_code = 1)

friends_data <- friends_data |>
  bind_rows(studies_missing_from_friends_data |> filter(!eppi_id %in% friends_data$eppi_id)) |>
  mutate(
    included_final  = as.integer(eppi_id %in% final_inc$eppi_id),
    human_and_ai_in = as.integer(decision_binary == 1L & human_code == 1L)
  )

# Deduplicate the dataset to ensure unique records (by eppi_id).
friends_data <- CiteSource::dedup_citations(friends_data) |>
  # Drop "duplicate_id"
  select(-duplicate_id)

attr(friends_data, "data_name") <- "friends_data"

#saveRDS(friends_data, "friends/data/friends_FRIENDS_2_cleaned.rds")
