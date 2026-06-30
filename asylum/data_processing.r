# Load necessary libraries
library(dplyr)
library(readr)
library(stringr)
library(AIscreenR)
library(readxl)
library(CiteSource)

#----------------------------------------------------------------------------------
# Check for duplicates using citesource
#----------------------------------------------------------------------------------

# Read with AIscreenR and attach labels directly
included_studies <-
  AIscreenR::read_ris_to_dataframe("asylum/data/asylum_incl.ris") |>
  mutate(cite_source = "search", cite_label = "included", human_code = 1)

excluded_studies <-
  bind_rows(
    AIscreenR::read_ris_to_dataframe("asylum/data/asylum_excl1.ris"),
    AIscreenR::read_ris_to_dataframe("asylum/data/asylum_excl2.ris"),
    AIscreenR::read_ris_to_dataframe("asylum/data/asylum_excl3.ris")
  ) |>
  mutate(cite_source = "search", cite_label = "excluded", human_code = 0)

all_dat_with_labels <- bind_rows(included_studies, excluded_studies) |>
  as_tibble() |>
  mutate(
    across(where(is.character), ~ na_if(.x, "")),
    record_id = row_number()
  )

# Deduplicate
unique_citations_obj <- CiteSource::dedup_citations(all_dat_with_labels, manual = TRUE)
unique_citations     <- unique_citations_obj$unique

# Load benchmark studies
benchmark_studies <- AIscreenR::read_ris_to_dataframe("asylum/data/included_from_review.ris") |>
  mutate(cite_source = "benchmark", cite_label = "benchmark", human_code = 1)

# Tag benchmark studies in unique_citations by eppi_id
benchmark_eppi_ids <- benchmark_studies |>
  filter(eppi_id != "") |>
  pull(eppi_id)

# Studies not in eppi
non_eppi_studies <- benchmark_studies |>
  filter(eppi_id == "")

unique_citations <- unique_citations |>
  mutate(cite_label = if_else(eppi_id %in% benchmark_eppi_ids, "benchmark", cite_label))
unique(unique_citations$cite_label)

# If study is both included and excluded set human_code to 1 (included)
combinations <- c("excluded, included", 
                "included, excluded", 
                "excluded, excluded, included")

if (any(unique_citations$cite_label %in% combinations)) {
    print("Some studies are both included and excluded. Setting human_code to 1 for these studies.")
    cat("Studies with both included and excluded labels:\n")
    print(unique_citations |>
            filter(cite_label %in% combinations) |>
            select(cite_label, human_code))
  unique_citations <- unique_citations |>
    mutate(human_code = if_else(cite_label %in% combinations, 1, as.numeric(human_code)))
}

# Add benchmark studies that are not in eppi to unique_citations
unique_citations <- unique_citations |>
  mutate(year = as.integer(year))

unique_citations <- bind_rows(unique_citations, non_eppi_studies) |>
  # random shuffle the rows
  slice_sample(n = nrow(unique_citations))

# Save the unique citations to an RDS file
saveRDS(unique_citations, "asylum/data/all_studies_with_labels.rds")