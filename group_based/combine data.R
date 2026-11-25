library(dplyr)
library(AIscreenR)

#--------------------------------------------------------------------------
# Read the ris files from screen_benchmarks
#--------------------------------------------------------------------------
files <- c("group_based/data/group_based_excl1.ris",
           "group_based/data/group_based_excl2.ris",
           "group_based/data/group_based_excl3.ris",
           "group_based/data/group_based_excl4.ris",
           "group_based/data/group_based_excl5.ris",
           "group_based/data/group_based_incl.ris")

# human_code is 0 for the excl files and 1 for the incl file
excl <- lapply(files[grepl("excl", files)], read_ris_to_dataframe) |>
  bind_rows() |>
  mutate(human_code = 0)

incl <- read_ris_to_dataframe(files[grepl("incl", files)]) |>
  mutate(human_code = 1)

raw_group_based_data <- bind_rows(excl, incl)


#--------------------------------------------------------------------------
# Finally included studies
# Match the titles and dois of the final included studies to the screening data to set included_final = 1 for those studies
#--------------------------------------------------------------------------------------------------
# Load the 62 finally included studies. 
# Made by hand from the preprint
final_included <- read_ris_to_dataframe("group_based/data/final_inc_from_review.ris")

# Prepare the titles and dois of the final included studies for matching
final_titles <- substr(tolower(gsub("[^A-Za-z0-9]", "", final_included$title)), 1, 80)
final_dois <- tolower(final_included$doi[which(final_included$doi != "")])

# included_final is 1 if the title or the doi is the same as a final study
group_based_data <- raw_group_based_data |>
  mutate(title_key = substr(tolower(gsub("[^A-Za-z0-9]", "", title)), 1, 80),
         doi_key = sub(".*doi.org/", "", tolower(doi)),
         included_final = as.integer(title_key %in% final_titles | doi_key %in% final_dois),
         human_code = ifelse(included_final == 1, 1, human_code)) |>
  select(-title_key, -doi_key)


#--------------------------------------------------------------------------
# Remove duplicates using CiteSource
#--------------------------------------------------------------------------

# record_id is the row number
all_studies <- group_based_data |> mutate(record_id = as.character(row_number()))

unique_studies <- CiteSource::dedup_citations(all_studies, manual = TRUE)$unique |>
  mutate(included_final = as.integer(included_final))

# CiteSource only keeps the label of one of the duplicates
# human_code is therefore set to the highest human_code of the duplicates (record_ids)
human_codes <- unique_studies |>
  select(duplicate_id, record_ids) |>
  tidyr::separate_rows(record_ids, sep = ", ") |>
  left_join(select(all_studies, record_id, human_code), by = c("record_ids" = "record_id")) |>
  group_by(duplicate_id) |>
  summarise(human_code = max(human_code))

unique_studies <- unique_studies |>
  select(-human_code) |>
  left_join(human_codes, by = "duplicate_id")

# Bond 1991 is in the data twice, remove the copy with the cut-off title
unique_studies <- unique_studies |>
  filter(!title %in% "Assertive community treatment and reference groups: An")

# Duplicates with the same title that CiteSource did not find (case, punctuation and html escapes are ignored)
# One copy is kept, the one with the longest abstract. It gets the highest human_code and included_final of the copies
# Titles without any letters or numbers (e.g. in cyrillic) are not compared
unique_studies <- unique_studies |>
  mutate(row = row_number(),
         title_key = gsub("[^a-z0-9]", "", gsub("&apos;|&amp;|&quot;|&#39;", "", tolower(title))),
         title_key = ifelse(nzchar(title_key), title_key, paste0("no_title_", duplicate_id)),
         abstract_chars = nchar(coalesce(abstract, ""))) |>
  group_by(title_key) |>
  mutate(human_code = max(human_code), included_final = max(included_final)) |>
  arrange(desc(abstract_chars), row, .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  arrange(row) |>
  select(-row, -title_key, -abstract_chars)


#--------------------------------------------------------------------------
# Remove studies with no abstract
#--------------------------------------------------------------------------
group_based_data <- unique_studies |>
  filter(!is.na(abstract), trimws(abstract) != "")


#--------------------------------------------------------------------------
# Keep the removed studies
#--------------------------------------------------------------------------
removed_studies <- all_studies |>
  filter(!record_id %in% group_based_data$duplicate_id) |>
  # Set the reason for removal. If study has no abstract, reason is "no abstract". If study is a duplicate, reason is "duplicate"
  mutate(reason = ifelse(record_id %in% unique_studies$duplicate_id, "no abstract", "duplicate"))
save_dataframe_to_ris(removed_studies, "group_based/data/removed_studies.ris")

#--------------------------------------------------------------------------
# Add the final studies that are not in the screening files
#--------------------------------------------------------------------------
# Made by hand gathering titles and abstracts from the final included studies not found in the screening files
added_studies <- read_ris_to_dataframe("group_based/data/added_final_included.ris") |>
  mutate(year = as.character(year), included_final = 1, human_code = 1)
group_based_data <- bind_rows(group_based_data, added_studies)

# The added studies have no eppi_id, so they get a fake one (added_1, added_2, ...)
group_based_data <- group_based_data |>
  mutate(eppi_id = ifelse(is.na(eppi_id), paste0("added_", cumsum(is.na(eppi_id))), eppi_id))


#--------------------------------------------------------------------------
# Checks
#--------------------------------------------------------------------------
glimpse(removed_studies)
sum(group_based_data$included_final)
glimpse(group_based_data)

# Print authors of the finally included studies
for (i in seq_len(nrow(group_based_data))) {
  if (group_based_data$included_final[i] == 1) {
    cat("Title:", group_based_data$title[i], "\n")
    cat("Authors:", group_based_data$author[i], "\n")
    cat("Abstract:", group_based_data$abstract[i], "\n")
  }
}

# Sum of columns in the full data:
names(group_based_data)
sum(group_based_data$human_code)
sum(group_based_data$included_final)
unique(group_based_data$duplicate_id) |> length()

#--------------------------------------------------------------------------
# Save the data
#--------------------------------------------------------------------------
saveRDS(group_based_data, "group_based/data/group_based_data.rds")
