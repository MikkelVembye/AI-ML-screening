# Load necessary libraries
library(dplyr)
library(readr)
library(stringr)
library(AIscreenR)
library(readxl)
library(writexl)

# Load ris files

# Excluded studies:
path_ex1 <- "C:\\Users\\B375477\\Desktop\\AI-ML-screening\\service_learning\\data\\service learning excluded 1.ris"
path_ex2 <- "C:\\Users\\B375477\\Desktop\\AI-ML-screening\\service_learning\\data\\service learning excluded 2.ris"

excluded_studies <- list(path_ex1, path_ex2) |>
  lapply(AIscreenR::read_ris_to_dataframe) |>
  bind_rows()

# Add column to identify excluded studies
excluded_studies <- excluded_studies |>
  mutate(human_code = 0)

# Included studies:
path_in1 <- "C:\\Users\\B375477\\Desktop\\AI-ML-screening\\service_learning\\data\\service learning included.ris"
included_studies <- AIscreenR::read_ris_to_dataframe(path_in1) |>
  mutate(human_code = 1)

# Review studies:
path_rev1 <- "C:\\Users\\B375477\\Desktop\\AI-ML-screening\\service_learning\\data\\service learning review.ris"
review_studies <- AIscreenR::read_ris_to_dataframe(path_rev1) |>
  mutate(human_code = 0)

# Combine all studies into one dataframe
all_studies <- bind_rows(excluded_studies, included_studies, review_studies)

# Add a column called "included_full" which is 0 for all the studies
all_studies <- all_studies |>
  mutate(included_full = 0)

# Save the combined dataframe to an xlsx file
output_path <- "C:\\Users\\B375477\\Desktop\\AI-ML-screening\\service_learning\\data\\combined_studies.xlsx"
write_xlsx(all_studies, output_path)

# Load the combined dataframe from the xlsx file
combined_studies <- read_excel(output_path)
sum(combined_studies$included_full)


# Save the combined dataframe to a ris file
output_ris_path <- "C:\\Users\\B375477\\Desktop\\AI-ML-screening\\service_learning\\data\\combined_studies.ris"
AIscreenR::save_dataframe_to_ris(combined_studies, output_ris_path)
