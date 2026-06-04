# Running the screening process for the systematic review on Service Learning interventions

# Load necessary libraries
library(dplyr)
library(readr)
library(stringr)
library(AIscreenR)
library(future)


prompt <- "We are screening titles and abstracts of studies for a systematic review about service learning interventions in primary/secondary education (grades K–12). INCLUDE if ALL are true:
A) Study Design:
- Uses a control group (e.g., students not in service learning, with treatment-as-usual or alternative treatment).
- Eligible designs: randomised/quasi-randomised controlled trials (individual/cluster-level) OR non-randomised studies (with ≥2 units in treatment/control groups, able to isolate intervention effects).
- Excludes: single-group pre-post, instrumental variable approaches, or studies where treatment/control groups are confounded (e.g., school-level effects).

B) Participants:
- Children in primary/secondary education (ages ~5–19, depending on country; excludes preschool/home-school).

C) Intervention:
- Service learning defined as:
  - Curriculum-based community service integrated with classroom instruction (e.g., discussions, reflections).
  - Addresses real community needs, with structured reflection/analysis.
  - Excludes: standalone community service or extracurricular activities without academic integration.

D) Outcomes:
- Primary: Standardised measures of academic success (e.g., test scores, attendance, dropout) OR NEET status (post-compulsory).
- Secondary: Standardised measures of personal/social skills (e.g., self-esteem, locus of control) OR risk behaviours (e.g., sexual risk-taking).
- Excludes: non-standardised measures or subsets of standardised scales.

EXCLUDE if ANY are true:
1) No control group or inability to isolate intervention effects.
2) Participants outside K–12 general education (e.g., preschool, home-school).
3) Intervention lacks both community service and classroom integration.
4) Outcomes are non-standardised or unrelated to academic/NEET/personal/social/risk domains.
5) Follow-up exceeds 2 years post-intervention (unless post-intervention data is also reported).
    
REMEBER: These are titles and abstracts, so information may be limited. If a study shows potential to meet inclusion criteria based on the abstract, it should be included for full-text screening."

# Load the RIS file with human annotations
# human_code for title and abstract screening: 1 for included, 0 for excluded
# included_full for full text screening: 1 for included, 0 for excluded.
ris_path <- "C:\\Users\\B375477\\Desktop\\AI-ML-screening\\service_learning\\data\\combined_studies.ris"
filges2022_full_data <- read_ris_to_dataframe(ris_path)

# To conduct the screening we will start by evaluating the prompt on a small sample of studies. 
# We will use 150 studies for the first round of screening. 100 with human code 0 and 50 with human code 1.
# We need at least 2 of the 50 studies with human code 1 to have an included_full code of 1. These are studies that we know should be included in the final report.

set.seed(123) # For reproducibility

# Sample 48 studies with human_code == 1, and 2 with included_full == 1, and 100 studies with human_code == 0
sample_included <- filges2022_full_data |>
  filter(human_code == 1) |>
  sample_n(48)

sample_included_full <- filges2022_full_data |>
  filter(included_full == 1) |>
  sample_n(2)

sample_excluded <- filges2022_full_data |>
  filter(human_code == 0) |>
  sample_n(100)

screening_sample <- bind_rows(sample_included, sample_excluded) |>
  sample_frac(1) # Shuffle the rows

# Run the screening
plan(multisession)
results_sample <-
    AIscreenR::tabscreen_gpt(
        data = screening_sample,
        prompt = prompt,
        studyid = eppi_id,
        title = title,
        abstract = abstract,
        model = "gpt-5-mini",
        decision_description = FALSE,
        overinclusive = TRUE,
    )
plan(sequential)

# Check the results
AIscreenR::screen_analyzer(results_sample)

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
