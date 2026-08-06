library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(ggplot2)
library(AIscreenR)
library(reticulate)

asylum_data <- readRDS("asylum/data/all_studies_with_labels.rds") |>
# Drop records without an abstract
  filter(!is.na(abstract), str_trim(abstract) != "") |>
  # Benchmark studies data_processing.r couldn't match to an eppi_id (non_eppi_studies) all share
  # eppi_id == "" - give each a unique placeholder so eppi_id stays a genuine unique key throughout
  # run_priority_screening() (distinct(), embeddings rownames, match()) instead of colliding, and so
  # distinct() below doesn't collapse them all down to one row.
  mutate(eppi_id = if_else(eppi_id == "" | is.na(eppi_id), paste0("no_eppi_", row_number()), eppi_id)) |>
  distinct(eppi_id, .keep_all = TRUE)

#-------------------------------------------------------------------------------------------------
# Step 1: Screen the studies using AIscreenR::tabscreen_ollama() with the ministral model
#-------------------------------------------------------------------------------------------------

prompt <- "
You are screening abstracts for a systematic review on the impact of detention on the mental and physical health of asylum seekers. This is a liberal, first-pass title/abstract screen - when in doubt, lean toward including a study; stricter criteria (such as requiring a comparison group) are applied later at full-text review, not here.

Answer two questions, then give a final decision.

---

## Question 1: Detained asylum seekers

**Question:** Does this study concern asylum seekers with a plausible history of detention? Answer 'Yes' or 'No'.

**Description:** Detention means confinement in an immigration holding centre, camp, or jail to process an asylum claim or enforce deportation. Answer 'Yes' if the abstract says the population was detained, OR if it describes indicators strongly linked to a detention history - temporary protection visa status, insecure residency status, or postmigration stress in an asylum-seeker population - even without the word 'detention'. Answer 'No' only if the abstract clearly rules out detention (e.g. an explicitly non-detained community sample) or is about an unrelated population (general immigration detainees, criminal detainees, detention in the person's home country).

**Examples:**
- 'Yes' — 'We compared PTSD symptoms of detained and non-detained asylum seekers.'
- 'Yes' — 'We compared psychological outcomes of asylum seekers on temporary versus permanent protection visas.'
- 'No' — 'This study examines asylum seekers living in the community, none of whom have been detained.'

**Default if missing:** 'Yes'

---

## Question 2: Reports data

**Question:** Does the abstract report any empirical findings - quantitative results, or specific figures/statistics from qualitative or mixed-methods work? Answer 'Yes' or 'No'.

**Description:** Answer 'Yes' for studies with quantitative measures (even a single group, no comparison group needed), mixed-methods studies, and reports or interview studies that cite concrete numbers (counts, durations, percentages). Answer 'No' only for pure commentaries, editorials, letters, or opinion pieces with no findings at all.

**Examples:**
- 'Yes' — 'Detained asylum seekers (n=62) were assessed for PTSD and depression.'
- 'Yes' — 'Based on interviews, this report documents migrants detained for an average of 3.4 months.'
- 'No' — 'In this commentary, we discuss critiques of detention policy.'

**Default if missing:** 'Yes'

---

## Question 3: Inclusion decision

**Question:** Should this abstract be included for full-text review? Answer 'Include' or 'Exclude'.

**Description:** Answer 'Include' if Question 1 is 'Yes' and Question 2 is 'Yes', or if either is uncertain. Answer 'Exclude' only if Question 1 or Question 2 is clearly 'No'.

**Default if missing:** 'Include'

---
"
# Make a small test set with 100 studies with 20 human_code == 1 and 80 human_code == 0, plus every
# benchmark (finally included) study, however many that adds on top of the 20/80 split.
set.seed(123)
benchmark_studies <- asylum_data |> filter(cite_label == "benchmark")

included_pool <- asylum_data |> filter(human_code == 1) |> filter(!eppi_id %in% benchmark_studies$eppi_id)
excluded_pool <- asylum_data |> filter(human_code == 0)

n_included_needed <- max(20 - nrow(benchmark_studies), 0)

test_set <- bind_rows(
  benchmark_studies,
  included_pool |> slice_sample(n = n_included_needed),
  excluded_pool |> slice_sample(n = 80)
)

future::plan(future::multisession)
screening_results <- AIscreenR::tabscreen_ollama(
    data                  = asylum_data,
    prompt                = prompt,
    studyid               = eppi_id,
    title                 = title,
    abstract              = abstract,
    model                 = "ministral-3:8b",
    decision_description  = FALSE,
    overinclusive         = FALSE,
)
future::plan(future::sequential)
screening_results$answer_data$decision_gpt
sum(screening_results$answer_data$decision_gpt == "1")
# Remove errors from screening_results$answer_data$decision_gpt
screening_results$answer_data <- screening_results$answer_data |>
  filter(decision_gpt %in% c("0", "1"))

AIscreenR::screen_analyzer(screening_results)

# Print abstracts of false negatives
false_negatives <- screening_results$answer_data |>
  filter(decision_gpt == "0" & human_code == 1)

# Print abstracts where cite_label is "benchmark"
false_negatives_benchmark <- screening_results$answer_data |>
  filter(cite_label == "benchmark")
false_negatives_benchmark$abstract

unique(false_negatives$cite_label)

saveRDS(screening_results, "asylum/data/screening_results_asylum.rds")

#-------------------------------------------------------------------------------------------------
# Step 3: run the priority screening method once by hand, the same way as the commented-out example
# at the bottom of priority_screening/priority_functions.r.
#-------------------------------------------------------------------------------------------------

source("priority_screening/priority_functions.r")

# Make included_final column in asylum_data for the priority screening function 1 if cite_label is "benchmark"
screening_results$answer_data$included_final <- ifelse(screening_results$answer_data$cite_label == "benchmark", 1, 0)

python_dir <- "C:/Users/B375477/AppData/Local/miniconda3/envs/positron-python/python.exe"

result <- run_priority_screening(
  data          = screening_results$answer_data,
  model         = "all-MiniLM-L6-v2", # alternatively, "all-mpnet-base-v2" or "all-MiniLM-L6-v2"
  python_dir    = python_dir,
  relevant_col  = c("human_code", "decision_binary"), # the column(s) in data that indicate whether a study is relevant (1) or not (0)
  c_target      = 0.90,
  R_c           = 0.90,
  alpha         = 0, # Ridge regression
  RandomForrest = FALSE,
  seed_pct      = 0.5,
  seed          = 123
)

last_target_row <- max(result$priority_list$row_number[result$priority_list$eppi_id %in% result$target_ids])
last_s20_row <- max(result$priority_list$row_number[result$priority_list$eppi_id %in% result$s20_ids])
last_human_row <- max(result$priority_list$row_number[as.numeric(result$priority_list$human_code) == 1])
last_benchmark_row <- max(result$priority_list$row_number[result$priority_list$cite_label == "benchmark"])
last_ai_row <- max(result$priority_list$row_number[as.numeric(result$priority_list$decision_binary) == 1])
workload_saved  <- round((1 - last_target_row / nrow(result$priority_list)) * 100, 2)
last_target_row
last_s20_row
last_human_row
last_benchmark_row
last_ai_row
workload_saved # % of the pile that wouldn't need manual screening under this method

#-------------------------------------------------------------------------------------------------
# Cumulative recall plot: how quickly benchmark, human-included, and AI-included studies are
# recovered by priority-list position, relative to the stopping point (last_target_row). Adapted
# from the muted example at the bottom of priority_screening/priority_functions.r.
#-------------------------------------------------------------------------------------------------

recall_curve <- result$priority_list |>
  arrange(row_number) |>
  mutate(
    benchmark_inc     = cite_label == "benchmark",
    human_inc         = as.numeric(human_code) == 1,
    ai_inc            = as.numeric(decision_binary) == 1,
    cum_benchmark_inc = cumsum(benchmark_inc) / sum(benchmark_inc) * 100,
    cum_human_inc     = cumsum(human_inc) / sum(human_inc) * 100,
    cum_ai_inc        = cumsum(ai_inc) / sum(ai_inc) * 100
  ) |>
  select(row_number, cum_benchmark_inc, cum_human_inc, cum_ai_inc) |>
  pivot_longer(
    cols = starts_with("cum_"),
    names_to = "group",
    values_to = "recall"
  ) |>
  mutate(
    group = recode(
      group,
      cum_benchmark_inc = "Benchmark (finally included)",
      cum_human_inc      = "Human included",
      cum_ai_inc         = "AI included"
    )
  )

ggplot(recall_curve, aes(x = row_number, y = recall, color = group)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = last_target_row, linetype = "dashed", color = "black") +
  annotate(
    "text", x = last_target_row, y = 10,
    label = "Stopping point\n(last target study)",
    hjust = -0.05, size = 3.5, color = "black"
  ) +
  labs(
    x = "Number of studies screened (priority list position)",
    y = "Cumulative recall (%)",
    color = NULL,
    title = "Cumulative recall by priority-list position"
  ) +
  theme_minimal()

#-------------------------------------------------------------------------------------------------
# Step 4: run the full simulation via the shared engine.
#-------------------------------------------------------------------------------------------------

source("priority_screening/simulate_priority_screening.r") # f_generate/f_analyze/f_summarize/sim_driver/run_sim

# The full design - varies AI quality and reviewer prior knowledge, same structure as
# friends/run_simulation.r's main_design.
main_design <- tidyr::expand_grid(
  dat_full             = list(screening_results$answer_data),
  model                = "all-MiniLM-L6-v2",
  relevant_col         = list(
                            "human_code",
                            "decision_binary",
                            c("human_code", "decision_binary")
                          ),
  pool_size            = NA_real_,
  relevant_pct         = NA_real_,
  alpha                = 0,
  RandomForrest        = FALSE,
  c_target             = 0.95,
  R_c                  = 0.95,
  ai_miss_pct          = 0,
  seed_pct             = 1,
)

sim_results_asylum <- run_sim(iterations = 1, design_factors = main_design)

saveRDS(sim_results_asylum, "asylum/data/simulation_results_asylum.rds")
sim_results_asylum <- readRDS("asylum/data/simulation_results_asylum.rds")

# Inspect the simulation results
res1 <- sim_results_asylum |>
  select(-dat_full) |>
  mutate(relevant_col = purrr::map_chr(relevant_col, paste, collapse = ", ")) |>
  select(alpha, relevant_col, RandomForrest, c_target, R_c, ai_miss_pct, seed_pct,
         n_failed, mean_k_min, mean_last_target_row, mean_workload_saved_pct, mean_last_s20_row, mean_last_benchmark_row, mean_last_human_row, mean_last_ai_row) |>
  arrange(desc(mean_workload_saved_pct))
res1
