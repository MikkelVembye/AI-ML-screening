# Runs the priority-screening simulation on the FRIENDS dataset, using the reusable engine in
# priority_screening/simulate_priority_screening.r.
library(reticulate)

python_dir <- "C:/Users/B375477/AppData/Local/miniconda3/envs/positron-python/python.exe"

source("priority_screening/simulate_priority_screening.r") # f_generate/f_analyze/f_summarize/sim_driver/run_sim

friends_data <- readRDS("friends/data/friends_cleaned.rds")

#-------------------------------------------------------------------------------------------------
# Quick check before running anything expensive: one design, two repeats.
#-------------------------------------------------------------------------------------------------
one_design <- tibble(
  # design_parameters
  dat_full = list(friends_data),
  model = "all-MiniLM-L6-v2", relevant_col = list(c("human_code", "decision_binary")),
  pool_size = NA_real_, relevant_pct = NA_real_, alpha = 1, RandomForrest = FALSE,
  # focal_parameters
  c_target = 0.90, R_c = c(0.75, 0.90),
  # auxiliary_parameters
  ai_miss_pct = 0, seed_pct = 1
)
# one_design has 2 rows (R_c sweeps two values, recycling everything else) - use just the first row.
one_row <- one_design[1, ]
set.seed(1) # seeds f_generate()'s ambient RNG state; f_analyze() still draws its own seed for run_priority_screening()
one_dat <- f_generate(dat_full = one_row$dat_full[[1]], pool_size = one_row$pool_size, relevant_pct = one_row$relevant_pct)
f_analyze(
  dat = one_dat, model = one_row$model, relevant_col = one_row$relevant_col[[1]],
  c_target = one_row$c_target, R_c = one_row$R_c, alpha = one_row$alpha,
  RandomForrest = one_row$RandomForrest, ai_miss_pct = one_row$ai_miss_pct,
  seed_pct = one_row$seed_pct
)   # should return one row of results

smoke_test <- run_sim(iterations = 2, design_factors = one_design)
smoke_test |> glimpse()

#-------------------------------------------------------------------------------------------------
# The full simulation - two separate experiments.
#   - main_design: real pile size, varies AI quality and reviewer prior knowledge.
#   - n_l_design: fixed AI quality/prior knowledge, varies pile size and relevant-paper rarity.
#-------------------------------------------------------------------------------------------------

main_design <- tidyr::expand_grid(
  # design_parameters
  dat_full             = list(friends_data),
  model                = "all-MiniLM-L6-v2",
  relevant_col         = list(c("human_code", "decision_binary")),
  pool_size            = NA_real_,   # NA = use the real pile as it is
  relevant_pct         = NA_real_,   # NA = keep the real, natural mix of relevant and irrelevant papers
  alpha                = 1,
  RandomForrest        = c(FALSE, TRUE),
  # focal_parameters
  c_target             = c(0.90, 0.95),
  R_c                  = c(0.90, 0.95),
  # auxiliary_parameters
  ai_miss_pct          = c(0, 0.3),  # how poor the AI is: 0 = normal, higher values = worse
  seed_pct             = c(0.1, 1)   # how much reviewers already know: low = starting mostly from scratch, 1 = mostly already known
)

n_l_design <- tidyr::expand_grid(
  # design_parameters
  dat_full             = list(friends_data),
  model                = "all-MiniLM-L6-v2",
  relevant_col         = list(c("human_code", "decision_binary")),
  pool_size            = c(500, 1000, 2500),
  relevant_pct         = c(0.01, 0.05),
  alpha                = 1,
  RandomForrest        = FALSE,
  # focal_parameters
  c_target             = 0.95,
  R_c                  = 0.95,
  # auxiliary_parameters
  ai_miss_pct           = 0,
  seed_pct             = 1
)

sim_results     <- run_sim(iterations = 1, design_factors = main_design)
sim_results_n_l <- run_sim(iterations = 5, design_factors = n_l_design)

saveRDS(sim_results, "friends/data/simulation_results.rds")
saveRDS(sim_results_n_l, "friends/data/simulation_results_n_l.rds")
