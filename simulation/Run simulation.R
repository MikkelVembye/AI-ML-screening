#--------------------------------------------------------------------------
# AI-ML screening simulation
#--------------------------------------------------------------------------

# Required packages
library(dplyr)
library(purrr)
library(tidyr)

source("simulation/Simulation function.R")
friends_data <- readRDS("friends/data/friends_cleaned.rds")

python_dir <- "C:/Users/B375477/AppData/Local/miniconda3/envs/positron-python/python.exe"
embedding_dir <- "simulation/embeddings"

#--------------------------------------------------------------------------
# Experimental design
#--------------------------------------------------------------------------

params <- 
 tidyr::expand_grid(
     model         = c("all-MiniLM-L6-v2", "all-mpnet-base-v2"),
     included_var  = c("human_and_ai_in", "decision_binary", "human_code"),
     c_target      = 0.90,
     R_c           = 0.95,
     alpha         = c(0, 1),
     seed_pct      = 0.2,
     ai_miss_pct   = 0L,
     seed          = 12
 ) |> 
  mutate(iterations = 1000) |>
  relocate(iterations) |>
  as.data.frame() |>
  # Sort by model so each worker gets a contiguous block of rows sharing one embedding matrix:
  # the worker-local cache then reloads roughly once per worker instead of once per row.
  arrange(model, alpha)

# All look right?
params
nrow(params)

#--------------------------------------------------------------------------
# Embeddings
#--------------------------------------------------------------------------
# One matrix per (dataset, model), written to simulation/embeddings once and reused by every
# design row and every iteration.

for (m in unique(params$model)) embed_corpus(friends_data, m, python_dir, dir = embedding_dir)

#--------------------------------------------------------------------------
# Run simulation
#--------------------------------------------------------------------------
set.seed(13082026)

library(future)
library(furrr)

workers <- min(nrow(params), future::availableCores() - 1)
previous_plan <- future::plan()
future::plan(future::multisession, workers = workers)

tictoc::tic()
results <- tryCatch(
  furrr::future_pmap(
    .l = params,
    .f = run_sim,
    data = friends_data,
    embed_dir = normalizePath(embedding_dir, winslash = "/"),
    .progress = TRUE,
    .options = furrr::furrr_options(
      seed = TRUE,
      globals = c(
        "params", "friends_data", "run_sim", "generate_prioritized_data",
        "estimate_f", "assess_performance",
        "load_embeddings", "embedding_dir"
      ),
      packages = c("dplyr", "purrr", "tibble", "glmnet", "ranger", "AIscreenR")
    )
  ),
  finally = future::plan(previous_plan)
) |> 
  purrr::list_rbind()

tictoc::toc()
results$wl_mean

#--------------------------------------------------------
# Save results and details
#--------------------------------------------------------

session_info <- sessionInfo()
run_date <- date()

save(params, results, session_info, run_date, file = "simulation/friends-simulation-results.Rdata")
