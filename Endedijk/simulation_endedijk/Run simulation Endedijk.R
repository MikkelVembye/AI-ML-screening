#--------------------------------------------------------------------------
# AI-ML screening simulation: Endedijk et al. (2022) data
#--------------------------------------------------------------------------
library(dplyr)
library(purrr)
library(tidyr)

source("simulation/Simulation function.R")

endedijk_data <- readRDS("Endedijk/data/endedijk_cleaned.rds")
stopifnot(attr(endedijk_data, "data_name") == "endedijk_data")

#python_dir <- "C:/Users/B199526/AppData/Local/miniconda3/envs/positron-python/python.exe"
python_dir <- "C:/Users/B375477/AppData/Local/miniconda3/envs/positron-python/python.exe"
embedding_dir <- "simulation/embeddings"

#--------------------------------------------------------------------------
# Embeddings
#--------------------------------------------------------------------------
tictoc::tic()
embed_corpus(endedijk_data, "tfidf", python_dir = python_dir, dir = embedding_dir)
embed_corpus(endedijk_data, "all-MiniLM-L6-v2", python_dir = python_dir, dir = embedding_dir)
embed_corpus(endedijk_data, "Alibaba-NLP/gte-base-en-v1.5", python_dir = python_dir, dir = embedding_dir)
tictoc::toc()

#--------------------------------------------------------------------------
# Experimental design
#--------------------------------------------------------------------------
params <-
  tidyr::expand_grid(
    model          = c("tfidf",
                       "all-MiniLM-L6-v2",
                       "Alibaba-NLP/gte-base-en-v1.5",
                       "Alibaba-NLP/gte-large-en-v1.5"),
    included_var   = c("human_and_ai_in", "decision_binary"),
    c_target       = 0.90,
    R_c            = c(0.8, 0.9, 0.95),
    alpha          = c(0, 0.5, 1, 2),
    seed_pct       = 0.2,
    seed_train_pct = c(0.5, 1),
    ai_miss_pct    = c(0, 0.1, 0.2, 0.4),
    seed           = 03112026
  ) |>
  mutate(iterations = 10) |>
  relocate(iterations) |>
  as.data.frame() |>
  # Sort by model so each worker gets a contiguous block of rows sharing one embedding matrix:
  # the worker-local cache then reloads roughly once per worker instead of once per row.
  arrange(model, alpha) |>
  tibble()

# All look right?
params
nrow(params)

#--------------------------------------------------------------------------
# Run simulation
#--------------------------------------------------------------------------
set.seed(03112026)

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
    data = endedijk_data,
    embed_dir = normalizePath(embedding_dir, winslash = "/"),
    .progress = TRUE,
    .options = furrr::furrr_options(
      seed = TRUE,
      globals = c(
        "params", "endedijk_data", "run_sim", "generate_prioritized_data",
        "sample_target", "estimate_f", "assess_performance",
        "load_embeddings", "embedding_dir"
      ),
      packages = c("dplyr", "purrr", "tibble", "glmnet", "ranger", "AIscreenR")
    )
  ),
  finally = future::plan(previous_plan)
) |>
  purrr::list_rbind()

tictoc::toc()

#--------------------------------------------------------------------------
# Save results and details
#--------------------------------------------------------------------------
session_info <- sessionInfo()
run_date <- date()

save(params, results, session_info, run_date, file = "simulation/endedijk-simulation-results.Rdata")
