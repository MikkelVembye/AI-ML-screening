#--------------------------------------------------------------------------
# AI-ML screening simulation
#--------------------------------------------------------------------------

# Required packages
library(dplyr)
library(purrr)
library(tidyr)

source("simulation/Simulation function.R")
friends_data <- readRDS("friends/data/friends_FRIENDS_2_cleaned.rds")

# Models that can embed full title + abstract text (max seq_length = 8192):
# Alibaba-NLP/gte-base-en-v1.5
# Alibaba-NLP/gte-large-en-v1.5

# Be aware:
# sentence-transformers/all-MiniLM-L6-v2: median 352, p90 639, max 18907, share over limit 74.0%
# sentence-transformers/all-mpnet-base-v2: median 352, p90 639, max 18907, share over limit 42.2%
# BAAI/bge-large-en-v1.5: median 352, p90 639, max 18907, share over limit 19.8%

#python_dir <- "C:/Users/B199526/AppData/Local/miniconda3/envs/positron-python/python.exe"
python_dir <- "C:/Users/B375477/AppData/Local/miniconda3/envs/positron-python/python.exe"
embedding_dir <- "simulation/embeddings"

# Embed corpus
tictoc::tic()
embed_corpus(friends_data, "tfidf", python_dir = python_dir, dir = embedding_dir)
tictoc::toc()
# 15.44 sec elapsed
# 58.86 sec elapsed
# 160.07 sec elapsed
#--------------------------------------------------------------------------
# Experimental design
#--------------------------------------------------------------------------
params <- 
 tidyr::expand_grid(
     model         = c("all-MiniLM-L6-v2", "tfidf", "jinaai/jina-embeddings-v5-text-small", "BAAI/bge-m3", "Alibaba-NLP/gte-base-en-v1.5", "Alibaba-NLP/gte-large-en-v1.5"),
     included_var  = c("human_and_ai_in", "decision_binary"),
     c_target      = 0.90,
     R_c           = c(0.8, 0.9, 0.95),
     alpha         = c(0, 0.5, 1, 2),
     seed_pct      = 0.2,
     ai_miss_pct   = c(0, 0.1, 0.2, 0.4),
     seed          = 21092026
 ) |> 
  mutate(iterations = 2500) |>
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
set.seed(21092026)

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
# 104515.14 sec elapsed

results$wl_mean

#--------------------------------------------------------
# Save results and details
#--------------------------------------------------------

session_info <- sessionInfo()
run_date <- date()

save(params, results, session_info, run_date, file = "simulation/friends-simulation-results3.Rdata")

#--------------------------------------------------------------------------
# TEST
#-------------------------------------------------------------------------

params_test <- 
 tidyr::expand_grid(
     model         = c("tfidf", "Alibaba-NLP/gte-base-en-v1.5", "jinaai/jina-embeddings-v5-text-small", "BAAI/bge-m3", "Alibaba-NLP/gte-large-en-v1.5"),
     included_var  = c("human_and_ai_in", "decision_binary", "human_code"),
     c_target      = 0.90,
     R_c           = 0.9,
     alpha         = c(0, 0.5, 1, 2),
     seed_pct      = 0.2,
     ai_miss_pct   = 0,
     seed          = 21092026
 ) |> 
  mutate(iterations = 10) |>
  relocate(iterations) |>
  as.data.frame() |>
  # Sort by model so each worker gets a contiguous block of rows sharing one embedding matrix:
  # the worker-local cache then reloads roughly once per worker instead of once per row.
  arrange(model, alpha)

# All look right?
params_test
nrow(params_test)

set.seed(21092026)

library(future)
library(furrr)

workers <- min(nrow(params_test), future::availableCores() - 1)
previous_plan <- future::plan()
future::plan(future::multisession, workers = workers)

tictoc::tic()
results_test <- tryCatch(
  furrr::future_pmap(
    .l = params_test,
    .f = run_sim,
    data = friends_data,
    embed_dir = normalizePath(embedding_dir, winslash = "/"),
    .progress = TRUE,
    .options = furrr::furrr_options(
      seed = TRUE,
      globals = c(
        "params_test", "friends_data", "run_sim", "generate_prioritized_data",
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

glimpse(results_test)

save(results_test, file = "simulation/friends-simulation-results-test.Rdata")

# EMBED NEW MODELS
# tictoc::tic()
# embed_corpus(friends_data, "answerdotai/ModernBERT-base", python_dir = python_dir, dir = embedding_dir)
# print("answerdotai/ModernBERT-base done")
# tictoc::toc()

tictoc::tic()
embed_corpus(friends_data, "BAAI/bge-m3", python_dir = python_dir, dir = embedding_dir)
print("BAAI/bge-m3 done")
tictoc::toc()

tictoc::tic()
embed_corpus(friends_data, "jinaai/jina-embeddings-v5-text-small",
             python_dir = python_dir, dir = embedding_dir, task = "classification", batch_size = 2)
print("jinaai/jina-embeddings-v5-text-small done")
tictoc::toc()

# Hvis version ikke er korrekt. Brug:
# "C:/Users/B375477/AppData/Local/miniconda3/envs/positron-python/python.exe" -m pip install "transformers==4.57.6" "huggingface_hub>=0.34,<1.0" "tokenizers>=0.22,<=0.23.0"
# Restart R 