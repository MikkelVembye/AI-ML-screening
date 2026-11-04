#--------------------------------------------------------------------------
# AI-ML screening simulation: Endedijk et al. (2022) data
#--------------------------------------------------------------------------
library(dplyr)
library(purrr)
library(tidyr)

source("simulation/Simulation function.R")

endedijk_data <- readRDS("Endedijk/data/endedijk_cleaned.rds")
stopifnot(attr(endedijk_data, "data_name") == "endedijk_data")

# The simulation functions look records up by "eppi_id"
endedijk_data$eppi_id <- as.character(endedijk_data$id)
attr(endedijk_data, "data_name") <- "endedijk_data"

#python_dir <- "C:/Users/B199526/AppData/Local/miniconda3/envs/positron-python/python.exe"
python_dir <- "C:/Users/B375477/AppData/Local/miniconda3/envs/positron-python/python.exe"
embedding_dir <- "Endedijk/data/embeddings"

#--------------------------------------------------------------------------
# Small test: one run of generate_prioritized_data() with one model
#--------------------------------------------------------------------------
embed_corpus(endedijk_data, "tfidf", python_dir = python_dir, dir = embedding_dir)

test_run <- generate_prioritized_data(
  data           = endedijk_data,
  model          = "tfidf",
  included_var   = "human_and_ai_in",
  c_target       = 0.95,
  R_c            = 0.95,
  alpha          = 0,
  seed_pct       = 0.2,
  seed_train_pct = 1,
  ai_miss_pct    = 0,
  seed           = 03112026,
  embed_dir      = embedding_dir
)

dim(test_run)
head(test_run)
summary <- estimate_f(test_run)
summary$workload_saved

# Finally included studies that are still unscreened after the last target study
last_target_row <- max(test_run$row_number[test_run$is_target == 1], na.rm = TRUE)
left_after_target <- test_run |>
  filter(included_final == 1, row_number > last_target_row) |>
  select(row_number, eppi_id, title, abstract, decision_binary, human_code)

cat("Last target at row:", last_target_row, "of", nrow(test_run), "\n")
cat("Finally included studies in the priority set:", sum(test_run$included_final == 1, na.rm = TRUE), "\n")
cat("Finally included studies left after the last target:", nrow(left_after_target), "\n")
left_after_target

#--------------------------------------------------------------------------
# Embeddings
#--------------------------------------------------------------------------
tictoc::tic()
embed_corpus(endedijk_data, "tfidf", python_dir = python_dir, dir = embedding_dir)
embed_corpus(endedijk_data, "all-MiniLM-L6-v2", python_dir = python_dir, dir = embedding_dir)
embed_corpus(endedijk_data, "BAAI/bge-m3", python_dir = python_dir, dir = embedding_dir)
embed_corpus(endedijk_data, "Alibaba-NLP/gte-base-en-v1.5", python_dir = python_dir, dir = embedding_dir)
embed_corpus(endedijk_data, "Alibaba-NLP/gte-large-en-v1.5", python_dir = python_dir, dir = embedding_dir)
tictoc::toc()

#--------------------------------------------------------------------------
# Experimental design
#--------------------------------------------------------------------------
params <-
  tidyr::expand_grid(
    model          = c("tfidf",
                       "all-MiniLM-L6-v2",
                       "BAAI/bge-m3",
                       "Alibaba-NLP/gte-base-en-v1.5",
                       "Alibaba-NLP/gte-large-en-v1.5"),
    included_var   = c("human_and_ai_in", "decision_binary"),
    c_target       = 0.95,
    R_c            = 0.95,
    alpha          = c(0, 0.5, 1, 2),
    seed_pct       = 0.2,
    seed_train_pct = 1,
    ai_miss_pct    = 0,
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
glimpse(results)

#--------------------------------------------------------------------------
# Save results and details
#--------------------------------------------------------------------------
session_info <- sessionInfo()
run_date <- date()

save(params, results, session_info, run_date, file = "simulation/endedijk-simulation-results.Rdata")

#--------------------------------------------------------------------------
# Plot: workload saved for each design
#--------------------------------------------------------------------------
library(ggplot2)

# results already holds the design columns, one row per row in params
results |>
  mutate(model = sub(".*/", "", model)) |> # short model names
  ggplot(aes(x = model, y = wl_mean, color = factor(alpha))) +
  geom_pointrange(
    aes(ymin = wl_mean - 1.96 * wl_se, ymax = wl_mean + 1.96 * wl_se),
    position = position_dodge(width = 0.6)
  ) +
  facet_grid(included_var ~ ai_miss_pct + R_c + seed_train_pct, labeller = label_both) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Workload saved", color = "alpha",
       title = "Workload saved by design (mean and 95% CI)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 7))
