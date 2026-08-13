#--------------------------------------------------------------------------
# AI-ML screening simulation
#--------------------------------------------------------------------------

# Required packages
library(dplyr)
library(purrr)
library(tidyr)

source("simulation/Simulation function.R")
friends_data <- readRDS("friends/data/friends_cleaned.rds")

#--------------------------------------------------------------------------
# Experimental design
#--------------------------------------------------------------------------
set.seed(13082026)

params <- 
 tidyr::expand_grid(
     model         = "all-MiniLM-L6-v2",
     included_var  = "human_and_ai_in",
     c_target      = 0.90,
     R_c           = 0.95,
     alpha         = c(0, 1),
     seed_pct      = 0.2,
     ai_miss_pct   = 0L,
 ) |> 
  mutate(
    iterations = 2,
    seed = round(runif(1) * 2^30) + 1:n()
  ) |> 
  relocate(iterations) |> 
  as.data.frame()

# All look right?
params
nrow(params)

#--------------------------------------------------------------------------
# Run simulation
#--------------------------------------------------------------------------

library(future)
library(furrr)

workers <- min(3L, future::availableCores())
previous_plan <- future::plan()
future::plan(future::multisession, workers = workers)

results <- tryCatch(
  furrr::future_pmap(
    .l = params,
    .f = run_sim,
    data = friends_data,
    python_dir = "C:/Users/B199526/AppData/Local/miniconda3/envs/positron-python/python.exe",
    .progress = TRUE,
    .options = furrr::furrr_options(
      seed = TRUE,
      globals = c(
        "params", "friends_data", "run_sim", "generate_prioritized_data",
        "estimate_f", "assess_performance"
      ),
      packages = c("dplyr", "purrr", "tibble", "reticulate", "glmnet", "ranger", "AIscreenR")
    )
  ),
  finally = future::plan(previous_plan)
) |> 
  purrr::list_rbind()


#--------------------------------------------------------
# Save results and details
#--------------------------------------------------------

session_info <- sessionInfo()
run_date <- date()

save(params, results, session_info, run_date, file = "simulation/friends-simulation-results.Rdata")
