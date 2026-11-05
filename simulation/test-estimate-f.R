local({
  # Avoid the simulation script's sourcing side effects.
  definitions <- parse(file = "simulation/Simulation function.R")
  for (definition in definitions) {
    if (is.call(definition) && identical(definition[[1L]], as.name("<-")) &&
        as.character(definition[[2L]]) %in% c("estimate_f", "assess_performance")) {
      eval(definition)
    }
  }

  make_data <- function(n_missed, data_name = "synthetic", n_irrelevant_test_records = 200L) {
    data <- tibble::tibble(
      row_number = seq_len(10L),
      is_target = as.integer(row_number %in% c(2L, 4L)),
      included_final = as.integer(row_number %in% c(1L, 4L, 4L + seq_len(n_missed))),
      is_seed = 0L,
      is_ai_missed = 0L
    )
    attr(data, "info_dat") <- tibble::tibble(
      data_name = data_name,
      model = "synthetic",
      train_model = "Ridge",
      c_target = 0.95,
      R_c = 0.95,
      alpha = 0,
      seed_pct = 1,
      seed_train_pct = 1,
      included_var = "human_and_ai_in",
      ai_miss_pct = 0,
      n_irrelevant_test_records = n_irrelevant_test_records,
      run_time_sec = 0
    )
    attr(data, "screen_decisions_info") <- tibble::tibble(workload_saved = 0.5)
    data
  }

  expected_counts <- c(0L, 1L, 2L, 3L, 5L)
  results <- dplyr::bind_rows(lapply(expected_counts, function(n) estimate_f(make_data(n))))
  stopifnot(
    identical(results$n_missed_pstar, expected_counts),
    identical(results$any_relevant_after_target, expected_counts > 0L),
    identical(results$exactly_1_missed, as.integer(expected_counts == 1L)),
    identical(results$exactly_2_missed, as.integer(expected_counts == 2L)),
    identical(results$more_than_2_missed, as.integer(expected_counts > 2L)),
    all(results$n_relevant_in_pstar == 2L + expected_counts),
    all(results$recall_pstar == 2 / (2 + expected_counts))
  )

  results <- dplyr::bind_rows(
    results,
    estimate_f(make_data(0L, "no_misses")),
    estimate_f(make_data(2L, n_irrelevant_test_records = 100L))
  )
  summary <- assess_performance(results)
  stopifnot(
    nrow(summary) == 3L,
    identical(summary$n_irrelevant_test_records, c(200L, 200L, 100L)),
    !"n_missed_pstar" %in% names(summary),
    identical(summary$n_missed_pstar_when_relevant_after[[1L]], expected_counts[-1L]),
    length(summary$n_missed_pstar_when_relevant_after[[1L]]) ==
      length(summary$last_target_rows_when_relevant_after[[1L]]),
    identical(summary$n_missed_pstar_when_relevant_after[[2L]], integer(0)),
    identical(summary$n_missed_pstar_when_relevant_after[[3L]], 2L),
    summary$n_exactly_1_missed[[1L]] == 1L,
    summary$n_exactly_2_missed[[1L]] == 1L,
    summary$n_more_than_2_missed[[1L]] == 2L
  )
})
