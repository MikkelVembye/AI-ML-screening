sample_target <- function(
    data, # data.frame containing the records to sample from
    relevant_col, # character vector of column names in `data` indicating relevance (1 = relevant, 0 = not relevant)
    c_target, # desired recall threshold (0 < c_target < 1)
    R_c, # desired reliability guarantee (0 <= R_c < 1)
    id_col = "record_id", # name of the column in `data` that contains unique record identifiers
    seed = 123 # random seed for reproducibility
) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (!id_col %in% names(data)) {
    stop(
      "`id_col` = \"", id_col, "\" was not found in `data`.\n",
      "Please specify the name of the record ID column via the `id_col` argument."
    )
  }

  if (!all(relevant_col %in% names(data))) {
    stop(
      "`relevant_col` = ", paste(dQuote(setdiff(relevant_col, names(data)), q = FALSE), collapse = ", "),
      " was not found in `data`."
    )
  }

  if (!(c_target > 0 && c_target < 1)) stop("`c_target` must be in (0, 1).")
  if (!(R_c >= 0 && R_c < 1)) stop("`R_c` must be in [0, 1).")

  k_min <- ceiling(log(1 - R_c) / log(c_target))            # k_min = ⌈ ln(1 − R_c) / ln(c) ⌉

  N <- nrow(data)                                           # P = {P_1, ..., P_N}
  is_relevant <- Reduce(`&`, lapply(relevant_col, function(col) data[[col]] == 1))
  is_relevant[is.na(is_relevant)] <- FALSE                  # 1{P_i is relevant}, i = 1, ..., N
  L <- sum(is_relevant)                                     # L = Σ_i 1{P_i is relevant}

  # With replacement T can never exceed L distinct records, so |T| = k_min is unreachable when L < k_min
  if (L < k_min) {
    stop(paste0(
      "Only ", L, " relevant records available but k_min = ", k_min, " target records required.\n",
      "Consider reducing `c_target` or `R_c`."
    ))
  }

  # ---- Algorithm 3 ----
  T_idx <- integer(0)                                       # 1: T = ∅
  while (length(T_idx) < k_min && N > 0) {                  # 2: while |T| < k_min and P ≠ ∅ do
    I <- sample.int(N, size = 1)                            # 3:   draw P_I, I ~ Uniform{1, ..., N}; screen P_I
    if (is_relevant[I]) {                                   # 4:   if P_I is relevant then
      T_idx <- union(T_idx, I)                              # 5:     T ← T ∪ {P_I}
    }
    # 6: P_I is placed back into P, since the next draw is again from all N records (with replacement)
  }
  target_data <- data[T_idx, , drop = FALSE]                # 7: return T

  reliability_guarantee <- 1 - c_target^k_min               # R_c(S) = Pr[Recall(S) ≥ c] ≥ 1 − c^k_min

  list(
    target_set = target_data,
    target_ids = target_data[[id_col]],
    k = k_min,
    c_target = c_target,
    R_c = R_c,
    reliability_guarantee = reliability_guarantee
  )
}

# Debugging example:
#friends_data <- readRDS("friends/data/friends_FRIENDS_2_cleaned.rds")
# debugonce(sample_target)
# target <- sample_target(
#   data         = friends_data,
#   relevant_col = "human_and_ai_in",
#   c_target     = 0.9,
#   R_c          = 0.9,
#   id_col       = "eppi_id",
#   seed         = 123,
#   verbose      = TRUE
# )
