library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

#--------------------------------------------------------------------------
# Labels, colours and theme (same look as simulation/visualizing_res.R)
#--------------------------------------------------------------------------
# Fixed colour per embedding model, so a model has the same colour in every dataset
cat_pal <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#7b61a8")
model_colors <- setNames(
  cat_pal,
  c("all-MiniLM-L6-v2", "bge-m3", "gte-base-en-v1.5", "gte-large-en-v1.5", "tfidf")
)
text_primary   <- "#0b0b0b"
text_secondary <- "#52514e"

# Which records can become target studies (included_var)
included_var_labels <- c(
  "human_and_ai_in" = "Human and AI",
  "human_code"      = "Human only",
  "decision_binary" = "AI only"
)

train_model_levels <- c("Ridge", "Elastic Net", "LASSO", "Random Forest")

pct_axis <- scales::label_percent(accuracy = 1)
dodge    <- position_dodge(width = 0.5)

theme_sim <- theme_minimal(base_size = 15) +
  theme(
    panel.background   = element_rect(fill = "white", colour = NA),
    panel.border       = element_rect(fill = NA, colour = text_secondary, linewidth = 0.5),
    plot.background    = element_rect(fill = "white", colour = NA),
    legend.background  = element_rect(fill = "white", colour = NA),
    strip.background   = element_rect(fill = "#CCCCCC", colour = NA),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#E0E0E0", linewidth = 0.3),
    legend.position    = "top",
    text               = element_text(color = text_primary),
    axis.text          = element_text(color = text_secondary),
    plot.subtitle      = element_text(color = text_secondary)
  )

model_scales <- list(
  scale_color_manual(values = model_colors, name = "Embedding model"),
  scale_fill_manual(values = model_colors, name = "Embedding model")
)

# One panel per training model (rows) x reliability (columns)
facets <- facet_grid(train_model_f ~ R_c_lab)

#--------------------------------------------------------------------------
# Load and prepare one results file
#--------------------------------------------------------------------------
load_results <- function(file) {
  env <- new.env()
  load(file, envir = env)
  res <- env$results

  # Older result files call it seed_pct
  if ("seed_pct" %in% names(res)) res <- rename(res, known_seed_pct = seed_pct)

  # Only runs where all known seed studies are used for training (as in visualizing_res.R)
  res <- filter(res, seed_train_pct == 1)

  res |>
    mutate(
      included_lab  = factor(unname(included_var_labels[included_var]), levels = included_var_labels),
      model_lab     = sub("^.*/", "", model),
      train_model_f = factor(train_model, levels = train_model_levels),
      # Number of target studies needed: ceiling(ln(1 - R_c) / ln(c_target))
      n_target      = ceiling(log1p(-R_c) / log(c_target)),
      R_c_lab       = reorder(paste0("Reliability = ", R_c, "\n(", n_target, " targets)"), R_c),
      # Iterations that left a relevant study unscreened at the stop are listed in
      # last_target_rows_when_relevant_after, so the rest found all relevant studies
      n_iter        = lengths(last_target_rows),
      p_no_miss     = 1 - lengths(last_target_rows_when_relevant_after) / n_iter
    )
}

# Width and height that fit the number of panels
fig_size <- function(d) {
  c(w = max(11, 2.5 + 3.8 * n_distinct(d$R_c)), # at least 11 so the legend fits
    h = 2.5 + 1.9 * n_distinct(d$train_model))
}

#--------------------------------------------------------------------------
# workload saved
#--------------------------------------------------------------------------
fig_workload <- function(d) {
  ggplot(d, aes(x = included_lab, y = wl_mean, color = model_lab, fill = model_lab)) +
    geom_boxplot(width = 0.55, linewidth = 0.5, alpha = 0.7,
                 position = position_dodge(width = 0.7)) +
    facets +
    model_scales +
    scale_y_continuous(labels = pct_axis) +
    labs(
      title = "Workload saved by target-sampling design",
      x     = "Target studies are drawn from records relevant by",
      y     = "Mean workload saved"
    ) +
    theme_sim
}

#--------------------------------------------------------------------------
# reliability
#--------------------------------------------------------------------------
# Share of iterations where recall in P* reached c_target; dashed line = promised R_c
fig_reliability <- function(d) {
  d <- filter(d, !is.na(reliability_pstar))
  y_min <- min(0.7, floor(20 * min(d$reliability_pstar, d$R_c)) / 20)

  ggplot(d, aes(x = included_lab, y = reliability_pstar, color = model_lab, fill = model_lab)) +
    geom_hline(aes(yintercept = R_c), linetype = "dashed", color = "grey50") +
    geom_boxplot(width = 0.55, linewidth = 0.5, alpha = 0.7, outlier.shape = NA,
                 position = position_dodge(width = 0.7)) +
    facets +
    model_scales +
    scale_y_continuous(labels = pct_axis) +
    coord_cartesian(ylim = c(y_min, 1)) +
    labs(
      title = "Reliability across simulation design rows",
      x     = "Target studies are drawn from records relevant by",
      y     = sprintf("Share of iterations with recall ≥ target recall (%g%%)", 100 * d$c_target[1])
    ) +
    theme_sim
}

#--------------------------------------------------------------------------
# all relevant studies found before the last target
#--------------------------------------------------------------------------
# Uses the highest share of artificially AI-missed studies in the results
fig_all_found <- function(d) {
  ai_miss <- max(d$ai_miss_pct)
  d <- filter(d, ai_miss_pct == ai_miss)

  ggplot(d, aes(x = included_lab, y = p_no_miss, color = model_lab)) +
    geom_hline(aes(yintercept = R_c), linetype = "dashed", color = "grey50") +
    geom_point(size = 3, alpha = 0.8, position = dodge) +
    facets +
    scale_color_manual(values = model_colors, name = "Embedding model") +
    scale_y_continuous(labels = pct_axis, limits = c(0, 1)) +
    labs(
      title    = "% times all relevant studies are found before last target",
      subtitle = if (ai_miss == 0) {
        "No extra AI-missed studies added (only the studies the AI missed itself)"
      } else {
        sprintf("%g%% of the finally included studies are flipped to AI-missed (on top of the AI's own misses)",
                100 * ai_miss)
      },
      x        = "Target studies are drawn from records relevant by",
      y        = "% times all relevant studies are found before last target"
    ) +
    theme_sim
}

#--------------------------------------------------------------------------
# row where the last target study is found
#--------------------------------------------------------------------------
# Peak of the smoothed distribution (the density needs at least two different values)
smooth_peak <- function(x) {
  if (length(unique(x)) < 2) return(x[1])
  dens <- density(x)
  dens$x[which.max(dens$y)]
}

stack_levels <- c("All iterations", "Missed 1+ relevant studies",
                  "Missed 2+ relevant studies", "Missed 3+ relevant studies")

# One panel per share of extra AI-missed studies (rows) x target definition (columns)
fig_last_target <- function(d, R, hist_model, hist_alpha) {
  d <- d |>
    filter(R_c == R, model == hist_model, alpha == hist_alpha,
           included_var %in% c("human_and_ai_in", "decision_binary")) |>
    mutate(
      included_lab = factor(included_var,
                            levels = c("human_and_ai_in", "decision_binary"),
                            labels = c("Human and AI included", "AI included only")),
      miss_lab = factor(ai_miss_pct, levels = sort(unique(ai_miss_pct)),
                        labels = paste0("Extra AI-missed: ", 100 * sort(unique(ai_miss_pct)), "%"))
    )
  if (nrow(d) == 0) return(NULL)

  # One row per iteration (all) and per iteration that missed relevant studies
  all_rows <- d |>
    select(included_lab, miss_lab, last_target_row = last_target_rows) |>
    unnest(last_target_row)
  missed_rows <- d |>
    select(included_lab, miss_lab, last_target_row = last_target_rows_when_relevant_after,
           n_missed = n_missed_pstar_when_relevant_after) |>
    unnest(c(last_target_row, n_missed))

  # Bin the rows so the bars are not too thin when the stopping rows are spread out
  # (about 100 bins over the full range; width 1 when the range is small)
  bin_width <- max(1, ceiling(diff(range(all_rows$last_target_row)) / 100))
  bin_row <- function(x) floor(x / bin_width) * bin_width + bin_width / 2

  counts <- bind_rows(
    mutate(all_rows, outcome = stack_levels[1]),
    mutate(filter(missed_rows, n_missed >= 1), outcome = stack_levels[2]),
    mutate(filter(missed_rows, n_missed >= 2), outcome = stack_levels[3]),
    mutate(filter(missed_rows, n_missed >= 3), outcome = stack_levels[4])
  ) |>
    mutate(last_target_row = bin_row(last_target_row)) |>
    count(included_lab, miss_lab, last_target_row, outcome, name = "n") |>
    mutate(outcome = factor(outcome, levels = stack_levels)) |>
    arrange(outcome) # draw the nested subsets last so they lie on top

  peaks <- all_rows |>
    summarise(peak = smooth_peak(last_target_row), .by = c(included_lab, miss_lab))
  medians <- all_rows |>
    summarise(median_row = median(last_target_row), .by = c(included_lab, miss_lab))

  ggplot(counts, aes(last_target_row, n, fill = outcome, alpha = outcome)) +
    geom_col(width = bin_width, position = "identity") +
    geom_vline(data = peaks, aes(xintercept = peak), inherit.aes = FALSE,
               linetype = "dashed", color = text_primary, linewidth = 0.5) +
    geom_vline(data = medians, aes(xintercept = median_row), inherit.aes = FALSE,
               color = text_primary, linewidth = 0.5) +
    facet_grid(miss_lab ~ included_lab, scales = "free",
               labeller = labeller(included_lab = ~ paste0("Target and training studies\ndrawn from: ", .x))) +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 5)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_fill_manual(values = setNames(cat_pal[c(1, 5, 2, 3)], stack_levels), name = NULL) +
    scale_alpha_manual(values = setNames(c(0.45, 0.9, 1, 1), stack_levels), name = NULL) +
    guides(fill = guide_legend(nrow = 1), alpha = guide_legend(nrow = 1)) +
    labs(
      title    = "When screening can stop, by share of AI-missed studies",
      subtitle = sprintf("%s, %s, %g%% reliability, %g%% target\n%d target studies required; %s simulations per panel",
                         sub("^.*/", "", hist_model), tolower(d$train_model[1]),
                         100 * R, 100 * d$c_target[1], d$n_target[1],
                         format(max(d$n_iter), big.mark = ",")),
      x        = "Row at which the last target study is found",
      y        = "Iterations",
      caption  = "Solid line: median stopping row; dashed line: smoothed distribution peak."
    ) +
    theme_sim +
    theme(plot.title = element_text(face = "bold"), plot.title.position = "plot",
          strip.text = element_text(face = "bold"),
          legend.position = "bottom", plot.caption = element_text(hjust = 0),
          plot.caption.position = "plot")
}

#--------------------------------------------------------------------------
# Make and save the figures
#--------------------------------------------------------------------------
make_figures <- function(name, results_file, out_dir,
                         hist_model = "Alibaba-NLP/gte-base-en-v1.5",
                         hist_alpha = 0) {
  if (!file.exists(results_file)) {
    stop(results_file, " not found. Run the simulation for ", name, " first.", call. = FALSE)
  }
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  figs <- list()
  save_fig <- function(fig, file_name, width, height) {
    if (is.null(fig)) return(invisible(NULL))
    ggsave(file.path(out_dir, paste0(file_name, ".png")), fig,
           width = width, height = height, dpi = 300, bg = "white")
    message("Saved ", file_name, ".png")
    figs[[file_name]] <<- fig
  }

  res <- load_results(results_file)

  for (ct in sort(unique(res$c_target))) {
    d   <- filter(res, c_target == ct)
    tag <- sprintf("%s_c%g", name, 100 * ct) # e.g. endedijk_c95
    sz  <- fig_size(d)

    save_fig(fig_workload(d),    paste0(tag, "_workload"),    sz[["w"]], sz[["h"]])
    save_fig(fig_reliability(d), paste0(tag, "_reliability"), sz[["w"]], sz[["h"]])
    save_fig(fig_all_found(d),   paste0(tag, "_all_found"),   sz[["w"]], sz[["h"]])

    # one histogram figure for each reliability level
    for (R in sort(unique(d$R_c))) {
      fig    <- fig_last_target(d, R, hist_model, hist_alpha)
      n_rows <- n_distinct(d$ai_miss_pct)
      save_fig(fig, sprintf("%s_last_target_Rc%g", tag, 100 * R), 11, 2.5 + 2.2 * n_rows)
    }
  }

  invisible(figs)
}

#--------------------------------------------------------------------------
# Make the figures
#--------------------------------------------------------------------------
figs <- make_figures(
  name         = "fong",
  results_file = "Fong/data/results/fong-simulation-results.Rdata",
  out_dir      = "Fong/data/results/figures"
)

# Print one figure, e.g. the workload saved (see names(figs) for all of them)
names(figs)
figs[[1]]
