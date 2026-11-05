library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)
library(tidyverse)

# Load results from simulation\friends-simulation-results-test.Rdata
load("simulation/friends-simulation-results3.Rdata")
stopifnot(nrow(results) == nrow(params))
# Older result files have no ai_embedded column; treat those runs as not AI-embedded
results$ai_embedded <- if ("ai_embedded" %in% names(params)) params$ai_embedded else FALSE

# print the different values for each parameter
params %>%
  summarise_all(~list(unique(.))) %>%
  pivot_longer(cols = everything(), names_to = "parameter", values_to = "values") %>%
  mutate(values = map(values, ~paste(.x, collapse = ", "))) %>%
  unnest(cols = c(values)) %>%
  print(n = Inf)

names(results)

#--------------------------------------------------------------------------
# Plot prep
#--------------------------------------------------------------------------
# Validated categorical order (blue, orange, aqua, yellow); models take slots in fixed order
cat_pal <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#7b61a8")
text_primary   <- "#0b0b0b"
text_secondary <- "#52514e"

# Which records can become target studies (included_var)
included_var_labels <- c(
  "human_and_ai_in" = "Human and AI",
  "human_code"      = "Human only",
  "decision_binary" = "AI only"
)

plot_dat <- results %>%
  mutate(
    included_lab  = factor(unname(included_var_labels[included_var]), levels = included_var_labels),
    model_lab     = sub("^.*/", "", model),
    R_c_f         = factor(R_c),
    train_model_f = factor(train_model, levels = c("Ridge", "Elastic Net", "LASSO", "Random Forest"))
  )

model_colors <- setNames(cat_pal[seq_along(unique(plot_dat$model_lab))], sort(unique(plot_dat$model_lab)))
model_shapes <- setNames(c(16, 17, 15, 18, 8)[seq_along(model_colors)], names(model_colors))

pct_axis <- scales::label_percent(accuracy = 1)
dodge    <- position_dodge(width = 0.4)

# One panel per recall target x training model, so the plots still work when the grid grows
facets <- facet_grid(
  train_model_f ~ R_c_f,
  labeller = labeller(R_c_f = as_labeller(~ paste0("R_c = ", .x)))
)

theme_sim <- theme_minimal(base_size = 15) +
  theme(
    panel.background   = element_rect(fill = "#E6E6E6", colour = NA),
    plot.background    = element_rect(fill = "#E6E6E6", colour = NA),
    legend.background  = element_rect(fill = "#E6E6E6", colour = NA),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#B3B3B3", linewidth = 0.4),
    legend.position    = "top",
    text               = element_text(color = text_primary),
    axis.text          = element_text(color = text_secondary),
    plot.subtitle      = element_text(color = text_secondary)
  )

model_scales <- list(
  scale_color_manual(values = model_colors, name = "Embedding model"),
  scale_shape_manual(values = model_shapes, name = "Embedding model")
)

#--------------------------------------------------------------------------
# Figure 1 - Workload saved
#--------------------------------------------------------------------------
fig1 <- ggplot(plot_dat, aes(x = included_lab, y = wl_mean, color = model_lab, shape = model_lab)) +
  geom_errorbar(aes(ymin = wl_mean - 1.96 * wl_se, ymax = wl_mean + 1.96 * wl_se),
                width = 0.1, linewidth = 0.5, position = dodge) +
  geom_point(size = 3, position = dodge) +
  facets +
  model_scales +
  scale_y_continuous(labels = pct_axis) +
  labs(
    title    = "Workload saved by target-sampling design",
    subtitle = "Mean with 95% CI across iterations",
    x        = "Target studies are drawn from records relevant by",
    y        = "Mean workload saved"
  ) +
  theme_sim

fig1

#--------------------------------------------------------------------------
# Figure 2 - Reliability in P*
#--------------------------------------------------------------------------
# Share of iterations where recall over P* reached c_target; dashed line = promised R_c
fig2 <- ggplot(plot_dat, aes(x = included_lab, y = reliability_pstar , color = model_lab, shape = model_lab)) +
  geom_hline(aes(yintercept = R_c), linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(ymin = pmax(0, reliability_pstar  - 1.96 * reliability_pstar_se),
                    ymax = pmin(1, reliability_pstar  + 1.96 * reliability_pstar_se)),
                width = 0.1, linewidth = 0.5, position = dodge) +
  geom_point(size = 3, position = dodge) +
  facets +
  model_scales +
  scale_y_continuous(labels = pct_axis, limits = c(0.7, 1)) +
  labs(
    title    = "Reliability: how often recall in P* reached c_target",
    subtitle = "Dashed line = promised reliability R_c; points below it break the promise",
    x        = "Target studies are drawn from records relevant by",
    y        = "Share of iterations with recall ≥ c_target"
  ) +
  theme_sim

fig2

#--------------------------------------------------------------------------
# Figure 2a - Reliability distributions across design rows
#--------------------------------------------------------------------------
# These boxplots summarize the available rows in plot_dat. They are not
# iteration-level boxplots because results contains one summary row per design.
fig2a_dat <- plot_dat |>
  dplyr::filter(!is.na(reliability_pstar)) |>
  dplyr::mutate(
    reliability_group = interaction(
      included_lab,
      model_lab,
      drop = TRUE
    )
  ) |> 
  filter_out(ai_embedded) |> 
  filter_out(ai_miss_pct == 0)

fig2a <- ggplot2::ggplot(
  fig2a_dat,
  ggplot2::aes(
    x = included_lab,
    y = reliability_pstar,
    color = model_lab,
    group = reliability_group
  )
) +
  ggplot2::geom_hline(
    ggplot2::aes(yintercept = R_c),
    linetype = "dashed",
    color = "grey50"
  ) +
  ggplot2::geom_boxplot(
    width = 0.55,
    position = ggplot2::position_dodge(width = 0.7),
    outlier.shape = NA,
    linewidth = 0.5
  ) +
  facets +
  ggplot2::scale_color_manual(
    values = model_colors,
    name = "Embedding model"
  ) +
  ggplot2::scale_y_continuous(
    labels = pct_axis,
    limits = c(0.55, 1)
  ) +
  ggplot2::labs(
    title = "Reliability across simulation design rows",
    subtitle = "Boxes summarize reliability_pstar; points show individual design summaries",
    x = "Target studies are drawn from records relevant by",
    y = "Share of iterations with recall ≥ c_target"
  ) +
  theme_sim +
  ggplot2::guides(
    color = ggplot2::guide_legend(nrow = 2, byrow = TRUE)
  ) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  )

fig2a

fig2b_dat <- 
  plot_dat |>
  dplyr::filter(!is.na(reliability_pstar)) |>
  dplyr::mutate(
    reliability_group = interaction(
      included_lab,
      model_lab,
      drop = TRUE
    )
  ) |> 
  filter_out(ai_embedded) |> 
  filter_out(ai_miss_pct == 0)

fig2b <- 
  ggplot2::ggplot(
  fig2b_dat,
  ggplot2::aes(
    x = included_lab,
    y = target_achieved_pct,
    color = model_lab,
    group = reliability_group
  )
) +
  ggplot2::geom_hline(
    ggplot2::aes(yintercept = R_c),
    linetype = "dashed",
    color = "grey50"
  ) +
  ggplot2::geom_boxplot(
    width = 0.55,
    position = ggplot2::position_dodge(width = 0.7),
    outlier.shape = NA,
    linewidth = 0.5
  ) +
  facets +
  ggplot2::scale_color_manual(
    values = model_colors,
    name = "Embedding model"
  ) +
  ggplot2::scale_y_continuous(
    labels = pct_axis,
    limits = c(0.55, 1)
  ) +
  ggplot2::labs(
    title = "Reliability across simulation design rows",
    subtitle = "Boxes summarize reliability_pstar; points show individual design summaries",
    x = "Target studies are drawn from records relevant by",
    y = "Target covered among missed studies"
  ) +
  theme_sim +
  ggplot2::guides(
    color = ggplot2::guide_legend(nrow = 2, byrow = TRUE)
  ) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  )

fig2b

fig2c_dat <- 
  plot_dat |>
  dplyr::filter(!is.na(reliability_pstar)) |>
  dplyr::mutate(
    reliability_group = interaction(
      included_lab,
      model_lab,
      drop = TRUE
    ),
    no_missed_after_target = 1 - missed_after_target_pct
  ) |> 
  filter_out(ai_embedded) |> 
  filter_out(ai_miss_pct == 0)

fig2c <- 
  ggplot2::ggplot(
  fig2c_dat,
  ggplot2::aes(
    x = included_lab,
    y = no_missed_after_target,
    color = model_lab,
    fill = model_lab,
    group = reliability_group
  )
) +
  ggplot2::geom_hline(
    ggplot2::aes(yintercept = R_c),
    linetype = "dashed",
    color = "grey50"
  ) +
  ggplot2::geom_boxplot(
    width = 0.55,
    position = ggplot2::position_dodge(width = 0.7),
    outlier.shape = NA,
    linewidth = 0.5,
    alpha = 0.4
  ) +
  facets +
  ggplot2::scale_color_manual(
    values = model_colors,
    name = "Embedding model"
  ) +
  ggplot2::scale_fill_manual(
    values = model_colors,
    name = "Embedding model"
  ) +
  ggplot2::scale_y_continuous(
    labels = pct_axis,
    limits = c(0, 1)
  ) +
  ggplot2::labs(
    x = "Target studies are drawn from records relevant by",
    y = "% times no relevant study is missed after target"
  ) +
  theme_sim +
  ggplot2::guides(
    color = ggplot2::guide_legend(nrow = 2, byrow = TRUE)
  ) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  )

fig2c

fig2d_dat <- 
  plot_dat |>
  dplyr::filter(!is.na(reliability_pstar)) |>
  dplyr::mutate(
    reliability_group = interaction(
      included_lab,
      model_lab,
      drop = TRUE
    ),
    no_missed_after_target = 1 - missed_after_target_pct
  ) |> 
  filter_out(ai_embedded) |> 
  filter(ai_miss_pct == 0.4)

fig2d <- 
  ggplot2::ggplot(
  fig2d_dat,
  ggplot2::aes(
    x = included_lab,
    y = no_missed_after_target,
    color = model_lab,
    fill = model_lab,
    group = reliability_group
  )
) +
  ggplot2::geom_hline(
    ggplot2::aes(yintercept = R_c),
    linetype = "dashed",
    color = "grey50"
  ) +
  #ggplot2::geom_boxplot(
  #  width = 0.55,
  #  position = ggplot2::position_dodge(width = 0.7),
  #  outlier.shape = NA,
  #  linewidth = 0.5,
  #  alpha = 0.4
  #) +
  geom_point(
    position = dodge,
    alpha = 0.5
  ) +
  facets +
  ggplot2::scale_color_manual(
    values = model_colors,
    name = "Embedding model"
  ) +
  ggplot2::scale_fill_manual(
    values = model_colors,
    name = "Embedding model"
  ) +
  ggplot2::scale_y_continuous(
    labels = pct_axis,
    limits = c(0, 1)
  ) +
  ggplot2::labs(
    title = "AI missing 40% relevant studies",
    x = "Target studies are drawn from records relevant by",
    y = "% times no relevant study is missed after target"
  ) +
  theme_sim +
  ggplot2::guides(
    color = ggplot2::guide_legend(nrow = 2, byrow = TRUE)
  ) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  )

fig2d


#--------------------------------------------------------------------------
# Figure 3 - Mean recall in P*
#--------------------------------------------------------------------------
fig3 <- 
  plot_dat |> 
  filter_out(ai_miss_pct == 0) |> 
  #filter(str_detect(model, "gte")) |> 
  ggplot(aes(x = included_lab, y = recall_pstar_mean, color = model_lab, shape = model_lab)) +
  geom_hline(aes(yintercept = c_target), linetype = "dashed", color = "grey50") +
  geom_point() +
  facets +
  model_scales +
  scale_y_continuous(labels = pct_axis) +
  labs(
    title    = "Mean recall in P* when the last target study is found",
    subtitle = "Dashed line = target recall c_target",
    x        = "Target studies are drawn from records relevant by",
    y        = "Mean recall in P*"
  ) +
  theme_sim

fig3

#--------------------------------------------------------------------------
# Figure 4 - Share of P* screened before stopping
#--------------------------------------------------------------------------
fig4 <- ggplot(plot_dat, aes(x = included_lab, y = need_see_mean, color = model_lab, shape = model_lab)) +
  geom_errorbar(aes(ymin = need_see_mean - 1.96 * need_see_se, ymax = need_see_mean + 1.96 * need_see_se),
                width = 0.1, linewidth = 0.5, position = dodge) +
  geom_point(size = 3, position = dodge) +
  facets +
  model_scales +
  scale_y_continuous(labels = pct_axis) +
  expand_limits(y = 0) +
  labs(
    title    = "Share of P* screened before all target studies are found",
    subtitle = "Mean with 95% CI across iterations",
    x        = "Target studies are drawn from records relevant by",
    y        = "Share of P* screened"
  ) +
  theme_sim

fig4

#--------------------------------------------------------------------------
# Figure 5 - Workload saved vs. percent recovered, per embedding model x training model
#--------------------------------------------------------------------------
# Top-right is best: more workload saved and more relevant studies in P* recovered at stop
train_shapes <- setNames(c(16, 17, 15, 18), levels(plot_dat$train_model_f))

fig5 <- 
  plot_dat |> 
  #filter(str_detect(model, "gte")) |> 
  #filter(train_model_f, "R") |> 
  ggplot(aes(x = wl_mean, y = recall_pstar_mean, color = model_lab, shape = train_model_f)) +
  geom_hline(aes(yintercept = c_target), linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(xmin = wl_mean - 1.96 * wl_se, xmax = wl_mean + 1.96 * wl_se),
                 height = 0, linewidth = 0.5) +
  geom_point(size = 3.5) +
  facet_grid(R_c_f ~ included_lab,
             labeller = labeller(R_c_f = as_labeller(~ paste0("R_c = ", .x)))) +
  scale_color_manual(values = model_colors, name = "Embedding model") +
  scale_shape_manual(values = train_shapes, name = "Training model", drop = TRUE) +
  scale_x_continuous(labels = pct_axis) +
  scale_y_continuous(labels = pct_axis) +
  labs(
    title    = "Workload saved vs. percent recovered",
    subtitle = "Columns = which records can become targets; dashed line = c_target; bars = 95% CI for workload",
    x        = "Mean workload saved",
    y        = "Mean recall in P* at stop"
  ) +
  theme_sim +
  theme(panel.grid.major.x = element_line(), legend.box = "vertical")

fig5

fig6_dat <- 
  plot_dat |>
  dplyr::filter(
    stringr::str_detect(model, "gte"),
    ai_miss_pct == 0.4,
    !is.na(mean_pct_caugt_target)
  ) |> 
  filter_out(ai_embedded)

fig6 <- 
  ggplot2::ggplot(
  fig6_dat,
  ggplot2::aes(
    x = included_lab,
    y = mean_pct_caugt_target,
    color = model_lab,
    shape = model_lab
  )
) +
  ggplot2::geom_point(size = 3, position = dodge) +
  facets +
  model_scales +
  ggplot2::scale_y_continuous(labels = pct_axis, limits = c(0.92, 1)) +
  ggplot2::labs(
    title = "% AI-missed studies recovered at target",
    subtitle = "Groups with no AI-missed studies are excluded because recovery is undefined",
    x = "Target studies are drawn from records relevant by",
    y = "Mean share of AI-missed studies recovered"
  ) +
  theme_sim

fig6


# Last target position

gte_base_dat <- 
  results |> 
  filter(R_c == 0.95, str_detect(model, "gte-base"), alpha == 0, c_target == 0.9, ai_miss_pct != 0, seed_train_pct == 1)


#--------------------------------------------------------------------------
# Last target position by target definition x share of AI-missed studies
#--------------------------------------------------------------------------
grid_dat <- gte_base_dat |>
  filter(included_var %in% c("human_and_ai_in", "decision_binary")) |>
  mutate(
    included_lab = factor(
      included_var,
      levels = c("human_and_ai_in", "decision_binary"),
      labels = c("Human and AI included", "AI included only")
    ),
    miss_lab = factor(ai_miss_pct, levels = sort(unique(ai_miss_pct)),
                      labels = paste0(100 * sort(unique(ai_miss_pct)), "% AI-missed"))
  )

ltr_g <- grid_dat |>
  select(included_lab, miss_lab, last_target_row = last_target_rows) |>
  unnest(last_target_row)

rel_after_g <- grid_dat |>
  select(included_lab, miss_lab, last_target_row = last_target_rows_when_relevant_after) |>
  unnest(last_target_row)

grid_counts <- count(ltr_g, included_lab, miss_lab, last_target_row, name = "n_total") |>
  left_join(
    count(rel_after_g, included_lab, miss_lab, last_target_row, name = "n_missed"),
    by = c("included_lab", "miss_lab", "last_target_row")
  ) |>
  mutate(n_missed = coalesce(n_missed, 0L)) |>
  pivot_longer(c(n_total, n_missed), names_to = "outcome", values_to = "n") |>
  mutate(outcome = factor(outcome, levels = c("n_total", "n_missed"), labels = stack_lvls))

# Axis limits are shared within each column (same x and y range across AI-missed shares),
# but differ between the two target definitions
column_limits <- grid_counts |>
  group_by(included_lab) |>
  summarise(x_min = min(last_target_row) - 1, x_max = max(last_target_row) + 1,
            y_max = max(n), .groups = "drop") |>
  pivot_longer(c(x_min, x_max), values_to = "last_target_row") |>
  mutate(n = if_else(name == "x_min", 0, y_max)) |>
  tidyr::crossing(miss_lab = levels(grid_counts$miss_lab)) |>
  mutate(miss_lab = factor(miss_lab, levels = levels(grid_counts$miss_lab)))

# Peak of the (smoothed) distribution of stopping rows in each panel
peak_dat <- ltr_g |>
  group_by(included_lab, miss_lab) |>
  summarise(peak = {d <- density(last_target_row); d$x[which.max(d$y)]}, .groups = "drop")

median_dat <- ltr_g |>
  group_by(included_lab, miss_lab) |>
  summarise(median_row = median(last_target_row), .groups = "drop")

fig_last_target_grid <- ggplot(grid_counts, aes(last_target_row, n, fill = outcome, alpha = outcome)) +
  geom_col(width = 1, position = "identity") +
  geom_blank(data = column_limits, aes(last_target_row, n), inherit.aes = FALSE) +
  geom_vline(data = peak_dat, aes(xintercept = peak), inherit.aes = FALSE,
             linetype = "dashed", color = text_primary, linewidth = 0.5) +
  geom_vline(data = median_dat, aes(xintercept = median_row),
             color = text_primary, linewidth = 0.5) +
  ggh4x::facet_grid2(miss_lab ~ included_lab, scales = "free", independent = "all",
             labeller = labeller(included_lab = ~ stringr::str_wrap(paste0("Target and training studies drawn from: ", .x), width = 40))) +
  scale_x_continuous(breaks = function(limits) {
    if (max(limits) > 300) seq(100, 600, 100) else seq(50, 250, 50)
  }) +
  scale_fill_manual(values = setNames(c(cat_pal[1], cat_pal[5]), stack_lvls), name = NULL) +
  scale_alpha_manual(values = setNames(c(0.45, 0.9), stack_lvls), name = NULL) +
  labs(
    title    = "When screening can stop, by share of AI-missed studies",
    subtitle = sprintf("gte-base-en-v1.5, ridge, 95%% reliability, 90%% target\n%d target studies required; %s simulations per panel",
                       ceiling(log1p(-0.95) / log(0.9)),
                       format(nrow(ltr_g) / nrow(grid_dat), big.mark = ",")),
    x        = "Row at which the last target study is found",
    y        = "Iterations"
  ) +
  theme_sim +
  theme(plot.title = element_text(face = "bold"), plot.title.position = "plot",
        axis.line.x = element_line(colour = text_secondary, linewidth = 0.3),
        axis.ticks.x = element_line(colour = text_secondary, linewidth = 0.3),
        axis.text.x = element_text(face = "plain"),
        axis.title.x = element_text(face = "plain"),
        axis.ticks.length.x = grid::unit(2, "mm"),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "#CFCFCF", colour = NA),
        legend.position = "bottom",
        panel.background  = element_rect(fill = "white", colour = NA),
        plot.background   = element_rect(fill = "white", colour = NA),
        legend.background = element_rect(fill = "white", colour = NA))

fig_last_target_grid <- fig_last_target_grid +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(caption = "Solid line: median stopping row; dashed line: smoothed distribution peak.") +
  theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot")

fig_last_target_grid

ggsave("simulation/last_target_row_histogram_by_ai_miss.png", fig_last_target_grid,
       width = 11, height = 8, dpi = 300)

# Reuse the histogram styling for the other reliability settings.
make_last_target_grid <- function(reliability, template = fig_last_target_grid) {
  design <- results |>
    filter(R_c == reliability, str_detect(model, "gte-base"), alpha == 0,
           c_target == 0.9, ai_miss_pct %in% c(0.1, 0.2, 0.4), seed_train_pct == 1,
           included_var %in% c("human_and_ai_in", "decision_binary")) |>
    mutate(
      included_lab = factor(included_var,
                            levels = c("human_and_ai_in", "decision_binary"),
                            labels = c("Human and AI included", "AI included only")),
      miss_lab = factor(ai_miss_pct, levels = c(0.1, 0.2, 0.4),
                        labels = c("10% AI-missed", "20% AI-missed", "40% AI-missed"))
    )
  stopifnot(nrow(design) == 6L)

  all_rows <- design |>
    select(included_lab, miss_lab, last_target_row = last_target_rows) |>
    unnest(last_target_row)
  missed_rows <- design |>
    select(included_lab, miss_lab, last_target_row = last_target_rows_when_relevant_after) |>
    unnest(last_target_row)
  counts <- count(all_rows, included_lab, miss_lab, last_target_row, name = "n_total") |>
    left_join(count(missed_rows, included_lab, miss_lab, last_target_row, name = "n_missed"),
              by = c("included_lab", "miss_lab", "last_target_row")) |>
    mutate(n_missed = coalesce(n_missed, 0L)) |>
    pivot_longer(c(n_total, n_missed), names_to = "outcome", values_to = "n") |>
    mutate(outcome = factor(outcome, levels = c("n_total", "n_missed"), labels = stack_lvls))
  limits <- counts |>
    group_by(included_lab) |>
    summarise(x_min = min(last_target_row) - 1, x_max = max(last_target_row) + 1,
              y_max = max(n), .groups = "drop") |>
    pivot_longer(c(x_min, x_max), values_to = "last_target_row") |>
    mutate(n = if_else(name == "x_min", 0, y_max)) |>
    tidyr::crossing(miss_lab = levels(counts$miss_lab)) |>
    mutate(miss_lab = factor(miss_lab, levels = levels(counts$miss_lab)))
  peaks <- all_rows |>
    group_by(included_lab, miss_lab) |>
    summarise(peak = {d <- density(last_target_row); d$x[which.max(d$y)]}, .groups = "drop")
  medians <- all_rows |>
    group_by(included_lab, miss_lab) |>
    summarise(median_row = median(last_target_row), .groups = "drop")

  # Retain scales, facets and theme, but rebuild layers to avoid using the 0.95 data.
  template$layers <- list()
  template$data <- counts
  template +
    geom_col(width = 1, position = "identity") +
    geom_blank(data = limits, aes(last_target_row, n), inherit.aes = FALSE) +
    geom_vline(data = peaks, aes(xintercept = peak),
               linetype = "dashed", color = text_primary, linewidth = 0.5) +
    geom_vline(data = medians, aes(xintercept = median_row),
               color = text_primary, linewidth = 0.5) +
    labs(subtitle = sprintf(
      "gte-base-en-v1.5, ridge, %g%% reliability, 90%% target\n%d target studies required; %s simulations per panel",
      100 * reliability, ceiling(log1p(-reliability) / log(0.9)),
      paste(format(unique(lengths(design$last_target_rows)), big.mark = ","), collapse = "–")
    ))
}

fig_last_target_grid_80 <- make_last_target_grid(0.8)
fig_last_target_grid_90 <- make_last_target_grid(0.9)

ggsave("simulation/last_target_row_histogram_by_ai_miss_Rc_80.png", fig_last_target_grid_80,
       width = 11, height = 8, dpi = 300)
ggsave("simulation/last_target_row_histogram_by_ai_miss_Rc_90.png", fig_last_target_grid_90,
       width = 11, height = 8, dpi = 300)



#--------------------------------------------------------------------------
# Workload saved vs. iterations with no relevant study missed (gte-base, ridge)
#--------------------------------------------------------------------------
gte_perf <- results |>
  filter(str_detect(model, "gte-base"), train_model == "Ridge", alpha == 0, seed_train_pct == 1,
         ai_miss_pct > 0) |>
  mutate(
    included_lab = factor(included_var, levels = c("human_and_ai_in", "decision_binary"),
                          labels = c("Human and AI included", "AI included only")),
    miss_f       = factor(paste0(100 * ai_miss_pct, "%"),
                          levels = paste0(100 * sort(unique(ai_miss_pct)), "%")),
    n_target     = ceiling(log1p(-R_c) / log(c_target)),
    R_c_lab      = paste0("R_c = ", R_c, "\n", n_target, " target studies"),
    # Iterations that left a relevant study unscreened at the stopping point are listed in
    # last_target_rows_when_relevant_after, so the remainder missed nothing
    p_no_miss    = 1 - lengths(last_target_rows_when_relevant_after) / n_sim,
    p_no_miss_se = sqrt(p_no_miss * (1 - p_no_miss) / n_sim)
  )

fig_workload_no_miss <- ggplot(gte_perf, aes(wl_mean, p_no_miss, color = miss_f, shape = included_lab)) +
  geom_hline(data = distinct(gte_perf, R_c_lab, R_c), aes(yintercept = R_c),
             linetype = "dashed", colour = text_primary, linewidth = 0.5) +
  geom_errorbar(aes(ymin = pmax(0, p_no_miss - 1.96 * p_no_miss_se),
                    ymax = pmin(1, p_no_miss + 1.96 * p_no_miss_se)),
                width = 0, linewidth = 0.6, alpha = 0.7) +
  geom_point(size = 4) +
  facet_grid(cols = vars(R_c_lab)) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),
                     limits = c(0.85, 0.90), breaks = seq(0.85, 0.90, 0.01),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_color_manual(values = cat_pal[c(1, 2, 4)], name = "AI-missed studies") +
  scale_shape_manual(values = c(16, 17), name = "Target and training\nstudies drawn from") +
  labs(
    title    = "Workload saved versus iterations with no relevant study missed",
    subtitle = sprintf("gte-base-en-v1.5, ridge\n%s iterations (simulations) per point; vertical bars are 95%% CIs; dashed lines mark R_c",
                       format(unique(gte_perf$n_sim), big.mark = ",")),
    x        = "Mean workload saved",
    y        = "Iterations with no relevant study missed"
  ) +
  theme_sim +
  theme(plot.title = element_text(face = "bold"), plot.title.position = "plot",
        legend.position = "bottom", legend.box = "vertical",
        strip.background = element_rect(fill = "#CFCFCF", colour = NA),
        strip.text = element_text(face = "bold"),
        panel.background  = element_rect(fill = "white", colour = NA),
        plot.background   = element_rect(fill = "white", colour = NA),
        legend.background = element_rect(fill = "white", colour = NA))

fig_workload_no_miss

ggsave("simulation/workload_vs_no_miss_by_R_c.png", fig_workload_no_miss, width = 14, height = 7, dpi = 300)

#f <- "simulation/visualizing_res.R"
#txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
#start <- regexpr("\n\n#-+\n# Workload saved and iterations with no relevant study missed", txt)
#if (start < 0) start <- regexpr("\n\n#-+\n# Workload saved vs\\. iterations", txt)
#start