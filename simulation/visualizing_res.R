library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)
library(tidyverse)

# Load results from simulation\friends-simulation-results-test.Rdata
load("simulation/friends-simulation-results4.Rdata")
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

results <- results |> filter(seed_train_pct == 1)


#--------------------------------------------------------------------------
# Plot prep
#--------------------------------------------------------------------------
# Validated categorical order (blue, orange, aqua, yellow); models take slots in fixed order
cat_pal <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#7b61a8")
text_primary   <- "#0b0b0b"
text_secondary <- "#52514e"

# Display-only labels; retain the original simulation parameter values.
ai_miss_labels <- c("0" = "10%", "0.1" = "20%", "0.2" = "30%", "0.4" = "50%")

# Which records can become target studies (included_var)
included_var_labels <- c(
  "human_and_ai_in" = "Human and AI",
  "human_code"      = "Human only",
  "decision_binary" = "AI only"
)

plot_dat <- 
  results %>%
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
  labeller = labeller(R_c_f = as_labeller(c(
    "0.8" = "Reliability = 0.8\n(16 targets)",
    "0.9" = "Reliability = 0.9\n(22 targets)",
    "0.95" = "Reliability = 0.95\n(29 targets)"
  )))
)

theme_sim <- theme_minimal(base_size = 15) +
  theme(
    panel.background   = element_rect(fill = "white", colour = NA),
    panel.border       = element_rect(fill = NA, colour = "#52514e", linewidth = 0.5),
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
  scale_shape_manual(values = model_shapes, name = "Embedding model")
)

#--------------------------------------------------------------------------
# Figure 1 - Workload saved
#--------------------------------------------------------------------------
fig1 <- 
  ggplot(plot_dat, aes(x = included_lab, y = wl_mean, color = model_lab, fill = model_lab)) +
  geom_boxplot(width = 0.55, linewidth = 0.5, alpha = 0.7,
               position = position_dodge(width = 0.7)) +
  facets +
  scale_color_manual(values = model_colors, name = "Embedding model") +
  scale_fill_manual(values = model_colors, name = "Embedding model") +
  scale_y_continuous(
    labels = pct_axis,
    limits = c(NA, 0.9),
    breaks = seq(0, 1, by = 0.1),
    minor_breaks = seq(0.05, 0.95, by = 0.1)
  ) +
  labs(
    title    = "Workload saved by target-sampling design",
    x        = "Target studies are drawn from records relevant by",
    y        = "Mean workload saved"
  ) +
  theme_sim +
  theme(panel.grid.minor.y = element_line(color = "#E0E0E0", linewidth = 0.3))

fig1

ggsave("simulation/fig1_boxplot.png", fig1, width = 14, height = 10, dpi = 300, bg = "white")

fig1a <-
  ggplot(plot_dat, aes(x = included_lab, y = need_see_mean, color = model_lab, fill = model_lab)) +
  geom_boxplot(width = 0.55, linewidth = 0.5, alpha = 0.7,
               position = position_dodge(width = 0.7)) +
  facets +
  scale_color_manual(values = model_colors, name = "Embedding model") +
  scale_fill_manual(values = model_colors, name = "Embedding model") +
  scale_y_continuous(labels = pct_axis, breaks = seq(0, 1, by = 0.1),
                     minor_breaks = seq(0.05, 0.95, by = 0.1)) +
  expand_limits(y = 0) +
  labs(
    title = "Share of AI-excluded studies screened before all target studies are found",
    x = "Target studies are drawn from records relevant by",
    y = "Mean share of AI-excluded studies screened"
  ) +
  theme_sim +
  theme(panel.grid.minor.y = element_line(color = "#E0E0E0", linewidth = 0.3))

fig1a

ggsave("simulation/fig1a_boxplot.png", fig1a, width = 14, height = 10, dpi = 300, bg = "white")


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
  scale_y_continuous(labels = pct_axis, limits = c(0.5, 1)) +
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
  filter_out(ai_embedded) 

fig2a <- ggplot2::ggplot(
  fig2a_dat,
  ggplot2::aes(
    x = included_lab,
    y = reliability_pstar,
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
    alpha = 0.7,
    linewidth = 0.5
  ) +
  facets +
  ggplot2::scale_color_manual(
    values = model_colors,
    name = "Embedding model"
  ) +
  ggplot2::scale_fill_manual(values = model_colors, name = "Embedding model") +
  ggplot2::scale_y_continuous(
    labels = pct_axis
  ) +
  ggplot2::labs(
    title = "Reliability across simulation design rows",
    x = "Target studies are drawn from records relevant by",
    y = "Share of iterations with recall ≥ target recall (90%)"
  ) +
  theme_sim +
  ggplot2::guides(
    color = ggplot2::guide_legend(nrow = 1, byrow = TRUE),
    fill = ggplot2::guide_legend(nrow = 1, byrow = TRUE)
  ) +
  ggplot2::theme(
    legend.position = "top",
    legend.box = "horizontal",
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
    plot.background = ggplot2::element_rect(fill = "white", colour = NA),
    legend.background = ggplot2::element_rect(fill = "white", colour = NA)
  )

# Keep the full boxes and whiskers visible while approaching a 70% lower bound.
fig2a_y_min <- min(0.7, floor(min(ggplot2::ggplot_build(fig2a)$data[[2]]$ymin,
                                na.rm = TRUE) * 100) / 100)
fig2a <- fig2a + ggplot2::coord_cartesian(ylim = c(fig2a_y_min, 1))

fig2a

ggsave("simulation/fig2a_high_resolution.png", fig2a,
       width = 14, height = 10, dpi = 600, bg = "white")

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
  filter_out(ai_embedded) 

fig2b <- 
  ggplot2::ggplot(
  fig2b_dat,
  ggplot2::aes(
    x = included_lab,
    y = target_achieved_pct,
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
    alpha = 0.7,
    na.rm = TRUE,
    linewidth = 0.5
  ) +
  facets +
  ggplot2::scale_color_manual(
    values = model_colors,
    name = "Embedding model"
  ) +
  ggplot2::scale_fill_manual(values = model_colors, name = "Embedding model") +
  ggplot2::scale_y_continuous(
    labels = pct_axis
  ) +
  ggplot2::coord_cartesian(ylim = c(0.55, 1)) +
  ggplot2::labs(
    title = "Reliability across simulation design rows",
    x = "Target studies are drawn from records relevant by",
    y = "Target covered among missed studies",
    caption = "Missing target-coverage values in the original zero-miss condition (displayed as 10% AI-missed) are omitted."
  ) +
  theme_sim +
  ggplot2::guides(
    color = ggplot2::guide_legend(nrow = 2, byrow = TRUE),
    fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE)
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
  filter_out(ai_embedded) 

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
    alpha = 0.4,
    na.rm = TRUE
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
    breaks = seq(0.5, 1, by = 0.1)
  ) +
  ggplot2::coord_cartesian(ylim = c(0.5, 1)) +
  ggplot2::labs(
    x = "Target studies are drawn from records relevant by",
    y = "% times all relevant studies are found before last target"
  ) +
  theme_sim +
  ggplot2::guides(
    color = ggplot2::guide_legend(nrow = 1, byrow = TRUE)
  ) +
  ggplot2::theme(
    legend.position = "top",
    legend.box = "horizontal"
  )

fig2c

ggsave("simulation/fig2c_high_resolution.png", fig2c,
       width = 14, height = 10, dpi = 600, bg = "white")

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
    alpha = 0.7
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
    title = paste("AI missing", ai_miss_labels[["0.4"]], "relevant studies"),
    x = "Target studies are drawn from records relevant by",
    y = "% times all relevant studies are found before last target"
  ) +
  theme_sim +
  ggplot2::guides(
    color = ggplot2::guide_legend(nrow = 1, byrow = TRUE)
  ) +
  ggplot2::theme(
    legend.position = "top",
    legend.box = "horizontal"
  )

fig2d

ggsave("simulation/fig2d_high_resolution.png", fig2d,
       width = 14, height = 10, dpi = 600, bg = "white")


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
                 width = 0, linewidth = 0.5, orientation = "y") +
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
  filter(R_c == 0.95, str_detect(model, "gte-base"), alpha == 0, c_target == 0.9, seed_train_pct == 1)


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
    miss_lab = factor(ai_miss_pct, levels = names(ai_miss_labels),
                      labels = paste(ai_miss_labels, "AI-missed"))
  )

ltr_g <- grid_dat |>
  select(included_lab, miss_lab, last_target_row = last_target_rows) |>
  unnest(last_target_row)

rel_after_g <- grid_dat |>
  select(included_lab, miss_lab, last_target_row = last_target_rows_when_relevant_after,
         n_missed = n_missed_pstar_when_relevant_after) |>
  unnest(c(last_target_row, n_missed))

stack_lvls <- c("All iterations", "Missed at least one relevant study",
                "Missed at least two relevant studies")

grid_counts <- count(ltr_g, included_lab, miss_lab, last_target_row, name = "n_total") |>
  left_join(
    count(rel_after_g, included_lab, miss_lab, last_target_row, name = "n_missed"),
    by = c("included_lab", "miss_lab", "last_target_row")
  ) |>
  left_join(
    rel_after_g |>
      filter(n_missed >= 2L) |>
      count(included_lab, miss_lab, last_target_row, name = "n_missed_two_plus"),
    by = c("included_lab", "miss_lab", "last_target_row")
  ) |>
  mutate(n_missed = coalesce(n_missed, 0L),
         n_missed_two_plus = coalesce(n_missed_two_plus, 0L)) |>
  pivot_longer(c(n_total, n_missed, n_missed_two_plus), names_to = "outcome", values_to = "n") |>
  mutate(outcome = factor(outcome, levels = c("n_total", "n_missed", "n_missed_two_plus"),
                          labels = stack_lvls)) |>
  # Draw nested subsets last so they overlay the larger distributions.
  arrange(outcome)

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
  geom_rug(data = filter(rel_after_g, n_missed >= 3L), aes(x = last_target_row),
           inherit.aes = FALSE, sides = "b", outside = TRUE, colour = cat_pal[3],
           length = grid::unit(2, "mm"), linewidth = 0.5) +
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
  scale_fill_manual(values = setNames(cat_pal[c(1, 5, 2)], stack_lvls), name = NULL) +
  scale_alpha_manual(values = setNames(c(0.45, 0.9, 1), stack_lvls), name = NULL) +
  guides(fill = guide_legend(nrow = 1), alpha = guide_legend(nrow = 1)) +
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
        axis.text.x = element_text(face = "plain", margin = margin(t = 4, unit = "mm")),
        axis.title.x = element_text(face = "plain"),
        axis.ticks.length.x = grid::unit(2, "mm"),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "#CFCFCF", colour = NA),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        panel.background  = element_rect(fill = "white", colour = NA),
        plot.background   = element_rect(fill = "white", colour = NA),
        legend.background = element_rect(fill = "white", colour = NA))

fig_last_target_grid <- fig_last_target_grid +
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(caption = paste("Solid line: median stopping row; dashed line: smoothed distribution peak.",
                       "Green rug marks below the x-axis: iterations missing three or more relevant studies.", sep = "\n")) +
  theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot")

fig_last_target_grid

ggsave("simulation/last_target_row_histogram_by_ai_miss.png", fig_last_target_grid,
       width = 11, height = 10, dpi = 600)

# Reuse the histogram styling for the other reliability settings.
make_last_target_grid <- function(reliability, template = fig_last_target_grid) {
  design <- results |>
    filter(R_c == reliability, str_detect(model, "gte-base"), alpha == 0,
           c_target == 0.9, ai_miss_pct %in% as.numeric(names(ai_miss_labels)), seed_train_pct == 1,
           included_var %in% c("human_and_ai_in", "decision_binary")) |>
    mutate(
      included_lab = factor(included_var,
                            levels = c("human_and_ai_in", "decision_binary"),
                            labels = c("Human and AI included", "AI included only")),
      miss_lab = factor(ai_miss_pct, levels = names(ai_miss_labels),
                        labels = paste(ai_miss_labels, "AI-missed"))
    )
  stopifnot(nrow(design) == 2L * length(ai_miss_labels))

  all_rows <- design |>
    select(included_lab, miss_lab, last_target_row = last_target_rows) |>
    unnest(last_target_row)
  missed_rows <- design |>
    select(included_lab, miss_lab, last_target_row = last_target_rows_when_relevant_after,
           n_missed = n_missed_pstar_when_relevant_after) |>
    unnest(c(last_target_row, n_missed))
  counts <- count(all_rows, included_lab, miss_lab, last_target_row, name = "n_total") |>
    left_join(count(missed_rows, included_lab, miss_lab, last_target_row, name = "n_missed"),
              by = c("included_lab", "miss_lab", "last_target_row")) |>
    left_join(
      missed_rows |>
        filter(n_missed >= 2L) |>
        count(included_lab, miss_lab, last_target_row, name = "n_missed_two_plus"),
      by = c("included_lab", "miss_lab", "last_target_row")
    ) |>
    mutate(n_missed = coalesce(n_missed, 0L),
           n_missed_two_plus = coalesce(n_missed_two_plus, 0L)) |>
    pivot_longer(c(n_total, n_missed, n_missed_two_plus), names_to = "outcome", values_to = "n") |>
    mutate(outcome = factor(outcome, levels = c("n_total", "n_missed", "n_missed_two_plus"),
                            labels = stack_lvls)) |>
    arrange(outcome)
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
  # Scales are mutable; isolate changes from the original and other reliability plots.
  template$scales <- template$scales$clone()
  if (reliability == 0.8) {
    legend_levels <- c(stack_lvls, "Missed at least three relevant studies")
    legend_labels <- c("All iterations", "Missed 1+ relevant studies",
                       "Missed 2+ relevant studies", "Missed 3+ relevant studies")
    template$scales$scales <- Filter(
      function(scale) !any(scale$aesthetics %in% c("fill", "alpha")),
      template$scales$scales
    )
    template <- template +
      scale_fill_manual(values = setNames(cat_pal[c(1, 5, 2, 3)], legend_levels),
                        breaks = legend_levels, labels = legend_labels, name = NULL) +
      scale_alpha_manual(values = setNames(c(0.45, 0.9, 1, 1), legend_levels),
                         breaks = legend_levels, labels = legend_labels, name = NULL)
  }
  template +
    geom_col(width = 1, position = "identity") +
    (if (reliability == 0.8) {
      geom_col(
        data = missed_rows |>
          filter(n_missed >= 3L) |>
          count(included_lab, miss_lab, last_target_row),
        aes(x = last_target_row, y = n,
            fill = "Missed at least three relevant studies",
            alpha = "Missed at least three relevant studies"),
        inherit.aes = FALSE, width = 1
      )
    } else {
      geom_rug(data = filter(missed_rows, n_missed >= 3L), aes(x = last_target_row),
               inherit.aes = FALSE, sides = "b", outside = TRUE, colour = cat_pal[3],
               length = grid::unit(2, "mm"), linewidth = 0.5)
    }) +
    geom_blank(data = limits, aes(last_target_row, n), inherit.aes = FALSE) +
    geom_vline(data = peaks, aes(xintercept = peak),
               linetype = "dashed", color = text_primary, linewidth = 0.5) +
    geom_vline(data = medians, aes(xintercept = median_row),
               color = text_primary, linewidth = 0.5) +
    labs(caption = if (reliability == 0.8) {
      "Solid line: median stopping row; dashed line: smoothed distribution peak."
    } else template$labels$caption,
    subtitle = sprintf(
      "gte-base-en-v1.5, ridge, %g%% reliability, 90%% target\n%d target studies required; %s simulations per panel",
      100 * reliability, ceiling(log1p(-reliability) / log(0.9)),
      paste(format(unique(lengths(design$last_target_rows)), big.mark = ","), collapse = "–")
    ))
}

fig_last_target_grid_80 <- make_last_target_grid(0.8)
fig_last_target_grid_90 <- make_last_target_grid(0.9)

ggsave("simulation/last_target_row_histogram_by_ai_miss_Rc_80.png", fig_last_target_grid_80,
       width = 11, height = 10, dpi = 300)
ggsave("simulation/last_target_row_histogram_by_ai_miss_Rc_90.png", fig_last_target_grid_90,
       width = 11, height = 10, dpi = 300)



#--------------------------------------------------------------------------
# Workload saved vs. iterations with no relevant study missed (gte-base, ridge)
#--------------------------------------------------------------------------
gte_perf <- results |>
  filter(str_detect(model, "gte-base"), train_model == "Ridge", alpha == 0, seed_train_pct == 1,
         ai_miss_pct %in% as.numeric(names(ai_miss_labels))) |>
  mutate(
    included_lab = factor(included_var, levels = c("human_and_ai_in", "decision_binary"),
                          labels = c("Human and AI included", "AI included only")),
    miss_f       = factor(ai_miss_pct, levels = names(ai_miss_labels),
                          labels = ai_miss_labels),
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
  scale_color_manual(values = setNames(cat_pal[1:4], ai_miss_labels), name = "AI-missed studies") +
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