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
results$ai_embedded <- params$ai_embedded

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
