library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)

# Load results from simulation\friends-simulation-results-test.Rdata
load("simulation/friends-simulation-results2.Rdata")
stopifnot(nrow(results) == nrow(params))
results$ai_embedded <- params$ai_embedded

# print the different values for each parameter
params %>%
  summarise_all(~list(unique(.))) %>%
  pivot_longer(cols = everything(), names_to = "parameter", values_to = "values") %>%
  mutate(values = map(values, ~paste(.x, collapse = ", "))) %>%
  unnest(cols = c(values)) %>%
  print(n = Inf)

# # A tibble: 10 × 2
#    parameter    values                                                     
#    <chr>        <chr>                                                      
#  1 iterations   2500                                                       
#  2 model        BAAI/bge-large-en-v1.5, all-MiniLM-L6-v2, all-mpnet-base-v2
#  3 included_var human_and_ai_in, decision_binary                           
#  4 ai_embedded  TRUE, FALSE                                                
#  5 c_target     0.9                                                        
#  6 R_c          0.8, 0.9, 0.95                                             
#  7 alpha        0, 0.5, 1, 2                                               
#  8 seed_pct     0.2                                                        
#  9 ai_miss_pct  0, 0.1, 0.2, 0.4                                           
# 10 seed         21092026 

names(results)

#--------------------------------------------------------------------------
# Plot prep
#--------------------------------------------------------------------------
cat_pal <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100")

included_var_labels <- c(
  "human_and_ai_in" = "Human and ai included",
  "decision_binary" = "AI decision"
)

plot_dat <- results %>%
  mutate(
    included_lab  = unname(included_var_labels[included_var]),
    R_c_f         = factor(R_c),
    ai_miss_f     = factor(ai_miss_pct),
    train_model_f = factor(train_model, levels = c("Ridge", "Elastic Net", "LASSO", "Random Forest"))
  )

model_colors       <- setNames(cat_pal[1:3], sort(unique(plot_dat$model)))
Rc_colors          <- setNames(cat_pal[1:3], sort(unique(plot_dat$R_c)))
train_model_colors <- setNames(cat_pal[1:4], levels(plot_dat$train_model_f))

pct_axis <- scales::label_percent(accuracy = 1)

#--------------------------------------------------------------------------
# Figure 1
#--------------------------------------------------------------------------
fig1_dat <- plot_dat %>% filter(ai_miss_pct == 0)

fig1 <- ggplot(fig1_dat, aes(x = train_model_f, y = wl_mean, color = R_c_f,
                              linetype = ai_embedded, group = interaction(R_c_f, ai_embedded))) +
  geom_errorbar(aes(ymin = wl_mean - 1.96 * wl_se, ymax = wl_mean + 1.96 * wl_se),
                width = 0.08, linewidth = 0.5, show.legend = FALSE) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  facet_grid(included_lab ~ model) +
  scale_y_continuous(labels = pct_axis) +
  scale_color_manual(values = Rc_colors, name = "Recall target (R_c)") +
  scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "dashed"),
                         labels = c("FALSE" = "Title + abstract", "TRUE" = "+ AI decision"),
                         name = "Embedded text") +
  labs(
    title = "Workload saved by training model, recall target, and embedded text",
    subtitle = "Target recall c_target = 0.9; no simulated AI screening errors (ai_miss_pct = 0)",
    x = "Training model",
    y = "Mean workload saved"
  ) +
  theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

fig1

# Som tidligere, Lasso er dårligst. RF og Ridge ca lige gode. AI decision hjælper kun med BAAI/large, da denne har størst seq_length.
# Se
# sentence-transformers/all-MiniLM-L6-v2: median 352, p90 639, max 18907, share over limit 74.0%
# sentence-transformers/all-mpnet-base-v2: median 352, p90 639, max 18907, share over limit 42.2%
# BAAI/bge-large-en-v1.5: median 352, p90 639, max 18907, share over limit 19.8%

#--------------------------------------------------------------------------
# Figure 2
#--------------------------------------------------------------------------
fig2_dat <- plot_dat %>% filter(R_c == 0.95, ai_embedded == FALSE)

fig2 <- ggplot(fig2_dat, aes(x = train_model_f, y = ai_miss_f, fill = wl_mean)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = pct_axis(wl_mean)), size = 5, color = "#0b0b0b") +
  facet_grid(included_lab ~ model) +
  scale_fill_gradient(low = "#cde2fb", high = "#0d366b", labels = pct_axis, name = "Workload\nsaved") +
  labs(
    title = "Workload saved across training model and AI-miss rate",
    subtitle = "Recall target R_c = 0.95",
    x = "Training model",
    y = "Simulated AI screening miss rate"
  ) +
  theme_minimal(base_size = 15) +
  theme(panel.grid = element_blank(), legend.position = "right",
        axis.text.x = element_text(angle = 30, hjust = 1, size = rel(0.7)))

fig2

# Igen, Lasso er dårligst, RF og Ridge ca lige gode. Højere AI-miss-rate giver næsten ikke ændret workloadbesparelse.

#--------------------------------------------------------------------------
# Figure 3
#--------------------------------------------------------------------------
# ai_embedded == FALSE baseline, as in fig2 (avoids duplicate rows per cell).
fig3_dat <- plot_dat %>%
  filter(ai_miss_pct == 0, ai_embedded == FALSE) %>%
  select(model, train_model_f, R_c_f, included_var, need_see_mean) %>%
  pivot_wider(names_from = included_var, values_from = need_see_mean, names_prefix = "need_see_")

fig3 <- ggplot(fig3_dat, aes(x = need_see_decision_binary, y = need_see_human_and_ai_in,
                              color = train_model_f)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") + # diagonal line where % screened is equal for both designs
  geom_point(size = 2.4, alpha = 0.85) +
  facet_grid(R_c_f ~ model,
             labeller = labeller(R_c_f = as_labeller(~ paste0("R_c = ", .x)))) +
  scale_x_continuous(labels = pct_axis) +
  scale_y_continuous(labels = pct_axis) +
  scale_color_manual(values = train_model_colors, name = "Training model") +
  labs(
    title = "Head-to-head: screening effort to reach the recall target",
    subtitle = "AI decision design vs. Human and ai included design, matched by model x R_c x training model",
    x = "% of priority-ranked pool screened to reach target (AI decision)",
    y = "% of priority-ranked pool screened to reach target (Human and ai included)"
  ) +
  theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

fig3

#--------------------------------------------------------------------------
# Figure 4
#--------------------------------------------------------------------------
fig4_dat <- plot_dat %>% filter(R_c == 0.95, ai_embedded == FALSE)

fig4 <- ggplot(fig4_dat, aes(x = train_model_f, y = mean_n_ai_missed_after_target,
                              color = ai_miss_f, group = ai_miss_f)) +
  geom_errorbar(aes(ymin = mean_n_ai_missed_after_target - 1.96 * se_n_ai_missed_after_target,
                     ymax = mean_n_ai_missed_after_target + 1.96 * se_n_ai_missed_after_target),
                width = 0.08, linewidth = 0.5) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  facet_grid(included_lab ~ model) +
  scale_color_manual(values = setNames(cat_pal, levels(fig4_dat$ai_miss_f)), name = "Simulated AI-miss rate") +
  labs(
    title = "Number of AI-missed studies still unscreened once the target is reached",
    subtitle = "Recall target R_c = 0.95",
    x = "Training model",
    y = "Mean number of AI-missed studies after target"
  ) +
  theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank(), legend.position = "top",
        axis.text.x = element_text(angle = 30, hjust = 1, size = rel(0.7)))

fig4

#--------------------------------------------------------------------------
# Figure 5
#--------------------------------------------------------------------------
fig5_dat <- plot_dat %>%
  filter(R_c == 0.95, ai_embedded == FALSE, ai_miss_pct > 0) %>%
  mutate(ai_miss_f = droplevels(ai_miss_f)) # only 3 of the 4 ai_miss_pct levels remain after the filter

fig5 <- ggplot(fig5_dat, aes(x = train_model_f, y = target_achieved_pct,
                              color = ai_miss_f, group = ai_miss_f)) +
  geom_errorbar(aes(ymin = target_achieved_pct - 1.96 * target_achieved_se,
                     ymax = target_achieved_pct + 1.96 * target_achieved_se),
                width = 0.08, linewidth = 0.5) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  facet_grid(included_lab ~ model) +
  scale_y_continuous(labels = pct_axis) +
  scale_color_manual(values = setNames(cat_pal[2:4], levels(fig5_dat$ai_miss_f)), name = "Simulated AI-miss rate") +
  labs(
    title = "How often the stopping rule catches most of the AI-missed studies",
    subtitle = "Share of simulations catching > c_target (0.9) of the AI-missed studies. R_c = 0.95",
    x = "Training model",
    y = "Share of simulations"
  ) +
  theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank(), legend.position = "top",
        axis.text.x = element_text(angle = 30, hjust = 1, size = rel(0.7)))

fig5

#--------------------------------------------------------------------------
# Figure 6
#--------------------------------------------------------------------------
fig6_dat <- plot_dat %>%
  filter(R_c == 0.95, ai_embedded == FALSE, ai_miss_pct > 0) %>%
  mutate(ai_miss_f = droplevels(ai_miss_f))

fig6 <- ggplot(fig6_dat, aes(x = wl_mean, y = target_achieved_pct,
                              color = train_model_f, shape = included_lab)) +
  geom_point(size = 2.4, alpha = 0.85) +
  facet_grid(ai_miss_f ~ model,
             labeller = labeller(ai_miss_f = as_labeller(~ paste0("AI-miss rate = ", .x)))) +
  scale_x_continuous(labels = pct_axis) +
  scale_y_continuous(labels = pct_axis) +
  scale_color_manual(values = train_model_colors, name = "Training model") +
  scale_shape_manual(values = c(16, 17), name = "Included variables") +
  labs(
    title = "Trade-off: workload saved vs. catching the AI-missed studies",
    subtitle = "Recall target R_c = 0.95",
    x = "Mean workload saved",
    y = "Share of simulations catching > c_target of AI-missed studies"
  ) +
  theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank(), legend.position = "top", legend.box = "vertical")

fig6

# Samme mønster som tidligere. BAAI/large er ikke god med human og ai. I sær ridge er dårlig.
# der er ikke noget klart, konsistent negativt trade-off (høj workload-besparelse fører til lav reliability)

#--------------------------------------------------------------------------
# Figure 7
#--------------------------------------------------------------------------
fig7_dat <- plot_dat %>% filter(ai_embedded == FALSE)

fig7_hline <- fig7_dat %>% distinct(R_c_f, model, R_c)

fig7 <- ggplot(fig7_dat, aes(x = wl_mean, y = reliability,
                              color = ai_miss_f, shape = included_lab)) +
  geom_hline(data = fig7_hline, aes(yintercept = R_c), linetype = "dashed", color = "grey50") +
  geom_point(size = 2.2, alpha = 0.85) +
  facet_grid(R_c_f ~ model,
             labeller = labeller(R_c_f = as_labeller(~ paste0("R_c = ", .x)))) +
  scale_x_continuous(labels = pct_axis) +
  scale_y_continuous(labels = pct_axis) +
  scale_color_manual(values = setNames(cat_pal, levels(fig7_dat$ai_miss_f)), name = "Simulated AI-miss rate") +
  scale_shape_manual(values = c(16, 17), name = "Included variables") +
  labs(
    title = "Trade-off: workload saved vs. keeping the R_c promise",
    subtitle = "Dashed line = promised R_c for that row; points below it break the promise (review-level reliability)",
    x = "Mean workload saved",
    y = "Reliability (share of simulations meeting c_target)"
  ) +
  theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank(), legend.position = "top", legend.box = "vertical")

fig7

# BAAI/bge-large-en-v1.5 med "Human and ai included" (trekanter i højre kolonne) har langt flere punkter under den lovede R_c-linje end noget andet pane

# BGE embedding modeller har brug for instruks, hvilket betyder at de ikke er lige så gode til at omsætte data til embedding information.
# De har svært ved at lave meget distinkte embeddings, hvilket betyder at de ikke kan skelne mellem om tekster skal inkluderes eller ekskluderes.
# Derfor, når human og ai er inkluderet, har vi et mindre, mere koncentreret træningssæt, og klassifikationen er ekstra
# sårbar, når den kun har et lille, snævert sæt positive eksempler at lære grænsen fra.

#--------------------------------------------------------------------------
# Figure 8
#--------------------------------------------------------------------------
fig8_dat <- plot_dat %>%
  filter(R_c == 0.95, ai_embedded == FALSE) %>%
  mutate(ai_miss_f = droplevels(ai_miss_f))

fig8 <- ggplot(fig8_dat, aes(x = ai_miss_f, y = wl_mean,
                              color = train_model_f, group = train_model_f)) +
  geom_errorbar(aes(ymin = wl_mean - 1.96 * wl_se,
                    ymax = wl_mean + 1.96 * wl_se),
                width = 0.08, linewidth = 0.5) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  facet_grid(included_lab ~ model) +
  scale_y_continuous(labels = pct_axis) +
  scale_color_manual(values = train_model_colors, name = "Training model") +
  labs(
    title = "Workload saved as the AI-miss rate increases",
    subtitle = "Recall target R_c = 0.95; title + abstract embeddings only",
    x = "Simulated AI-miss rate",
    y = "Mean workload saved"
  ) +
  theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank(), legend.position = "top",
        axis.text.x = element_text(angle = 30, hjust = 1, size = rel(0.8)))

fig8

# Workloadbesparelsen er næsten uændret, når AI-miss-raten stiger.

#--------------------------------------------------------------------------
# Figure 9
#--------------------------------------------------------------------------
fig9_dat <- plot_dat %>%
  filter(R_c == 0.95, ai_embedded == FALSE, ai_miss_pct > 0) %>%
  mutate(ai_miss_f = droplevels(ai_miss_f))

fig9 <- ggplot(fig9_dat, aes(x = ai_miss_f, y = target_achieved_pct,
                              color = train_model_f, group = train_model_f)) +
  geom_errorbar(aes(ymin = target_achieved_pct - 1.96 * target_achieved_se,
                    ymax = target_achieved_pct + 1.96 * target_achieved_se),
                width = 0.08, linewidth = 0.5) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  facet_grid(included_lab ~ model) +
  scale_y_continuous(labels = pct_axis) +
  scale_color_manual(values = train_model_colors, name = "Training model") +
  labs(
    title = "Target achievement as the AI-miss rate increases",
    subtitle = "Share of simulations achieving the target; recall target R_c = 0.95",
    x = "Simulated AI-miss rate",
    y = "Share of simulations achieving target"
  ) +
  theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank(), legend.position = "top",
        axis.text.x = element_text(angle = 30, hjust = 1, size = rel(0.8)))

fig9

# Målopfyldelsen falder især ved 20% AI-miss, men mønstret varierer mellem modellerne.

#--------------------------------------------------------------------------
# Figure 10
#--------------------------------------------------------------------------
fig10_dat <- plot_dat %>%
  filter(ai_embedded == FALSE, ai_miss_pct > 0) %>%
  mutate(
    ai_miss_f = droplevels(ai_miss_f),
    R_c_f = factor(R_c)
  )

fig10 <- ggplot(fig10_dat, aes(x = R_c_f, y = target_achieved_pct,
                                color = train_model_f, group = train_model_f)) +
  geom_errorbar(aes(ymin = target_achieved_pct - 1.96 * target_achieved_se,
                    ymax = target_achieved_pct + 1.96 * target_achieved_se),
                width = 0.08, linewidth = 0.5) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  facet_grid(included_lab ~ ai_miss_f + model,
             labeller = labeller(ai_miss_f = as_labeller(~ paste0("AI-miss rate = ", .x)))) +
  scale_y_continuous(labels = pct_axis) +
  scale_color_manual(values = train_model_colors, name = "Training model") +
  labs(
    title = "Target achievement across recall targets",
    subtitle = "Title + abstract embeddings only; AI-miss rate greater than zero",
    x = "Recall target (R_c)",
    y = "Share of simulations achieving target"
  ) +
  theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

fig10

# En højere recall-target giver generelt højere målopfyldelse, men forskellene mellem træningsmodeller består.

#--------------------------------------------------------------------------
# Figure 11
#--------------------------------------------------------------------------
fig11_dat <- plot_dat %>%
  filter(R_c == 0.95, ai_embedded == FALSE, ai_miss_pct > 0) %>%
  mutate(ai_miss_f = droplevels(ai_miss_f))

fig11 <- ggplot(fig11_dat, aes(x = train_model_f, y = ai_miss_f,
                                fill = target_achieved_pct)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = pct_axis(target_achieved_pct)), size = 4.5, color = "#0b0b0b") +
  facet_grid(included_lab ~ model) +
  scale_fill_gradient(low = "#fde0dd", high = "#99000d",
                      labels = pct_axis, name = "Target\nachieved") +
  labs(
    title = "Target achievement by training model and AI-miss rate",
    subtitle = "R_c = 0.95; title + abstract embeddings only; values are shares of simulations",
    x = "Training model",
    y = "Simulated AI-miss rate"
  ) +
  theme_minimal(base_size = 15) +
  theme(panel.grid = element_blank(), legend.position = "right",
        axis.text.x = element_text(angle = 30, hjust = 1, size = rel(0.75)))

fig11

# Random Forest og Ridge ligger oftest højest, mens LASSO typisk ligger lavere ved højere AI-miss-rater.

#--------------------------------------------------------------------------
# Figure 12 - Comparing AI-embedded vs. non-AI-embedded designs
#--------------------------------------------------------------------------
fig12_dat <- plot_dat %>%
  filter(R_c == 0.95, ai_miss_pct > 0) %>%
  mutate(
    ai_miss_f = droplevels(ai_miss_f),
    ai_embedded_f = factor(
      ai_embedded,
      levels = c(FALSE, TRUE),
      labels = c("Title + abstract", "+ AI decision")
    )
  )

fig12 <- ggplot(
  fig12_dat,
  aes(
    x = ai_miss_f,
    y = target_achieved_pct,
    color = ai_embedded_f,
    shape = included_lab,
    group = interaction(ai_embedded_f, included_lab)
  )
) +
  geom_errorbar(aes(
    ymin = target_achieved_pct - 1.96 * target_achieved_se,
    ymax = target_achieved_pct + 1.96 * target_achieved_se
  ), width = 0.08, linewidth = 0.5) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  facet_grid(train_model_f ~ model) +
  scale_y_continuous(labels = pct_axis) +
  scale_color_manual(values = c("Title + abstract" = cat_pal[1],
                                 "+ AI decision" = cat_pal[2]),
                     name = "Embedded text") +
  scale_shape_manual(values = c(16, 17), name = "Included variables") +
  labs(
    title = "Target achievement with and without the AI decision embedded",
    subtitle = "R_c = 0.95; comparison across simulated AI-miss rates",
    x = "Simulated AI-miss rate",
    y = "Share of simulations achieving target"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.box = "vertical",
    axis.text.x = element_text(angle = 30, hjust = 1, size = rel(0.8))
  )

fig12

# At inkludere AI-beslutningen i embedding-inputtet giver ikke en ensartet forbedring på tværs af modeller og designs.

#--------------------------------------------------------------------------
# Figure 13 - Workload saved across recall targets
#--------------------------------------------------------------------------
fig13_dat <- plot_dat %>%
  filter(ai_embedded == FALSE, ai_miss_pct == 0) %>%
  mutate(R_c_f = factor(R_c))

fig13 <- ggplot(fig13_dat, aes(x = R_c_f, y = wl_mean,
                               color = train_model_f, group = train_model_f)) +
  geom_errorbar(aes(ymin = wl_mean - 1.96 * wl_se,
                    ymax = wl_mean + 1.96 * wl_se),
                width = 0.08, linewidth = 0.5) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  facet_grid(included_lab ~ model) +
  scale_y_continuous(labels = pct_axis) +
  scale_color_manual(values = train_model_colors, name = "Training model") +
  labs(
    title = "Workload saved across recall targets",
    subtitle = "No simulated AI screening errors; title + abstract embeddings only",
    x = "Recall target (R_c)",
    y = "Mean workload saved"
  ) +
  theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

fig13

# En højere recall-target reducerer typisk workloadbesparelsen en smule.

#--------------------------------------------------------------------------
# Figure 14 - Reliability across recall targets
#--------------------------------------------------------------------------
fig14_dat <- plot_dat %>%
  filter(ai_embedded == FALSE, ai_miss_pct == 0) %>%
  mutate(R_c_f = factor(R_c))

fig14 <- ggplot(fig14_dat, aes(x = R_c_f, y = reliability,
                               color = train_model_f, group = train_model_f)) +
  geom_hline(aes(yintercept = R_c), linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(ymin = reliability - 1.96 * reliability_se,
                    ymax = reliability + 1.96 * reliability_se),
                width = 0.08, linewidth = 0.5) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  facet_grid(included_lab ~ model) +
  scale_y_continuous(labels = pct_axis) +
  scale_color_manual(values = train_model_colors, name = "Training model") +
  labs(
    title = "Reliability relative to the promised recall target",
    subtitle = "Dashed lines indicate the recall target R_c",
    x = "Recall target (R_c)",
    y = "Reliability"
  ) +
  theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

fig14

# Reliability følger generelt den lovede R_c, men nogle modeller og designs ligger under målet.

#--------------------------------------------------------------------------
# Figure 15 - Recall at the target stopping point
#--------------------------------------------------------------------------
fig15_dat <- plot_dat %>%
  filter(ai_embedded == FALSE, ai_miss_pct == 0) %>%
  mutate(R_c_f = factor(R_c))

fig15 <- ggplot(fig15_dat, aes(x = R_c_f, y = recall_at_target_mean,
                               color = train_model_f, group = train_model_f)) +
  geom_hline(aes(yintercept = c_target), linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  facet_grid(included_lab ~ model) +
  scale_y_continuous(labels = pct_axis) +
  scale_color_manual(values = train_model_colors, name = "Training model") +
  labs(
    title = "Mean recall at the target stopping point",
    subtitle = "Dashed line indicates the target recall c_target",
    x = "Recall target (R_c)",
    y = "Mean recall at target"
  ) +
  theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

fig15

# Den gennemsnitlige recall ved stop ligger generelt tæt på eller over den lovede target.

#--------------------------------------------------------------------------
# Figure 16 - AI-missed studies caught by the priority screen
#--------------------------------------------------------------------------
fig16_dat <- plot_dat %>%
  filter(R_c == 0.95, ai_embedded == FALSE, ai_miss_pct > 0) %>%
  mutate(ai_miss_f = droplevels(ai_miss_f))

fig16 <- ggplot(fig16_dat, aes(x = ai_miss_f, y = mean_pct_caugt_target,
                               color = train_model_f, group = train_model_f)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  facet_grid(included_lab ~ model) +
  scale_y_continuous(labels = pct_axis) +
  scale_color_manual(values = train_model_colors, name = "Training model") +
  labs(
    title = "AI-missed studies caught by the priority screen",
    subtitle = "R_c = 0.95; title + abstract embeddings only",
    x = "Simulated AI-miss rate",
    y = "Mean share of AI-missed studies caught"
  ) +
  theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank(), legend.position = "top",
        axis.text.x = element_text(angle = 30, hjust = 1, size = rel(0.8)))

fig16

# Priority-screeningen fanger omkring 95% af de AI-missede studier, men BGE-modellen klarer sig dårligere i Human and ai-designets paneler.

