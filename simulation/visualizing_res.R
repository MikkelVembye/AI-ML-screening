library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)

# Load results from simulation\friends-simulation-results-test.Rdata
load("simulation/friends-simulation-results-test.Rdata")

# print the different values for each parameter
params %>%
  summarise_all(~list(unique(.))) %>%
  pivot_longer(cols = everything(), names_to = "parameter", values_to = "values") %>%
  mutate(values = map(values, ~paste(.x, collapse = ", "))) %>%
  unnest(cols = c(values)) %>%
  print(n = Inf)

# # A tibble: 9 × 2
#   parameter    values
#   <chr>        <chr>
# 1 iterations   1000
# 2 model        BAAI/bge-large-en-v1.5, all-MiniLM-L6-v2, all-mpnet-base-v2
# 3 included_var human_and_ai_in, decision_binary
# 4 c_target     0.9
# 5 R_c          0.8, 0.9, 0.95
# 6 alpha        0, 0.5, 1, 2
# 7 seed_pct     0.2
# 8 ai_miss_pct  0, 0.1, 0.2, 0.4
# 9 seed         15092027

#  names(results)
#  [1] "data_name"                     "model"                         "train_model"                   "c_target"
#  [5] "R_c"                           "alpha"                         "seed_pct"                      "seed_train_pct"
#  [9] "included_var"                  "ai_miss_pct"                   "n_sim"                         "n_successful"
# [13] "n_failed"                      "failed_iterations"             "error_messages"                "cnvg"
# [17] "wl_mean"                       "wl_se"                         "need_see_mean"                 "need_see_se"
# [21] "missed_after_target_pct"       "missed_after_seed_pct"         "times_seed_after_target_pct"   "mean_n_ai_missed_after_target"
# [25] "var_n_ai_missed_after_target"  "mean_n_ai_missed_after_seed"   "var_n_ai_missed_after_seed"

#--------------------------------------------------------------------------
# Plot prep
#--------------------------------------------------------------------------
# `results` has one row per fully-crossed design cell (model x included_var x
# R_c x alpha x ai_miss_pct), already averaged over the `iterations` Monte
# Carlo draws for that cell (see assess_performance() in "Simulation
# function.R"). There is no per-iteration raw data saved, so distributions are
# shown via mean +/- SE rather than boxplots.

# Fixed, colorblind-validated categorical slots (blue, orange, aqua, yellow)
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
# Figure 1 - Interaction plot: workload saved by regularization (alpha) and
# recall target (R_c), small-multiples across model x included variables.
# (ai_miss_pct fixed at 0: the "clean" AI-screening baseline.)
#--------------------------------------------------------------------------
fig1_dat <- plot_dat %>% filter(ai_miss_pct == 0)

fig1 <- ggplot(fig1_dat, aes(x = train_model_f, y = wl_mean, color = R_c_f, group = R_c_f)) +
  geom_errorbar(aes(ymin = wl_mean - 1.96 * wl_se, ymax = wl_mean + 1.96 * wl_se),
                width = 0.08, linewidth = 0.5) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  facet_grid(included_lab ~ model) +
  scale_y_continuous(labels = pct_axis) +
  scale_color_manual(values = Rc_colors, name = "Recall target (R_c)") +
  labs(
    title = "Workload saved by training model and recall target",
    subtitle = "No simulated AI screening errors (ai_miss_pct = 0)",
    x = "Training model",
    y = "Mean workload saved"
  ) +
  theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

fig1

# We see from the plot that LASSO clearly preforms worst across all embedding models, R_c targets, and included-variable designs.
# Especially for the BAAI/bge-large-en-v1.5. We also see a slight drop in performance when only using AI decisions. 
# This is likely because the AI tends to be more over-inclusive than humans which causes the model to be trained on more irrelevant records, which in turn causes the model to be less effective at identifying relevant records.
# Furthermore, we see that the workload saved seems to follow the recall target, i.e. a higher recall target leads to a lower workload. Except for for the BAAI/bge-large-en-v1.5 model where the workload saved is close to similar for Ridge and RF.
# Lastly, we see that for the BAAI/bge-large-en-v1.5 model, RF and Ridge show similar performance with elastic net performing slightly worse. 
# For the other models and incuded-variable designs, Ridge, Elastic Net and RF perform on par with each other, with LASSO performing worse than the other three models.
# With one small exception being the all-mpnet

#--------------------------------------------------------------------------
# Figure 2 - Heat map: workload saved across alpha x AI-miss rate, at the
# strictest recall target (R_c = 0.95), small-multiples across model x
# included variables.
#--------------------------------------------------------------------------
fig2_dat <- plot_dat %>% filter(R_c == 0.95)

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

# We see that LASSO is consistently the most sensitive training model to a rising AI-miss rate,
# with workload saved dropping several points from ai_miss_pct = 0 to 0.4, most visibly for
# BAAI/bge-large-en-v1.5 (80% -> 77%) and all-mpnet-base-v2 (78% -> 75%) under the Human and ai
# included design. Ridge, Elastic Net and Random Forest stay comparatively flat as the AI-miss
# rate rises, which lines up with the LASSO pattern we already saw in fig1.
# Interestingly, the AI decision design looks almost completely unaffected by the AI-miss rate
# across all three embedding models and training methods, while the Human and ai included design
# clearly is not. This is due to how the target set is sampled AIscreenR::sample_references: 
# the AI decision design draws its target-eligible pool from every
# record with decision_binary == 1, which in friends_data is 79 records, on top of the 33 truly 
# relevant ones. So flipping a handful of truly relevant studies via ai_miss_pct barely moves that pool. 
# The Human and ai included design
# draws from human_and_ai_in == 1 instead, a much smaller and more relevant-concentrated pool
# (40 non-included + 33 truly relevant), so the same number of flipped studies removes a much
# bigger share of it and shifts workload saved more visibly.
# The BAAI/bge-large-en-v1.5 model again stands out: it reaches the highest workload savings
# overall (82-85%) under the Human and ai included design.

#--------------------------------------------------------------------------
# Figure 3 - Head-to-head scatter: screening effort (need_see_mean) under the
# AI decision design vs. the Human and ai included design, for matched design
# cells (same model x R_c x training model). Points above the diagonal need to
# screen more of the priority-ranked pool under Human and ai included; points
# below need more under AI decision. (ai_miss_pct fixed at 0: the "clean"
# AI-screening baseline, as in fig1.)
#--------------------------------------------------------------------------
fig3_dat <- plot_dat %>%
  filter(ai_miss_pct == 0) %>%
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

# We see that for all-MiniLM-L6-v2 and all-mpnet-base-v2, Ridge, Elastic Net and Random Forest sit
# almost exactly on the diagonal across all three R_c targets, meaning the two included-variable
# designs need to screen roughly the same share of the pool to reach the target. LASSO is the
# one consistent exception, sitting clearly above the diagonal for both models, i.e. it needs to
# screen more of the pool under Human and ai included than under AI decision - in line with LASSO
# being the weakest training model throughout fig1 and fig2.
# BAAI/bge-large-en-v1.5 tells a different story: every point sits well below the diagonal,
# regardless of training model or R_c. Human and ai included consistently needs to screen far less
# of the pool than AI decision for this embedding model, and the gap widens as R_c increases.
# This matches what we already saw in fig1 and fig2, where BAAI/bge-large-en-v1.5 was
# the model that benefited most from the Human and ai included design This might be because its
# embedding space is far more compressed, which makes its ranking of the bulk candidate 
# pool more sensitive to training on the noisier, AI-decision-only positive set.
# So overall, the choice of included-variable design barely matters for MiniLM and mpnet (outside
# LASSO), but for BAAI/bge-large-en-v1.5 it is the single biggest lever on screening effort.

#--------------------------------------------------------------------------
# Figure 4 - Number of AI-missed studies still unscreened once the target is
# reached (mean_n_ai_missed_after_target, an absolute count, not a share of
# simulations), by training model and AI-miss rate, small-multiples across
# model x included variables. (Fixed at R_c = 0.95.)
#--------------------------------------------------------------------------
fig4_dat <- plot_dat %>%
  filter(R_c == 0.95) %>%
  mutate(n_ai_missed_se = sqrt(var_n_ai_missed_after_target / n_sim))

fig4 <- ggplot(fig4_dat, aes(x = train_model_f, y = mean_n_ai_missed_after_target,
                              color = ai_miss_f, group = ai_miss_f)) +
  geom_errorbar(aes(ymin = mean_n_ai_missed_after_target - 1.96 * n_ai_missed_se,
                     ymax = mean_n_ai_missed_after_target + 1.96 * n_ai_missed_se),
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

