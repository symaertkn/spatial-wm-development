library(epidist)
library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(tidybayes)
library(cmdstanr) 
install_cmdstan()
library(brms)
library(data.table)

# Data Prep ####
mole_long_strict <- readRDS(file.path(data_dir, "mole_long_strict.rds"))
#scale the data 

mole_long_strict <- mole_long_strict %>%
  mutate(
    item_index_in_session_daily_scaled    = as.numeric(scale(item_index_in_session_daily)),
    day_gap_between_sessions_daily_scaled = as.numeric(scale(day_gap_between_sessions_daily)),
    days_since_first_day_scaled           = as.numeric(scale(days_since_first_day)),
    session_id_daily_scaled               = as.numeric(scale(session_id_daily)),
    starting_age_scaled                   = as.numeric(scale(starting_age)),
    elo_logit                             = as.numeric(scale(new_user_domain_rating)),
    item_scaled                           = as.numeric(scale(item_rating))
  )


## maturation and practice ####
pri_logit <- c(
  prior(normal(0, 1.5), class = "b"),
  prior(normal(0, 2.5), class = "Intercept"),
  prior(exponential(1), class = "sd"))


seeds <- c(101, 202, 303, 404)
# we might need (505, 606, 707, 808, 909, 1010)

for (s in seeds) { 
  fit <- brm(
    correct_answered ~
      difficulty_f +
      starting_age_scaled +
      days_since_first_day_scaled +
      item_index_in_session_daily_scaled + 
      day_gap_between_sessions_daily_scaled +
      (1 + days_since_first_day_scaled || user_id) + (1 | item_id),
    data = mole_long_strict,
    family = bernoulli(),
    prior = pri_logit,
    algorithm = "meanfield",
    backend = "cmdstanr",
    iter = 10000,
    output_samples = 2000,
    save_pars = save_pars(all = FALSE),
    seed = s)
  
  saveRDS(fit, file.path(data_dir, paste0("m2_full_lngt_VIbayes_v3seed", s, "_small.rds")))
  rm(fit)
  gc()
}


m2fixed_v3seed101 <- as.data.table(fixef(m2_full_lngt_VIbayes_v3seed101), keep.rownames = "term")[, seed := "Seed 101"]
m2fixed_v3seed202 <- as.data.table(fixef(m2_full_lngt_VIbayes_v3seed202), keep.rownames = "term")[, seed := "Seed 202"]
m2fixed_v3seed303 <- as.data.table(fixef(m2_full_lngt_VIbayes_v3seed303), keep.rownames = "term")[, seed := "Seed 303"]
m2fixed_v3seed404 <- as.data.table(fixef(m2_full_lngt_VIbayes_v3seed404), keep.rownames = "term")[, seed := "Seed 404"]

### Plots
library(data.table); library(ggplot2)

# Combine
fixed_all <- rbindlist(
  list(m2fixed_v3seed101, m2fixed_V3seed202, m2fixed_v3seed303, m2fixed_v3seed404),
  use.names = TRUE
)


term_labels <- c(
  Intercept                             = "Intercept",
  starting_age_scaled                   = "Starting age",
  days_since_first_day_scaled           = "Days since first game-play",
  item_index_in_session_daily_scaled    = "Trial index within a day",
  day_gap_between_sessions_daily_scaled = "Gap between game-plays",
  difficulty_f0                         = "Game difficulty: Easy",
  difficulty_f1                         = "Game difficulty: Hard"   # <- real term name
)

fixed_all[, label := unname(fifelse(term %in% names(term_labels),
                                    term_labels[term], term))]
lev <- c(unname(term_labels), setdiff(unique(fixed_all$label), unname(term_labels)))
fixed_all[, label := factor(label, levels = rev(lev))]

fixed_all[, seed := factor(seed,
                           levels = c("Seed 101", "Seed 202", "Seed 303", "Seed 404"),
                           labels = c("Seed 101 (reported)", "Seed 202", "Seed 303", "Seed 404"))]

pd <- position_dodge(width = 0.6)

m2_seed_plot <- ggplot(fixed_all, aes(Estimate, label,
                                      colour = seed, shape = seed)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "grey40", linewidth = 0.3) +
  geom_errorbar(aes(xmin = Q2.5, xmax = Q97.5),
                orientation = "y", width = 0.25,
                linewidth = 0.4, position = pd) +
  geom_point(size = 2, position = pd) +
  scale_colour_manual(values = c("Seed 101 (reported)" = "black",
                                 "Seed 202" = "grey55",
                                 "Seed 303" = "grey65",
                                 "Seed 404" = "grey75")) +
  scale_shape_manual(values = c(16, 17, 15, 18)) +
  labs(x = "Estimates", y = NULL,
       colour = NULL, shape = NULL) +
  theme_bw(base_family = "serif", base_size = 11) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border       = element_blank(),
    axis.line          = element_line(colour = "black", linewidth = 0.4),
    axis.text          = element_text(colour = "black"),
    legend.position    = "bottom",
    legend.key         = element_blank()
  )

m2_seed_plot

ggsave(file.path(data_dir, "m2_seed_stability.png"), m2_seed_plot,
       width = 6.5, height = 4, dpi = 300)

