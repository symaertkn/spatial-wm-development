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
train_data_full_strict <- readRDS(file.path(data_dir, "train_data_full_strict.rds"))
#scale the data 

train_data_full_strict <- train_data_full_strict %>%
  mutate(
    item_index_in_session_daily_scaled    = as.numeric(scale(item_index_in_session_daily)),
    day_gap_between_sessions_daily_scaled = as.numeric(scale(day_gap_between_sessions_daily)),
    days_since_first_day_scaled           = as.numeric(scale(days_since_first_day)),
    session_id_daily_scaled               = as.numeric(scale(session_id_daily)),
    starting_age_scaled                   = as.numeric(scale(starting_age)),
    elo_logit                             = as.numeric(scale(new_user_domain_rating)),
    item_scaled                           = as.numeric(scale(item_rating))
  )


## Null Model ####
pri_logit <- c(
  prior(normal(0, 1.5), class = "b"),
  prior(normal(0, 2.5), class = "Intercept"),
  prior(exponential(1), class = "sd")) 

# validate_prior(pri_logit,
#                correct_answered ~ difficulty_f + (1 | user_id) + (1 | item_id),
#                data = train_data_full_strict,
#                family = bernoulli())

m0_full_lngt_training_VIbayes_strict_v3seed101_small <- brm(
  correct_answered ~
    difficulty_f +
    (1 | user_id) + (1 | item_id),
  data = train_data_full_strict,
  family = bernoulli(),
  prior = pri_logit,
  algorithm = "meanfield",
  backend = "cmdstanr",
  iter = 10000,
  output_samples = 2000,
  save_pars = save_pars(all = FALSE),
  seed = 101)

saveRDS(m0_full_lngt_training_VIbayes_strict_v3seed101_small, file.path(data_dir, "m0_full_lngt_training_VIbayes_strict_v3seed101_small.rds"))
## Maturation #### 

pri_logit <- c(
  prior(normal(0, 1.5), class = "b"),
  prior(normal(0, 2.5), class = "Intercept"),
  prior(exponential(1), class = "sd")) 

m1_full_lngt_training_VIbayes_strict_v3seed101_small <- brm(
  correct_answered ~
    difficulty_f +
    starting_age_scaled +
    days_since_first_day_scaled +
    (1 + days_since_first_day_scaled || user_id) + (1 | item_id),
  data = train_data_full_strict,
  family = bernoulli(),
  prior = pri_logit,
  algorithm = "meanfield",
  backend = "cmdstanr",
  iter = 10000,
  output_samples = 2000,
  save_pars = save_pars(all = FALSE),
  seed = 101)

saveRDS(m1_full_lngt_training_VIbayes_strict_v3seed101_small, file.path(data_dir, "m1_full_lngt_training_VIbayes_strict_v3seed101_small.rds"))
## Maturation and Practice ####
pri_logit <- c(
  prior(normal(0, 1.5), class = "b"),
  prior(normal(0, 2.5), class = "Intercept"),
  prior(exponential(1), class = "sd")) 

m2_full_lngt_training_VIbayes_strict_v3seed101_small <- brm(
  correct_answered ~
    difficulty_f +
    starting_age_scaled +
    days_since_first_day_scaled +
    item_index_in_session_daily_scaled + 
    day_gap_between_sessions_daily_scaled +
    (1 + days_since_first_day_scaled || user_id) + (1 | item_id),
  data = train_data_full_strict,
  family = bernoulli(),
  prior = pri_logit,
  algorithm = "meanfield",
  backend = "cmdstanr",
  iter = 10000,
  output_samples = 2000,
  save_pars = save_pars(all = FALSE),
  seed = 101)

saveRDS(m2_full_lngt_training_VIbayes_strict_v3seed101_small, file.path(data_dir, "m2_full_lngt_training_VIbayes_strict_v3seed101_small.rds"))


