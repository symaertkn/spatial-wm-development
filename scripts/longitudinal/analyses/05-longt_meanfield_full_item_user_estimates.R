library(tidyverse); library(brms)

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


fit <- readRDS(file.path(data_dir, "m2_full_lngt_VIbayes_v3seed101_small.rds"))



library(purrr)
library(tidyverse)
preds <- c("days_since_first_day_scaled",
           "starting_age_scaled",
           "item_index_in_session_daily_scaled",
           "day_gap_between_sessions_daily_scaled")

f_all <- map_dfr(preds, function(p) {
  
  r  <- quantile(mole_long_strict[[p]], c(.01, .99))
  
  nd <- expand_grid(
    x            = seq(r[1], r[2], length.out = 100),
    difficulty_f = levels(mole_long_strict$difficulty_f)
  )
  for (q in preds) nd[[q]] <- 0   # hold everything at the mean
  nd[[p]] <- nd$x                 # then let this one vary
  
  fitted(fit, newdata = nd, re_formula = NA) %>%
    as_tibble() %>%
    bind_cols(nd) %>%
    mutate(predictor = p)
})

# --- back-transform x to raw units  ---

scale_pars <- tibble(predictor = preds) %>%
  mutate(
    raw_var = str_remove(predictor, "_scaled"),
    mu      = map_dbl(raw_var, ~ mean(mole_long_strict[[.x]], na.rm = TRUE)),
    s       = map_dbl(raw_var, ~ sd(mole_long_strict[[.x]],   na.rm = TRUE))
  )

f_all <- f_all %>%
  left_join(scale_pars, by = "predictor") %>%
  mutate(x_raw = x * s + mu)

labs_pred <- c(
  days_since_first_day_scaled           = "days since start (days)",
  starting_age_scaled                   = "starting age (years)",
  item_index_in_session_daily_scaled    = "within-day trial index ",
  day_gap_between_sessions_daily_scaled = "between-day gap (days)"
)

p_long_pred <- f_all %>%
  mutate(predictor = factor(predictor, levels = preds)) %>%
  ggplot(aes(x = x_raw, y = Estimate, ymin = Q2.5, ymax = Q97.5,
             fill = difficulty_f, color = difficulty_f)) +
  geom_smooth(stat = "identity", alpha = 1/4, linewidth = 1/2) +
  facet_wrap(~ predictor, scales = "free_x",
             labeller = labeller(predictor = labs_pred)) +
  scale_x_continuous(breaks = scales::breaks_pretty(4)) +
  labs(x = NULL, y = "P(correct)",
       color = "Difficulty", fill = "Difficulty") +
  theme_light(base_size = 14)

p_long_pred


ggsave(file.path(data_dir, "m2_pred.png"), p_long_pred,
       width = 6.5, height = 4, dpi = 300)



