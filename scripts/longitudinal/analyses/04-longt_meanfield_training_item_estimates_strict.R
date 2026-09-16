library(brms)
library(data.table)
library(dplyr)

# functions ###

get_item_be <- function(fit) {
  re  <- ranef(fit)$item_id            
  est <- re[, "Estimate", "Intercept"]  
  data.frame(
    item_id = names(est),
    be      = unname(est),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

## maturation and practice ####
model_training <- readRDS(file.path(data_dir, "m2_full_lngt_training_VIbayes_strict_v2seed101_small.rds"))
item_re_m2_vi_strict <- get_item_be(model_training)


## data ####
train_data_full_strict <- readRDS(file.path(data_dir, "train_data_full_strict.rds"))

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


item_obs_full <- train_data_full_strict %>%
  group_by(item_id) %>%
  summarise(avg_item_rating = mean(item_rating),
            n = n()) %>% 
  mutate(item_id = as.character(item_id))

item_check <- left_join(item_re_m2_vi_strict, item_obs_full, by = "item_id") %>%
  mutate(low_n = n < 10000)

cor(item_check$avg_item_rating, item_check$be, use = "complete.obs")

library(ggplot2)
ggplot(item_check, aes(x = avg_item_rating, y = be, colour = low_n)) +
  geom_point(aes(size = n), alpha = 0.6) +
  scale_colour_manual(values = c("FALSE" = "grey40", "TRUE" = "red"),
                      labels = c("n ≥ 10000", "n < 10000"), name = "") +
  scale_size_continuous(range = c(0.5, 4), guide = "none") +
  ggtitle("m2_vi") +
  theme_bw(base_size = 9)



