library(brms)
library(data.table)
library(dplyr)

# out-of-sample prediction ####

# Data Prep ####
test_data_full_strict <- readRDS(file.path(data_dir, "test_data_full_strict.rds"))

#scale the data 

test_data_full_strict <- test_data_full_strict %>%
  mutate(
    item_index_in_session_daily_scaled    = as.numeric(scale(item_index_in_session_daily)),
    day_gap_between_sessions_daily_scaled = as.numeric(scale(day_gap_between_sessions_daily)),
    days_since_first_day_scaled           = as.numeric(scale(days_since_first_day)),
    session_id_daily_scaled               = as.numeric(scale(session_id_daily)),
    starting_age_scaled                   = as.numeric(scale(starting_age)),
    elo_logit                             = as.numeric(scale(new_user_domain_rating)),
    item_scaled                           = as.numeric(scale(item_rating))
  )

# 5% of children
set.seed(1815)
u  <- unique(test_data_full_strict$user_id)
ev <- test_data_full_strict[test_data_full_strict$user_id %in%
                              sample(u, ceiling(length(u) * 0.05)), ]
ev <- as.data.frame(ev)

ev %>%
  group_by(user_id) %>% 
  filter(new_user_domain_modified_count == max(new_user_domain_modified_count)) %>% 
  ungroup() %>% 
  group_by(grade) %>%
  summarise(n_students = n_distinct(user_id))


chunk_log_probs <- function(model, data, chunk_size = 20000, ndraws = 1000, seed = 101) {
  set.seed(seed)
  n   <- nrow(data)
  idx <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
  out <- numeric(n)
  
  for (j in seq_along(idx)) {
    i <- idx[[j]]
    epred <- posterior_epred(model,
                             newdata = data[i, , drop = FALSE],
                             re_formula = NULL,
                             allow_new_levels = TRUE,
                             sample_new_levels = "gaussian",
                             ndraws = ndraws)
    y <- data$correct_answered[i]
    p_obs  <- sweep(epred, 2, y, function(p, y) ifelse(y == 1, p, 1 - p))
    out[i] <- log(colMeans(p_obs))
    rm(epred, p_obs); gc(verbose = FALSE)
    message("chunk ", j, " / ", length(idx))
  }
  out
}

## null  #####

m0 <- readRDS(file.path(data_dir, "m0_full_lngt_training_VIbayes_strict_v3seed101_small.rds"))
set.seed(1815)
lp_m0_5 <- chunk_log_probs(m0, ev)
saveRDS(lp_m0_5, file.path(data_dir, "lp_m0_5.rds"))
log_loss_m0_5 <- -mean(lp_m0_5) #0.5560409

## maturation  #####
m1 <- readRDS(file.path(data_dir, "m1_full_lngt_training_VIbayes_strict_v3seed101_small.rds"))
              
set.seed(1815)
lp_m1_5 <- chunk_log_probs(m1, ev)
saveRDS(lp_m1_5, file.path(data_dir, "lp_m1_5.rds"))
log_loss_m1_5 <- -mean(lp_m1_5) #0.5546585

## maturation and practice  #####
m2 <- readRDS(file.path(data_dir, "m2_full_lngt_training_VIbayes_strict_v3seed101_small.rds"))
set.seed(1815)
lp_m2_5 <- chunk_log_probs(m2, ev)
saveRDS(lp_m2_5, file.path(data_dir, "lp_m2_5_training.rds"))
log_loss_m2_5 <- -mean(lp_m2_5)#0.5540894


# item rating correlations  ####

#training data 
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
#item ratings in training data
item_obs_full <- train_data_full_strict %>%
  group_by(item_id) %>%
  summarise(avg_item_rating = mean(item_rating),
            n = n()) %>% 
  mutate(item_id = as.character(item_id))

rm(list = c("train_data_full_strict"))


#function to get the item estimates 
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


#model estimtes

#m0 
item_re_m0_vi_strict <- get_item_be(m0)
#m1 
item_re_m1_vi_strict <- get_item_be(m1)
#m2
item_re_m2_vi_strict_training <- get_item_be(m2)

# correlations
item_check_m0 <- left_join(item_re_m0_vi_strict, item_obs_full, by = "item_id")
cor(item_check_m0$avg_item_rating, item_check_m0$be, use = "complete.obs") #-0.8977283

item_check_m1 <- left_join(item_re_m1_vi_strict, item_obs_full, by = "item_id")
cor(item_check_m1$avg_item_rating, item_check_m1$be, use = "complete.obs") # -0.9543308

item_check_m2 <- left_join(item_re_m2_vi_strict_training, item_obs_full, by = "item_id")
cor(item_check_m2$avg_item_rating, item_check_m2$be, use = "complete.obs") #-0.9480473

# Table and plots for the manuscript ####
#PLOT
library(ggplot2)
library(dplyr)

item_check_all <- bind_rows(
  "Null"                   = item_check_m0,
  "Maturation"             = item_check_m1,
  "Maturation + Practice"  = item_check_m2,
  .id = "model"
) |>
  mutate(model = factor(model,
                        levels = c("Null", "Maturation", "Maturation + Practice")))


ggplot(item_check_all, aes(x = avg_item_rating, y = be)) +
  geom_point(aes(size = n), alpha = 0.3, stroke = 0) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.4, colour = "grey40") +
  scale_size_continuous(range = c(0.3, 3), guide = "none") +
  facet_wrap(~ model, nrow = 1) +
  labs(x = "Average observed list difficulty",
       y = "Estimated list difficulty") +
  theme_bw(base_size = 9) +
  theme(
    aspect.ratio = 1,
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey95", colour = NA),
    strip.text = element_text(face = "bold")
  )

item_check_scaled <- item_check_all |>
  mutate(across(c(be, avg_item_rating), ~ as.numeric(scale(.x))), .by = model)


item_check_all %>% 
  mutate(across(c(be, avg_item_rating), ~ as.numeric(scale(.x))), .by = model) |>
  ggplot(aes(avg_item_rating, -be)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(aes(size = n), alpha = 0.3, stroke = 0) +
  scale_size_continuous(range = c(0.3, 3), guide = "none") +
  facet_wrap(~ model, nrow = 1) +
  coord_fixed() +
  labs(x = "Average observed list difficulty (z)",
       y = "Estimated list difficulty (z)") +
  theme_bw(base_size = 9)


