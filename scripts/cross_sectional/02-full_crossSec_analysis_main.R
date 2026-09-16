library(dplyr)
library(lubridate)
library(tidyr)
# In this section, we want to make sure whether mean, max, medium or 90th percentile set size explain the capacity the best. Here we have an additional data selection. 

##Data and Analysis 1####

mole_cross_sectional <- readRDS(file.path(data_dir, "mole_cross_sectional.rds"))

mole_cross_sectional_second_semester <- mole_cross_sectional %>% 
  mutate(created = ymd_hms(created)) %>% 
  mutate(month_played = month(created)) %>% 
  filter(month_played %in% c(2,3,4,5,6,7))

mole_second <- mole_cross_sectional_second_semester %>% 
  mutate(
    created = ymd_hms(created),
    date    = as.Date(created)
  )

daily_counts_by_student <- mole_second %>%  #every row is a unique day with item count for each student. Takes too much time 
  count(user_id, date, name = "n") %>%  
  group_by(user_id) %>% 
  complete(
    date = seq(min(date), max(date), by = "day"),
    fill = list(n = 0L) # if they did not played in that day = 0
  ) %>% 
  arrange(user_id, date)

library(slider)


windows_by_student <- daily_counts_by_student %>% 
  group_by(user_id) %>% 
  mutate(
    first_date = min(date),
    window_sum = slide_dbl(
      n,
      sum,
      .before   = 89,     # Look back 89 days 
      .complete = FALSE    #allow <90-day 
    )
  ) %>% 
  slice_max(window_sum, n = 1, with_ties = FALSE) %>% 
  mutate(
    end_date   = date,
    start_date = if_else(
      end_date - 89L < first_date,
      first_date,         # student has <90 days 
      end_date - 89L      # student has ≥90 days
    )
  ) %>% 
  mutate(range_date = as.integer(end_date - start_date) + 1) %>% 
  ungroup() %>% 
  dplyr::select(user_id, start_date, end_date,range_date, window_sum)

mole_second_90days_per_student <- mole_second %>% 
  mutate(date = as.Date(created)) %>% 
  inner_join(windows_by_student, by = "user_id") %>% 
  filter(date >= start_date, date <= end_date)

mole_second_90days_per_student_2of3session <- mole_second_90days_per_student %>%
  group_by(user_id) %>% 
  filter(n() >= 30) %>% 
  arrange(created, .by_group = TRUE) %>% 
  mutate(item_order_in_window = row_number()) %>% 
  filter(item_order_in_window > 10) %>% 
  ungroup()

saveRDS(mole_second_90days_per_student_2of3session, file = file.path(data_dir, "mole_second_90days_per_student_2of3session_full.rds"))

dist_set_struct <- mole_second_90days_per_student_2of3session %>%
  count(set_size, structured_count, name = "n_plays") %>%
  arrange(set_size, structured_count) %>%
  group_by(set_size) %>%
  mutate(
    pct_within_set = n_plays / sum(n_plays) * 100
  ) %>%
  ungroup()

library(ggplot2)

p0 <- ggplot(dist_set_struct,
       aes(x = factor(set_size),
           y = factor(structured_count),
           fill = n_plays)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n_plays), size = 3, color = "black") +
  scale_fill_gradient(low = "grey90", high = "grey40") +
  labs(
    x = "Set Size",
    y = "Structure Level",
    fill = "Number of Plays"
  ) +
  theme_classic(base_family = "Arial") +
  theme(
    text             = element_text(color = "black"),
    axis.text        = element_text(color = "black"),
    axis.line        = element_line(color = "black"),
    axis.ticks       = element_line(color = "black"),
    legend.position  = "right",
    legend.title     = element_text(size = 10),
    plot.caption     = element_text(hjust = 0, size = 10, face = "italic"),
    plot.title       = element_blank()
  )

ggsave(file.path(data_dir, "method2_supp.png"), plot = p0, width = 8, height = 5, dpi = 300)


##Analyses 2 ####

latest_rating <- mole_second_90days_per_student_2of3session %>%
  filter(difficulty == 2) %>% 
  mutate(created_posix = as.POSIXct(created)) %>%
  group_by(user_id, grade) %>%
  summarise(latest_created = max(created_posix),
            latest_rating  = new_user_domain_rating[which.max(created_posix)],
            .groups = "drop"
  ) %>% 
  dplyr::select(-c(grade, latest_created))

logs_setSize_correct <- mole_second_90days_per_student_2of3session %>%
  filter(correct_answered == 1) %>% 
  filter(difficulty == 2) %>% 
  mutate(created_posix = as.POSIXct(created)) %>%
  group_by(user_id, grade) %>%
  summarise(max_set = max(set_size),
            mean_set = mean(set_size),
            median_set = median(set_size),
            p90_set        = quantile(set_size, 0.90),
            .groups = "drop"
  )

logs_summary <- merge(logs_setSize_correct,latest_rating, by="user_id" )

library(BayesFactor)
### Max, Mean and Median
setsize_model_hard <- generalTestBF(
  latest_rating ~ max_set + mean_set + median_set + p90_set, 
  data = logs_summary)

setsize_model_hard
#[1] max_set                                    : 2.797088e+7097  ±0.01%
#[2] mean_set                                   : 9.706372e+11442 ±0.01%
#[4] median_set                                 : 3.534836e+8990  ±0.01%
#[8] p90_set                                    : 2.054204e+7145  ±0.01%


model_mean <- lm(latest_rating ~ mean_set, data = logs_summary)
summary(model_mean)$r.squared
# 0.8113699


## Analysis 3 - Main Analysis - WM Capacity Measure Development ####

library(data.table)

# Load and convert
mole <- as.data.table(readRDS(file.path(data_dir, "mole_second_90days_per_student_2of3session_full.rds")))

# Filter correct answers
mole_correct <- mole[correct_answered == 1]

# Per-student summary by set size
mole_correct_summary <- mole_correct[,
                                     .(set_size_max  = max(set_size),
                                       set_size_mean = mean(set_size),
                                       n             = .N),
                                     by = .(user_id, structured_count, grade)
]

# Filter out structured_count 5,6,7 once
mole_filtered <- mole_correct_summary[
  !structured_count %in% c(5, 6, 7,8)
][, grade := as.factor(grade)]

mole_filtered$grade <- as.ordered(mole_filtered$grade)


length(unique(mole_filtered$user_id)) 
nrow(mole_filtered) 

### Regression Models 

### model selection ####
#library(BayesFactor)
#generalTestBF(set_size_mean ~ grade * structured_count, data = mole_filtered)

# [3] grade + structured_count                          : 1.063828e+60010 ±4.25%
# [4] grade + structured_count + grade:structured_count : 5.91021e+61188  ±0.71%
# Against denominator:Intercept only 
# Bayes factor type: BFlinearModel, JZS

### final model ####
library(brms)

#MCMC

pri = c(
  # Population-level intercept
  prior(normal(3, 1.5), class = "Intercept"), # Baseline: Grade 3, Unstructured
  # Fixed effects (βgrade, βcount, βint)
  prior(normal(0.5, 0.2), class = "b", coef = "mograde"),      # grade effect
  prior(normal(0.5, 0.2), class = "b", coef = "structured_count"), # structured count effect
  prior(normal(0, 0.05), class = "b", coef = "mograde:structured_count"),  #Negative effects may accur: lower gardes might benefit sturucture more
  # Residual SD (trial-level)
  prior(normal(0, 0.5), class = "sigma"),
  # Random intercept SD
  prior(normal(0, 1), class = "sd", group = "user_id") #SDs and sigma is positive in brms
)

meanSetsize_bayes_intr_full_MCMC101 <- brm(
  set_size_mean ~ mo(grade) * structured_count + (1|user_id),
  data = mole_filtered,
  family = gaussian(),
  prior = pri,
  backend   = "cmdstanr",
  chains = 4, iter = 2000, warmup = 1000, cores = 4, threads= threading(4),
  save_pars = save_pars(all = FALSE),
  seed = 101)
saveRDS(meanSetsize_bayes_intr_full_MCMC101, file.path(data_dir, "meanSetsize_bayes_intr_full_MCMC101.rds"))

# Prediction Plots
library(brms); library(ggplot2)

fit <- meanSetsize_bayes_intr_full_MCMC101
dat <- fit$data

grid <- expand.grid(
  structured_count = 0:4,
  grade            = sort(unique(dat$grade))
)

ep   <- fitted(fit, newdata = grid, re_formula = NA) #no group-level effects 
pred <- cbind(grid, as.data.frame(ep))

pred$grade_c <- as.integer(as.character(pred$grade)) - 2

library(papaja)   
p1 <- ggplot(pred, aes(structured_count, Estimate,
                       colour = factor(grade_c), fill = factor(grade_c))) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.7) +
  scale_colour_viridis_d(end = 0.88) + scale_fill_viridis_d(end = 0.88) +
  scale_x_continuous(breaks = 0:4) +
  labs(x = "Structure Count", y = "WM Capacity Measure",
       colour = "Grade", fill = "Grade") +
  theme_apa()

p2 <- ggplot(pred, aes(grade_c, Estimate,
                       colour = factor(structured_count),
                       fill   = factor(structured_count))) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.7) +
  scale_colour_viridis_d(end = 0.88) + scale_fill_viridis_d(end = 0.88) +
  scale_x_continuous(breaks = sort(unique(pred$grade_c))) +
  labs(x = "Grade", y = "WM Capacity Measure",
       colour = "Structure Count", fill = "Structure Count") +
  theme_apa()

library(patchwork)

p_both <- (p1 | p2) + plot_annotation(tag_levels = "A")

ggsave(file.path(data_dir, "supp_cross_pred.png"), p_both,
       width = 11, height = 4.5, dpi = 300)

### plot ####
#data
mole <- as.data.table(readRDS("~/research-collaboration/data/2024-WM-Dev/full/mole_second_90days_per_student_2of3session_full.rds"))

# Filter correct answers
mole_correct <- mole[correct_answered == 1]

# Per-student summary by set size
mole_correct_summary <- mole_correct[,
                                     .(set_size_max  = max(set_size),
                                       set_size_mean = mean(set_size),
                                       n             = .N),
                                     by = .(user_id, structured_count, grade)
]

# compute mean + CI columns

ci_cols <- function(x) {
  m  <- mean(x, na.rm = TRUE)
  s  <- sd(x,   na.rm = TRUE)
  n  <- sum(!is.na(x))
  se <- s / sqrt(n)
  list(n = n, mean_setsize = m, sd_setsize = s, se_setsize = se,
       ci_lower = m - 1.96 * se, ci_upper = m + 1.96 * se)
}

# Filter out structured_count 5,6,7 and turn grade level to intern. 
mole_filtered <- mole_correct_summary[
  !structured_count %in% c(5, 6, 7, 8)
][, grade := grade - 2]



# Means by grade x structured_count
means_setSize <- mole_filtered[,
                               ci_cols(set_size_mean),
                               by = .(grade, structured_count)
]

# Overall means by grade only
overall_means <- mole_filtered[,
                               ci_cols(set_size_mean),
                               by = .(grade)
]

library(papaja)
library(dplyr)
library(ggplot2)
plot_mean_setsize <- means_setSize %>%
  ggplot(aes(x = grade, 
             y = mean_setsize,
             group = as.factor(structured_count),
             colour = as.factor(structured_count))) +
  
  geom_point(size = 2) +
  geom_line(size = 1) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.2
  ) +
  geom_line(
    data = overall_means,
    aes(x = grade, y = mean_setsize, group = 1),
    colour = "black",
    size = 1.3,
    linetype = "dashed",
    inherit.aes = FALSE
  ) +
  geom_point(
    data = overall_means,
    aes(x = grade, y = mean_setsize),
    colour = "black",
    size = 3,
    inherit.aes = FALSE
  ) +
  geom_text(
    aes(label = n),
    size = 2.5,
    vjust = 2,
    hjust = - 0,
    position = position_dodge(width = 0.2),
    show.legend = FALSE
  ) +
  scale_colour_brewer(palette = "Set2") +  
  scale_x_continuous(breaks = 1:6) +
  labs(x = "Grade", y = "Mean of WM Capacity Measure", colour = "Structure Count") +
  theme(
    axis.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
    legend.title = "Structure Count"
  ) + 
  theme_minimal() +
  theme(axis.line = element_line(colour = "black", linewidth = 0.5))


plot_mean_setsize
ggsave(file.path(data_dir, "results_cross1.png"), plot = plot_mean_setsize, width = 8, height = 5, dpi = 300)
