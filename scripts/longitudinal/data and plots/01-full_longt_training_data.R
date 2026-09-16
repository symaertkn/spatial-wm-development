
# Training/Test Data split for model fits

library(dplyr)

#################### STRICT ######################

mole_long_strict <- readRDS(file.path(data_dir, "mole_long_strict.rds"))

#split the data set for model selection

set.seed(2026)

mole_long_split <-
  mole_long_strict %>%
  group_by(user_id, grade) %>%
  mutate(
    split = if_else(
      row_number() %in% sample(seq_len(n()), size = ceiling(0.25 * n())),
      "test",
      "train"
    )
  ) %>%
  ungroup()

train_data <- mole_long_split %>% 
  filter(split == "train")

test_data <- mole_long_split %>% 
  filter(split == "test")

#check data 
#we include items from prereg training data and make sure every item_id in test occurs in the train 
prereg_train <- readRDS(file.path(data_dir, "train_data_filtered"))

train_data_filtered <- train_data %>% 
  filter(item_id %in% prereg_train$item_id) %>%
  ungroup() 

length(unique(train_data_filtered$user_id)) 

train_data_filtered <- train_data_filtered %>% 
  group_by(user_id) %>% 
  filter(n()>9) %>% 
  ungroup()

train_users <- unique(train_data_filtered$user_id)
train_items <- unique(train_data_filtered$item_id)


test_data_filtered <- test_data %>% #we need test data with the same users
  filter(user_id %in% train_users)

test_data_filtered <- test_data_filtered %>% #we need test data with the same items
  filter(item_id %in% train_items)

rm(list = setdiff(ls(), c("train_data_filtered", "test_data_filtered")))

#scaling 

# Variables to scale
scale_vars <- c(
  "item_index_in_session_daily",
  "day_gap_between_sessions_daily", 
  "days_since_first_day",
  "session_id_daily",
  "starting_age",
  "new_user_domain_rating",
  "item_rating"
)

# Compute scale parameters on TRAIN only
scale_params <- lapply(scale_vars, function(v) {
  list(
    center = mean(train_data_filtered[[v]], na.rm = TRUE),
    scale  = sd(train_data_filtered[[v]],   na.rm = TRUE)
  )
})
names(scale_params) <- scale_vars

# Function to apply train parameters to any dataset
apply_scaling <- function(df) {
  df %>%
    mutate(
      item_index_in_session_daily_scaled    = as.numeric(scale(item_index_in_session_daily,    center = scale_params$item_index_in_session_daily$center,    scale = scale_params$item_index_in_session_daily$scale)),
      day_gap_between_sessions_daily_scaled = as.numeric(scale(day_gap_between_sessions_daily, center = scale_params$day_gap_between_sessions_daily$center, scale = scale_params$day_gap_between_sessions_daily$scale)),
      days_since_first_day_scaled           = as.numeric(scale(days_since_first_day,           center = scale_params$days_since_first_day$center,           scale = scale_params$days_since_first_day$scale)),
      session_id_daily_scaled               = as.numeric(scale(session_id_daily,               center = scale_params$session_id_daily$center,               scale = scale_params$session_id_daily$scale)),
      starting_age_scaled                   = as.numeric(scale(starting_age,                   center = scale_params$starting_age$center,                   scale = scale_params$starting_age$scale)),
      elo_logit                             = as.numeric(scale(new_user_domain_rating,          center = scale_params$new_user_domain_rating$center,         scale = scale_params$new_user_domain_rating$scale)),
      item_scaled                           = as.numeric(scale(item_rating,                     center = scale_params$item_rating$center,                    scale = scale_params$item_rating$scale))
    )
}

# Apply to both
train_data_full <- apply_scaling(train_data_filtered)
test_data_full  <- apply_scaling(test_data_filtered)

rm(list = setdiff(ls(), c("train_data_full", "test_data_full")))

saveRDS(train_data_full, file.path(data_dir, "train_data_full_strict.rds"))
saveRDS(test_data_full, file.path(data_dir, "test_data_full_strict.rds"))

