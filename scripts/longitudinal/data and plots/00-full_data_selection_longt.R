library(tidyr)
library(dplyr)
library(lubridate)

data_dir <- "data"

#logs data by year (2019-2025), loaded and bound together
logs_40 <- do.call(rbind, lapply(2019:2025, function(y) {
  load(file.path(data_dir, paste0("data", y, "_2026_06_02.Rdata")))
  one_year
}))

#user data 
load(file.path(data_dir, "user_info_2026_06_02.Rdata"))


###### Longitudinal Data #######

# ============================================================
### logs data ###
# ============================================================
# calculate the age and whether it makes sense that they are in that grade level as some students have the wrong grade info


user_info <- user_info %>%
  drop_na(date_of_birth) %>%
  dplyr::mutate(date_of_birth = ymd(date_of_birth),
                date_of_birth_year = year(ymd(date_of_birth))) %>% 
  dplyr::select(-grade)
  

logs_40 <- logs_40 %>%
  inner_join(user_info, by = "user_id")

#Compute the age of students in years and months, select relevant grades

logs_40 <- logs_40 %>%
  filter(grade %in% 3:8) %>%
  mutate(age_during_game = as.numeric(as.Date(created, format="%Y-%m-%d") - as.Date(date_of_birth, format="%Y-%m-%d"))/ 365.25)


user_stats <- logs_40 %>% # compute the 5% extremes on each end
  group_by(user_id) %>%
  summarise(min_age = min(age_during_game),
            min_grade = min(grade))

grade_stats <- user_stats %>%
  group_by(min_grade) %>%
  summarise(q_low = quantile(min_age, probs = 0.05),
            q_high = quantile(min_age, probs = 0.95))

user_stats <- user_stats %>% left_join(grade_stats)

user_sel <- user_stats %>%
  filter(min_age >= q_low,
         min_age <= q_high)

logs_40 <- logs_40 %>%
  filter(user_id %in% user_sel$user_id)

rm(user_info, user_sel, user_stats, grade_stats)

#each kid at least 30 items

mole_logs <- logs_40 %>%
  group_by(user_id) %>%
  filter(n() >= 30) %>%
  ungroup()

rm(logs_40)

# ============================================================
# ##### Game Confusion #########
# ============================================================
#Exclude the first 10 items to rule out the confusion in the beginning of game

mole_logs <- mole_logs %>%
  arrange(user_id, created) %>%
  group_by(user_id) %>%
  slice(-(1:10)) %>%
  ungroup()

# ============================================================
# Time Coding 
# ============================================================
#Now we define sessions

mole_logs_sessions <- mole_logs %>%
  mutate(created_date = as.Date(created)) %>%
  arrange(user_id, created) %>%  # this works as the format is "YYYY-MM-DD HH:MM:SS"
  group_by(user_id) %>%
  mutate(
    gap_from_prev_day = as.integer(created_date - lag(created_date)),
    
    # each day = new session
    new_session_daily = if_else(is.na(gap_from_prev_day) | gap_from_prev_day >= 1, 1L, 0L),
    session_id_daily = cumsum(new_session_daily),
    session_gap_days_firstrow_daily = if_else(new_session_daily == 1L, coalesce(gap_from_prev_day, 0L), NA_integer_)
  ) %>%
  
  # Daily session metrics
  group_by(user_id, session_id_daily) %>%
  mutate(
    day_gap_between_sessions_daily = first(session_gap_days_firstrow_daily),
    item_index_in_session_daily = row_number()
  ) %>% ungroup()


#days_since_first_session

mole_long <- mole_logs_sessions %>%
  group_by(user_id) %>%
  mutate(
    first_day = min(as.Date(created), na.rm = TRUE),
    days_since_first_day = as.integer(as.Date(created) - first_day)
  ) %>%
  ungroup()

# ============================================================
#  DATA PREPARATION FOR THE MODELLING
# ============================================================
mole_long <- mole_long %>%
  mutate(difficulty_num = as.numeric(difficulty),
         difficulty_f   = relevel(factor(as.numeric(difficulty)), ref = "1")) %>%
  arrange(user_id, new_user_domain_modified_count) %>%
  group_by(user_id) %>%
  mutate(starting_age = round(first(age_during_game))) %>%
  ungroup()  
# ============================================================
### Item Filtering ####
# ============================================================
mole_preReg_long <- readRDS(file.path(data_dir, "mole_preReg_long.rds"))

filter_mole_items <- function(df, user_min_n) {
  
  df <- df %>%
    filter(item_id %in% mole_preReg_long$item_id ) %>% 
    group_by(user_id) %>% filter(n() > user_min_n) %>% ungroup() %>%
    filter(item_index_in_session_daily <= 50)
  
  singleton_sessions <- df %>%
    group_by(user_id, session_id_daily) %>%
    summarise(n_items = n(), .groups = "drop") %>%
    filter(n_items == 1) %>%
    mutate(user_session_index = paste(user_id, session_id_daily, sep = "-")) %>%
    pull(user_session_index)
  
  df %>%
    mutate(user_session_index = paste(user_id, session_id_daily, sep = "-")) %>%
    filter(!user_session_index %in% singleton_sessions)
}

# APPLY
mole_long_strict   <- filter_mole_items(mole_long, user_min_n = 9)

rm(list = setdiff(ls(), c("mole_long_strict")))

#### we also need to exclude children from prereg #####
mole_long_strict <- mole_long_strict %>% 
  filter(!(user_id %in% mole_preReg_long$user_id)) #now 124,120

saveRDS(mole_long_strict, file.path(data_dir, "mole_long_strict.rds"))
