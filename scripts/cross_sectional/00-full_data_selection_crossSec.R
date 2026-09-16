library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)

# ---- Paths ----
# All data files are in a folder called "data", next to this script.
# The filtered data are saved to the same folder.

# The data folder must contain:
#   data2019_2026_06_02.Rdata ... data2025_2026_06_02.Rdata   (yearly logs)
#   user_info_2026_06_02.Rdata                                 (user info)
#   items.Rdata                                                (item info)
#   mole_TenPercent_cross_sectional.rds                        (children used for the preregistration)

data_dir <- "data"

#logs data by year (2019-2025), loaded and bound together
logs_40 <- do.call(rbind, lapply(2019:2025, function(y) {
  load(file.path(data_dir, paste0("data", y, "_2026_06_02.Rdata")))
  one_year
}))

#user data 
load(file.path(data_dir, "user_info_2026_06_02.Rdata"))
#item data 
load(file.path(data_dir, "items.Rdata"))

###### Cross-Sectional Data #######

################## Data Selection Explanation ##########################################################################################

#1) to ensure reliable age and grade information, we excluded students without a recorded date of birth and trimmed the 10% most
#extreme age values within each grade, as these likely reflected incorrect grade assignments. 
#2) we included only children in grades three through eight of the Dutch school system. 
#3) to ensure that each student’s ability was estimated from a single grade level, we retained only
#data from the most recent school year for students who had completed at least 30 items (three sessions), not necessarily consecutively.

#########################################################################################################################################

### item characteristics #########

#first add set size and structure variable to the items and then to the logs

items_40 <- items %>%
  filter(domain_id == "40")

#turn answer options into coordinates
items_40$coordinates <- lapply(
  regmatches(items_40$answer_options, gregexpr("[0-9]", items_40$answer_options)),
  function(x) as.numeric(x)
)
items_40 <- items_40 %>%
  mutate(item_id = id) %>%
  dplyr::select(item_id, coordinates, answer_options, rating )

for (i in 1:nrow(items_40)) {
  coords <- items_40$coordinates[[i]]
  
  # Turn into coordinate pairs
  mat <- matrix(coords, ncol = 2, byrow = TRUE)
  
  items_40$coord_matrix[[i]] <- mat
}


#Structured vs Unstructured

items_40$structured_count <- 0

for (i in 1:nrow(items_40)) {
  coord_matrix <- items_40$coord_matrix[[i]]
  
  # Check if coord_matrix exists and has at least 2 rows
  if (is.null(coord_matrix) || nrow(coord_matrix) < 2) {
    items_40$structured_count[i] <- 0
    next
  }
  
  structured_counter <- 0
  
  for (k in 2:nrow(coord_matrix)) {
    dx <- coord_matrix[k, 1] - coord_matrix[k - 1, 1]
    dy <- coord_matrix[k, 2] - coord_matrix[k - 1, 2]
    
    if (dx == 0 || dy == 0 || abs(dx) == abs(dy)) {
      structured_counter <- structured_counter + 1
    }
  }
  
  items_40$structured_count[i] <- structured_counter
}

# set size

items_40$answer_options_clean <-
  sapply(regmatches(items_40$answer_options, gregexpr("[0-9]", items_40$answer_options)), paste, collapse = "")

items_40 <- items_40 %>%
  mutate(set_size = nchar(answer_options_clean)/2)


keep <- c("logs_40",
          "user_info",
          "items_40")

rm(list = setdiff(ls(), keep))

##############


### logs data ###

# calculate the age and whether it makes sense that they are in that grade level as some students have the wrong grade info

user_info = drop_na(user_info, date_of_birth) 
user_info <- mutate(user_info, date_of_birth = ymd(date_of_birth))
user_info <- mutate(user_info, date_of_birth_year = year(ymd(date_of_birth)))
user_info <- user_info %>% select(-grade)

logs_40_birthday <- inner_join(logs_40,user_info, by="user_id")

#Compute the age of students
logs_40_age <- mutate(logs_40_birthday,
                      age_during_game = as.numeric(as.Date(created, format="%Y-%m-%d") - as.Date(date_of_birth, format="%Y-%m-%d"))/ 365.25)

rm(list = c("logs_40", "logs_40_birthday"))


logs_40_age_trimmed<- logs_40_age %>%
  # compute the 5% extremes on each end
  group_by(grade) %>%
  mutate(q_low = quantile(age_during_game, probs = 0.05),
         q_high = quantile(age_during_game, probs = 0.95)) %>%
  # remove the datapoints where children are younger or older than the 10% extremes per grade
  filter(age_during_game > q_low & age_during_game < q_high)

rm(list = c("logs_40_age", "user_info"))


#each kid:
# - grade/group 3 to 8
#- only last school year and cross-sectional data
# - at least 30 items in the year they are sellected


mole_lastgrade <- logs_40_age_trimmed %>%
  group_by(user_id, grade) %>%
  # keep only grades where the user has at least 30 items
  filter(n() >= 30) %>% 
  ungroup()


mole_lastgrade <- mole_lastgrade %>%  # now within each user, find the latest grade with ≥30 items
  group_by(user_id) %>%
  filter(grade == max(grade)) %>%
  ungroup()


mole_lastgrade_grade3to8 <- mole_lastgrade %>%
  filter(grade %in% 3:8)

# add item characteristics to the logs data

items_40 <- items_40 %>% select(item_id, rating, structured_count, set_size)

mole_cross_sectional <- merge(mole_lastgrade_grade3to8, items_40, by = "item_id")


rm(list = c("mole_lastgrade", "mole_lastgrade_grade3to8", "items_40", "logs_40_age_trimmed"))

#Exclude children from preregistration
mole_TenPercent_cross_sectional <- readRDS(file.path(data_dir, "mole_TenPercent_cross_sectional.rds"))

mole_cross_sectional <- mole_cross_sectional %>% 
  filter(!(user_id %in% mole_TenPercent_cross_sectional$user_id)) 


saveRDS(mole_cross_sectional, file.path(data_dir, "mole_cross_sectional.rds"))

