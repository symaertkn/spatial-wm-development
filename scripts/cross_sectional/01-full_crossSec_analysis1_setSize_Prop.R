library(ggplot2)
library(cowplot)
library(dplyr)
library(BayesFactor)
library(brms)
library(lubridate)
library(tidyr)
library(slider)

mole_cross_sectional <- readRDS(file.path(data_dir, "mole_cross_sectional.rds"))

#change the Dutch grade level to international levels 
mole_cross_sectional <- mole_cross_sectional %>% 
  mutate(grade = grade - 2)

#correct answered
mole_cross_sectional_correct <- mole_cross_sectional %>% 
  filter(correct_answered == 1)

length(unique(mole_cross_sectional_correct$user_id)) #114,410
length(unique(mole_cross_sectional_correct$item_id)) #720
nrow(mole_cross_sectional_correct)#5,834,873
table(mole_cross_sectional_correct$difficulty)
#0       1       2 
#1332662 1672488 2829723 

#LATEST ABILITY
mole_cross_sectional_correct_latest <- mole_cross_sectional_correct %>% 
  group_by(user_id) %>% 
  filter(new_user_domain_modified_count == max(new_user_domain_modified_count)) %>% 
  ungroup()

length(unique(mole_cross_sectional_correct_latest$user_id)) #114,410
nrow(mole_cross_sectional_correct_latest)  #114,410
length(unique(mole_cross_sectional_correct_latest$item_id)) #696
table(mole_cross_sectional_correct$difficulty)
#0       1       2 
#1332662 1672488 2829723 

##### Data Prep for the Plot

####--LATEST TRIAL-------
mole_cross_sectional_correct_latest %>%
  mutate(grade = as.factor(grade), difficulty = as.factor(difficulty)) %>% 
  group_by(grade) %>%
  summarise(total = n()) -> total_count_per_grade_latest

mole_cross_sectional_correct_latest %>%
  mutate(grade = as.factor(grade), difficulty = as.factor(difficulty)) %>% 
  group_by(grade, set_size, difficulty) %>%
  count(set_size) %>%
  left_join(total_count_per_grade_latest, by = "grade") %>%
  mutate(proportion = n / total) -> data_with_proportions_latest

# Set-size proportion for each grade level by difficulty (Only-Correct-Items)(Cross-Sectional)
data_with_proportions_latest <- data_with_proportions_latest %>%
  mutate(difficulty_text = factor(case_when(
    difficulty == 0 ~ "Easy",
    difficulty == 1 ~ "Medium",
    difficulty == 2 ~ "Hard"
  ), levels = c("Easy", "Medium", "Hard"))) %>% 
  filter(set_size < 8)

### Plot ####
library(papaja)
p2 <- ggplot(data_with_proportions_latest, aes(x = factor(as.integer(set_size)), y = proportion)) +
  facet_grid(grade ~ difficulty_text, scales = "free_y", space = "free") +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = round(proportion, 2)),
    vjust = -0.5,
    position = position_dodge(width = 0.9),
    check_overlap = TRUE,
    size = 2
  ) +
  scale_fill_brewer(palette = "Set2") +
  xlab("Set Size") +
  ylab("Proportion") +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 10, face = "bold"),
    strip.background = element_blank(),
    strip.text = element_text(size = 10, face = "bold"),
    plot.margin = margin(5.5, 55, 5.5, 5.5) 
  ) +
  ylim(0, 0.4) +
  theme(axis.line = element_line(colour = "black", linewidth = 0.5))

ggdraw() +
  draw_plot(p2) +
  draw_label("Grade",
             x = 0.95, y = 0.55,                    
             hjust = -0.15, vjust = 0.5,        
             angle = -90, fontface = "bold", size = 10)

ggsave(file.path(data_dir, "cross_anly1_latest.png"), width = 8, height = 6, dpi = 300)

### Chi-Square Test - LATEST TRIALS ####

#### HARD ####
mole_cross_sectional_correct_hard <- mole_cross_sectional_correct_latest %>%
  filter(difficulty == 2,  set_size < 8) %>%  
  mutate(grade = as.factor(grade))

setsize_counts_hard <- mole_cross_sectional_correct_hard %>%
  group_by(grade, set_size) %>%
  summarise(count = n(), .groups = "drop")

total_by_grade_hard <- setsize_counts_hard %>%
  group_by(grade) %>%
  summarise(total = sum(count), .groups = "drop")

setsize_table_hard <- setsize_counts_hard %>%
  tidyr::pivot_wider(
    names_from = set_size,
    values_from = count,
    values_fill = 0
  )
setsize_table_hard

# matrix for BayesFactor
setsize_matrix_hard <- as.matrix(setsize_table_hard %>% dplyr::select(-grade))
rownames(setsize_matrix_hard) <- setsize_table_hard$grade   # was $grade on the matrix

setsize_model_hard <- contingencyTableBF(setsize_matrix_hard, sampleType = "poisson") #Null model (independence) ->  grade and set_size are independent.

#### MEDIUM ####

mole_cross_sectional_correct_medium <- mole_cross_sectional_correct_latest %>%
  filter(difficulty == 1,   set_size < 8) %>%  
  mutate(grade = as.factor(grade))

setsize_counts_medium <- mole_cross_sectional_correct_medium %>%
  group_by(grade, set_size) %>%
  summarise(count = n(), .groups = "drop")

total_by_grade_medium <- setsize_counts_medium %>%
  group_by(grade) %>%
  summarise(total = sum(count), .groups = "drop")

setsize_table_medium <- setsize_counts_medium %>%
  tidyr::pivot_wider(
    names_from = set_size,
    values_from = count,
    values_fill = 0
  )
setsize_table_medium

# matrix for BayesFactor
setsize_medium_matrix <- as.matrix(setsize_table_medium %>% dplyr::select(-grade))
rownames(setsize_medium_matrix) <- setsize_table_medium$grade

setsize_model_medium <- contingencyTableBF(setsize_medium_matrix, sampleType = "poisson") #Null model (independence) ->  grade and set_size are independent.

setsize_model_medium


#### EASY ####

mole_cross_sectional_correct_easy <- mole_cross_sectional_correct_latest %>%
  filter(difficulty == 0,  set_size < 8) %>%  
  mutate(grade = as.factor(grade))

setsize_counts_easy <- mole_cross_sectional_correct_easy %>%
  group_by(grade, set_size) %>%
  summarise(count = n(), .groups = "drop")

total_by_grade_easy <- setsize_counts_easy %>%
  group_by(grade) %>%
  summarise(total = sum(count), .groups = "drop")

setsize_table_easy <- setsize_counts_easy %>%
  tidyr::pivot_wider(
    names_from = set_size,
    values_from = count,
    values_fill = 0
  )
setsize_table_easy

# matrix for BayesFactor
setsize_easy_matrix <- as.matrix(setsize_table_easy %>% dplyr::select(-grade))
rownames(setsize_easy_matrix) <- setsize_table_easy$grade

setsize_model_easy <- contingencyTableBF(setsize_easy_matrix, sampleType = "poisson") #Null model (independence) ->  grade and set_size are independent.

setsize_model_easy

#make a table to call in manuscript 

library(BayesFactor)

models <- list(easy = setsize_model_easy,
               medium = setsize_model_medium,
               hard = setsize_model_hard)

bf_table_latest <- data.frame(
  condition = names(models),
  log10BF   = sapply(models, function(m) extractBF(m, logbf = TRUE)$bf / log(10))
)

saveRDS(bf_table_latest, file.path(data_dir, "cross_anly1_bf_table_latest.rds"))
