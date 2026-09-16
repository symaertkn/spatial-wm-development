library(dplyr)
library(ggplot2)
library(scales)
library(jtools)

# ---- data --------------------------------------------------------

mole_long_strict <- readRDS(file.path(data_dir, "mole_long_strict.rds"))

# ---- trials per user --------------------------------------------------------
plays_per_user <- mole_long_strict %>%
  count(user_id, name = "n_plays")
  
p_trials <- ggplot(plays_per_user, aes(n_plays)) +
  geom_histogram(bins = 80, fill = "steelblue", colour = "white") +
  coord_cartesian(xlim = c(0, 500)) +          # cut the extreme cases
  labs(x = "Trials per user", y = "Children") +
  theme_apa()

p_trials

# ---- duration per user ------------------------------------------------------
max_days <- mole_long_strict %>%
  group_by(user_id) %>%
  summarise(max_day = max(days_since_first_day), .groups = "drop")

cuts  <- c(182, 730, 1460)
ann <- data.frame(
  x     = cuts,
  label = c("6 months+", "2 years+", "4 years+"),
  vjust = c(1.5, 4.5, 6)
)

ann$label <- paste0(ann$label, "\nn = ", sapply(cuts, \(d) sum(max_days$max_day >= d)))

p0 <- ggplot(max_days, aes(max_day)) +
  geom_histogram(bins = 80, fill = "steelblue", colour = "white") +
  geom_vline(xintercept = cuts, linetype = "dashed", colour = "grey30") +
  geom_text(data = ann, aes(x, Inf, label = label, vjust = vjust),
            inherit.aes = FALSE, hjust = -0.1, size = 3, colour = "grey30") +
  labs(x = "Game duration in days per user", y = "Children") +
  theme_minimal()

ggsave(file.path(data_dir, "supp_long_data_descrp.png"), p0, width = 6, height = 3, dpi = 300)  

# ---- children behavior in long data  ---------------------------------------
#selecting children who was in the system more than 4 years- 1460 days
lt_ids <- mole_long_strict %>%
  group_by(user_id) %>%
  summarise(max_day = max(days_since_first_day), .groups = "drop") %>%
  filter(max_day >= 1460) %>%
  pull(user_id)

mole_long_strict_LT <- mole_long_strict %>%
  filter(user_id %in% lt_ids)

length(unique(mole_long_strict_LT$user_id)) 

#selecting children who started in grade 3 in 2019

gr3_id <- mole_long_strict_LT %>%
  mutate(created_date = as.Date(created)) %>%
  group_by(user_id) %>%
  summarise(grade_min = min(as.integer(as.character(grade)), na.rm = TRUE),
            year_min  = as.integer(format(min(created_date), "%Y")),
            .groups   = "drop") %>%
  filter(grade_min == 3, year_min == 2019) %>%
  pull(user_id)

  
mole_long_strict_LT_gr3 <- mole_long_strict_LT %>%
  filter(user_id %in% gr3_id)
  
#selecting children who played at least 10 correct items at least in 3 days 

threeTimes_id <- mole_long_strict_LT_gr3 %>%
  mutate(created_date = as.Date(created)) %>%
  count(user_id, created_date, name = "n_trials") %>%   # trials per child per day
  filter(n_trials >= 10) %>%                            # keep qualifying days
  count(user_id, name = "day_num") %>%                  # how many such days
  filter(day_num >= 10) %>%
  pull(user_id)

mole_long_strict_LT_gr3_days <- mole_long_strict_LT_gr3 %>%
  filter(user_id %in% threeTimes_id)

# compute set size 

#item data 
load(file.path(data_dir, "items.Rdata"))

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

# set size

items_40$answer_options_clean <-
  sapply(regmatches(items_40$answer_options, gregexpr("[0-9]", items_40$answer_options)), paste, collapse = "")

items_40 <- items_40 %>%
  mutate(set_size = nchar(answer_options_clean)/2)


# add item characteristics to the logs data

items_40 <- items_40 %>% select(item_id, set_size)

mole_item_cha <- merge(mole_long_strict_LT_gr3_days, items_40, by = "item_id")

# compute  mean set size for each day 
plotMeanSet <- mole_item_cha %>% 
  group_by(user_id, created_date) %>% 
  summarise(mean_set_size = mean(set_size), .groups = "drop_last")
  

#select student from the data and plot the individual growth

set.seed(1815)
some_ids <- sample(unique(plotMeanSet$user_id), 12)

p3 <- plotMeanSet %>%
  filter(user_id %in% some_ids) %>%
  mutate(child = sprintf("Child %02d", match(user_id, some_ids))) %>%
  ggplot(aes(created_date, mean_set_size)) +
  geom_line(colour = "grey60", linewidth = 0.4) +
  geom_point(colour = "steelblue", size = 0.7) +
  facet_wrap(~ child, ncol = 3,  axes = "all") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Date", y = "Mean set size") +
  theme_minimal() +
  theme(strip.text = element_blank(),
        axis.line   = element_line(colour = "black", linewidth = 0.5),
        axis.text.x = element_text(angle = 30, hjust = 1)) 

ggsave(file.path(data_dir, "discussion_sample_description_v3.png"), p3, width = 7, height = 6, dpi = 300)




