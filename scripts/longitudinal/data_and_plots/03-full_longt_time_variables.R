library(dplyr)
library(ggplot2)
library(scales)
library(jtools)

# ---- data --------------------------------------------------------

mole_long_strict <- readRDS(file.path(data_dir, "mole_long_strict.rds"))

# select the student
library(dplyr)

user_days <- mole_long_strict |>
  filter(grade == 4) %>% 
  filter(days_since_first_day < 15) |>
  group_by(user_id, created_date, days_since_first_day) |>
  summarise(
    n_items    = n(),
    n_sessions = n_distinct(session_id_daily),
    .groups    = "drop"
  )

candidates <- user_days |>
  group_by(user_id) |>
  summarise(
    n_active_days = n(),
    span          = max(days_since_first_day),
    days          = paste(days_since_first_day, collapse = "-"),
    gap1          = days_since_first_day[2] - days_since_first_day[1],
    gap2          = days_since_first_day[3] - days_since_first_day[2],
    items_min     = min(n_items),
    items_max     = max(n_items),
    sessions_max  = max(n_sessions),
    .groups = "drop"
  ) |>
  filter(n_active_days == 4) %>% 
  filter(items_min > 9)

user_plot <- mole_long_strict %>% 
  filter(user_id == 3996842576) %>% 
  filter(days_since_first_day < 15) %>% 
  select(created,starting_age, days_since_first_day, 
         item_index_in_session_daily, day_gap_between_sessions_daily) %>% 
  arrange(created)

user_plot <- user_plot %>%
  mutate(
    created_day = as.POSIXct(as.Date(created))
  )

library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)

user_plot <- user_plot %>%
  mutate(created_day = as.Date(created_day))

# Summarize data
user_summary <- user_plot %>%
  group_by(created_day) %>%
  summarize(
    max_y = max(item_index_in_session_daily),
    days_since_first_day = unique(days_since_first_day),
    day_gap_between_sessions_daily = unique(day_gap_between_sessions_daily),
    starting_age = unique(starting_age)[1],
    .groups = "drop"
  ) %>%
  arrange(created_day) %>%
  mutate(
    first_day = min(created_day),
    prev_day  = lag(created_day),
    blue_tier = row_number() - 1
  )

y_base <- max(user_plot$item_index_in_session_daily) + 3

red_ticks <- user_summary %>%
  filter(!is.na(prev_day)) %>%
  rowwise() %>%
  reframe(
    tick_date = seq.Date(from = prev_day, to = created_day, by = "day"),
    y_pos = y_base + 1
  )

blue_ticks <- user_summary %>%
  filter(created_day != first_day) %>%
  rowwise() %>%
  reframe(
    tick_date = seq.Date(from = first_day, to = created_day, by = "day"),
    y_pos = y_base + 5 + (blue_tier * 4)
  )

variable_plot <- ggplot(user_plot, aes(x = created_day, y = item_index_in_session_daily)) +
  geom_point() +
  
  # --- POINT LABELS ---
  geom_text(
    aes(label = item_index_in_session_daily),
    hjust = -0.4,
    vjust = 0.5,
    size = 2.5,
    color = "black"
  ) +
  
  # --- TWO-SIDED ARROWS & TICKS: Between-Day Gap (Red) ---
  geom_segment(
    data = filter(user_summary, !is.na(prev_day)),
    aes(x = prev_day, xend = created_day, 
        y = y_base + 1, yend = y_base + 1),
    arrow = arrow(length = unit(0.2, "cm"), ends = "both", type = "closed"),
    color = "red", linewidth = 0.6
  ) +
  geom_point(
    data = red_ticks,
    aes(x = tick_date, y = y_pos),
    color = "red", shape = 124, size = 3.5
  ) +
  geom_text(
    data = user_summary,
    aes(x = created_day, 
        y = y_base + 2.5,
        label = paste0("between-day \ngap = ", day_gap_between_sessions_daily)),
    color = "red", size = 3, hjust = 0.5, vjust = 0
  ) +
  
  # --- ONE-SIDED ARROWS & TICKS: Days Since Start (Blue) ---
  geom_segment(
    data = filter(user_summary, created_day != first_day),
    aes(x = first_day, xend = created_day, 
        y = y_base + 8 + (blue_tier * 4), 
        yend = y_base + 8 + (blue_tier * 4)),
    arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed"),
    color = "blue", linewidth = 0.6
  ) +
  geom_point(
    data = blue_ticks,
    aes(x = tick_date, y = y_pos + 3),
    color = "blue", shape = 124, size = 3
  ) +
  # Centered text directly over the end tip of the blue arrow
  geom_text(
    data = user_summary,
    aes(x = created_day, 
        y = y_base + 9.2 + (blue_tier * 4),
        label = paste0("days since start = ", days_since_first_day)),
    color = "blue", size = 3, 
    hjust = 0.5, # Centers the text midpoint directly over the x = created_day position
    vjust = 0
  ) +
  
  scale_x_date(
    breaks = unique(user_summary$created_day),
    date_labels = "%Y-%m-%d",
    expand = expansion(mult = c(0.1, 0.1)) # Slightly expanded margin to accommodate centered text at the right edge
  ) +
  
  labs(
    x = "date",
    y = "within-trial index",
    subtitle = paste0("starting age = ", round(unique(user_plot$starting_age), 2))
  ) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black", linewidth = 0.5),
        plot.subtitle = element_text(color = "darkgreen"))


ggsave(file.path(data_dir, "method_variable_plot.png"), variable_plot, width = 10, height = 6, dpi = 300)
