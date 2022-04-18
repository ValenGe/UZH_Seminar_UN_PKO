library(tidyverse)
library(lubridate)
library(readr)
library(extrafont)

## Load dataset

minusma_violence <- read_csv("MINUSMA/2017-01-01-2020-12-31-Mali.csv")

### Overview ###

## Plot 1: Time series

# Create blank time series

time_data <-
  as_tibble(rep(seq(ymd('2017-01-01'),ymd('2020-12-31'), by = 'months'), 10)) %>%
  rename("event_month" = "value") %>%
  arrange(event_month)
admin1 <- unique(minusma_violence$admin1)
time_data$admin1 <- rep(admin1, 48)

# Plotting

trial_1 <- 
  full_join(x = time_data, y= minusma_violence %>%
  filter(event_type == "Violence against civilians") %>%
  mutate(event_date = dmy(event_date, 
                          truncated = 2L),
         event_month = floor_date(event_date, "month")) %>%
  group_by(data_id, event_month, admin1) %>%
  summarise(fatalities = mean(fatalities)) %>%
  group_by(event_month, admin1) %>%
  summarise(poc_events = n(),
            fatalities = sum(fatalities)), 
  by = c("event_month", "admin1")) %>%
  mutate(poc_events = ifelse(is.na(poc_events), 0, poc_events),
         fatalities = ifelse(is.na(fatalities), 0, fatalities)) %>%
  ggplot(aes(x = event_month, 
             y = poc_events, 
             color = admin1)) + 
  geom_line(size = 1) +
  stat_summary(fun = sum, na.rm = TRUE, linetype = "dashed", color = 'black', geom ='line') +
  scale_color_viridis_d() +
  scale_x_date(date_breaks = "2 month",  
               date_labels = "%m/%Y",
               limits = ymd(c("2017-01-01", "2020-12-31")),
               expand = expansion(mult = c(0.025, 0.025))) +
  scale_y_continuous(limits = c(0, 60),
                     n.breaks = 8) +
  labs(title = "PoC incidents in Mali",
       subtitle = "January 2017 - December 2021",
       x = "Time",
       y = "Number of events") +
  theme(text = element_text(family = "Arial"), # UN color: #009edb
        panel.background = element_blank(),
        plot.title = element_text(size = 30,
                                  face = "bold"),
        plot.subtitle = element_text(size = 15, 
                                     face = "italic"),
        axis.line = element_line(linetype = "solid"),
        axis.title = element_blank(),
        legend.position = c(0.85,1),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   hjust=1)) +
  guides(color = guide_legend(ncol = 2)) 
trial_1

#ggsave(filename = "trial_1.svg",
#       plot = trial_1,
#       device = svg)  

## Plot 2: PoC events in 2020

trial_2 <- 
  minusma_violence %>%
  mutate(event_date = dmy(event_date, 
                          truncated = 2L),
         event_month = floor_date(event_date, "month")) %>%
  filter(event_type == "Violence against civilians",
         year == 2020) %>%
  group_by(data_id, admin1) %>%
  summarise(fatalities = mean(fatalities)) %>%
  group_by(admin1) %>%
  summarise(poc_events = n()) %>%
  ggplot(aes(x = reorder(admin1, poc_events),
             y = poc_events)) +
  geom_col(width = 0.8, fill = "#009edb") +
  labs(title = "PoC incidents in Mali",
       subtitle = "January 2020 - December 2020",
       y = "Number of events") +
  theme(text = element_text(family = "Arial"), 
        panel.background = element_blank(),
        plot.title = element_text(size = 30,
                                  face = "bold"),
        plot.subtitle = element_text(size = 15, 
                                     face = "italic"),
        axis.line = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_flip()
trial_2

#ggsave(filename = "trial_2.svg",
#       plot = trial_2,
#       device = svg) 

## Plot 3: PoC events and fatalities

trial_3 <- 
  minusma_violence %>% 
  mutate(event_date = dmy(event_date, 
                          truncated = 2L),
         event_month = floor_date(event_date, "month")) %>%
  filter(event_type == "Violence against civilians",
         year == 2020) %>%
  group_by(data_id, admin1) %>%
  summarise(fatalities = mean(fatalities)) %>%
  group_by(admin1) %>%
  summarise(poc_events = n(),
            fatalities = sum(fatalities)) %>%
  arrange(desc(poc_events)) %>%
  slice(1:5) %>%
  pivot_longer(c(poc_events, fatalities),
               names_to = "type",
               values_to = "count") %>%
  ggplot(aes(x = reorder(admin1, count),
             y = count,
             fill = type)) +
  geom_col(width = 0.9, 
           position = "dodge") +
  labs(title = "PoC incidents in Mali",
       subtitle = "January 2020 - December 2020") +
  scale_y_continuous(limits = c(0, 750),
                     n.breaks = 7) +
  scale_fill_manual(breaks = c("poc_events", "fatalities"),
                    labels = c("poc_events" = "Number of events", "fatalities" = "Fatalities"),
                    values = c("poc_events" = "#009edb", "fatalities" = "#b4d2ff")) +
  theme(text = element_text(family = "Arial"), 
        panel.background = element_blank(),
        plot.title = element_text(size = 30,
                                  face = "bold"),
        plot.subtitle = element_text(size = 15, 
                                     face = "italic"),
        axis.line = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = c(0.8,1)) +
  coord_flip()
trial_3

#ggsave(filename = "trial_3.svg",
#       plot = trial_3,
#       device = svg)

### Gao & Menaka ###

## Plot 1: Time series

# Create blank time series

time_data_gm <-
  as_tibble(rep(seq(ymd('2020-01-01'),ymd('2020-12-31'), by = 'months'), 6)) %>%
  rename("event_month" = "value") %>%
  arrange(event_month)
sub_event_type_gm <- 
  minusma_violence %>%
  mutate(event_date = dmy(event_date, truncated = 2L),
         event_month = floor_date(event_date, "month")) %>%
  filter(event_type == "Violence against civilians",
         admin1 %in% c("Gao", "Menaka"),
         event_month >= "2020-01-01",
         event_month <= "2020-12-01") %>%
  group_by(data_id, event_month, admin1, sub_event_type) %>%
  summarise(fatalities = mean(fatalities)) %>%
  group_by(event_month, admin1, sub_event_type) %>%
  summarise(poc_events = n(),
            fatalities = sum(fatalities))
sub_event_type_gm <- unique(sub_event_type_gm$sub_event_type)
time_data_gm$sub_event_type <- rep(sub_event_type_gm, 24)
time_data_gm$admin1 <- rep(c(rep("Gao",3), rep("Menaka", 3)), 12)

# Plotting

gm_ts <- 
  full_join(x = time_data_gm, 
            y= minusma_violence %>%
              mutate(event_date = dmy(event_date, 
                                      truncated = 2L),
                     event_month = floor_date(event_date, "month")) %>%
              filter(event_type == "Violence against civilians",
                     admin1 %in% c("Gao", "Menaka"),
                     event_month >= "2020-01-01",
                     event_month <= "2020-12-01") %>%
              group_by(data_id, event_month, admin1, sub_event_type) %>%
              summarise(fatalities = mean(fatalities)) %>%
              group_by(event_month, admin1, sub_event_type) %>%
              summarise(incidents = n(),
                        fatalities = sum(fatalities)), 
            by = c("event_month", "admin1", "sub_event_type")) %>%
  mutate(incidents = ifelse(is.na(incidents), 0, incidents),
         fatalities = ifelse(is.na(fatalities), 0, fatalities)) %>%
  ggplot(aes(x = event_month, 
             y = incidents)) + 
  geom_line(aes(linetype = sub_event_type, 
             color = admin1),
            size = 1) +
  scale_color_viridis_d() +
  scale_x_date(date_breaks = "month",  
               date_labels = "%m/%Y",
               limits = ymd(c("2020-01-01", "2020-12-01")),
               expand = expansion(mult = c(0.025, 0.025))) +
  scale_y_continuous(limits = c(0, 15),
                     n.breaks = 8) +
  labs(title = "PoC incidents in Mali",
       subtitle = "January 2020 - December 2020",
       x = "Time",
       y = "Number of events") +
  theme(text = element_text(family = "Arial"), # UN color: #009edb
        panel.background = element_blank(),
        plot.title = element_text(size = 30,
                                  face = "bold"),
        plot.subtitle = element_text(size = 15, 
                                     face = "italic"),
        axis.line = element_line(linetype = "solid"),
        axis.title = element_blank(),
        legend.position = c(0.8,1),
        legend.box = "horizontal",
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   hjust=1)) 
gm_ts
