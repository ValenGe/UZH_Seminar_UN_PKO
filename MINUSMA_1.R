library(tidyverse)
library(lubridate)
library(readr)
library(extrafont)

minusma_violence <- read_csv("MINUSMA/2017-01-01-2020-12-31-Mali.csv")

trial_1 <- 
  minusma_violence %>%
  filter(event_type == "Violence against civilians") %>%
  mutate(event_date = dmy(event_date, 
                          truncated = 2L),
         event_month = floor_date(event_date, "month")) %>%
  group_by(data_id, event_month, admin1) %>%
  summarise(fatalities = mean(fatalities)) %>%
  group_by(event_month, admin1) %>%
  summarise(poc_events = n()) %>%
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

ggsave(filename = "trial_1.svg",
       plot = trial_1,
       device = svg)  

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

ggsave(filename = "trial_2.svg",
       plot = trial_2,
       device = svg) 

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
  scale_fill_manual(labels = c("fatalities" = "Fatalities", "poc_events" = "Number of events"),
                    values = c("fatalities" = "#b4d2ff", "poc_events" = "#009edb")) +
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

ggsave(filename = "trial_3.svg",
       plot = trial_3,
       device = svg)