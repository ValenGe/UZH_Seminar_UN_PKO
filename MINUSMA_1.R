library(tidyverse)
library(lubridate)
library(readr)
library(extrafont)

## Load dataset

minusma_violence <- read_csv("MINUSMA/2017-01-01-2020-12-31-Mali.csv")

### Overview ###

### Plot 1: Overview Time series

## Create blank time series

time_data <-
  as_tibble(rep(seq(ymd('2017-01-01'),ymd('2020-12-31'), by = 'months'), 10)) %>%
  rename("event_month" = "value") %>%
  arrange(event_month)
admin1 <- unique(minusma_violence$admin1)
time_data$admin1 <- rep(admin1, 48)

## Create time series overview

overview_ts <- 
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
         fatalities = ifelse(is.na(fatalities), 0, fatalities))

## Plotting

overview_ts_plot <-
  overview_ts %>%
  ggplot(aes(x = event_month, 
             y = poc_events, 
             color = admin1)) + 
  geom_line(size = 1) +
  stat_summary(fun = sum, 
               na.rm = TRUE, 
               linetype = "dotted", 
               color = 'black', 
               geom ='line', 
               size = 1, 
               aes(alpha = "Total number of incidents")) +
  scale_alpha_discrete(range = c(1, 1)) +
  scale_color_viridis_d(option = "D") +
  scale_x_date(date_breaks = "2 month",  
               date_labels = "%m/%Y",
               limits = ymd(c("2017-01-01", "2020-12-31")),
               expand = expansion(mult = c(0.025, 0.025))) +
  scale_y_continuous(limits = c(0, 65),
                     n.breaks = 8) +
  labs(title = "PoC incidents in Mali",
       subtitle = "January 2017 - December 2021") +
  theme(text = element_text(family = "Arial"), # UN color: #009edb
        panel.background = element_blank(),
        plot.title = element_text(size = 26,
                                  face = "bold"),
        plot.subtitle = element_text(size = 15, 
                                     face = "italic"),
        plot.background = element_rect(color = "black"),
        axis.line = element_line(linetype = "solid"),
        axis.title = element_blank(),
        legend.box = "vertical",
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(color = "black", 
                                   size = 12),
        legend.position = c(.8,.95),
        legend.spacing.y=unit(0, "lines"),
        axis.text = element_text(size = 12,
                                 colour = "black"),
        axis.text.x = element_text(angle = 45, 
                                   hjust=1, size = 12)) +
  guides(color = guide_legend(ncol = 2)) 
overview_ts_plot

ggsave(filename = "overview_ts_plot.svg",
       plot = overview_ts_plot,
       device = svg,
       path = "Graphics")  

## Plotting (subset)

overview_ts_subset_plot <- 
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
  filter(admin1 %in% c("Mopti", "Gao", "Tombouctou", "Kidal", "Menaka")) %>%
  ggplot(aes(x = event_month, 
             y = poc_events, 
             color = admin1)) + 
  geom_line(size = 1) +
  stat_summary(fun = sum, 
               na.rm = TRUE, 
               linetype = "dotted", 
               color = 'black', 
               geom ='line', 
               size = 1, 
               aes(alpha = "Total number of incidents")) +
  scale_alpha_discrete(range = c(1, 1)) +
  scale_color_viridis_d(option = "D") +
  scale_x_date(date_breaks = "2 month",  
               date_labels = "%m/%Y",
               limits = ymd(c("2017-01-01", "2020-12-31")),
               expand = expansion(mult = c(0.025, 0.025))) +
  scale_y_continuous(limits = c(0, 50),
                     n.breaks = 8) +
  labs(title = "PoC incidents in Mali",
       subtitle = "January 2017 - December 2021") +
  theme(text = element_text(family = "Arial"), # UN color: #009edb
        panel.background = element_blank(),
        plot.title = element_text(size = 26,
                                  face = "bold"),
        plot.subtitle = element_text(size = 15, 
                                     face = "italic"),
        plot.background = element_rect(color = "black"),
        axis.line = element_line(linetype = "solid"),
        axis.title = element_blank(),
        legend.box = "vertical",
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(color = "black", 
                                   size = 12),
        legend.position = c(.8,.95),
        legend.spacing.y=unit(0, "lines"),
        axis.text = element_text(size = 12,
                                 colour = "black"),
        axis.text.x = element_text(angle = 45, 
                                   hjust=1, size = 12)) +
  guides(color = guide_legend(ncol = 2)) 
overview_ts_subset_plot

ggsave(filename = "overview_ts_subset_plot.svg",
       plot = overview_ts_subset_plot,
       device = svg,
       path = "Graphics") 

### Plot 2: Overview PoC events and fatalities

overview_regions_plot <- 
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
  #arrange(desc(poc_events)) %>%
  #slice(1:5) %>%
  pivot_longer(c(poc_events, fatalities),
               names_to = "type",
               values_to = "count") %>%
  ggplot(aes(x = reorder(admin1, -count),
             y = count,
             fill = type)) +
  geom_col(width = 0.9, 
           position = "dodge") +
  scale_y_continuous(limits = c(0, 700),
                     n.breaks = 7) +
  labs(title = "PoC incidents in Mali",
       subtitle = "January 2020 - December 2020") +
  scale_fill_manual(breaks = c("poc_events", "fatalities"),
                    labels = c("poc_events" = "Number of events", "fatalities" = "Fatalities"),
                    values = c("poc_events" = "#009edb", "fatalities" = "#b4d2ff")) +
  theme(text = element_text(family = "Arial"), # UN color: #009edb
        panel.background = element_blank(),
        plot.background = element_rect(color = "black"),
        plot.title = element_text(size = 26,
                                  face = "bold"),
        plot.subtitle = element_text(size = 15, 
                                     face = "italic"),
        axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        legend.box = "vertical",
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(color = "black", 
                                   size = 12),
        legend.position = c(.8,.9),
        axis.text = element_text(size = 12,
                                 colour = "black"),
        axis.text.x = element_text(size = 12)) +
  coord_flip()
overview_regions_plot

ggsave(filename = "overview_regions_plot.svg",
       plot = overview_regions_plot,
       device = svg,
       path = "Graphics")

### Plot 3: Overview actors

## Create table containing number of incidents and fatalities per perpetrator

overview_actors <- 
  minusma_violence %>%
  filter(event_type == "Violence against civilians", 
         actor1 != "Civilians (Mali)", 
         actor1 != "Civilians (Benin)",
         actor1 != "Civilians (International)",
         actor1 != "Civilians (Niger)",
         actor1 != "Civilians (Burkina Faso)",
         actor1 != "Civilians (Switzerland)",
         year == 2020) %>%
  mutate(event_date = dmy(event_date, 
                          truncated = 2L),
         event_month = floor_date(event_date, "month")) %>%
  group_by(actor1) %>%
  summarise(poc_events = n(),
            fatalities = sum(fatalities)) %>%
  pivot_longer(c(poc_events, fatalities),
               names_to = "type",
               values_to = "count")

overview_actors_plot <-
  overview_actors %>%
  ggplot(aes(x = reorder(actor1, -count),
             y = count,
             fill = type)) +
  geom_col(width = 0.925, 
           position = "dodge")  +
  scale_y_continuous(limits = c(0, 250),
                     n.breaks = 7) +
  scale_x_discrete(labels = c("Islamic State (West Africa) - Greater Sahara Faction" = "Islamic State (West Africa) \n- Greater Sahara Faction",
                              "JNIM: Group for Support of Islam and Muslims and/or Islamic State (West Africa) - Greater Sahara Faction" = "JNIM: Group for Support of Islam and Muslims and/or \nIslamic State (West Africa) - Greater Sahara Faction",
                              "Unidentified Armed Group (Mali)" = "Unidentified Armed Group",
                              "Military Forces of Niger (2011-2021)" = "Military Forces of Niger \n(2011-2021)",
                              "JNIM: Group for Support of Islam and Muslims" = "JNIM: Group for Support \nof Islam and Muslims",
                              "Arab Ethnic Militia (Mali)" = "Arab Ethnic Militia",
                              "Military Forces of France (2017-)" = "Military Forces of France \n(2017-)",
                              "Military Forces of Mali (2013-2020)" = "Military Forces of Mali \n(2013-2020)",
                              "Unidentified Communal Militia (Mali)" = "Unidentified Communal Militia",
                              "Tuareg Ethnic Militia (Mali)" = "Tuareg Ethnic Militia",
                              "Soninke Ethnic Militia (Mali)" = "Soninke Ethnic Militia",
                              "Police Forces of Mali (2013-2020)" = "Police Forces of Mali \n(2013-2020)",
                              "Military Forces of Mali (2020-2021)" = "Military Forces of Mali \n(2020-2021)",
                              "Military Forces of Burkina Faso (2015-)" = "Military Forces of Burkina Faso \n(2015-)",
                              "Dozo Communal Militia (Mali)" = "Dozo Communal Militia")) +
  scale_fill_manual(breaks = c("poc_events", "fatalities"),
                    labels = c("poc_events" = "Number of events", "fatalities" = "Fatalities"),
                    values = c("poc_events" = "#009edb", "fatalities" = "#b4d2ff")) +
  theme(text = element_text(family = "Arial"), # UN color: #009edb
        panel.background = element_blank(),
        plot.background = element_rect(color = "black"),
        axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        legend.box = "vertical",
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(color = "black", 
                                   size = 12),
        legend.position = c(.8,.9),
        axis.text = element_text(size = 12,
                                 colour = "black"),
        axis.text.x = element_text(size = 12)) +
  coord_flip()
overview_actors_plot

ggsave(filename = "overview_actors_plot.svg",
       plot = overview_actors_plot,
       device = svg,
       path = "Graphics",
       width = 8.97,
       height = 11.2)

### Mopti ###

### Plot 1: Time series

## Create blank time series data Mopti (01/2020 - 12/2020)

# Create blank time series ranging from 01/2020 to 12/2020
time_data_m <-
  as_tibble(rep(seq(ymd('2020-01-01'),ymd('2020-12-31'), by = 'months'), 3)) %>%
  rename("event_month" = "value") %>%
  arrange(event_month)
# Extract sub-event types occuring in Mopti between 01/2020 and 12/2020
sub_event_type_m <- 
  minusma_violence %>%
  mutate(event_date = dmy(event_date, truncated = 2L),
         event_month = floor_date(event_date, "month")) %>%
  filter(event_type == "Violence against civilians",
         admin1 %in% c("Mopti"),
         event_month >= "2020-01-01",
         event_month <= "2020-12-01") %>%
  group_by(data_id, event_month, admin1, sub_event_type) %>%
  summarise(fatalities = mean(fatalities)) %>%
  group_by(event_month, admin1, sub_event_type) %>%
  summarise(poc_events = n(),
            fatalities = sum(fatalities))
sub_event_type_m <- unique(sub_event_type_m$sub_event_type)
# Include sub-event types in blank time series
time_data_m$sub_event_type <- rep(sub_event_type_m, 12)
time_data_m$admin1 <- rep("Mopti", 36)

# Create time series dataframe for Mopti (01/2020 - 12/2020)

m_ts <- 
  full_join(x = time_data_m, 
            y= minusma_violence %>%
              mutate(event_date = dmy(event_date, 
                                      truncated = 2L),
                     event_month = floor_date(event_date, "month")) %>%
              filter(event_type == "Violence against civilians",
                     admin1 %in% c("Mopti"),
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
  group_by(event_month, admin1, sub_event_type) %>%
  summarise(incidents = sum(incidents),
            fatalities = sum(fatalities))

## Plotting

m_ts_plot <-
  m_ts %>%
  ggplot(aes(x = event_month, 
             y = incidents)) + 
  geom_col(aes(fill = sub_event_type),
           position = "dodge",
            size = 1) +
  stat_summary(fun = sum, 
               na.rm = TRUE, 
               linetype = "dotted", 
               color = 'black', 
               geom ='line', 
               size = 1, 
               aes(alpha = "Total number of incidents")) +
  scale_alpha_discrete(range = c(1, 1)) +
  scale_fill_viridis_d(option = "D") +
  scale_x_date(date_breaks = "month",  
               date_labels = "%m/%Y",
               expand = expansion(mult = c(0.025, 0.025))) +
  scale_y_continuous(limits = c(0, 40),
                     n.breaks = 4) +
  labs(title = "PoC incidents in Mopti Region",
       subtitle = "January 2020 - December 2020") +
  theme(text = element_text(family = "Arial"), # UN color: #009edb
        panel.background = element_blank(),
        plot.title = element_text(size = 26,
                                  face = "bold"),
        plot.subtitle = element_text(size = 15, 
                                     face = "italic"),
        plot.background = element_rect(color = "black"),
        axis.line = element_line(linetype = "solid"),
        axis.title = element_blank(),
        legend.box = "vertical",
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(color = "black", 
                                   size = 12),
        legend.position = c(.8,.9),
        legend.spacing.y=unit(0, "lines"),
        axis.text = element_text(size = 12,
                                 colour = "black"),
        axis.text.x = element_text(angle = 45, 
                                   hjust=1, size = 12)) 
m_ts_plot

ggsave(filename = "m_ts_plot.svg",
       plot = m_ts_plot,
       device = svg,
       path = "Graphics")

### Plot 2: Actors

## Create table containing number of incidents and fatalities per perpetrator

m_actors <- 
  minusma_violence %>%
  filter(event_type == "Violence against civilians",
         admin1 %in% c("Mopti"), 
         actor1 != "Civilians (Mali)",
         year == 2020) %>%
  mutate(event_date = dmy(event_date, 
                          truncated = 2L),
         event_month = floor_date(event_date, "month")) %>%
  group_by(actor1) %>%
  summarise(poc_events = n(),
            fatalities = sum(fatalities)) %>%
  pivot_longer(c(poc_events, fatalities),
               names_to = "type",
               values_to = "count")

## Plotting

m_actors_plot <-
  m_actors %>%
  ggplot(aes(x = reorder(actor1, -count),
             y = count,
             fill = type)) +
  geom_col(width = 0.925, 
           position = "dodge")  +
  scale_y_continuous(limits = c(0, 170),
                     n.breaks = 7) +
  scale_x_discrete(labels = c("Fulani Ethnic Militia (Mali)" = "Fulani Ethnic Militia",
                              "Military Forces of Mali (2013-2020)" = "Military Forces of Mali \n(2013-2020)",
                              "JNIM: Group for Support of Islam and Muslims" = "JNIM: Group for Support \nof Islam and Muslims",
                              "Unidentified Armed Group (Mali)" = "Unidentified Armed Group",
                              "Military Forces of Burkina Faso (2015-)" = "Military Forces of Burkina Faso \n(2015-)",
                              "Dogon Ethnic Militia (Mali)" = "Dogon Ethnic Militia",
                              "Dozo Communal Militia (Mali)" = "Dozo Communal Militia",
                              "Military Forces of Mali (2020-2021)" = "Military Forces of Mali \n(2020-2021)",
                              "JNIM: Group for Support of Islam and Muslims and/or Islamic State (West Africa) - Greater Sahara Faction" = "JNIM: Group for Support of Islam and Muslims and/or \nIslamic State (West Africa) - Greater Sahara Faction",
                              "Police Forces of Mali (2013-2020)" = "Police Forces of Mali \n(2013-2020)")) +
  scale_fill_manual(breaks = c("poc_events", "fatalities"),
                    labels = c("poc_events" = "Number of events", "fatalities" = "Fatalities"),
                    values = c("poc_events" = "#009edb", "fatalities" = "#b4d2ff")) +
  theme(text = element_text(family = "Arial"), # UN color: #009edb
        panel.background = element_blank(),
        plot.background = element_rect(color = "black"),
        axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        legend.box = "vertical",
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(color = "black", 
                                   size = 12),
        legend.position = c(.8,.9),
        axis.text = element_text(size = 12,
                                 colour = "black"),
        axis.text.x = element_text(size = 12)) +
  coord_flip()
m_actors_plot

ggsave(filename = "m_actors_plot.svg",
       plot = m_actors_plot,
       device = svg,
       path = "Graphics")

### Kidal & Tombouctou ###

### Plot 1: Time series

## Create blank time series data Kidal & Tombouctou (01/2020 - 12/2020)

time_data_tk <-
  as_tibble(rep(seq(ymd('2020-01-01'),ymd('2020-12-31'), by = 'months'), 6)) %>%
  rename("event_month" = "value") %>%
  arrange(event_month)
time_data_tk$sub_event_type <- rep(sub_event_type_m, 24)
time_data_tk$admin1 <- rep(c(rep("Tombouctou",3), rep("Kidal", 3)), 12)

# Plotting

tk_ts <- 
  full_join(x = time_data_tk, 
            y= minusma_violence %>%
              mutate(event_date = dmy(event_date, 
                                      truncated = 2L),
                     event_month = floor_date(event_date, "month")) %>%
              filter(event_type == "Violence against civilians",
                     admin1 %in% c("Tombouctou", "Kidal"),
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
  group_by(event_month, admin1, sub_event_type) %>%
  summarise(incidents = sum(incidents),
            fatalities = sum(fatalities))

tk_ts_plot <-
  tk_ts %>%
  ggplot(aes(x = event_month, 
             y = incidents)) + 
  geom_col(aes(fill = sub_event_type),
           position = "dodge",
           size = 1) +
  stat_summary(fun = sum, na.rm = TRUE, linetype = "dotted", color = 'black', geom ='line', size = 1, aes(alpha = "Total number of incidents")) +
  scale_alpha_discrete(range = c(1, 1)) +
  scale_fill_viridis_d(option = "D") +
  scale_x_date(date_breaks = "month",  
               date_labels = "%m/%Y",
               expand = expansion(mult = c(0.025, 0.025))) +
  scale_y_continuous(limits = c(0, 6),
                     n.breaks = 4) +
  labs(title = "PoC incidents in Tombouctou & Kidal Regions",
       subtitle = "January 2020 - December 2020") +
  theme(text = element_text(family = "Arial"), # UN color: #009edb
        panel.background = element_blank(),
        plot.title = element_text(size = 26,
                                  face = "bold"),
        plot.subtitle = element_text(size = 15, 
                                     face = "italic"),
        plot.background = element_rect(color = "black"),
        axis.line = element_line(linetype = "solid"),
        axis.title = element_blank(),
        legend.box = "vertical",
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(color = "black", 
                                   size = 12),
        legend.position = c(.8,.9),
        legend.spacing.y=unit(0, "lines"),
        axis.text = element_text(size = 12,
                                 colour = "black"),
        axis.text.x = element_text(angle = 45, 
                                   hjust=1, size = 12)) 
tk_ts_plot

ggsave(filename = "tk_ts_plot.svg",
       plot = tk_ts_plot,
       device = svg,
       path = "Graphics")

### Plot 2: Actors

## Create table containing number of incidents and fatalities per perpetrator

tk_actors <- 
  minusma_violence %>%
  filter(event_type == "Violence against civilians",
         admin1 %in% c("Tombouctou", "Kidal"), 
         actor1 != "Civilians (Mali)", 
         actor1 != "Civilians (Switzerland)",
         year == 2020) %>%
  mutate(event_date = dmy(event_date, 
                          truncated = 2L),
         event_month = floor_date(event_date, "month")) %>%
  group_by(actor1) %>%
  summarise(poc_events = n(),
            fatalities = sum(fatalities)) %>%
  pivot_longer(c(poc_events, fatalities),
               names_to = "type",
               values_to = "count")

## Plotting

tk_actors_plot <-
  tk_actors %>%
  ggplot(aes(x = reorder(actor1, -count),
             y = count,
             fill = type)) +
  geom_col(width = 0.925, 
           position = "dodge")  +
  scale_y_continuous(limits = c(0, 20),
                     n.breaks = 7) +
  scale_x_discrete(labels = c("Islamic State (West Africa) - Greater Sahara Faction" = "Islamic State (West Africa) \n- Greater Sahara Faction",
                              "Unidentified Armed Group (Mali)" = "Unidentified Armed Group",
                              "Military Forces of Niger (2011-2021)" = "Military Forces of Niger \n(2011-2021)",
                              "JNIM: Group for Support of Islam and Muslims" = "JNIM: Group for Support \nof Islam and Muslims",
                              "Military Forces of Mali (2013-2020)" = "Military Forces of Mali \n(2013-2020)",
                              "JNIM: Group for Support of Islam and Muslims and/or Islamic State (West Africa) - Greater Sahara Faction" = "JNIM: Group for Support of Islam and Muslims and/or \nIslamic State (West Africa) - Greater Sahara Faction",
                              "Military Forces of Burkina Faso (2015-)" = "Military Forces of Burkina Faso \n(2015-)",
                              "Tuareg Ethnic Militia (Mali)" = "Tuareg Ethnic Militia")) +
  scale_fill_manual(breaks = c("poc_events", "fatalities"),
                    labels = c("poc_events" = "Number of events", "fatalities" = "Fatalities"),
                    values = c("poc_events" = "#009edb", "fatalities" = "#b4d2ff")) +
  theme(text = element_text(family = "Arial"), # UN color: #009edb
        panel.background = element_blank(),
        plot.background = element_rect(color = "black"),
        axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        legend.box = "vertical",
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(color = "black", 
                                   size = 12),
        legend.position = c(.8,.9),
        axis.text = element_text(size = 12,
                                 colour = "black"),
        axis.text.x = element_text(size = 12)) +
  coord_flip()
tk_actors_plot

ggsave(filename = "tk_actors_plot.svg",
       plot = tk_actors_plot,
       device = svg,
       path = "Graphics") 

### Gao & Menaka ###

### Plot 1: Time series

## Create blank time series data Gao & Menaka (01/2020 - 12/2020)

time_data_gm <-
  as_tibble(rep(seq(ymd('2020-01-01'),ymd('2020-12-31'), by = 'months'), 6)) %>%
  rename("event_month" = "value") %>%
  arrange(event_month)
time_data_gm$sub_event_type <- rep(sub_event_type_m, 24)
time_data_gm$admin1 <- rep(c(rep("Gao",3), rep("Menaka", 3)), 12)

# Plotting

gm_ts <- 
  full_join(x = time_data_m, 
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
  group_by(event_month, sub_event_type) %>%
  summarise(incidents = sum(incidents),
            fatalities = sum(fatalities)) 

gm_ts_plot <-
  gm_ts %>%
  ggplot(aes(x = event_month, 
             y = incidents)) + 
  geom_col(aes(fill = sub_event_type),
           position = "dodge",
           size = 1) +
  stat_summary(fun = sum, na.rm = TRUE, linetype = "dotted", color = 'black', geom ='line', size = 1, aes(alpha = "Total number of incidents")) +
  scale_alpha_discrete(range = c(1, 1)) +
  scale_fill_viridis_d(option = "D") +
  scale_x_date(date_breaks = "month",  
               date_labels = "%m/%Y",
               expand = expansion(mult = c(0.025, 0.025))) +
  scale_y_continuous(limits = c(0, 20),
                     n.breaks = 5) +
  labs(title = "PoC incidents in Gao & Menaka Regions",
       subtitle = "January 2020 - December 2020") +
  theme(text = element_text(family = "Arial"), # UN color: #009edb
        panel.background = element_blank(),
        plot.title = element_text(size = 26,
                                  face = "bold"),
        plot.subtitle = element_text(size = 15, 
                                     face = "italic"),
        plot.background = element_rect(color = "black"),
        axis.line = element_line(linetype = "solid"),
        axis.title = element_blank(),
        legend.box = "vertical",
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(color = "black", 
                                   size = 12),
        legend.position = c(.8,.9),
        legend.spacing.y=unit(0, "lines"),
        axis.text = element_text(size = 12,
                                 colour = "black"),
        axis.text.x = element_text(angle = 45, 
                                   hjust=1, size = 12)) 
gm_ts_plot

ggsave(filename = "gm_ts_plot.svg",
       plot = gm_ts_plot,
       device = svg,
       path = "Graphics")

### Plot 2: Actors

## Create table containing number of incidents and fatalities per perpetrator

gm_actors <- 
  minusma_violence %>%
  filter(event_type == "Violence against civilians",
         admin1 %in% c("Gao", "Menaka"), 
         actor1 != "Civilians (Mali)", 
         actor1 != "Civilians (Benin)",
         actor1 != "Civilians (International)",
         actor1 != "Civilians (Niger)",
         year == 2020) %>%
  mutate(event_date = dmy(event_date, 
                          truncated = 2L),
         event_month = floor_date(event_date, "month")) %>%
  group_by(actor1) %>%
  summarise(poc_events = n(),
            fatalities = sum(fatalities)) %>%
  pivot_longer(c(poc_events, fatalities),
               names_to = "type",
               values_to = "count")

gm_actors_plot <-
  gm_actors %>%
  ggplot(aes(x = reorder(actor1, -count),
             y = count,
             fill = type)) +
  geom_col(width = 0.925, 
           position = "dodge")  +
  scale_y_continuous(limits = c(0, 55),
                     n.breaks = 7) +
  scale_x_discrete(labels = c("Islamic State (West Africa) - Greater Sahara Faction" = "Islamic State (West Africa) \n- Greater Sahara Faction",
                              "Unidentified Armed Group (Mali)" = "Unidentified Armed Group",
                              "Military Forces of Niger (2011-2021)" = "Military Forces of Niger \n(2011-2021)",
                              "JNIM: Group for Support of Islam and Muslims" = "JNIM: Group for Support \nof Islam and Muslims",
                              "Arab Ethnic Militia (Mali)" = "Arab Ethnic Militia",
                              "Military Forces of France (2017-)" = "Military Forces of France \n(2017-)",
                              "Military Forces of Mali (2013-2020)" = "Military Forces of Mali \n(2013-2020)",
                              "Unidentified Communal Militia (Mali)" = "Unidentified Communal Militia")) +
  scale_fill_manual(breaks = c("poc_events", "fatalities"),
                    labels = c("poc_events" = "Number of events", "fatalities" = "Fatalities"),
                    values = c("poc_events" = "#009edb", "fatalities" = "#b4d2ff")) +
  theme(text = element_text(family = "Arial"), # UN color: #009edb
        panel.background = element_blank(),
        plot.background = element_rect(color = "black"),
        axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        legend.box = "vertical",
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(color = "black", 
                                   size = 12),
        legend.position = c(.8,.9),
        axis.text = element_text(size = 12,
                                 colour = "black"),
        axis.text.x = element_text(size = 12)) +
  coord_flip()
gm_actors_plot

ggsave(filename = "gm_actors_plot.svg",
       plot = gm_actors_plot,
       device = svg,
       path = "Graphics")           
