# #30DayChartChallenge | April 2023 - Day 23 | tiles
# Data Source is wikipedia
# https://en.wikipedia.org/wiki/Special:Statistics

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(ggtext)
library(viridis)

# add font ----------------------------------------------------------------
font_add_google(name = "Source Sans Pro", family = "Source Sans Pro")
font <- "Source Sans Pro"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999)

# load data ---------------------------------------------------------------
data <- readr::read_csv("siteviews-20220101-20221231.csv")

# data wrangle and create data frame --------------------------------------
df_data <- data %>%
  clean_names() %>% 
  rename(views = en_wikipedia_org)

# make calendar df --------------------------------------------------------
# used this site for code to build calendars
# https://cmdlinetips.com/2022/12/make-a-calender-in-r-with-ggplot2/

# create start and end date variables
start_day <- as.Date("2022-01-01")
end_day <- as.Date("2022-12-31")

# create calendar df
df <- tibble(date = seq(start_day, end_day, by = "1 day")) %>% 
  mutate(year = year(date),
         month = month(date, label = TRUE, abbr = FALSE),
         month_abb = month(date, label = TRUE, abbr = TRUE),
         day = wday(date, label = TRUE),
         mday = mday(date),
         month_week = (5 + day(date) + wday(floor_date(date, "month"))) %/% 7) %>% 
  left_join(df_data)

# create plot -------------------------------------------------------------
df %>%  
  ggplot(aes(y = month_week, x = day, fill = views)) +
  geom_tile(color = "#FFFFFF", size = 0.2) +
  facet_wrap(~ month) +
  scale_y_continuous(trans = scales::reverse_trans()) +
  scale_fill_viridis(option = "B", trans = "log", direction = -1, 
                     breaks = c(min(df$views), max(df$views)), 
                     labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  coord_equal() +
  theme_void() +
  theme(plot.title = element_text(family = font, size = 40, hjust = 0.5, color = "#000000", face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 10, hjust = 0.5, color = "#000000", margin = margin(b = 20)),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 9, family = font, color = "#000000", hjust = 0.5),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 8, family = font, color = "#000000"),
        legend.key.height = unit(0.25, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.margin = margin(t = -0.4, b = 0.5, unit='cm'),
        strip.text = element_text(family = font, size = 9, hjust = 0.5, color = "#000000", margin = margin(b = 5)),
        plot.margin = unit(c(0.75, 0.75, 0.75, 0.75), "cm"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  guides(color = guide_colourbar(title.position = "top", title.hjust = 0.5)) +
  labs(title = "WIKIPEDIA",
       subtitle = "Daily Page Views in 2022 for en.wikipedia.org",
       caption = "\n\n#30DayChartChallenge | Data: en.wikipedia.org | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("day_23_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


