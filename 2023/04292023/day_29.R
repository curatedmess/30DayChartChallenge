# #30DayChartChallenge | April 2023 - Day 29 | monochrome
# Data Source is wikipedia
# https://en.wikipedia.org/wiki/Special:Statistics

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(janitor)
library(ungeviz)
library(ggtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Courier Prime", family = "Courier Prime")
font <- "Courier Prime"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999)

# load data ---------------------------------------------------------------
data <- readr::read_csv("siteviews-20180101-20221231.csv")

# data wrangle and create data frame --------------------------------------
df <- data %>%
  clean_names() %>% 
  rename(views = en_wikipedia_org) %>% 
  mutate(date = ymd(date)) %>% 
  mutate(year = year(date),
         month = month(date, label = TRUE, abbr = FALSE),
         month_abb = month(date, label = TRUE, abbr = TRUE),
         day = wday(date, label = TRUE, abbr = FALSE))

# Reference information on visualizing uncertainty
# https://clauswilke.com/dataviz/visualizing-uncertainty.html
# https://wilkelab.org/ungeviz/

# reference to code/idea for the stat_confidence_density + the jitter
# https://wilkelab.org/ungeviz/reference/stat_confidence_density.html

# # calculate summary data ------------------------------------------------
df_summary <- df %>% 
  group_by(day) %>% 
  summarise(sd = sd(views), 
            moe = sd * 1.96, #95% 
            views = mean(views))

# change order of days of the week to start at Monday ---------------------
df$day <- factor(df$day, c("Monday", "Tuesday", "Wednesday", "Thursday","Friday","Saturday", "Sunday"))
df_summary$day <- factor(df_summary$day, c("Monday", "Tuesday", "Wednesday", "Thursday","Friday","Saturday", "Sunday"))

# colors ------------------------------------------------------------------
color1 <- "#dee2e6" #background, bar
color2 <- "#212529" #axis, points, text

# create plot -------------------------------------------------------------
ggplot(data = df_summary, aes(x = views, y = day)) +
  stat_confidence_density(aes(moe = moe, fill = after_stat(ndensity)), height = 0.5) +
  geom_point(data = df, position = position_jitter(width = 0.05), size = 0.2, color = color2) +
  geom_errorbarh(aes(xmin = views - sd, xmax = views + sd), height = 0.3, color = color1, linewidth = 0.7) +
  geom_point(size = 2, color = color1) +
  scale_fill_gradient(low = "#adb5bd", high = "#495057") +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(breaks = seq(200000000, 325000000, by = 25000000), labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  theme_void() +
  theme(plot.title = element_text(family = font, size = 45, hjust = 0.5, color = color2, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 9, hjust = 0.5, color = color2, margin = margin(t = 5, b = 10)),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 9, family = font, color = color2, hjust = 0.5, margin = margin(t = 30)),
        legend.position = "none",
        axis.text = element_text(size = 8, family = font, color = color2, hjust = 0, margin = margin(t = 5)),
        axis.title.y = element_blank(),
        axis.title.x = element_markdown(size = 9, family = font, color = color2, margin = margin(t = 10)),
        axis.line.x = element_line(color = color2, linewidth = 0.3),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(color = NA, fill =  color1)) +
  labs(title = "WIKIPEDIA",
       subtitle = "Daily page views by day of the week from 2018 to 2022",
       x = "Page Views<br><br><span style='font-size:7pt'>(Confidence Density Band: 95%, Error Bar: 68%)</span>",
       caption = "#30DayChartChallenge | Data: en.wikipedia.org | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("day_29_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

