# #30DayChartChallenge | April 2024 - Day 20 | correlation
# Data Source is from https://www.basketball-reference.com/leagues/NBA_stats_per_game.html

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Roboto", family = "Roboto")
font <- "Roboto"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999)

# get data ----------------------------------------------------------------
raw <- read.csv("nba.csv")

# wrangle and create data frame -------------------------------------------
df <- raw %>% 
  select(Rk, Season, FTA, X3PA, PTS) %>% 
  slice(1:45) 

# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(x = X3PA, y = FTA)) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "#000000", linetype = "dashed") +
  geom_point(aes(fill = Rk, size = PTS), alpha = 0.7, shape = 21, color = "#000000") +
 # geom_text(aes(label = Rk)) +
  scale_y_continuous(breaks = c(22, 24, 26, 28, 30)) +
  scale_x_continuous(breaks = c(2, 8, 14, 20, 26, 32)) +
  scale_size_continuous() +
  scale_fill_scico(palette = "grayC", direction = -1, breaks = c(min(df$Rk), mean(df$Rk), max(df$Rk)), labels = c("'2023-'2024", "'2002-'2003", "'1979-'1980")) +
  guides(fill = guide_colorbar(theme = theme(legend.frame = element_rect(colour = "#000000")))) +
  theme_void() +
  theme(plot.title = element_text(family = font, size = 24, color = "#000000", face = "bold", hjust = 0, margin = margin(b = 5)),
        plot.subtitle = element_text(family = font, size = 10, color = "#000000", hjust = 0, margin = margin(b = 25), lineheight = 1.1),
        plot.title.position = "plot",
        plot.caption = element_text(family = font, hjust = 0.5, size = 7.5, color = "#000000", margin = margin(t = 25)),
        plot.caption.position = "plot",
        legend.position = "inside",
        legend.box = "horizontal",
        legend.title = element_text(family = font, hjust = 0.5, size = 8, color = "#000000", face = "bold", margin = margin(b = 10)),
        legend.text = element_text(family = font, hjust = 0.5, size = 6, color = "#000000"),
        legend.justification.inside = c(0.9, 1),
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        axis.line = element_line(linewidth = 0.8, color = "#000000"),
        axis.title.x = element_text(family = font, size = 10, color = "#000000", face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(family = font, size = 10, color = "#000000", face = "bold", angle = 90, margin = margin(r = 10)),
        axis.text.x = element_text(family = font, size = 8, color = "#000000", margin = margin(t = 5)),
        axis.text.y = element_text(family = font, size = 8, color = "#000000", margin = margin(r = 5)),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "Evolution of the NBA",
       x = "3 Point Attempts",
       y = "Free Throw Attempts",
       fill = "Seasons",
       size = "Points",
       subtitle = "Relationship between the season average for 3-point shot and free throw attempts\nalong with average points per season since the NBA introduced the 3-point line\nduring the 1979-1980 season.",
       caption = "#30DayChartChallenge | Data: basketball-reference.com | Design: Ryan Hart")

# save plot ———————————————————————————————————————————————————————————————
ggsave(paste0("3pta_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)
