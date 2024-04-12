# #30DayChartChallenge | April 2024 - Day 11 | Mobile Friendly
# Data Source is from wikipedia.com
# https://en.wikipedia.org/wiki/IPhone#See_also

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(ggtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Courier Prime", family = "Courier Prime")
font <- "Courier Prime"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# options(scipen = 999)

# get data ----------------------------------------------------------------
raw <- read.csv("iphone.csv")

# wrangle and create data frame -------------------------------------------
df <- raw %>% 
  mutate(Stem = as.integer(floor(grams / 10))) %>% 
  mutate(Leaf = as.integer(grams %% 10)) %>%
  group_by(Stem) %>% 
  mutate(row_id = row_number()) %>%
  ungroup() %>% 
  group_by(Stem) %>% 
  mutate(col_id = cur_group_id())

# create plot -------------------------------------------------------------
df %>% 
  ggplot() +
  geom_text(aes(x = 1, y = col_id, label = Stem), family = font, size = 5, color = "#000000") +
  geom_text(aes(x = row_id + 2, y = col_id, label = Leaf), family = font, size = 5, color = "#000000") +
  annotate("segment", x = 2, xend = 2, y = 0.5, yend = 11.5, linewidth = 2, color = "#000000") +
  scale_x_continuous(limits = c(-1, 10)) +
  scale_y_continuous(trans = "reverse", limits = c(12, 0)) +
  theme_void() +
  theme(plot.title = element_text(family = font, size = 30, color = "#000000", face = "bold", hjust = 0.5, margin = margin(b = 10)),
        plot.subtitle = element_text(family = font, size = 11, color = "#000000", hjust = 0.5, margin = margin(b = 25)),
        plot.title.position = "plot",
        plot.caption = element_text(family = font, hjust = 0.5, size = 7.5, color = "#000000", margin = margin(t = 25)),
        plot.caption.position = "plot",
        legend.position = "none",
        axis.title.x = element_markdown(family = font, hjust = 0.5, size = 7, color = "#000000", margin = margin(t = 5), lineheight = 1.1),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "Apple iPhone", 
       subtitle = "Weight (in grams) for all 42 models from 2007 to 2023.",
       x = "<b>How to read this chart:</b> The three-digit weight for each iPhone is split into<br>a vertical stem (the leading two digits) to the left of the line with the rest<br>of the number to the right of the line (the trailing digit) to show clustering.",
       caption = "#30DayChartChallenge | Data: wikipedia.com | Design: Ryan Hart")

# save plot ———————————————————————————————————————————————————————————————
ggsave(paste0("grams_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


