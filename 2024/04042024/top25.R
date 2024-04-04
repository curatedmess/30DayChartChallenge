# #30DayChartChallenge | April 2024 - Day 4 | Waffle
# Data Source is fide.com

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(countrycode)
library(ggimage)
library(ggfx)

# add font ----------------------------------------------------------------
font_add_google(name = "Ubuntu", family = "Ubuntu")
font <- "Ubuntu"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999)

# get data ----------------------------------------------------------------
# data manually gathered from https://ratings.fide.com/ -------------------
# tried to use rvest to scrape the table directly, but had some issues and
# ran out of time to figure it out, so copied into excel to create csv ----
raw <- read.csv("top25players.csv")

# wrangle and create df ---------------------------------------------------
df <- raw %>% 
  mutate(iso2 = countrycode(country, "iso3c", "iso2c")) %>% 
  mutate(value = 1) %>% 
  select(4:5) %>% 
  group_by(iso2) %>% 
  mutate(total = n()) %>% 
  ungroup() %>% 
  arrange(desc(total), iso2)

# create function to map codes and create xy coordinates in a 5x5 grid ----
map_to_grid <- function(df, cols) {

  rows <- ceiling(nrow(df) / cols)
  grid <- expand.grid(x = 1:cols, y = 1:rows)
  
  grid$iso2 <- NA_character_
  grid$value <- NA_integer_
  for (i in seq_len(nrow(df))) {
    grid$iso2[i] <- df$iso2[i]
    grid$value[i] <- df$value[i]
  }
  
  return(grid)
}

cols <- 5

grid_df <- map_to_grid(df, cols)

# create plot -------------------------------------------------------------
grid_df %>% 
  ggplot(aes(x = x, y = y, fill = iso2)) +
  with_shadow(geom_flag(aes(image = iso2), size = 0.1), sigma = 7, colour = "#C3C3C3")  +
  scale_x_continuous(limits = c(0, 6)) +
  scale_y_continuous(limits = c(0, 6)) +
  coord_fixed() +
  theme_void() +
  theme(plot.title = element_text(family = font, hjust = 0.5, size = 24, color = "#000000", margin = margin(b = 7)),
        plot.subtitle = element_text(hjust = 0.5, family = font, size = 10, color = "#000000", margin = margin(b = 0)),
        plot.title.position = "plot",
        plot.caption = element_text(family = font, hjust = 0.5, size = 7.5, color = "#000000", margin = margin(t = 0)),
        plot.caption.position = "plot",
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#F2F2F2"),
        plot.background = element_rect(color = NA, fill = "#F2F2F2")) +
  labs(title = "TOP 25 OPEN CHESS PLAYERS",
       subtitle = "Grouped by Nationality - Rankings as of April 2024",
       caption = "#30DayChartChallenge | Data: fide.com | Design: Ryan Hart")

# save plot ———————————————————————————————————————————————————————————————
ggsave(paste0("top25_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)
  
