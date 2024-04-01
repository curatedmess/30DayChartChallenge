# #30DayChartChallenge | April 2024 - Day 1 | part-to-whole
# Data Source is www.azlyrics.com

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(extrafont)
library(tidytext)
library(rvest)
library(ggfx)
library(ggtext)
library(ggfittext)

# add font ----------------------------------------------------------------
font_add_google(name = "Roboto", family = "Roboto")
font <- "Roboto"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# Load system fonts
loadfonts()

# Register system fonts
font_import()

# options(scipen = 999)

# WEB SCRAPE --------------------------------------------------------------
url_lyrics <- "https://www.azlyrics.com/lyrics/gorillaz/desole.html"
web_data_desole <- read_html(url_lyrics)

desole_lyrics_raw <- web_data_desole %>%
html_nodes("div:nth-child(10)") %>%
  html_text()

# wrangle data and create df ----------------------------------------------
tokenized_words <- tibble::tibble(word = strsplit(desole_lyrics_raw , "\\s+")[[1]])

df <- tokenized_words %>% 
  mutate(word = gsub(",", "", word)) %>% 
  # mutate(word = gsub("[^[:alnum:]']", "", word)) %>% #remove all special characters
  slice(-1) %>% 
  mutate(word = tolower(word)) %>% 
  mutate(category = ifelse(word == "désolé", "Désolé", "other")) %>%
  group_by(category) %>%
  summarise(count = n()) %>% 
  mutate(perc = count / sum(count)) %>% 
  mutate(perc_100 = 1) %>% 
  slice(1:1) %>% 
  mutate()
  
# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(x = category, label = category)) +
  as_reference(geom_bar_text(aes(y = perc_100), family = "Slackey", reflow = FALSE, grow = TRUE, color = "#ff0000", padding.x = grid::unit(0, "lines"), padding.y = grid::unit(0, "lines"), hjust = 0), id = "name") +
  with_blend(geom_col(aes(y = perc_100), fill = "#FFFFFF", width = 0.5), bg_layer = "name", blend_type = "atop") +
  with_blend(geom_col(aes(y = perc), fill = "#000000", width = 0.5), bg_layer = "name", blend_type = "atop") +
  geom_richtext(aes(x = 0.825, y = perc, label = paste0("In the song Désolé,<br>by the Gorillaz, the<br>word <b>désolé</b> is<br>repeated 49 times,<br>making up ", scales::percent(perc), "<br>of the lyrics.")), family = font, size = 4, vjust = "top", hjust = 1, label.padding = unit(c(0, 0, 0, 0), "lines"), label.colour = NA, lineheight = 1.6, fill = "#00feff") +
  coord_flip() +
  theme_void() +
  theme(plot.caption = element_text(family = font, hjust = 0.5, size = 8, color = "#000000", margin = margin(t = 45)),
        plot.caption.position = "plot",
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#00feff"),
        plot.background = element_rect(color = NA, fill = "#00feff")) +
  labs(caption = "#30DayChartChallenge | Data: azlyrics.com | Design: Ryan Hart")
  
# save plot ———————————————————————————————————————————————————————————————
ggsave(paste0("desole_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


