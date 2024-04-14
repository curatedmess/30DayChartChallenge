# #30DayChartChallenge | April 2024 - Day 14 | Heatmap
# Data Source is from Google Trends

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(gtrendsR)
library(viridis)

# install.packages("gtrendsR")

# add font ----------------------------------------------------------------
font_add_google(name = "Just Another Hand", family = "Just Another Hand")
font_t <- "Just Another Hand"

font_add_google(name = "Poppins", family = "Poppins")
font <- "Poppins"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999)

# get data ----------------------------------------------------------------
search_terms <- c("brunch")

raw <- gtrends(keyword = search_terms,
        geo = "US",
        time = "today 3-m")

# create data frame -------------------------------------------------------
df <- raw %>% 
  .$interest_over_time %>%  
  data.frame() %>% 
  mutate(year = year(date),
         month_abb = month(date, label = TRUE, abbr = TRUE),
         day = wday(date, label = TRUE),
         first_day_of_year = floor_date(date, "year"),
         week_of_year = as.integer((date - first_day_of_year + wday(first_day_of_year, week_start = 1) - 1) / 7) + 1) %>% 
  mutate(mm_dd = format(date, "%m-%d")) %>% 
  slice(2:91)


df$day <- factor(df$day, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
  

# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(x = day, y = week_of_year)) +
  geom_text(aes(label = mm_dd), family = font, fontface = "bold", size = 2.5, color = "#000000", hjust = 0.5) +
  geom_tile(aes(x = day, y = week_of_year, fill = hits), color = "#FFFFFF", alpha = 0.74, linewidth = 1) +
  scale_fill_viridis(option = "C", trans = "log", direction = -1, breaks = c(min(df$hits), max(df$hits)), labels = c("Less", "More"), alpha = 0.74, name = "Interest") +
  scale_color_identity() +
  scale_y_continuous(trans = "reverse", limits = c(NA, 1.5)) +
  theme_void() +
  theme(plot.title = element_text(family = font_t, size = 28, color = "#000000", face = "bold", hjust = 0.5, margin = margin(b = 25)),
      plot.title.position = "plot",
      plot.caption = element_text(family = font, hjust = 0.5, size = 8.5, color = "#000000", margin = margin(t = 25)),
      plot.caption.position = "plot",
      legend.position = "inside",
      legend.position.inside = c(0.5, 1),
      legend.direction = "horizontal",
      legend.title.position = "top",
      legend.title = element_text(family = font, hjust = 0.5, size = 8, color = "#000000", face = "bold"),
      legend.text = element_text(family = font, hjust = 0.5, size = 7, color = "#000000", face = "bold"),
      legend.key.height = unit(0.4, 'cm'),
      axis.text.x = element_text(family = font, color = "#000000", size = 10, face = "bold", margin = margin(t = 3)),
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      panel.background = element_rect(color = NA, fill = "#FFFFFF"),
      plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
labs(title = "Brunch, it's a weekend (and Easter) thing per Google Trends.",
     x = "DATE",
     caption = "#30DayChartChallenge | Data: Google Trends, last 90 days | Design: Ryan Hart")

# save plot ———————————————————————————————————————————————————————————————
ggsave(paste0("brunch_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


