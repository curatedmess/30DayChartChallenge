# #30DayChartChallenge | April 2024 - Day 15 | Historical
# Data Source is from www.baseball-almanac.com

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Graduate", family = "Graduate")
font_t <- "Graduate"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# options(scipen = 999)

# get data ----------------------------------------------------------------
raw <- read.csv("boone.csv")

# wrangle and create data frame -------------------------------------------
df <- raw %>% 
  mutate(start = year(start), end = year(end)) %>% 
  mutate(label = start + ((end-start)/2))
  
# set order ---------------------------------------------------------------
df$Name <- factor(df$Name, levels = c("Ray", "Bob", "Bret", "Aaron"))
  
# Combine start and end dates into a single column
combined_dates <- c(df$start, df$end)

# Create a new data frame with the combined dates
df_label <- data.frame(year = combined_dates)

# code is a little busy...
# create plot -------------------------------------------------------------
df %>% 
  ggplot() +
  geom_text(data = df_label, aes(x = year, y = 5.15, label = year), family = font_t, color = "#000000", size = 3, fontface = "bold", angle = 45, hjust = 0) +
  geom_point(data = df_label, aes(x = year, y = 5), color = "#000000", size = 2) +
  geom_point(aes(x = start, y = Name), color = "#000000", size = 2) +
  geom_point(aes(x = end, y = Name), color = "#000000", size = 2) +
  geom_segment(aes(x = start, xend = start, y = 5, yend = Name), color = "#000000", linetype = "dotted", linewidth = 0.5) +
  geom_segment(aes(x = end, xend = end, y = 5, yend = Name), color = "#000000", linetype = "dotted", linewidth = 0.5) +
  geom_hline(yintercept = 5, linewidth = 1.2, color = "#000000") +
  geom_segment(aes(x = start, xend = end, y = Name, yend = Name), linewidth = 20) +
  geom_text(aes(x = label, y = Name, label = Name), family = font_t, size = 5, color = "#FFFFFF", hjust = 0.5, fontface = "bold", vjust = 0) +
  geom_text(aes(x = label, y = Name, label = paste0(years, " seasons")), family = font_t, size = 3, color = "#FFFFFF", hjust = 0.5, fontface = "bold", vjust = 2) +
  annotate("text", x = 1948, y = "Bob", label = "The\nBoone\nFamily", family = font_t, size = 12, color = "#000000", fontface = "bold", hjust = 0, vjust = 1.75, lineheight = 0.7) +
  annotate("text", x = 1948, y = "Aaron", label = "Three generations playing major league baseball", family = font_t, size = 4, color = "#000000", hjust = 0, vjust = 7) +
  annotate("text", x = 1954, y = "Ray", label = "Grandfather", family = font_t, size = 4, color = "#999999", hjust = 0.5, vjust = 4) +
  annotate("text", x = 1981, y = "Bob", label = "Father", family = font_t, size = 4, color = "#999999", hjust = 0.5, vjust = 4) +
  annotate("text", x = 2000.5, y = "Aaron", label = "Brothers", family = font_t, size = 4, color = "#999999", hjust = 0.5, vjust = 4) +
  scale_y_discrete(limits = rev, expand = expansion(add = 2)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.caption = element_text(family = font_t, hjust = 0.5, size = 8, color = "#000000", margin = margin(t = 0)),
        plot.caption.position = "plot",
        legend.position = "none",
        axis.line.x.top = element_line(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(caption = "#30DayChartChallenge | Data: baseball-almanac.com | Design: Ryan Hart")

# save plot ———————————————————————————————————————————————————————————————
ggsave(paste0("boone_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

