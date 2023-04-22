# #30DayChartChallenge | April 2023 - Day 22 | green energy
# Data Source is usgs.gov

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(scico)

# add font ----------------------------------------------------------------
font_add_google(name = "Titan One", family = "Titan One")
font_add_google(name = "Open Sans", family = "Open Sans")
font_t <- "Titan One"
font <- "Open Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999)

# load data ---------------------------------------------------------------
data <- readr::read_csv("uswtdb_v5_3_20230113.csv")

# data wrangle and create data frame --------------------------------------
df <- data %>%
  group_by(p_year) %>% 
  summarise(n = n()) %>% 
  na.omit() %>% 
  filter(p_year > 2002) %>% 
  mutate(order = seq(0, 19, by = 1)) 

# create plot -------------------------------------------------------------
df %>% 
  ggplot() + 
  # credit for gradient background code to @tanya_shapiro
  # https://twitter.com/tanya_shapiro/status/1592328864176693248?s=20&t=vpCboM081dcuOqHkpwx1PQ
  ggpattern::geom_rect_pattern(data = df, aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), pattern = "gradient", pattern_fill = "#87CEEB", pattern_fill2 = "#FFFFFF", color = NA) +
  geom_tile(aes(x = order, y = 0, fill = n), color = "#FFFFFF", size = 0.5) +
  geom_textpath(aes(x = order, y = 0, label = p_year), family = font, color = "#000000", size = 2, vjust = -3) +
  geom_hline(yintercept = 0.5, color = "#000000") +
  geom_hline(yintercept = -0.5, color = "#000000") +
  scale_y_continuous(limits = c(-7, NA)) +
  scale_fill_scico(palette = "bamako", direction = -1, breaks = c(min(df$n), max(df$n))) +
  annotate("text", x = 10, y = -6.5, label = "U.S. Wind Turbines\n", size = 7, color = "#000000", family = font_t, vjust = "bottom") +
  annotate("text", x = 10, y = -6.5, label = "From 2003 to 2022, the yearly count of turbines that\nbecame operational and started generating power.\n", size = 3, color = "#000000", family = font) +
  # annotate("text", x = 0, y = -0.8, label = "2003", size = 3, color = "#000000", family = font, angle = -10, fontface = "bold") +
  # annotate("segment", x = 0.4, y = -0.8, xend = 1.2, yend = -0.8, linewidth = 0.5, arrow = arrow(length = unit(1.25, "mm")), color = "#000000") +
  coord_curvedpolar(start = 0, clip = "off") +
  theme_void() +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(size = 8, family = font, color = "#000000", hjust = 0.5),
        legend.position = c(0.5, 0.4),
        legend.direction = "horizontal",
        legend.key.height = unit(0.20, 'cm'),
        legend.key.width = unit(0.75, 'cm'),
        legend.title = element_blank(),
        legend.text = element_text(family = font, hjust = 0.5, size = 7, color = "#000000", face = "bold"),
        plot.margin = unit(c(0.5, 0.5, 1, 0.5), "cm"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(caption = "#30DayChartChallenge | Data: usgs.gov | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("day_22_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)



