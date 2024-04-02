# #30DayChartChallenge | April 2024 - Day 2 | Neo
# Data Source is from https://www.rsc.org/periodic-table/element/36/krypton

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(treemap)
library(ggfx)
library(ggtext)


# add font ----------------------------------------------------------------
font_add_google(name = "Staatliches", family = "Staatliches")
font_t <- "Staatliches"

font_add_google(name = "Work Sans", family = "Work Sans")
font <- "Work Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999)

# wrangle data and create df ----------------------------------------------
df <- data.frame(name = c(84, 86, 82, 83, 80, 78),
                 amount = c(56.987, 17.279, 11.593, 11.50, 2.286, 0.355))


# use this treemap hack to create a treemap -------------------------------
# https://yjunechoe.github.io/posts/2020-06-30-treemap-with-ggplot/

# treemap data from treemap -----------------------------------------------
tm <- treemap(
  dtf = df,
  index = c("name"),
  vSize = "amount")

# add center point for labels ---------------------------------------------
df_tm <- tm$tm %>% 
  mutate(x1 = x0 + w, y1 = y0 + h) %>% 
  mutate(x = (x0+x1)/2, y = (y0+y1)/2) %>% 
  mutate(y = ifelse(name == 78, y -0.22, y)) %>% 
  mutate(x = ifelse(name == 78, x + 0.1, x)) %>% 
  mutate(x0 = ifelse(name == 78, x0 + 0.1, x0)) %>%
  mutate(x1 = ifelse(name == 78, x1 + 0.1, x1)) %>% 
  mutate(y0 = ifelse(name == 78, y0 - 0.1, y0)) %>%
  mutate(y1 = ifelse(name == 78, y1 - 0.1, y1))
  
# create plot -------------------------------------------------------------
df_tm %>% 
  ggplot(aes(xmin = x0, ymin = y0, xmax = x1, ymax = y1)) + 
  with_outer_glow(with_inner_glow(geom_rect(color = "#FFFFFF", fill = NA, linewidth = 2), colour = "#04d9ff", sigma = 10, expand = 3), colour = "#04d9ff", sigma = 15, expand = 5) +
  geom_richtext(aes(x = x, y = y, label = paste0("<sup>", name, "</sup>", "Kr", " = ", round(vSize, digits = 3), "%")), family = font, color = "#FFFFFF", size = 2.6, fill = NA, label.padding = unit(c(0, 0, 0, 0), "lines"), label.colour = NA) +
  annotate("text", x = 0.4, y = -0.175, label = "Plus one isotope with an extremely long half-life", hjust = 0.5, family = font, size = 3.25, color = "#FFFFFF") +
  annotate("curve", x = 0.84, y = -0.165, xend = 0.95, yend = -0.1, linewidth = 0.5,  curvature = -0.2, arrow = arrow(length = unit(1.25, "mm")), color = "#FFFFFF") +
  scale_size(range = c(3, 6)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-0.2, 1.2)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.3, 1.1)) +
  theme_void() +
  theme(plot.title = element_text(family = font_t, hjust = 0.5, size = 34, color = "#FFFFFF", margin = margin(b = 5)),
        plot.subtitle = element_text(family = font, size = 10, color = "#FFFFFF", hjust = 0.5, margin = margin(b = 5)),
        plot.title.position = "plot",
        plot.caption = element_text(family = font, hjust = 0.5, size = 7, color = "#FFFFFF", margin = margin(t = 10)),
        plot.caption.position = "plot",
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#000000"),
        plot.background = element_rect(color = NA, fill = "#000000")) +
  labs(title = "KRYPTON",
       subtitle = "Natural abundance of five stable isotopes",
       caption = "#30DayChartChallenge | Data: rsc.org | Design: Ryan Hart")


# save plot ———————————————————————————————————————————————————————————————
ggsave(paste0("neon_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


