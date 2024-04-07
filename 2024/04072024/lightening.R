# #30DayChartChallenge | April 2024 - Day 7 | Hazards
# Data Source is https://hazards.fema.gov/nri/data-resources#csvDownload

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(ggpubr)
library(magick)
library(ggfx)
library(png)

# add font ----------------------------------------------------------------
font_add_google(name = "Sriracha", family = "Sriracha")
font_t <- "Sriracha"

font_add_google(name = "Work Sans", family = "Work Sans")
font <- "Work Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999)

# get image ---------------------------------------------------------------
url <-"https://images.pexels.com/photos/53459/lightning-storm-weather-sky-53459.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=2"
img <- image_read(url)

# get data ----------------------------------------------------------------
raw <- read.csv("NRI_Table_Counties_NorthCarolina.csv")

# wrangle and create data frame -------------------------------------------
df <- raw %>% 
  select(c(COUNTYFIPS, LTNG_EVNTS)) %>% 
  pivot_longer(cols = c("LTNG_EVNTS"), names_to = "type", values_to = "count")

# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(x = reorder(as.factor(COUNTYFIPS), desc(count)), y = count)) +
  as_reference(geom_col(), id = "histogram") +
  with_blend(background_image(img), bg_layer = "histogram", blend_type = "atop") +
  # scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_void() +
  theme(plot.title = element_text(family = font_t, size = 33, color = "#000000", face = "bold", hjust = 0.5, margin = margin(b = 5)),
        plot.subtitle = element_text(family = font, size = 9, color = "#000000", hjust = 0.5, margin = margin(b = 35)),
        plot.title.position = "plot",
        plot.caption = element_text(family = font, hjust = 0.5, size = 7, color = "#000000", margin = margin(t = 35)),
        plot.caption.position = "plot",
        # axis.text.x = element_text(family = font, hjust = 0.5, size = 8, color = "#000000", margin = margin(t = 5)),
        axis.text.y = element_text(family = font, hjust = 0.5, size = 8, color = "#000000", margin = margin(r = 5)),
        axis.title.y = element_text(family = font_t, hjust = 0.5, size = 10, color = "#000000", margin = margin(r = 10), angle = 90),
        axis.title.x = element_text(family = font_t, hjust = 0.5, size = 10, color = "#000000", margin = margin(t = 10)),
        axis.line.x = element_line(linewidth = 0.5),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "LIGHTNING",
       subtitle = "County-level distribution for North Carolina, USA from FEMA's National Risk Index",
       y = "# of Lightning Events", 
       x = "100 Counties of North Carolina",
       caption = "#30DayChartChallenge | Data: fema.gov | Design: Ryan Hart")

# save plot ———————————————————————————————————————————————————————————————
ggsave(paste0("hazards_nc", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

