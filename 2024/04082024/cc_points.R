# #30DayChartChallenge | April 2024 - Day 8 | circular
# Data Source is from espn.com to get career points

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(ggsvg)

# add font ----------------------------------------------------------------
font_add_google(name = "Permanent Marker", family = "Permanent Marker")
font_t <- "Permanent Marker"

font_add_google(name = "Open Sans", family = "Open Sans")
font <- "Open Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999)

# get basketball svg icon -------------------------------------------------
svg_url_bball <- 'https://openmoji.org/data/color/svg/1F3C0.svg'
svg_txt_bball <- paste(readLines(svg_url_bball), collapse = "\n")

# function to create points within a circle -------------------------------
# used the following site for reference and some chatgpt to develop this --
# https://meyavuz.wordpress.com/2018/11/15/generate-uniform-random-points-within-a-circle/
# https://mathworld.wolfram.com/DiskPointPicking.html

points <- function(n, radius = 1) {
  x <- numeric(n)
  y <- numeric(n)
  for (i in 1:n) {
    r <- radius * sqrt(runif(1))
    theta <- runif(1) * 2 * pi
    x[i] <- r * cos(theta)
    y[i] <- r * sin(theta)
  }
  return(data.frame(x = x, y = y))
}

num_points <- 3951
radius <- 1

# create points -----------------------------------------------------------
df <- points(num_points, radius)

# create plot -------------------------------------------------------------
df %>% 
  ggplot() +
  # geom_point(aes(x = x, y = y), color = "red", size = 1) +
  geom_point_svg(aes(x = x, y = y), svg = svg_txt_bball, size = 4) +
  annotate("text", x = -1.15, y = -1.05, label = scales::comma(num_points), family = font_t, size = 10, color = "#000000", hjust = 0) +
  annotate("text", x = -1.15, y = -1.2, label = paste0("Career points scored by Caitlin Clark"), family = font, size = 4, color = "#000000", hjust = 0, vjust = "top") +
  scale_x_continuous(limits = c(-1.15, 1)) +
  scale_y_continuous(limits = c(-1.25, 1)) +
  coord_equal() +
  theme_void() +
  theme(plot.caption = element_text(family = font, hjust = 0.5, size = 7.5, color = "#000000", margin = margin(t = 25)),
        plot.caption.position = "plot",
        legend.position = "none",
        plot.margin = unit(c(0.5, 0.5, 1, 0.5), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(caption = "#30DayChartChallenge | Data: espn.com | Design: Ryan Hart")

# save plot ———————————————————————————————————————————————————————————————
ggsave(paste0("cc_points", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

