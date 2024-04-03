# #30DayChartChallenge | April 2024 - Day 3 | Makeover
# Data Source is https://en.wikipedia.org/wiki/List_of_Pimp_My_Ride_episodes

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(ggsvg)

# add font ----------------------------------------------------------------
font_add_google(name = "Damion", family = "Damion")
font_t <- "Damion"

font_add_google(name = "Open Sans", family = "Open Sans")
font <- "Open Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999)


# get data ----------------------------------------------------------------
# data manually gathered from wikipedia -----------------------------------
raw <- read.csv("pmr.csv")

# get images --------------------------------------------------------------
# get emoji icon from https://openmoji.org --------------------------------
svg_url_truck <- 'https://openmoji.org/data/color/svg/1F6FB.svg'
svg_txt_truck <- paste(readLines(svg_url_truck), collapse = "\n")

svg_url_van <- 'https://openmoji.org/data/color/svg/1F690.svg'
svg_txt_van <- paste(readLines(svg_url_van), collapse = "\n")

svg_url_suv <- 'https://openmoji.org/data/color/svg/1F699.svg'
svg_txt_suv <- paste(readLines(svg_url_suv), collapse = "\n")

svg_url_car <- 'https://openmoji.org/data/color/svg/1F697.svg'
svg_txt_car <- paste(readLines(svg_url_car), collapse = "\n")

svg_url_delivery <- 'https://openmoji.org/data/color/svg/1F69A.svg'
svg_txt_delivery <- paste(readLines(svg_url_delivery), collapse = "\n")

# wrangle and create data frame -------------------------------------------
df <- raw %>% 
  na.omit() %>% 
  group_by(Season, Type) %>%
  mutate(count = n()) %>% 
  ungroup() %>% 
  arrange(desc(count)) %>%
  arrange(desc(count), Type) %>% 
  group_by(Season) %>% 
  mutate(row_num = row_number())

# create plot -------------------------------------------------------------
df %>% 
  ggplot() +
  geom_point_svg(aes(x = Season, y = row_num, svg = case_when(Type == "Car" ~ svg_txt_car,
                                                              Type == "Truck" ~ svg_txt_truck,
                                                              Type == "Van" ~ svg_txt_van,
                                                              Type == "SUV" ~ svg_txt_suv, 
                                                              Type == "Delivery" ~ svg_txt_delivery)), size = 12) +
  geom_hline(yintercept = 0, color = "#000000", linewidth = 1) +
  annotate("text", x = 3, y = 15, label = "Pimp my Ride", family = font_t, size = 12, color = "#000000", hjust = 0.5) +
  annotate("text", x = 3, y = 13.5, label = "A MTV television series that offered vehicle\nowners the chance to transform their cars,\nSUVs, trucks, vans, and even a delivery\nvehicle with a makeover (aired: 2004-2009)", family = font, size = 3.25, color = "#000000", hjust = 0.5, vjust = "top") +
  scale_x_continuous(breaks = c(1:6)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.caption = element_text(family = font, hjust = 0.5, size = 7, color = "#000000", margin = margin(t = 25)),
        plot.caption.position = "plot",
        axis.text.x = element_text(family = font, hjust = 0.5, size = 9, face = "bold", color = "#000000", margin = margin(t = 5)),
        axis.title.x = element_text(family = font, hjust = 0.5, size = 10, face = "bold", color = "#000000", margin = margin(t = 10)),
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#F2F2F2"),
        plot.background = element_rect(color = NA, fill = "#F2F2F2")) +
  labs(x = "Seasons",
       caption = "#30DayChartChallenge | Data: wikipedia.com | Icons: openmoji.org | Design: Ryan Hart")

# save plot ———————————————————————————————————————————————————————————————
ggsave(paste0("pmr_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)



