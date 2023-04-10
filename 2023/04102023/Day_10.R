# #30DayChartChallenge | April 2023 - Day 10 | hybrid
# Data Source is fueleconomy.gov (US Department of Energy)

# load libraries ----------------------------------------------------------
library(tidyverse)
library(showtext)
library(janitor)
library(ggimage)
library(ggtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Ubuntu", family = "Ubuntu")
font_add_google(name = "Inter", family = "Inter")

font_t <- "Ubuntu"
font <- "Inter"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# get image ---------------------------------------------------------------
car <- magick::image_read("https://creazilla-store.fra1.digitaloceanspaces.com/icons/7832008/car-icon-md.png") %>%
  magick::image_trim()

car_img <- magick::image_write(car, path = "car.img", format = "png")
image <- car_img

# load data ---------------------------------------------------------------
data <- read.csv("2023 FE Guide for DOE-release dates before 3-15-2023-no-sales -3-14-2023public.csv")

# wrangle data and create data frame --------------------------------------
df <- data %>% 
  clean_names() %>% 
  filter(grepl("Hybrid", descriptor_model_type_40_char_or_less)) %>% 
  select(division, carline, descriptor_model_type_40_char_or_less, comb_fe_guide_conventional_fuel) %>% 
  group_by(comb_fe_guide_conventional_fuel) %>% 
  mutate(size = n())

# create plot -------------------------------------------------------------
df %>% 
  ggplot() +
  geom_hline(yintercept = 0, color = "#000000", linewidth = 0.3) +
  geom_image(aes(x = 1.25, y = 0.15, image = image), size = 0.07, by = "height", asp = 1.75, hjust = 0) +
  geom_curve(aes(x = 0, y = 0, xend = comb_fe_guide_conventional_fuel, yend = 0), size = 0.5, curvature = -0.6, ncp = 500, color = "#000000") +
  geom_point(aes(x = comb_fe_guide_conventional_fuel, y = 0, size = size)) +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, 4)) +
  scale_x_continuous(expand = c(0, 0), breaks = c(0, seq(15, 55, by = 5))) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.title = element_markdown(family = font_t, size = 39, hjust = 0, face = "bold", color = "#000000"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 10, hjust = 0, color = "#000000", lineheight = 1.1),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 8, family = font_t, color = "#000000", hjust = 0.5),
        axis.title.x = element_text(size = 8, family = font, color = "#000000", vjust = -2.5),
        axis.text.x = element_text(size = 8, family = font, color = "#000000", vjust = -7),
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "<span style='font-size:14pt'>HYBRID VEHICLES (as of March 2023)</span><br>FUEL ECONOMY",
       subtitle = "Distribution of 176 hybrid vehicles categorized by fuel economy,\nmeasured in miles per gallon (MPG), based on a combined 55%\ncity driving and 45% highway driving",
       caption = "\n\n\n\n#30DayChartChallenge | Data: fueleconomy.gov | Design: Ryan Hart",
       x = "\n\nMiles Per Gallon (MPG)")

# save plot ---------------------------------------------------------------
ggsave(paste0("day_10_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

