# #30DayChartChallenge | April 2023 - Day 12 | theme day: BBC News
# Data Source is 

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(sysfonts)
library(png)
library(bbplot)

# load data ---------------------------------------------------------------
data <- readr::read_csv("potholes.csv")

# load default font -------------------------------------------------------
font_add("Helvetica", "/System/Library/Fonts/Helvetica.ttc")
font <- "Helvetica"

# wrangle and create df ---------------------------------------------------
df <- data %>%
  group_by(LOCALITY) %>% 
  summarise(n = n())

# create plot -------------------------------------------------------------
day_12_12042023 <- df %>% 
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 50, colour = "white", fill = "#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  bbc_style() +
  theme(plot.title = element_text(margin = margin(0, 0, 0, 0)),
    plot.subtitle = element_text(margin = margin(-10, 0, 20, 0))) +
  labs(title = "Historic potholes in Leeds",
       subtitle = "Distribution of the total number of potholes by city locality\nrepaired between Jan 2018 and Sept 2020")

# save plot ---------------------------------------------------------------
finalise_plot(plot_name = day_12_12042023,
              source = "Source: data.gov.uk",
              save_filepath = paste0("day_12_", format(Sys.time(), "%d%m%Y"), ".png"),
              width_pixels = 640,
              height_pixels = 450
              # , logo_image_path = "placeholder.png"
              )


