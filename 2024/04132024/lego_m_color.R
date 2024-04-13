# #30DayChartChallenge | April 2024 - Day 13 | Family
# Data Source is from rebrickable.com via #TidyTuesday
# idea inspired by https://brickarchitect.com/color/

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(scales)
library(plotwidgets)

# add font ----------------------------------------------------------------
font_add_google(name = "Open Sans", family = "Open Sans")
font <- "Open Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999)

# get data ----------------------------------------------------------------
inventories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')
# inventory_sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_sets.csv.gz')
sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')
colors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/colors.csv.gz')
# parts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/parts.csv.gz')
inventory_parts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_parts.csv.gz')


# function to get HSL color -----------------------------------------------
convert_hex_to_hsl <- function(hex_color) {
  # Check if hex color code is valid
  if (!grepl("^#[A-Fa-f0-9]{6}$", hex_color)) {
    return(NA)
  }
  
  # Convert hex color code to RGB
  rgb_values <- col2rgb(hex_color)
  r <- rgb_values[1] / 255
  g <- rgb_values[2] / 255
  b <- rgb_values[3] / 255
  
  # Convert RGB to HSL
  hsl_values <- col2hsl(rgb(r, g, b))
  
  # Format HSL values as a string
  hsl_string <- paste(hsl_values, collapse = ",")
  
  return(hsl_string)
}

# create data frame -------------------------------------------------------
df <- left_join(inventory_parts, colors, by = c("color_id" = "id")) %>% 
  select(inventory_id, part_num, color_id, rgb) %>% 
  left_join(inventories, colors, by = c("inventory_id" = "id")) %>%
  left_join(sets, by = "set_num") %>%
  filter(theme_id == 577) %>%
  filter(!version == 2) %>%
  filter(!name == "Minecraft Bundle") %>% 
  mutate(hex = paste0("#", rgb),
         hsl = sapply(hex, convert_hex_to_hsl)) %>% 
  separate(hsl, into = c("hue", "saturation", "lightness"), sep = ",") %>%
  mutate(across(c(hue, saturation, lightness), as.numeric)) %>%  
  group_by(hex) %>% 
  mutate(count = n()) %>% 
  distinct(hex, .keep_all = TRUE)

# create plot -------------------------------------------------------------
df %>% 
  ggplot() +
  geom_point(aes(x = saturation, y = lightness, fill = hex, size = count), shape = 21, color = "#000000") +
  scale_fill_identity() +
  scale_size(range = c(3, 9), breaks = c(min(df$count), 290, 580, 870, max(df$count)), name = "# of parts") +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_percent()) +
  theme_void() +
  theme(plot.title = element_text(family = font, size = 25, color = "#000000", face = "bold", hjust = 0.5, margin = margin(b = 1)),
        plot.subtitle = element_text(family = font, size = 10, color = "#000000", hjust = 0.5, margin = margin(b = 15)),
        plot.title.position = "plot",
        plot.caption = element_text(family = font, hjust = 0.5, size = 7, color = "#000000", margin = margin(t = 25)),
        plot.caption.position = "plot",
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(family = font, size = 7, color = "#000000"),
        panel.grid = element_line(color = "#D3D3D3", linetype = "dotted", linewidth = 0.2),
        axis.title.x = element_text(family = font, color = "#000000", face = "bold", size = 8, margin = margin(t = 15)),
        axis.text.x = element_text(family = font, color = "#000000", size = 8, margin = margin(t = 5)),
        axis.title.y = element_text(family = font, color = "#000000", face = "bold", size = 8, margin = margin(r = 15), angle = 90),
        axis.text.y = element_text(family = font, color = "#000000", size = 8, margin = margin(t = 5)),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#e8e8e8"),
        plot.background = element_rect(color = NA, fill = "#e8e8e8")) +
  labs(title = "COLORS OF LEGO MINECRAFT",
       subtitle = "Saturation vs lightness sized by the total number of unique parts in that color.",
       x = "SATURATION",
       y = "LIGHTNESS",
       caption = "#30DayChartChallenge | Data: rebrickable.com via #TidyTuesday | Design: Ryan Hart")

# save plot ———————————————————————————————————————————————————————————————
ggsave(paste0("lego_colors_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)



