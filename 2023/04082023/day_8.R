# #30DayChartChallenge | April 2023 - Day 8 | humans
# Data Source is rebrickable.com

# load libraries ----------------------------------------------------------
library(tidyverse)
library(showtext)
library(ggimage)
library(cropcircles)

# add font ----------------------------------------------------------------
font_add_google(name = "Slackey", family = "Slackey")
font_add_google(name = "Play", family = "Play")
font_t <- "Slackey"
font <- "Play"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data ---------------------------------------------------------------
raw_inv <- readr::read_csv("inventories.csv")
raw_inv_mini <- readr::read_csv("inventory_minifigs.csv")
raw_mini <- readr::read_csv("minifigs.csv")
raw_sets <- readr::read_csv("sets.csv")

# used this post to learn about the sequence(n()) trick for the plot -----
# https://www.r-bloggers.com/2020/02/gender-balance-in-the-irish-elections-a-k-a-an-excellent-excuse-to-learn-how-to-create-stacked-point-plots-and-butterfly-plots-in-r/

# wrangle data for df -----------------------------------------------------
df <- raw_inv %>% 
  left_join(raw_inv_mini, by = c("id" = "inventory_id")) %>%
  left_join(raw_mini, by = "fig_num") %>%
  left_join(raw_sets %>% 
              select(-num_parts,-img_url), by = "set_num") %>%
  filter(theme_id == 577) %>% 
  filter(grepl("Steve|Alex", name.x)) %>% 
  mutate(name.x = case_when(grepl("Steve", name.x) ~ "Steve",
                            grepl("Alex", name.x) ~ "Alex")) %>% 
  select(!c(version, quantity, fig_num, theme_id)) %>% 
  group_by(year, name.x) %>% 
  mutate(x = sequence(n())) %>% 
  mutate(x = ifelse(name.x == "Alex", -x, x)) %>% 
  ungroup() %>% 
  mutate(images_cropped = ifelse(name.x == "Alex", square_crop("https://cdn.rebrickable.com/media/thumbs/parts/elements/6128874.jpg/250x250p.jpg"), square_crop("https://cdn.rebrickable.com/media/thumbs/parts/elements/6103062.jpg/250x250p.jpg")))

# create plot -------------------------------------------------------------
df %>% 
  ggplot() +
  geom_image(aes(y = year, x = x, image = images_cropped), size = 0.1, by = "height", asp = 1.8) +
  geom_text(aes(y = year, x = 0, label = year), family = font, size = 3, color = "#000000") +
  scale_y_continuous(expand = c(0, 0), breaks = seq(2014, 2023, by = 1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-7.5, 7.5)) +
  coord_equal(clip = "off") +
  theme_void() +
  theme(plot.title = element_text(family = font_t, size = 40, hjust = 0.5, face = "bold", color = "#000000"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 10, hjust = 0.5, color = "#000000"),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 9, family = font, color = "#000000", hjust = 0.5),
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF"),
        aspect.ratio = 1/1.8) +
  labs(title = "ALEX or STEVE",
       subtitle = "Distribution of two mini-figures in Minecraft-themed Lego sets by release\nyear representing the most popular default player characters from the\nMinecraft game. Alex has appeared 17 fewer times through January 2023.\n\n\n",
       caption = "\n\n\n\n#30DayChartChallenge | Data: rebrickable.com | Design: Ryan Hart")

# reference for how to fix aspect ratio of images with using ggsave -------
# https://gist.github.com/jthomasmock/ac20e237b4cf954157f9142f3ce80c00

# save plot ---------------------------------------------------------------
ggsave(paste0("day_8_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6/1)


