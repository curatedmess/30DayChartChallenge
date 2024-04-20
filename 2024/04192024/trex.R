# #30DayChartChallenge | April 2024 - Day 19 | Dinosaurs
# Data Source is wikipedia.org

# libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(cropcircles)
library(ggimage)

# add font ----------------------------------------------------------------
font_add_google(name = "Black Han Sans", family = "Black Han Sans")
font_t <- "Black Han Sans"

font_add_google(name = "Open Sans", family = "Open Sans")
font <- "Open Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# options(scipen = 999)

# create list of image URLs -----------------------------------------------
images <- c(
  "https://upload.wikimedia.org/wikipedia/en/a/a2/TRex_1st_album_cover.jpg",
  "https://upload.wikimedia.org/wikipedia/en/0/0c/ProphetsSeersAndSages.jpg",
  "https://upload.wikimedia.org/wikipedia/en/0/0f/Unicorn%28Album%29.jpg",
  "https://upload.wikimedia.org/wikipedia/en/a/aa/BeardOfStars.jpg",
  "https://upload.wikimedia.org/wikipedia/en/f/fc/T._Rex_%28Album%29.jpg",
  "https://upload.wikimedia.org/wikipedia/en/e/e5/T_Rex_Electric_Warrior_UK_album_cover.jpg",
  "https://upload.wikimedia.org/wikipedia/en/c/cd/T_Rex_The_Slider.jpg",
  "https://upload.wikimedia.org/wikipedia/en/9/92/Tanx.jpg",
  "https://upload.wikimedia.org/wikipedia/en/0/03/T_Rex_Zinc_Alloy.jpg",
  "https://upload.wikimedia.org/wikipedia/en/0/08/T_Rex_Bolans_Zip_Gun.jpg",
  "https://upload.wikimedia.org/wikipedia/en/7/78/FuturisticDragon_TRexalbum.jpg",
  "https://upload.wikimedia.org/wikipedia/en/0/09/Dandy_in_the_Underworld_%28T.Rex_album%29_cover_art.jpg"
  )

# create data -------------------------------------------------------------
trex <- data.frame(order = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                   name = c("My People Were Fair and Had Sky in Their Hair... But Now They're Content to Wear Stars on Their Brows",
                            "Prophets, Seers & Sages: The Angels of the Ages",
                            "Unicorn",
                            "A Beard of Stars",
                            "T. Rex",
                            "Electric Warrior",
                            "The Slider",
                            "Tanx",
                            "Zinc Alloy and the Hidden Riders of Tomorrow",
                            "Bolan's Zip Gun",
                            "Futuristic Dragon",
                            "Dandy in the Underworld"),
                   year = c(1968, 1968, 1969, 1970, 1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977),
                   images = square_crop(images, border_size = 10, just = "center")) 

# variables
num_points <- 12
w <- 3
h <- 4


# create points -----------------------------------------------------------
df_points <- tibble(id = seq(1, num_points, 1)) %>% 
  mutate(x = ifelse(id %% 2 == 0, rep(w:1, by = 1, w), rep(1:w, by = 1, w))) %>% 
  mutate(y = rep(h:1, each = w, length.out = nrow(.)))

# merge -------------------------------------------------------------------
df <- df_points %>% 
  left_join(trex, by = c("id" = "order"))

# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(x = x, y = y)) +
  geom_line(aes(group = y), linewidth = 1.25) +
  annotate("segment", x = 0.5, y = 4, xend = 0.74, yend = 4, linewidth = 1.25) +
  annotate("curve", x = 3.25, y = 4, xend = 3.25, yend = 3, curvature = -1, ncp = 4500, linewidth = 1.25) +
  annotate("curve", x = 0.75, y = 3, xend = 0.75, yend = 2, curvature = 1, ncp = 4500, linewidth = 1.25) +
  annotate("curve", x = 3.25, y = 2, xend = 3.25, yend = 1, curvature = -1, ncp = 4500, linewidth = 1.25) +
  annotate("segment", x = 1, y = 1, xend = 0.5, yend = 1, linewidth = 1.25, arrow = arrow(length = unit(2, "mm"))) +
  geom_image(aes(image = images), size = 0.15, by = "height", asp = 1) +
  geom_text(aes(x = x, y = y + 0.47, label = year), family = font, color = "#000000", size = 2.5, fontface = "bold") +
  scale_y_continuous(limits = c(0, 5)) +
  scale_x_continuous(limits = c(0, 4)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.title = element_text(family = font_t, size = 28, color = "#000000", face = "bold", hjust = 0.5, margin = margin(b = 5)),
        plot.subtitle = element_text(family = font, size = 10, color = "#000000", hjust = 0.5, margin = margin(b = 0), lineheight = 1.1),
        plot.title.position = "plot",
        plot.caption = element_text(family = font, hjust = 0.5, size = 7.5, color = "#000000", margin = margin(t = 0)),
        plot.caption.position = "plot",
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "T.REX DISCOGRAPHY",
       subtitle = "This English rock band released their first four albums under the\nname Tyrannosaurus Rex before changing their name to T.Rex.",
       caption = "#30DayChartChallenge | Data: wikipedia.org | Design: Ryan Hart")
             
# save plot ———————————————————————————————————————————————————————————————
ggsave(paste0("trex_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

