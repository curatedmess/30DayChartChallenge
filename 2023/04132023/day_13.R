# #30DayChartChallenge | April 2023 - Day 13 | pop culture
# Data Source is wikipedia.org

# libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(igraph)
library(ggraph)

# add font ----------------------------------------------------------------
font_add_google(name = "Oswald", family = "Oswald")
font_add_google(name = "Source Sans Pro", family = "Source Sans Pro")
font_t <- "Oswald"
font <- "Source Sans Pro"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data ---------------------------------------------------------------
data <- read.csv("day13.csv")

df_data <- data %>% 
  mutate(shape = ifelse(From == "The Outsiders", "Movie", "Others"))

# create vertices for color group -----------------------------------------
vertices <- data.frame(
  name = unique(c(as.character(df_data$From), as.character(df_data$To))), 
  value = runif(380))

vertices$group = df_data$From[match(vertices$name, df_data$To, df_data$shape)] %>% 
  replace_na("none")

# create data frame -------------------------------------------------------
df_network <- graph_from_data_frame(df_data, directed = TRUE, vertices = vertices) 

# create plot -------------------------------------------------------------
set.seed(5)

df_network %>% 
  ggraph(layout = "dh") + 
  geom_edge_link(color = "#9E9E9E") +
  geom_node_point(aes(size = ifelse(group == "none", 8, 1)), color = "#9E9E9E") +
  geom_node_text(aes(label = ifelse(group == "The Outsiders", vertices$name, "")), family = font, fontface = "bold", size = 5, hjust = 0.5, color = "#FFFFFF", vjust = "middle") +
  scale_color_identity() +
  scale_size_identity() +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.title = element_text(family = font_t, size = 39, hjust = 0, face = "bold", color = "#FFFFFF"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 10, hjust = 0, color = "#FFFFFF", lineheight = 1.1),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 8, family = font, color = "#FFFFFF", hjust = 0.5),
        legend.position = "top",
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        plot.background = element_rect(color = NA, fill = "#000000")) +
  labs(title = "THE OUTSIDERS",
       subtitle = "This 1983 movie starred C. Thomas Howell, Ralph Macchio, Matt Dillon, Patrick Swayze,\nRob Lowe, Emilio Estevez, and Tom Cruise who have combined acting film credits of more\nthan 300 films.",
       caption = "\n\n#30DayChartChallenge | Data: wikipedia.org | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("day_13_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

