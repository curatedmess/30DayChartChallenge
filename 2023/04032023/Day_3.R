# #30DayChartChallenge | April 2023 - Day 3 | fauna/flora
# Data Source is ncnhp.org

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(janitor)
library(ggtext)
library(igraph)
library(ggraph)
library(ggimage)

# add font ----------------------------------------------------------------
font_add_google(name = "Josefin Sans", family = "Josefin Sans")
font_t <- "Josefin Sans"

font_add_google(name = "Open Sans", family = "Open Sans")
font <- "Open Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data ---------------------------------------------------------------
data <- readr::read_csv("nhp-species-search-results-3.csv")

# wrangle and create df ---------------------------------------------------
df <- data %>%
  clean_names() %>% 
  group_by(taxonomic_group, common_name) %>%
  count(nc_status) %>%
  ungroup() %>%
  # select(1,3,4) %>% 
  # distinct() %>% 
  group_by(nc_status)

# create edge -------------------------------------------------------------
edge_list1 <- df %>% 
  select(taxonomic_group, nc_status) %>% 
  unique %>% 
  rename(from = taxonomic_group, to = nc_status) %>% 
  mutate(color = to)

edge_list2 <- df %>% 
  select(nc_status, common_name) %>% 
  unique %>% 
  rename(from = nc_status, to = common_name) %>% 
  mutate(color = from)

edge_list = rbind(edge_list1, edge_list2)

# create vertices for color group -----------------------------------------
vertices <- data.frame(
  name = unique(c(as.character(edge_list$from), as.character(edge_list$to))), 
  value = runif(57))

vertices$group = edge_list$from[match(vertices$name, edge_list$to, edge_list$color)] %>% 
  replace_na("none")

vertices$group = edge_list$from[match(vertices$name, edge_list$to, edge_list$color)] %>% 
  replace_na("none")

# create data frame for dendrogram ----------------------------------------
dendro <- graph_from_data_frame(edge_list, vertices = vertices)

# get image ---------------------------------------------------------------
butterfly <- magick::image_read("https://upload.wikimedia.org/wikipedia/commons/thumb/a/a8/Butterfly_black.svg/350px-Butterfly_black.svg.png") %>%
  magick::image_trim()

butterfly_img <- magick::image_write(butterfly, path = "butterfly.img", format = "png")

image <- butterfly_img

# create plot -------------------------------------------------------------
dendro %>% 
  ggraph(layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal2(color = "#BDBDBD") +
  geom_image(aes(x = 0, y = 0, image = image), size = 0.08) +
  geom_node_point(aes(filter = leaf, color = group)) +
  geom_node_text(aes(x = x * 1.15, y = y * 1.15, filter = leaf, angle = -((-node_angle(x, y) + 90)%%180) + 90, label = name), hjust = "outward", size = 2.25) +
  annotate("text", x = -3, y = 2.65, label = "Butterflies", family = font_t, size = 14, color = "#000000", hjust = 0) +
  annotate("richtext", x = -3, y = 2.3, label = "There are 56 butterfly species tracked by the<br>North Carolina Natural Heritage Program<br>with a state protection status of<br><span style='color: #66BB6A;'><b>Rare but Relatively Secure</span>,<br><span style='color: #42A5F5;'><b>Poorly Known in NC</b></span>,<br><span style='color: #AB47BC;'><b>Threat to Habitat</b></span>,<br>and <span style='color: #EF5350;'><b>Significantly<br>Rare</b></span>", family = font, size = 3, hjust = 0, vjust = "top", fill = NA, label.color = NA, lineheight = 1.5, label.padding = unit(c(0, 0, 0, 0), "lines"),) +
  scale_x_continuous(limits = c(-3, 2)) +
  scale_y_continuous(limits = c(-2, 2.75)) +
  scale_color_manual(values = c("#EF5350", "#66BB6A", "#42A5F5", "#AB47BC")) +
  coord_equal(clip = "off") +
  theme_void() + 
  theme(plot.caption.position = "plot",
          plot.caption = element_text(size = 8, family = font, color = "#000000", hjust = 0.5),
          legend.position = "none",
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
    labs(caption = "\n#30DayChartChallenge | Data: ncnhp.org | Design: Ryan Hart")
  
# save plot ---------------------------------------------------------------
ggsave(paste0("day_3_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)



# angle of text labels
# https://stackoverflow.com/questions/43153004/how-to-read-a-text-label-in-ggraph-radial-graph


