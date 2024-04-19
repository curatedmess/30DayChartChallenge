# #30DayChartChallenge | April 2024 - Day 18 | Asian Development Bank (data day)
# Data Source is https://data.adb.org

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(ggtext)


# add font ----------------------------------------------------------------
font_add_google(name = "Open Sans", family = "Open Sans")
font <- "Open Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# options(scipen = 999)

# load data ---------------------------------------------------------------
data <- read.csv("Procurement by Origin of Goods and Services (2014 – 2024).csv", skip=1)

# create data frame -------------------------------------------------------
df <- data %>%
  filter(CONTRACT.YEAR == 2023) %>%
  # mutate(ADB = "ADB") %>% 
  filter(REGION == "South Asia") %>%
  mutate(ADB.FINANCED.AMOUNT = as.numeric(gsub(",", "", ADB.FINANCED.AMOUNT))) %>%
  group_by(REGION, BORROWING.COUNTRY, TYPE) %>%
  summarise(AMOUNT = sum(ADB.FINANCED.AMOUNT)) %>% 
  mutate(test = paste0(BORROWING.COUNTRY, "-", TYPE)) %>% 
  ungroup() 

# create edges ------------------------------------------------------------
edge_list2 <- df %>%
  select(REGION, BORROWING.COUNTRY) %>%
  unique %>%
  rename(from = REGION, to = BORROWING.COUNTRY)

edge_list3 <- df %>%
  select(BORROWING.COUNTRY, test) %>%
  unique %>%
  rename(from = BORROWING.COUNTRY, to = test)

# combine edges -----------------------------------------------------------
edge_list <- rbind(edge_list2, edge_list3)

vertices <- data.frame(
  name = unique(c(as.character(edge_list$from), as.character(edge_list$to))),
  value = runif(25))

# add size attribute ------------------------------------------------------
vertices$size = df$AMOUNT[match(vertices$name, df$test)]

# add color attribute -----------------------------------------------------
vertices$color <- ifelse(grepl("-", vertices$name), sub(".*-", "", vertices$name), NA)

# create data frame -------------------------------------------------------
df_network <- graph_from_data_frame(edge_list, directed = TRUE, vertices = vertices)

# create plot -------------------------------------------------------------
df_network %>% 
  ggraph('dendrogram', circular = TRUE) + 
  geom_edge_elbow(edge_width = 1, color = "#FFFFFF") +
  geom_node_point(aes(size = size, color = color, filter = leaf), alpha = 0.5) +
  geom_node_text(aes(x = x * 1.05, y = y * 1.05, filter = leaf, angle = -((-node_angle(x, y) + 90)%%180) + 90, label = scales::unit_format(prefix = "$", unit = "M", big.mark = ",", scale = 1e-6)(size)), family = font, fontface = "bold", color = "#FFFFFF", size = 2.5, hjust = "outward") +
  geom_node_label(aes(x = x, y = y, filter = !leaf, label = name, angle = -((-node_angle(x, y) + 90)%%180) + 90), family = font, size = 2.75, hjust = 0.5, vjust = 0.5, label.size = 0, fill = "#000000", color = "#FFFFFF") +
  scale_size(range = c(5, 20)) +
  scale_color_manual(values = c("#00FFFF", "#FF00FF", "#FFFF00")) +
  coord_equal(clip = "off") +
  theme_void() +
  theme(plot.title = element_text(family = font, size = 28, color = "#FFFFFF", face = "bold", hjust = 0.5, margin = margin(b = 5)),
        plot.subtitle = element_markdown(family = font, size = 10, color = "#FFFFFF", hjust = 0.5, margin = margin(b = 45), lineheight = 1.1),
        plot.title.position = "plot",
        plot.caption = element_text(family = font, hjust = 0.5, size = 7.5, color = "#FFFFFF", margin = margin(t = 45)),
        plot.caption.position = "plot",
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#000000"),
        plot.background = element_rect(color = NA, fill = "#000000")) +
  labs(title = "Asian Development Bank",
       subtitle = "Total amount financed by ADB in South Asia for goods, works, and<br>services through <span style = 'color: #00FFFF;'><b>Grants</b></span>, <span style = 'color: #FF00FF;'><b>Loans</b></span>, or <span style = 'color: #FFFF00;'><b>Technical Assistance</b></span> (2023)",
       caption = "#30DayChartChallenge | Data: adb.org | Design: Ryan Hart")

# save plot ———————————————————————————————————————————————————————————————
ggsave(paste0("adb_pacific_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

