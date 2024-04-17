# #30DayChartChallenge | April 2024 - Day 17 | networks
# Data Source is chatgpt

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(igraph)
library(ggraph)

# add font ----------------------------------------------------------------
font_add_google(name = "Londrina Solid", family = "Londrina Solid")
font_t <- "Londrina Solid"

font_add_google(name = "Open Sans", family = "Open Sans")
font <- "Open Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# options(scipen = 999)

# create the data ---------------------------------------------------------
# source chatgpt prompt (positive, neutral and negative)

positive_associations <- tibble(
  source = rep("ChatGPT", 5),
  target = c("Intelligent", "Efficient", "Innovative", "Helpful", "Responsive"),
  association_type = rep("Positive", 5))

neutral_associations <- tibble(
  source = rep("ChatGPT", 5),
  target = c("Automated", "Programmed", "Algorithmic", "Predictive", "Virtual"),
  association_type = rep("Neutral", 5))

negative_associations <- tibble(
  source = rep("ChatGPT", 5),
  target = c("Inaccurate", "Misleading", "Biased", "Frustrating", "Impersonal"),
  association_type = rep("Negative", 5))

# combine into one data frame ---------------------------------------------
all_associations <- bind_rows(positive_associations, neutral_associations, negative_associations)

# create edges ------------------------------------------------------------
edge_list1 <- all_associations %>%
  select(source, association_type) %>%
  unique %>%
  rename(from = source, to = association_type)

edge_list2 <- all_associations %>%
  select(association_type, target) %>%
  unique %>%
  rename(from = association_type, to = target)

# combine edges -----------------------------------------------------------
edge_list <- rbind(edge_list1, edge_list2)

# create vertices for color group -----------------------------------------
vertices <- data.frame(
  name = unique(c(as.character(edge_list$from), as.character(edge_list$to))),
  value = runif(19))

# create data frame -------------------------------------------------------
df_network <- graph_from_data_frame(edge_list, directed = TRUE, vertices = vertices)

# create plot -------------------------------------------------------------
set.seed(19)

df_network %>%
  ggraph(layout = "graphopt") +
  geom_edge_link(aes(start_cap = label_rect(node1.name, padding = margin(2, 2, 2, 2, "mm")), end_cap = label_rect(node2.name))) + 
  geom_node_text(aes(label = name, size = case_when(name %in% c("ChatGPT") ~ 6, name %in% c("Positive", "Neutral", "Negative") ~ 5, TRUE ~ 3.25)), family = font_t, fontface = "bold", hjust = 0.5) +
  annotate("text", x = 120, y = -65, label = "ChatGPT\non\nChatGPT", family = font_t, size = 8, color = "#000000", fontface = "bold", hjust = 1, lineheight = 0.8) +
  annotate("text", x = 120, y = -90, label = "Asked ChatGPT to tell me\nfive positive, negative,\nand neutral words to\ndescribe ChatGPT", family = font, size = 3.25, color = "#000000", hjust = 1, vjust = "top", lineheight = 1) +
  scale_size_identity() +
  coord_fixed(clip = "off") +
  theme_void() +
  theme(plot.caption = element_text(family = font, hjust = 0.9, size = 7.5, color = "#000000", margin = margin(t = 25)),
        plot.caption.position = "plot",
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(caption = "#30DayChartChallenge | Data: chatgpt | Design: Ryan Hart")

# save plot ———————————————————————————————————————————————————————————————
ggsave(paste0("word_assoc_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)
