# #30DayChartChallenge | April 2023 - Day 16 | family
# Data Source is census.gov
# Source:  U.S. Census Bureau, Current Population Survey, March and Annual Social and Economic Supplements, 2022 and earlier.				

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(janitor)
library(ggtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Red Hat Display", family = "Red Hat Display")
font_add_google(name = "Red Hat Text", family = "Red Hat Text")
font_t <- "Red Hat Display"
font <- "Red Hat Text"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999)

# load data ---------------------------------------------------------------
data <- readr::read_csv("fm3.csv")

# wrangle and create df ---------------------------------------------------
df <- data %>% 
  clean_names() %>% 
  select(1, 3:5) %>% 
  pivot_longer(cols = 2:4, names_to = "parental_status", values_to = "avg_num_kids") %>% 
  na.omit() %>% 
  mutate(parental_status = case_when(parental_status == "married_couple" ~ "Married Couple",
                                     parental_status == "woman_no_spouse_present" ~ "Woman with no Spouse",
                                     parental_status == "man_no_spouse_present" ~ "Man with no Spouse"))

# change order ------------------------------------------------------------
df$parental_status <- factor(df$parental_status, levels = c("Married Couple", "Woman with no Spouse", "Man with no Spouse"))

# create plot -------------------------------------------------------------
df %>% 
  ggplot() +
  geom_line(aes(x = year, y = avg_num_kids, group = parental_status, color = parental_status)) +
  scale_color_manual(values = c("#4b3f72", "#ffc857", "#119da4")) +
  theme_minimal() +
  theme(plot.title = element_text(family = font_t, size = 14, hjust = 0, face = "bold", color = "#000000"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 10, hjust = 0, color = "#000000"),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 8, family = font, color = "#000000", hjust = 0.5),
        legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(size = 8, family = font, color = "#000000"),
        axis.title.y = element_text(size = 9, family = font, color = "#000000", margin = margin(r = 10)),
        axis.title.x = element_blank(),
        axis.line = element_line(color = "#000000", linewidth = 0.3),
        panel.grid = element_line(color = "#E0E0E0", linewidth = 0.2),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "Average number of children by type of family household",
       subtitle = "U.S. Census data from 1970 to 2022\n\n",
       y = "Average Number of Children",
       caption = "\n\n\n#30DayChartChallenge | Data: census.gov | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("day_16_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


