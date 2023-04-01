# #30DayChartChallenge | April 2023 - Day 1 | part-to-whole
# Data Source is www.osbm.nc.gov

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(janitor)
library(MetBrewer)

# add font ----------------------------------------------------------------
font_add_google(name = "Rubik", family = "Rubik")
font_t <- "Rubik"

font_add_google(name = "Roboto Mono", family = "Roboto Mono")
font <- "Roboto Mono"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data ---------------------------------------------------------------
data <- readr::read_csv("vehicle-registration.csv")

# wrangle and create df ---------------------------------------------------
df <- data %>% 
  clean_names() %>% 
  filter(year == 2022) %>%
  filter(area_name %in% c("Hyde County", "Orange County")) %>% 
  select(2:3, 6:8) %>% 
  pivot_longer(!area_name, names_to = "type", values_to = "units") %>% 
  mutate(type = replace(type, type == "all_hybrids", "hybrid")) %>% 
  group_by(area_name, type) %>% 
  summarise(n = sum(units)) %>%
  mutate(percent = round(n / sum(n), 5)) %>% 
  group_by(type) %>% 
  mutate(ymax = if_else(type %in% c("gas", "electric"), sqrt(percent), 0),
         xmax = if_else(type %in% c("hybrid", "electric"), sqrt(percent), 0),
         xmin = if_else(type %in% c("gas", "diesel"), -sqrt(percent), 0),
         ymin = if_else(type %in% c("diesel", "hybrid"), -sqrt(percent), 0)) %>% 
  mutate(type = str_to_title(type)) %>% 
  mutate(area_name = case_when(area_name == "Hyde County" ~ "Hyde County\n(low)",
                               area_name == "Orange County" ~ "Orange County\n(high)"))

df$type <- factor(df$type, levels = c("Gas", "Diesel", "Hybrid", "Electric"))

# create plot -------------------------------------------------------------
df %>% 
  ggplot() +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = type), color = "#FFFFFF") +
  geom_text(aes(x = ifelse(type %in% c("Gas", "Electric"), xmin + 0.01, xmin + 0.01), y = ifelse(type %in% c("Gas", "Electric"), ymax + 0.05, ymin - 0.05), label = scales::percent(percent), color = type), family = font, size = 2.5, hjust = 0) +
  scale_color_manual(values = rev(met.brewer("Kandinsky", 4))) +
  scale_fill_manual(values = rev(met.brewer("Kandinsky", 4))) +
  facet_wrap(~ area_name) +
  coord_equal(clip = "off") +
  theme_void() +
  theme(plot.title = element_text(family = font_t, size = 24, hjust = 0, face = "bold", color = "#000000"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 9, hjust = 0, color = "#000000"),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 8, family = font, color = "#000000", hjust = 0.5),
        strip.text = element_text(size = 9, family = font_t, color = "#000000", hjust = 0.5, margin = margin(b = 1)),
        legend.position = "top",
        legend.margin = margin(b = 15),
        legend.text = element_text(size = 8, family = font, color = "#000000", hjust = 0.5),
        legend.title = element_blank(),
        plot.margin = unit(c(1, 0.25, 1, 0.25), "cm"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "What are people driving?",
       subtitle = "North Carolina's 2022 vehicle registration data highlight vehicle\npreferences of residents in low and high per capita income counties,\nsegmented by fuel type percentages.\n\n",
       caption = "\n\n\n#30DayChartChallenge | Data: osbm.nc.gov | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("day_1_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


