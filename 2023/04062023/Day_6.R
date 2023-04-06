# #30DayChartChallenge | April 2023 - Day 6 | data day: OWID
# Data Source is ourworldindata.org/water-access#access-to-safe-drinking-water

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(janitor)
library(MetBrewer)

# add font ----------------------------------------------------------------
font_add_google(name = "Oswald", family = "Oswald")
font_add_google(name = "Source Sans Pro", family = "Source Sans Pro")
font_t <- "Oswald"
font <- "Source Sans Pro"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data ---------------------------------------------------------------
data <- readr::read_csv("water-and-sanitation.csv")

# wrangle and create df ---------------------------------------------------
df <- data %>% 
  select(92, Entity, 1, 4, 5) %>% 
  na.omit() %>% 
  filter(Entity %in% c("High income", "Upper-middle income", "Lower-middle income", "Low income")) %>% 
  clean_names() %>%
  mutate(entity = str_to_title(entity)) %>% 
  gather(!c(year, entity), key = access, value = percent) %>% 
  mutate(access = case_when(access == "access_to_improved_drinking_water" ~ "Improved",
                            access == "access_to_unimproved_drinking_water" ~ "Unimproved",
                            access == "no_access_to_drinking_water" ~ "No Access (Surface Water)")) %>% 
  mutate(access = fct_relevel(access, "Improved", "Unimproved", "No Access (Surface Water)"))

# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(x = year, y = percent, fill = access)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = rev(met.brewer("Demuth", 3))) +
  facet_wrap(~ factor(entity, c("High Income", "Upper-Middle Income", "Lower-Middle Income", "Low Income"))) +
  theme_void() +
  theme(plot.title = element_text(family = font_t, size = 22, hjust = 0, face = "bold", color = "#000000"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 10, hjust = 0, color = "#000000"),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 8, family = font, color = "#000000", hjust = 0.5),
        strip.text = element_text(size = 8, family = font, color = "#000000", hjust = 0.5, margin = margin(b = 2)),
        legend.position = "top",
        legend.margin = margin(b = 10),
        legend.text = element_text(size = 7, family = font, color = "#000000", hjust = 0.5),
        legend.title = element_blank(),
        axis.text = element_text(size = 7, family = font, color = "#000000"),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "Drinking Water",
       subtitle = "Global access to improved drinking water has increased significantly over this decade, with\nall income groups benefiting. However, by the end of the decade, approximately 117 million\npeople still rely on surface water as their primary source of drinking water, most of whom\nare from lower-middle and low-income countries.\n\n",
       caption = "\n\n\n#30DayChartChallenge | Data: ourworldindata.org | Design: Ryan Hart")

  # save plot ---------------------------------------------------------------
ggsave(paste0("day_6_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

