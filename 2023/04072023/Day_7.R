# #30DayChartChallenge | April 2023 - Day 7 | hazards
# Data Source is fema.gov
# https://www.fema.gov/openfema-data-page/hazard-mitigation-assistance-projects-v2
# "FEMA and the Federal Government cannot vouch for the data or analyses derived from these data after the data have been retrieved from the Agency's website.”

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(ggtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Oswald", family = "Oswald")
font_add_google(name = "Source Sans Pro", family = "Source Sans Pro")
font_t <- "Oswald"
font <- "Source Sans Pro"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999)

# load data ---------------------------------------------------------------
# data <- readr::read_csv("fema_national_household_survey_data_and_codebook_2021.csv")
data <- readr::read_csv("HazardMitigationAssistanceProjects.csv")

# wrangle and create df ---------------------------------------------------
df <- data %>% 
  # filter(state == "California") %>% 
  filter(grepl("Safe", projectType)) %>% 
  filter(status == "Closed") %>% 
  filter(!projectCounties == "STATEWIDE") %>% 
  filter(region %in% c(6, 7)) %>% 
  filter(!id == "89309d7e-487b-456d-9049-37d73b84095c") # removed because closed date was bad

# create plot -------------------------------------------------------------
df %>% 
  ggplot() +
  geom_segment(aes(x = federalShareObligated, xend = federalShareObligated, y = -1, yend = 1)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1000000, 8000000, by = 1000000), labels = scales::dollar_format(scale = .000001, suffix = "M")) +
  facet_wrap(~ state, ncol = 1, strip.position = "left") +
  theme_void() +
  theme(plot.title = element_markdown(family = font_t, size = 15, hjust = 0, face = "bold", color = "#000000"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 10, hjust = 0, color = "#000000"),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 9, family = font, color = "#000000", hjust = 0.5),
        strip.text = element_text(size = 9, family = font, color = "#000000", margin = margin(r = 5)),
        axis.text.x = element_text(size = 8, family = font, color = "#000000", margin = margin(t = 10)),
        axis.title.x = element_text(size = 9, family = font, color = "#000000", margin = margin(t = 10)),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "<span style='font-size:26pt'>SAFE ROOMS</span> for Tornado and Severe Wind Shelter",
       subtitle = "Distribution of federal grant funding amounts for mitigation planning and projects that\nincluded safe rooms awarded between 9/5/2002 to 3/31/2023 to reduce disaster losses\nand protect life and property from future damage for U.S. states in regions 6 and 7.\n\n",
       caption = "\n\n\n#30DayChartChallenge | Data: fema.gov | Design: Ryan Hart", 
       x = "Federal Funding Amount")
  
# save plot ---------------------------------------------------------------
ggsave(paste0("day_7_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)
