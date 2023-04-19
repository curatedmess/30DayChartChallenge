# #30DayChartChallenge | April 2023 - Day 19 | anthropocene
# Data Source is oecd.org
# Data citation: OECD (2023), "Global Plastics Outlook: Plastic leakage to the environment - projections", OECD Environment Statistics (database), https://doi.org/10.1787/4ab65392-en (accessed on 19 April 2023).

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(janitor)
library(scales)
# library(scico)

# add font ----------------------------------------------------------------
font_add_google(name = "Roboto", family = "Roboto")
font_add_google(name = "Nunito", family = "Nunito")
font_t <- "Roboto"
font <- "Nunito"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999)

# load data ---------------------------------------------------------------
data <- readr::read_csv("PLASTIC_LEAKAGE_V2_3_19042023153343268.csv")

# data wrangle and create data frame --------------------------------------
df <- data %>%
  select(1, 2, 4, 5, 7) %>% 
  clean_names() %>% 
  group_by(scenario_2, plastic_category) %>% 
  mutate(perc_change = (value/lag(value) - 1) * 100) %>% 
  group_by(scenario_2, plastic_category) %>% 
  mutate(perc_change = (last(value) - first(value))/first(value)) %>%
  ungroup() %>% 
  mutate(scenario_2 = case_when(scenario_2 == "Baseline" ~ "Baseline scenario - No actions taken",
                                scenario_2 == "Regional Action policy scenario" ~ "Regional Action scenario - Mix of fiscal and regulatory policies",
                                scenario_2 == "Global Ambition policy scenario" ~ "Global Ambition scenario - Stringent policies implemented worldwide"))
  
# order of facets ---------------------------------------------------------
df$scenario_2 <-factor(df$scenario_2, levels = c("Baseline scenario - No actions taken", "Regional Action scenario - Mix of fiscal and regulatory policies" , "Global Ambition scenario - Stringent policies implemented worldwide"))

# create plot -------------------------------------------------------------
df %>% 
  ggplot() +
  geom_line(aes(x = time, y = value, group = plastic_category, color = plastic_category), size = 1) +
  geom_text(aes(x = ifelse(scenario == "MODERATE-FULL", 2045, 2055), y = ifelse(scenario == "MODERATE-FULL", 14.5, 20), label = ifelse(time == 2060 & plastic_category == "Total", paste0(scales::percent(perc_change), " change\n2019 to 2060"), ""), color = plastic_category), family = font, fontface = "bold", size = 3, show.legend = FALSE) +
  # scale_color_scico_d(palette = "buda") +
  scale_color_manual(values = c("#B29CDB", "#FCB249", "#FAFC97")) +
  facet_wrap(~ scenario_2, ncol = 1, scales = "free") +
  theme_minimal() +
  theme(plot.title = element_text(family = font_t, size = 32, hjust = 0.5, color = "#FFFFFF", lineheight = 1.2),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 9.5, hjust = 0.5, color = "#FFFFFF"),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 10, family = font, color = "#FFFFFF", hjust = 0.5),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 9, family = font, color = "#FFFFFF"),
        strip.text = element_text(size = 10, family = font, color = "#FFFFFF", hjust = 0.5, margin = margin(t = 10, b = 10)),
        axis.text = element_text(size = 9, family = font, color = "#FFFFFF"),
        axis.title.y = element_text(size = 10, family = font, color = "#FFFFFF"),
        axis.title.x = element_blank(),
        axis.line = element_line(color = "#FFFFFF", linewidth = 0.3),
        panel.grid = element_line(color = "#BDBDBD", linewidth = 0.4, linetype = "dotted"),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(color = NA, fill = "#202A44")) +
  labs(title = "PLASTIC WASTE LEAKAGE",
       subtitle = "Left unchecked, the forecast for plastic leakage in the environment may double in 40 years",
       y = "Million tonnes (Mt)\n",
       caption = "\n\n#30DayChartChallenge | Data: oecd.org | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("day_19_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 8)

