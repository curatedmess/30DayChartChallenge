# #30DayChartChallenge | April 2023 - Day 24 | theme day: UN Woman
# Data Source is data.un.org

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(ggtext)
library(patchwork)

# add font ----------------------------------------------------------------
font_add_google(name = "Open Sans", family = "Open Sans")
font <- "Open Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999)

# load data ---------------------------------------------------------------
data <- readr::read_csv("SYB65_327_202209_International Migrants and Refugees.csv", skip = 1)

# data wrangle and create data frame --------------------------------------
df <- data %>%
  clean_names() %>% 
  select(1:5) %>% 
  filter(x2 %in% c("Total, all countries or areas", "United States of America", "Canada")) %>% 
  filter(series %in% c("International migrant stock: Female (% total Population)", "International migrant stock: Male (% total Population)")) %>% 
  mutate(series = ifelse(series == "International migrant stock: Female (% total Population)", "Female", "Male"))
  
# create plot -------------------------------------------------------------
p1 <- df %>% 
  filter(x2 %in% c("Total, all countries or areas")) %>%
  ggplot() +
  geom_line(aes(y = value, x = year, group = series, color = series), linewidth = 0.7) +
  facet_wrap(~ x2, scales = "free_y") +
  scale_y_continuous(limits = c(0, NA)) +
  scale_color_manual(values = c("#1b8ac1", "#636463")) +
  theme_minimal() +
  theme(legend.position = "top",
        strip.text = element_text(size = 10, family = font, color = "#000000", margin = margin(b = 10)),
        axis.title.y = element_text(size = 8, family = font, color = "#000000", hjust = 0.5),
        axis.title.x = element_blank()) +
  labs(y = "Percent of Total Population (%)\n")


# create plot -------------------------------------------------------------
p2 <- df %>% 
  filter(x2 %in% c("United States of America", "Canada")) %>% 
  ggplot() +
  geom_line(aes(y = value, x = year, group = series, color = series), linewidth = 0.7) +
  facet_wrap(~ x2) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_color_manual(values = c("#1b8ac1", "#000000")) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 10, family = font, color = "#000000", margin = margin(b = 10)),
        axis.title = element_blank(),
        axis.title.x = element_blank(),
        panel.spacing = unit(2, "lines"))
  # labs(y = "Percent of Total Population (%)\n")



# combine plots -----------------------------------------------------------
final <- p1 | plot_spacer() | p2

final + plot_layout(widths =c(3, 0.05, 7), guides = "collect") + plot_annotation(title = "<span style='color: #1b8ac1;'>UNITED NATIONS</span><br><span style='font-size:24pt'><b>International Migrants and Refugees</b></span>",
                                                                                 subtitle = "Canada and United States have a greater percentage of <span style='color: #1b8ac1;'><b>Female</b></span> vs <span style='color: #000000;'><b>Male</b></span> immigrants relative to the global trend",
                                                                                 caption = "\n#30DayChartChallenge | Data: un.org | Design: Ryan Hart") &
  theme(plot.title = element_markdown(family = font, size = 12, hjust = 0, color = "#000000"),
        plot.subtitle = element_markdown(family = font, size = 11, hjust = 0, color = "#636463"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        legend.position = 'none',
        plot.margin = margin(1, 0.5, 1, 0.5,"cm"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.caption = element_text(size = 10, hjust = 0.5, family = font, color = "#000000")) 

# save plot ---------------------------------------------------------------
ggsave(paste0("day_24_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 9, height = 5)



