# #30DayChartChallenge | April 2023 - Day 20 | correlation
# Data Source is Yahoo Fiannce

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(janitor)

# add font ----------------------------------------------------------------
font_add_google(name = "Open Sans", family = "Open Sans")
font <- "Open Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999)

# load data ---------------------------------------------------------------
data_Bitcoin <- readr::read_csv("BTC-USD.csv") %>% 
  mutate(name = "Bitcoin") %>% 
  mutate(symbol = "BTC")

# data_TSLA <- readr::read_csv("TSLA.csv") %>% 
#   mutate(name = "TSLA")

data_MSTR <- readr::read_csv("MSTR.csv") %>% 
  mutate(name = "Microstrategy") %>% 
  mutate(symbol = "MSTR")


# data wrangle and create data frame --------------------------------------
df <- rbind(data_Bitcoin, data_MSTR) %>% 
  clean_names() %>%
  filter(date %in% data_MSTR$Date) %>% 
  group_by(name) %>%
  arrange(date) %>% 
  mutate(index_price = (adj_close * 100/first(adj_close)))

# create plot -------------------------------------------------------------
df %>% 
  ggplot() +
  geom_line(aes(x = date, y = index_price, color = name), linewidth = 0.8) +
  scale_color_manual(values = c("#000000", "red")) +
  theme_minimal() +
  theme(plot.title = element_text(family = font, size = 30, hjust = 0.5, color = "#000000", face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 12, hjust = 0.5, color = "#000000", margin = margin(b = 20)),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 9, family = font, color = "#000000", hjust = 0.5),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 10, family = font, color = "#000000"),
        axis.text = element_text(size = 8, family = font, color = "#000000"),
        axis.title.y = element_text(size = 9, family = font, color = "#000000"),
        axis.title.x = element_blank(),
        axis.line = element_line(color = "#000000", linewidth = 0.3),
        panel.grid = element_line(color = "#BDBDBD", linewidth = 0.4, linetype = "dotted"),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "Bitcoin and Microstrategy",
       subtitle = "Daily indexed closing prices (April 20, 2018 to April 19, 2023)",
       y = "Indexed Closing Price\n",
       caption = "\n\n#30DayChartChallenge | Data: yahoo finance | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("day_20_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

