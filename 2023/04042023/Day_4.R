# #30DayChartChallenge | April 2023 - Day 4 | historical
# Data Source is https://www.goodcarbadcar.net/jeep-wrangler-sales-figures/
# subtitle re-written using chat.openai.com

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(rvest)

# add font ----------------------------------------------------------------
font_add_google(name = "Abril Fatface", family = "Abril Fatface")
font_t <- "Abril Fatface"

font_add_google(name = "Poppins", family = "Poppins")
font <- "Poppins"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# WEB SCRAPE -----------------------------------------------------
# get data using rvest for screen scraping html ---------------------------
# url
url_jeep <- "https://www.goodcarbadcar.net/jeep-wrangler-sales-figures/"
web_data_jeep <- read_html(url_jeep)

url_toyota <- "https://www.goodcarbadcar.net/toyota-4runner-sales-figures/"
web_data_toyota <- read_html(url_toyota)

# get data and create df --------------------------------------------------
jeep_sales_raw <- web_data_jeep %>%
  html_nodes(xpath = '//*[@id="table_4"]') %>%
  html_table()

toyota_sales_raw <- web_data_toyota %>%
  html_nodes(xpath = '//*[@id="table_4"]') %>%
  html_table()

# wrangle and create df ---------------------------------------------------
df_jeep <- data.frame(jeep_sales_raw) %>% 
  mutate(sold = as.numeric(gsub(",", "", sold))) %>% 
  mutate(model = paste("Wrangler"))

df_toyota <- data.frame(toyota_sales_raw) %>% 
  filter(row_number() <= n()-1) %>% 
  mutate(sold = as.numeric(gsub(",", "", sold))) %>% 
  mutate(model = paste("4runner"))

df <- rbind(df_jeep, df_toyota) %>% 
  filter(!Year == 2023) %>% 
  group_by(Year) %>%
  mutate(diff = sold - lead(sold))

# update order ------------------------------------------------------------
df$model <- factor(df$model, levels = c("Wrangler", "4runner"))

# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(x = Year, y = sold, color = model)) +
  geom_area(aes(fill = model, group = model, alpha = model), position = "identity", color = "#000000", linewidth = 1) +
  annotate("text", x = 2013.5, y = 180000, label = "Wrangler", color = "#000000", family = font_t, size = 3.75, angle = 49) +
  annotate("text", x = 2015.5, y = 115000, label = "4Runner", color = "#FFFFFF", family = font_t, size = 3.75, angle = 38) +
  annotate("segment", x = 2018, y = 280000, xend = 2018, yend = 240100, color = "#000000", linewidth = 0.4, linetype = "dotted") +
  annotate("text", x = 2017.75, y = 280000, label = "Wrangler outsold 4Runner\nby 100,338 vehicles in 2018", color = "#000000", family = font, size = 3, hjust = 1, vjust = "top") +
  scale_fill_manual(values = c("#5D4037", "#A1887F")) +
  scale_alpha_manual(values = c(1, 0.8)) +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, 280000), breaks = seq(50000, 250000, by = 50000), labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(plot.title = element_text(family = font_t, size = 20, hjust = 0, face = "bold", color = "#000000"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 10, hjust = 0, color = "#000000"),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 8, family = font, color = "#000000", hjust = 0.5),
        legend.position = "none",
        axis.text = element_text(family = font, size = 8, hjust = 0.5, color = "#000000", vjust = -2),
        axis.title.y = element_text(family = font, size = 8, hjust = 0.5, color = "#000000"),
        axis.title.x = element_blank(),
        axis.line = element_line(color = "#000000"),
        panel.grid = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
labs(title = "Wrangler vs 4Runner - U.S. Sales",
       subtitle = "Well, well, well... the Jeep Wrangler has been flexing its off-road muscles\nand leaving the Toyota 4Runner eating its dust since 2007.\n\n\n",
       caption = "\n\n\n#30DayChartChallenge | Data: goodcarbadcar.net | Design: Ryan Hart",
     y = "Vehicles Sold\n")

# save plot ---------------------------------------------------------------
ggsave(paste0("day_4_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


