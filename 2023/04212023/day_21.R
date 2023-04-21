# #30DayChartChallenge | April 2023 - Day 21 | down/upwards
# Data Source is Google Trends

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(janitor)
library(ggtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Bangers", family = "Bangers")
font_add_google(name = "Open Sans", family = "Open Sans")
font_t <- "Bangers"
font <- "Open Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999)

# load data ---------------------------------------------------------------
data <- readr::read_csv("multiTimeline-2.csv", skip = 2)

# data wrangle and create data frame --------------------------------------
df <- data %>%
  clean_names() %>%
  mutate(chat_gpt_worldwide = ifelse(chat_gpt_worldwide == "<1", 0, chat_gpt_worldwide)) %>% 
  mutate(chat_gpt_worldwide = as.numeric(chat_gpt_worldwide)) %>%
  rename(chatGPT = chat_gpt_worldwide, NFT = nft_worldwide) %>% 
  pivot_longer(cols = c(2:3), names_to = "term", values_to = "interest")

# create plot -------------------------------------------------------------
df %>% 
  ggplot() +
  geom_line(aes(x = week, y = interest, color = term), linewidth = 1.5) +
  scale_color_manual(values = c("#673AB7", "#E91E63")) +
  scale_x_date(date_breaks = "3 month", date_labels = "%m-%y") +
  theme_minimal() +
  theme(plot.title = element_text(family = font, size = 12, hjust = 0.5, color = "#000000"),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(family = font_t, size = 50, hjust = 0.5, color = "#000000", margin = margin(b = 20)),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 9, family = font, color = "#000000", hjust = 0.5),
        legend.position = "none",
        axis.text = element_text(size = 8, family = font, color = "#000000"),
        axis.title = element_text(size = 8, family = font, color = "#000000"),
        axis.line = element_line(color = "#000000", linewidth = 0.3),
        panel.grid = element_line(color = "#BDBDBD", linewidth = 0.2, linetype = "dashed"),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "GOOGLE SEARCH INTEREST",
       subtitle = "<span style='color: #E91E63;'>NFT</span> <span style='font-size:40pt'>vs</span> <span style='color: #673AB7;'>chatGPT</span>",
       y = "Search Interest",
       x = "\nWeekly Numbers from April 18, 2021 to April 9, 2023",
       caption = "\n\n#30DayChartChallenge | Data: trends.google.com | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("day_21_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

