# #30DayChartChallenge | April 2023 - Day 2 | waffle
# Data Source is uscurrency.gov
# subtitle re-written using chat.openai.com

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(rvest)
library(waffle)
library(ggpubr)

# add font ----------------------------------------------------------------
font_add_google(name = "Archivo Black", family = "Archivo Black")
font_t <- "Archivo Black"

font_add_google(name = "Archivo", family = "Archivo")
font <- "Archivo"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# WEB SCRAPE CURRENCY DATA ------------------------------------------------

# get data using rvest for screen scraping html ---------------------------

# url
url <- "https://www.uscurrency.gov/life-cycle/data/circulation"

web_data <- read_html(url)

raw <- web_data %>%
  html_nodes(xpath = '//*[@id="block-usc-content"]/div/div/div/div/div/div/div[2]') %>%
  html_table() 

# wrangle and create df ---------------------------------------------------
df <- data.frame(raw) %>% 
  select(-c(TOTAL)) %>% 
  rename(Year = Var.1,
         One = X.1,
         Two = X.2,
         Five = X.5,
         Ten = X.10,
         Twenty = X.20,
         Fifty = X.50,
         Hundred = X.100, 
         Large = X.500.to..10.000) %>% 
  pivot_longer(!Year, names_to = "Currency", values_to = "Volume") %>%
  filter(Year == 2021) %>%
  mutate(Currency = case_when(
    Currency %in% c("One", "Two", "Five", "Ten", "Twenty", "Fifty", "Large") ~ "Total Bills",
    Currency %in% c("Hundred") ~ "Hundred")) %>% 
  group_by(Year, Currency) %>% 
  summarise(n = sum(Volume)) %>%
  mutate(percent = (n / sum(n) *100))

# load background ---------------------------------------------------------
# $100 image
background <- image_read("hundo.jpg")

# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(fill = Currency, values = percent)) +
  background_image(background) +  
  geom_waffle(n_rows = 20, make_proportional = TRUE, flip = TRUE, size = 0.5, color = "#FFFFFF") +
  geom_text(aes(x = 13.5, y = 3.2, label = ifelse(Currency == "Hundred", scales::percent(percent, scale = 1, accuracy = 0.1), "")), family = font_t, size = 8, hjust = 0, color = "#000000") +
  annotate("curve", x = 13.3, y = 3.2, xend = 12, yend = 2.7, linewidth = 0.5,  curvature = 0.35, arrow = arrow(length = unit(1.25, "mm")), color = "#000000") +
  annotate("text", x = 13.55, y = 2.75, label = "of bills in circulaton are hundreds", family = font, size = 3, color = "#000000", hjust = 0, vjust = "top") +
  scale_fill_manual(values = c(alpha("#85bb65", 0), "#E0E0E0")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  # coord_equal(clip = "off") +
  theme_void() +
  theme(plot.title = element_text(family = font_t, size = 45, hjust = 0.5, face = "bold", color = "#000000"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 11.5, hjust = 0.5, color = "#000000"),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 9, family = font, color = "#000000", hjust = 0.5),
        plot.margin = margin(1, 1, 1, 1,"cm"),
        legend.position = "none",
        axis.title.x = element_text(size = 9, family = font, color = "#000000"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),) +
  labs(title = "THE HUNDRED",
       subtitle = "On December 31, 2021, the total volume of U.S. currency in circulation\nwas 53.2 billion bills, with the one hundred dollar bill leading the pack at\n17.7 billion bills circulating worldwide. That's a whole lotta Benjamins!\n\n\n\n",
       caption = "\n\n\n\n\n\n#30DayChartChallenge | Data: uscurrency.gov | Design: Ryan Hart",
       x = "\nEach rectangle is the equivalent of one percentage point")


# save plot ---------------------------------------------------------------
ggsave(paste0("day_2_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

  
  
