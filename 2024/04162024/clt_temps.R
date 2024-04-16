# #30DayChartChallenge | April 2024 - Day 16 | weather
# Data Source is www.almanac.com

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(rvest)
library(ggtext)


# add font ----------------------------------------------------------------
font_add_google(name = "Passion One", family = "Passion One")
font_t <- "Passion One"

font_add_google(name = "Open Sans", family = "Open Sans")
font <- "Open Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# create function to get daily temperature data ---------------------------
get_weather_info <- function(year, month, day) {
  # URL pattern
  url <- paste0("https://www.almanac.com/weather/history/NC/Charlotte/", year, "-", month, "-", day)
  
  # read the html
  web_data <- read_html(url)
  
  # get the weather date from the xpath reference
  raw <- web_data %>% 
    html_node(xpath = '//*[@id="block-almanaco-content"]/div/table') %>%
    html_table()
  
  # create df
  data <- data.frame(raw)
  
  # filter to get only the min and max, ignoring the rest
  temp_data <- data %>%
    filter(Temperature %in% c("Minimum Temperature", "Maximum Temperature"))
  
  # add the year
  temp_data$Year <- year
  
  return(temp_data)
}

get_weather_last_75_years <- function(month, day) {
  
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  weather_data_list <- list()
  
  # Loop over the last 75 years
  for (year in (current_year - 74):current_year) {
    
    weather_data <- get_weather_info(year, month, day)
    
    weather_data_list[[as.character(year)]] <- weather_data
  }
  
  # combine into one df
  weather_data_df <- bind_rows(weather_data_list)
  
  return(weather_data_df)
}

# get 75 years of temp data for given year
weather_data_last_75_years <- get_weather_last_75_years("04", "13")

# create a new data frame for plot ----------------------------------------
df <- weather_data_last_75_years %>% 
  mutate(temp = as.numeric(gsub("°F", "", Temperature.1))) %>% 
  group_by(Temperature) %>% 
  mutate(avg = mean(temp)) %>% 
  ungroup()

# create plot -------------------------------------------------------------
df %>% 
  ggplot() +
  geom_hline(aes(yintercept = avg, color = Temperature), linewidth = 1, alpha = 0.7, linetype = "dashed") +
  geom_text(aes(x = 2, y = avg, label = paste0("The avg. was ", round(avg, 1), " °F"), color = Temperature), family = font_t, size = 4, hjust = 0.85, vjust = -0.5, check_overlap = TRUE) +
  geom_point(aes(y = temp, x = 1, color = Temperature),  size = 5, alpha = 0.6, position = position_jitter(width = 0.5, set.seed(393939))) +
  annotate("curve", x = 1.65, y = 40, xend = 1.55, yend = 43, linewidth = 0.5,  curvature = 0.35, arrow = arrow(length = unit(1.25, "mm")), color = "#FFFFFF") +
  annotate("text", x = 1.65, y = 39, label = "In 1950 the high\nwas colder than\nthe average low\ntemperature.", family = font, size = 2.75, color = "#FFFFFF", hjust = 0, vjust = 1) +
  scale_x_continuous(limits = c(0.25, 2)) +
  scale_color_manual(values = c("#cc3363", "#9ad1d4")) +
  scale_shape_identity() +
  theme_void() +
  theme(plot.title = element_text(family = font_t, size = 35, color = "#FFFFFF", face = "bold", hjust = 0.5, margin = margin(b = 5)),
        plot.subtitle = element_markdown(family = font, size = 8, color = "#FFFFFF", hjust = 0.5, margin = margin(b = 35)),
        plot.title.position = "plot",
        plot.caption = element_text(family = font, hjust = 0.5, size = 7.5, color = "#FFFFFF", margin = margin(t = 35)),
        plot.caption.position = "plot",
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.line.y = element_line(color = "#FFFFFF", linewidth = 1.5),
        axis.text.y = element_text(color = "#FFFFFF", family = font, face = "bold", size = 8, margin = margin(r = 10)),
        axis.title.y = element_text(color = "#FFFFFF", family = font, face = "bold", size = 10, margin = margin(r = 10), angle = 90),
        panel.background = element_rect(color = NA, fill = "#000000"),
        plot.background = element_rect(color = NA, fill = "#000000")) +
  labs(title = "April 13th Flashback",
       subtitle = "75 years of <span style = 'color: #cc3363;'><b>HIGH</b></span> and <span style = 'color: #9ad1d4;'><b>LOW</b></span> temperatures from this day in Charlotte, NC (1950-2024)",
       y = "Temperature (°F)",
       caption = "#30DayChartChallenge | Data: almanac.com | Design: Ryan Hart")

# save plot ———————————————————————————————————————————————————————————————
ggsave(paste0("temp_clt_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

  