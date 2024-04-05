# #30DayChartChallenge | April 2024 - Day 5 | Diverging
# Data Source is my Strava Data from the last four years (20, 21, 22, 23)

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(rStrava)
library(jsonlite)
library(httr)
library(lubridate)
library(ggforce)
library(ggtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Outfit", family = "Outfit")
font_t <- "Outfit"

font_add_google(name = "Inter", family = "Inter")
font <- "Inter"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# options(scipen = 999)


# get data ----------------------------------------------------------------
# Strava API instructions created using
# https://bldavies.com/blog/accessing-strava-api/

# # credentials -------------------------------------------------------------
# client_id <- enter client id here
# secret <- "enter your secret here"

client_id <- 44244

secret <- "883168b6a52e78640bde1a33edb83badd0f13b3f"

# OAuth application -------------------------------------------------------
app <- oauth_app("strava", client_id, secret)
endpoint <- oauth_endpoint(
  request = NULL,
  authorize = "https://www.strava.com/oauth/authorize",
  access = "https://www.strava.com/oauth/token"
)

# OAuth access token ------------------------------------------------------
token <- oauth2.0_token(endpoint, app, as_header = FALSE,
                        scope = "activity:read_all")

# get Activity List -------------------------------------------------------
strava_df_list <- list()
i <- 1
done <- FALSE
while (!done) {
  req <- GET(
    url = "https://www.strava.com/api/v3/athlete/activities",
    config = token,
    query = list(per_page = 200, page = i)
  )
  strava_df_list[[i]] <- fromJSON(content(req, as = "text"), flatten = TRUE)
  if (length(content(req)) < 200) {
    done <- TRUE
  } else {
    i <- i + 1
  }
}

# combine Activity into a df and wrangle ----------------------------------
strava_df <- rbind_pages(strava_df_list) %>%
  mutate(distance_miles = distance * 0.00062137119224) %>%
  filter(type == "Run") %>%
  mutate(date = lubridate::as_date(start_date_local)) %>% 
  mutate(year = lubridate::year(start_date_local),
         month = lubridate::month(start_date_local),
         day = lubridate::day(start_date_local),
         time = lubridate::ymd_hms(start_date_local)) %>%
  filter(year %in% c(2020, 2021, 2022, 2023)) %>%
  select(distance_miles, year, month, day, date, time) %>% 
  mutate(is_before_7AM = ifelse(hour(ymd_hms(time)) < 7, TRUE, FALSE)) %>% 
  group_by(year, is_before_7AM) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(perc = count / sum(count))

# create plot -------------------------------------------------------------
strava_df %>% 
  ggplot() +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = ifelse(is_before_7AM == "TRUE", -sqrt(count), sqrt(count)), start = 0, end = pi, fill = is_before_7AM), color = "#000000") +
  geom_text(aes(x = ifelse(is_before_7AM == "TRUE", -3, 3), y = 0, label = count), color = "#000000", family = font, size = 6) +
  geom_text(aes(y = -13, x = 13, label = year), family = font, size = 3, fontface = "bold", check_overlap = TRUE) +
  geom_vline(xintercept = 0, linewidth = 1.1) +
  scale_y_continuous(limits = c(-14, 14)) +
  scale_x_continuous(limits = c(-14, 14)) +
  scale_fill_manual(values = c("#FDB813", "#FFFFFF")) +
  coord_flip() +
  facet_wrap(~ year) +
  theme_void() +
  theme(plot.title = element_text(family = font_t, hjust = 0.5, size = 20, color = "#000000", face = "bold", margin = margin(b = 5)),
        plot.subtitle = element_markdown(hjust = 0.5, family = font, size = 12, color = "#000000", margin = margin(b = 15)),
        plot.title.position = "plot",
        plot.caption = element_text(family = font, hjust = 0.5, size = 7.5, color = "#000000", margin = margin(t = 15)),
        plot.caption.position = "plot",
        legend.position = "none",
        strip.text = element_blank(),
        panel.spacing.y = unit(0,'lines'),
        panel.spacing.x = unit(2,'lines'),
        aspect.ratio = 1,
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "TIME OF DAY I START MY RUN.",
       subtitle = "count of runs <span style = 'color: #FDB813;'>after 7am</span> or before 7am",
    caption = "#30DayChartChallenge | Data: strava | Design: Ryan Hart")

# save plot ———————————————————————————————————————————————————————————————
ggsave(paste0("tod_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


