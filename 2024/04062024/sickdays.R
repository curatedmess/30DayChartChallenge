# #30DayChartChallenge | April 2024 - Day 6 | OECD
# Data Source is from https://data-explorer.oecd.org/vis?fs[0]=Topic%2C1%7CHealth%23HEA%23%7CHealth%20status%23HEA_STA%23&pg=0&fc=Topic&bp=true&snb=16&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_HEALTH_STAT%40DF_AWDI&df[ag]=OECD.ELS.HD&df[vs]=1.0&pd=2010%2C&dq=.A...........&ly[rw]=REF_AREA&ly[cl]=TIME_PERIOD&to[TIME_PERIOD]=false

# overall it seems I made this all more complicated than it needed to be


# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(ggsvg)
library(countrycode)
library(ggimage)

# add font ----------------------------------------------------------------
font_add_google(name = "Roboto Slab", family = "Roboto Slab")
font_t <- "Roboto Slab"

font_add_google(name = "Roboto", family = "Roboto")
font <- "Roboto"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# options(scipen = 999)


# get data ----------------------------------------------------------------
raw <- read.csv("OECD.ELS.HD,DSD_HEALTH_STAT@DF_AWDI,1.0+all.csv")

# wrangle and create data frame -------------------------------------------
df <- raw %>% 
  select(REF_AREA, TIME_PERIOD, OBS_VALUE) %>% 
  filter(REF_AREA %in% c("CAN", "USA")) %>% 
  filter(TIME_PERIOD >= 1988, TIME_PERIOD <= 2021) %>% 
  mutate(iso2 = countrycode(REF_AREA, "iso3c", "iso2c"))

df_flag <- df  %>%
filter(TIME_PERIOD == 2021)
  

# create the diff lines ---------------------------------------------------
df_spread <- df %>% 
  select(!iso2) %>% 
  # pivot_wider(names_from = REF_AREA, values_from = OBS_VALUE) %>% 
  spread(REF_AREA, OBS_VALUE) %>% 
  mutate(diff = CAN - USA)

# create the diff points --------------------------------------------------
df_points <- df_spread %>% 
  pivot_longer(cols = c(USA, CAN), names_to = "REF_AREA", values_to = "OBS_VALUE") %>% 
  group_by(REF_AREA) %>% 
  mutate(avg = mean(OBS_VALUE))
  
# create min/max points ---------------------------------------------------
df_minmax <- df_points %>% 
  filter(diff == max(diff) | diff == min(diff))

# create start/end points -------------------------------------------------
df_startend <- df_points %>%
  filter(TIME_PERIOD %in% c(1988, 2021))

# create min/max call out text --------------------------------------------
df_minmax_text <- df_minmax %>% 
  filter(REF_AREA == "CAN")
  
# create plot -------------------------------------------------------------
df_points %>% 
  ggplot(aes(x = TIME_PERIOD, y = OBS_VALUE)) +
  geom_segment(data = df_spread, aes(x = TIME_PERIOD, xend = TIME_PERIOD, y = USA, yend = CAN), color = "#D3D3D3") +
  geom_step(aes(color = REF_AREA), linewidth = 1) +
  geom_point(data = df_minmax , aes(x = TIME_PERIOD, y = OBS_VALUE, color = REF_AREA), size = 4.75) +
  geom_text(data = df_minmax , aes(x = TIME_PERIOD, y = OBS_VALUE, label = OBS_VALUE), size = 2.25, color = "#FFFFFF", fontface = "bold") +
  geom_text(data = df_minmax_text %>% filter(TIME_PERIOD == 1993) , aes(x = TIME_PERIOD, y = OBS_VALUE - 2.5, label = paste0(TIME_PERIOD, " was\nthe minimum\ndifference at\n ", diff, " days")), size = 2.75, color = "#000000", vjust = "bottom", hjust = "right") +
  geom_text(data = df_minmax_text %>% filter(TIME_PERIOD == 2017) , aes(x = TIME_PERIOD, y = OBS_VALUE + 0.5, label = paste0(TIME_PERIOD, " was\nthe maximum\ndifference at\n ", diff, " days")), size = 2.75, color = "#000000", vjust = "bottom", hjust = "right") +
  geom_point(data = df_startend , aes(x = TIME_PERIOD, y = OBS_VALUE, color = REF_AREA), size = 4.75) +
  geom_text(data = df_startend , aes(x = TIME_PERIOD, y = OBS_VALUE, label = OBS_VALUE), size = 2.25, color = "#FFFFFF", fontface = "bold") +
  geom_flag(data = df_flag, aes(x = TIME_PERIOD + 1.5, y = OBS_VALUE - 0.5, image = iso2))  +
  annotate("text", x = 1988, y = 2, label = "SICK DAYS", family = font_t, color = "#000000", fontface = "bold", size = 8, hjust = "left") +
  annotate("text", x = 1988, y = 1.5, label = "Comparing the average number of days per year employees are absent\nfrom work due to illness in Canada vs the Unites States of America", family = font, color = "#000000", size = 3, hjust = "left", vjust = "top") +
  scale_y_continuous(limits = c(0, 10), breaks = c(2.5, 5, 7.5, 10), labels = scales::number_format(accuracy = 1)) +
  scale_x_continuous(breaks =  c(1988, 2004, 2021)) +
  scale_color_manual(values = c("red", "blue")) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.caption = element_text(family = font, hjust = 0.5, size = 7.5, color = "#000000", margin = margin(t = 25)),
        plot.caption.position = "plot",
        axis.text.y = element_text(family = font, hjust = 0.5, size = 8, color = "grey", margin = margin(r = 5)),
        axis.text.x = element_text(family = font, hjust = 0.5, size = 8, color = "#000000", margin = margin(t = 5)),
        axis.line.x = element_line(color = "#000000", linewidth = 1),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(x = "Mean Number of Days", 
       caption = "#30DayChartChallenge | Data: oecd.org | Design: Ryan Hart")

# save plot ———————————————————————————————————————————————————————————————
ggsave(paste0("sickdays_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

