# #30DayChartChallenge | April 2024 - Day 9 | Major/Minor
# Data Source is from goal.com

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(ggtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Ubuntu", family = "Ubuntu")
font <- "Ubuntu"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# options(scipen = 999)

# create data frame -------------------------------------------------------
df <- data.frame(team = c("Barcelona", "PSG", "Argentina", "Inter Miami"), goals = c(672, 32, 106, 17)) %>% 
  mutate(level = ifelse(team == "Inter Miami", "Minors", "Majors")) %>% 
  uncount(goals) %>%
  mutate(Value = 1 + (row_number() - min(row_number())) %% 5, max = max(Value)) %>% 
  mutate(Group_ID = cumsum(Value == 1 & lag(Value, default = 0) == 5)) %>%
  ungroup()

# create plot -------------------------------------------------------------
df %>%
  ggplot() +
  geom_segment(data = df %>% filter(level == "Majors"), aes(x = ifelse(Value == 5, 4.75, Value), y = ifelse(Value == 5, 2.9, 3), xend = ifelse(Value == 5, 0.25, Value), yend = ifelse(Value == 5, 0.1, 0)), color = "#999999", linewidth = 0.6, lineend = "round") +
  geom_segment(data = df %>% filter(level == "Minors"), aes(x = ifelse(Value == 5, 4.75, Value), y = ifelse(Value == 5, 2.9, 3), xend = ifelse(Value == 5, 0.25, Value), yend = ifelse(Value == 5, 0.1, 0)), color = "#f7b5cd", linewidth = 0.9, lineend = "round") +
  facet_wrap(~ Group_ID, ncol = 12) +
  coord_equal() +
  theme_void() +
  theme(plot.title = element_text(family = font, size = 28, color = "#FFFFFF", face = "bold", hjust = 0.5, margin = margin(b = 5)),
        plot.subtitle = element_markdown(family = font, size = 14, color = "#999999", hjust = 0.5, margin = margin(b = 25), lineheight = 1.1),
        plot.title.position = "plot",
        plot.caption = element_text(family = font, hjust = 0.5, size = 7.5, color = "#FFFFFF", margin = margin(t = 35)),
        plot.caption.position = "plot",
        legend.position = "none",
        strip.text = element_blank(),
        plot.margin = unit(c(1, 0, 1, 0), "cm"),
        panel.background = element_rect(color = NA, fill = "#000000"),
        plot.background = element_rect(color = NA, fill = "#000000")) +
  labs(title = "LIONEL MESSI GOALS",
       subtitle = "Barcelona, PSG, Argentina and <span style = 'color: #f7b5cd;'><b>Inter Miami</b></span><br><span style = 'font-size:8.5pt;'>as of 4.9.2024</span>",
    caption = "#30DayChartChallenge | Data: goal.com | Design: Ryan Hart")

# save plot ———————————————————————————————————————————————————————————————
ggsave(paste0("messi_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


  