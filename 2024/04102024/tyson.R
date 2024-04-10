# #30DayChartChallenge | April 2024 - Day 10 | Physical
# Data Source is from boxrec.com

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(ggbeeswarm)
library(ggtext)
library(ggnewscale)

# add font ----------------------------------------------------------------
font_add_google(name = "Russo One", family = "Russo One")
font_t <- "Russo One"

font_add_google(name = "Roboto", family = "Roboto")
font <- "Roboto"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# options(scipen = 999)

# get data ----------------------------------------------------------------
raw <- read.csv("tyson.csv")

# wrangle and create data frame -------------------------------------------
df <- raw %>% 
  mutate(x = 1) %>% 
  select(result, duration, seconds, KO, x) %>% 
  mutate(perc = sum(seconds <= 540) / nrow(df) * 100) %>% 
  mutate(perc2 = sum(KO == "yes") / nrow(df) * 100)
  # mutate(ko_color = case_when(KO == "yes" ~ "#FAEd27",
  #                             KO == "no" & result == "loss" ~ "#FF0000",
  #                             KO == "no" & result == "win" ~ "#000000",
  #                             KO == "no" & result == "no contest" ~ "#999999"))


# create data frame for tyson's pro record (wins-losses-no contest) -------
df_record <- df %>% 
  group_by(result) %>% 
  summarise(count = n())

# create a data frame for custom axis -------------------------------------
df_axis <- data.frame(seconds = c(60, 180, 540, 1080, 1800, 2160),
                      label = c("01:00", "03:00","09:00","18:00", "30:00", "36:00"))

# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(x = x, y = seconds)) +
  geom_text(data = df_axis, aes(x = 3, y = seconds + 25, label = label), family = font, size = 3, color = "#000000", hjust = "right", vjust = "bottom") +
  annotate("segment", x = -1, xend = 3, y = df_axis$seconds, color = "#000000", linewidth = 0.3) +
  annotate("richtext", x = -0.3, y = 380, label = "<span style = 'font-size:14pt;'><b>65.5%</b></span><br><br>of fights<br>ended in<br>9 minutes<br>or less", family = font, size = 3, color = "#FFFFFF", hjust = 0.5, vjust = "bottom", 
           fill = "#000000", label.padding = unit(c(1, .5, 1, .5), "lines"), angle = 10, lineheight = 1.2) +
  annotate("richtext", x = 2.1, y = 1280, label = "<span style = 'font-size:14pt;'><b>81.0%</b></span><br><br>of fights<br>decided by<br><span style = 'color: #FAEd27;'><b>KO</b></span> or <span style = 'color: #FAEd27;'><b>TKO</b></span>", family = font, size = 3, color = "#FFFFFF",
           hjust = 0.5, vjust = "bottom", fill = "#000000", label.padding = unit(c(1, .5, 1, .5), "lines"), angle = -10, lineheight = 1.2) +
  geom_beeswarm(aes(color = ko_color, fill = ifelse(KO == "yes", "#FAEd27", "#FFFFFF")), size = 4.5, cex = 4.25, color = "#000000", shape = 21) +
  scale_fill_identity() +
  geom_beeswarm(aes(color = result), cex = 4.25, size = 2) +
  scale_color_manual(values = c(win = "#000000", loss = "#FF0000", "#999999")) +
  scale_x_continuous(limits = c(-1, 3)) +
  theme_void() +
  theme(plot.title = element_text(family = font_t, size = 38, color = "#000000", face = "bold", hjust = 0.5, margin = margin(b = 1)),
        plot.subtitle = element_markdown(family = font, size = 9, color = "#000000", hjust = 0.5, margin = margin(b = 25)),
        plot.title.position = "plot",
        plot.caption = element_text(family = font, hjust = 0.5, size = 7.5, color = "#000000", margin = margin(t = 25)),
        plot.caption.position = "plot",
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "MIKE TYSON", 
       subtitle = "Fights by duration (mm:ss) with a professional record of <b>50 wins</b>, <span style = 'color: #FF0000;'><b>6 losses</b></span>, and <span style = 'color: #999999;'><b>2 no contest</b></span>",
       caption = "#30DayChartChallenge | Data: boxrec.com | Design: Ryan Hart")

# save plot ———————————————————————————————————————————————————————————————
ggsave(paste0("tyson_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

