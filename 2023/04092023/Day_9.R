# #30DayChartChallenge | April 2023 - Day 9 | high/low
# Data Source is www.midiworld.com

# load libraries ----------------------------------------------------------
library(tidyverse)
library(tuneR)
library(showtext)
library(ggridges)

# add font ----------------------------------------------------------------
font_add_google(name = "Ubuntu", family = "Ubuntu")
font_add_google(name = "Lora", family = "Lora")

font_t <- "Ubuntu"
font <- "Lora"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data ---------------------------------------------------------------
data <- readMidi("Led_Zeppelin_-_Black_Dog.mid")
data_notes <- getMidiNotes(data)

# create data frame -------------------------------------------------------
data_df <- data %>%
  filter(!is.na(parameterMetaSystem)) %>% 
  filter(parameterMetaSystem %in% c("Guitar 1", "Guitar 2", "Bass", "Drums", "Vocal")) %>% 
  left_join(data_notes, by = "track") %>% 
  select(parameterMetaSystem, note, notename, length)

# set y-axis order --------------------------------------------------------
# data_df$parameterMetaSystem <- factor(data_df$parameterMetaSystem, levels=c("Vocal", "Guitar 1", "Guitar 2", "Bass", "Drums"))

# create plot -------------------------------------------------------------
data_df %>%
  ggplot(aes(x = note, y = parameterMetaSystem)) + 
  geom_density_ridges(fill = "#E0E0E0", scale = 3, alpha = 0.8, color = "#FFFFFF", size = 0.6) +
  annotate("text", x = 40, y = 7, label = "BLACK DOG", family = font_t, fontface = "bold", size = 12, color = "#FFFFFF", hjust = 0.5) +
  annotate("text", x = 40, y = 6.45, label = "Led Zeppelin IV", family = font, size = 4.25, color = "#FFFFFF", hjust = 0.5) +
  annotate("text", x = 40, y = 6.05, label = "1971", family = font, size = 3.75, color = "#FFFFFF", hjust = 0.5) +
  scale_y_discrete(expand = c(0, 0.25)) +
  scale_x_continuous(expand = c(0, 0), breaks = c(28, 88), labels = c("Low", "High")) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.caption = element_text(family = font_t, size = 8, color = "#FFFFFF", hjust = 0.5),
        plot.caption.position = "plot",
        axis.line.x = element_line(),
        axis.title.x = element_text(size = 9, family = font, color = "#FFFFFF"),
        axis.text.y = element_text(size = 8, family = font, color = "#FFFFFF", hjust = 0, margin = margin(r = 5, b = 5)),
        axis.text.x = element_text(size = 8, family = font, color = "#FFFFFF", hjust = 0.5, margin = margin(t = 5)),
        legend.position = "Null",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(color = "#000000", fill = "#000000")) +
  labs(x = "\nMusical notes from song arranged low to high by midi track",
       caption = "\n\n\n#30DayChartChallenge | Data: midiworld.com | Design: Ryan Hart")

# save plot
ggsave(paste0("day_9_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

