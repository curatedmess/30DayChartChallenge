# #30DayChartChallenge | April 2023 - Day 15 | positive/negative
# Data Source is genius.com via genius API {geniusR}

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(tidytext)
library(geniusr)

# install.packages("geniusr")

# add font ----------------------------------------------------------------
font_add_google(name = "Special Elite", family = "Special Elite")
font_add_google(name = "Roboto", family = "Roboto")
font_t <- "Special Elite"
font <- "Roboto"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# options(scipen = 999)

# enter token -------------------------------------------------------------
genius_token()

# get artist id -----------------------------------------------------------
# Ed Sheeran
search_song(search_term = "Shape of You")
get_artist_df(artist_id = 12418)

songs <- get_artist_songs_df(artist_id = 12418)

# leveraged a great deal of code from this blog post to get lyrics  -------
# https://www.r-bloggers.com/2021/01/scraping-analysing-and-visualising-lyrics-in-r/

ids <- c(as.character(songs$song_id))

allLyrics <- data.frame()

while (length(ids) > 0) {
  for (id in ids) {
    tryCatch({
      allLyrics <- rbind(get_lyrics_id(id), allLyrics)
      successful <- unique(allLyrics$song_id)
      ids <- ids[!ids %in% successful]
      print(paste("done - ", id))
      print(paste("New length is ", length(ids)))
    }, error = function(e){})
  }
}

allIds <- data.frame(song_id = unique(allLyrics$song_id))
allIds$album <- ""

# add album to each song
for (song in allIds$song_id) {
  allIds[match(song,allIds$song_id),2] <- get_song_df(song)[12]
  print(allIds[match(song,allIds$song_id),])
}
allLyrics <- full_join(allIds, allLyrics) %>% 
  filter(album %in% c("+ (Plus) [Japanese Edition]", "× (Multiply) [Japanese Edition]", "÷ (Divide) [Deluxe/Japanese Edition]", "- (Subtract) [Deluxe Splatter Vinyl]", "= (Equals)"))

# text analysis -----------------------------------------------------------
# tokenize ----------------------------------------------------------------
allLyricsTokenised <- allLyrics %>%
  unnest_tokens(word, line)

# remove stop words -------------------------------------------------------
tidyLyrics <- allLyricsTokenised %>%
  anti_join(stop_words)

# get sentiment -----------------------------------------------------------
sentiments_df <- tidyLyrics %>%
  inner_join(get_sentiments("bing"))%>% 
  count(album, song_name, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# create data frame for plot ----------------------------------------------
df <- sentiments_df %>% 
  mutate(test = ifelse(sentiment < 0, "negative", "positive")) %>% 
  arrange(test, sentiment) %>% 
  group_by(test) %>%
  mutate(row_num = row_number()) %>% 
  mutate(row_num = ifelse(test == "negative", rev(row_num), row_num))

# update order ------------------------------------------------------------
df$test <- factor(df$test, levels=c('positive', 'negative'))

# create plot -------------------------------------------------------------
df %>% 
  ggplot() +
  geom_text(aes(x = test, y = ifelse(test == "positive", row_num, (-1 * row_num)), label = song_name), color = "#000000", size = 3.5, family = font_t) +
  geom_hline(yintercept = 0) +
  annotate("text", x = 1, y = -15, label = "Ed Sheeran", family = font_t, fontface = "bold", size = 11.5, color = "#000000", hjust = 0.5) +
  annotate("text", x = 1, y = -20, label = "Sentiment analysis of 64 songs from 5 albums,\narranged from most positive (top left)\nto most negative (bottom right)", family = font, size = 3, color = "#000000", hjust = 0.5) +
  annotate("text", x = 1, y = -24, label = "+ × ÷ = -", family = font_t, size = 8, color = "#000000", fontface = "bold", hjust = 0.5) +
  annotate("text", x = 1, y = -1.25, label = "Positive", family = font, size = 4, color = "#000000", fontface = "bold", hjust = 0.5) +
  annotate("text", x = 2, y = 1.25, label = "Negative", family = font, size = 4, color = "#000000", fontface = "bold", hjust = 0.5) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(size = 9, family = font, color = "#000000", hjust = 0.5),
        legend.position = "none",
        plot.margin = unit(c(0.25, 0.5, 0.5, 0.5), "cm"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(caption = "\n#30DayChartChallenge | Data: genius.com | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("day_15_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 9)


