# #30DayChartChallenge | April 2023 - Day 17 | Networks
# Data source is https://en.wikipedia.org/wiki/Top-rated_United_States_television_programs_by_season

# libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(janitor)
library(ggtext)
library(igraph)
library(ggraph)
library(rvest)

# add font ----------------------------------------------------------------
font_add_google(name = "Fjalla One", family = "Fjalla One")
font_add_google(name = "Inter", family = "Inter")
font_t <- "Fjalla One"
font <- "Inter"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# create url and read_html -----------------------------------------------------
url <- "https://en.wikipedia.org/wiki/Top-rated_United_States_television_programs_by_season"
web_data <- read_html(url)

# create df for each season -----------------------------------------------
# Terribly inefficient code and should be replaced with some sort of a loop
# October 1950 – April 1951
get_data_1 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[3]/table/tbody/tr/td[1]') %>%
  html_table(fill = TRUE, header = TRUE)

df1 <- data.frame(get_data_1) %>% 
  mutate(season = "October 1950 – April 1951") %>% 
  mutate(decade = "1950s")

# October 1951 – April 1952
get_data_2 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[3]/table/tbody/tr/td[2]') %>%
  html_table(fill = TRUE, header = TRUE)

df2 <- data.frame(get_data_2) %>% 
  mutate(season = "October 1951 – April 1952") %>% 
  mutate(decade = "1950s")

# October 1952 – April 1953
get_data_3 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[4]/table/tbody/tr/td[1]') %>%
  html_table(fill = TRUE, header = TRUE)

df3 <- data.frame(get_data_3) %>% 
  mutate(season = "October 1952 – April 1953")%>% 
  mutate(decade = "1950s")

# October 1953 – April 1954
get_data_4 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[4]/table/tbody/tr/td[2]') %>%
  html_table(fill = TRUE, header = TRUE)

df4 <- data.frame(get_data_4) %>% 
  mutate(season = "October 1953 – April 1954") %>% 
  mutate(decade = "1950s")

# October 1954 – April 1955
get_data_5 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[5]/table/tbody/tr/td[1]') %>%
  html_table(fill = TRUE, header = TRUE)

df5 <- data.frame(get_data_5) %>% 
  mutate(season = "October 1954 – April 1955") %>% 
  mutate(decade = "1950s")

# October 1955 – April 1956
get_data_6 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[5]/table/tbody/tr/td[2]') %>%
  html_table(fill = TRUE, header = TRUE)

df6 <- data.frame(get_data_6) %>% 
  mutate(season = "October 1955 – April 1956") %>% 
  mutate(decade = "1950s")

# October 1956 – April 1957
get_data_7 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[6]/table/tbody/tr/td[1]') %>%
  html_table(fill = TRUE, header = TRUE)

df7 <- data.frame(get_data_7) %>% 
  mutate(season = "October 1956 – April 1957") %>% 
  mutate(decade = "1950s")

# October 1957 – April 1958
get_data_8 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[6]/table/tbody/tr/td[2]') %>%
  html_table(fill = TRUE, header = TRUE)

df8 <- data.frame(get_data_8) %>% 
  mutate(season = "October 1957 – April 1958") %>% 
  mutate(decade = "1950s")

# October 1958 – April 1959
get_data_9 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[7]/table/tbody/tr/td[1]') %>%
  html_table(fill = TRUE, header = TRUE)

df9 <- data.frame(get_data_9) %>% 
  mutate(season = "October 1958 – April 1959") %>% 
  mutate(decade = "1950s")

# October 1959 – April 1960
get_data_10 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[7]/table/tbody/tr/td[2]') %>%
  html_table(fill = TRUE, header = TRUE)

df10 <- data.frame(get_data_10) %>% 
  mutate(season = "October 1959 – April 1960") %>% 
  mutate(decade = "1950s")

# October 1960 – April 1961
get_data_11 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[8]/table/tbody/tr/td[1]') %>%
  html_table(fill = TRUE, header = TRUE)

df11 <- data.frame(get_data_11) %>% 
  mutate(season = "October 1960 – April 1961") %>% 
  mutate(decade = "1960s")

# October 1961 – April 1962
get_data_12 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[8]/table/tbody/tr/td[2]') %>%
  html_table(fill = TRUE, header = TRUE)

df12 <- data.frame(get_data_12) %>% 
  mutate(season = "October 1961 – April 1962") %>% 
  mutate(decade = "1960s")

# October 1962 – April 1963
get_data_13 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[9]/table/tbody/tr/td[1]') %>%
  html_table(fill = TRUE, header = TRUE)

df13 <- data.frame(get_data_13) %>% 
  mutate(season = "October 1963 – April 1963") %>% 
  mutate(decade = "1960s")

# October 1963 – April 1964
get_data_14 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[9]/table/tbody/tr/td[2]') %>%
  html_table(fill = TRUE, header = TRUE)

df14 <- data.frame(get_data_14) %>% 
  mutate(season = "October 1963 – April 1964") %>% 
  mutate(decade = "1960s")

# October 1964 – April 1965
get_data_15 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[10]/table/tbody/tr/td[1]') %>%
  html_table(fill = TRUE, header = TRUE)

df15 <- data.frame(get_data_15) %>% 
  mutate(season = "October 1964 – April 1965") %>% 
  mutate(decade = "1960s")

# October 1965 – April 1966
get_data_16 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[10]/table/tbody/tr/td[2]') %>%
  html_table(fill = TRUE, header = TRUE)

df16 <- data.frame(get_data_16) %>% 
  mutate(season = "October 1965 – April 1966") %>% 
  mutate(decade = "1960s")

# October 1966 – April 1967
get_data_17 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[11]/table/tbody/tr/td[1]') %>%
  html_table(fill = TRUE, header = TRUE)

df17 <- data.frame(get_data_17) %>% 
  mutate(season = "October 1966 – April 1967") %>% 
  mutate(decade = "1960s")

# October 1967 – April 1968
get_data_18 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[11]/table/tbody/tr/td[2]') %>%
  html_table(fill = TRUE, header = TRUE)

df18 <- data.frame(get_data_18) %>% 
  mutate(season = "October 1967 – April 1968") %>% 
  mutate(decade = "1960s")

# October 1968 – April 1969
get_data_19 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[12]/table/tbody/tr/td[1]') %>%
  html_table(fill = TRUE, header = TRUE)

df19 <- data.frame(get_data_19) %>% 
  mutate(season = "October 1968 – April 1969") %>% 
  mutate(decade = "1960s")

# October 1969 – April 1970
get_data_20 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[12]/table/tbody/tr/td[2]') %>%
  html_table(fill = TRUE, header = TRUE)

df20 <- data.frame(get_data_20) %>% 
  mutate(season = "October 1969 – April 1970") %>% 
  mutate(decade = "1960s")

# October 1970 – April 1971
get_data_21 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[13]/table/tbody/tr/td[1]') %>%
  html_table(fill = TRUE, header = TRUE)

df21 <- data.frame(get_data_21) %>% 
  mutate(season = "October 1970 – April 1971") %>% 
  mutate(decade = "1970s")

# October 1971 – April 1972
get_data_22 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[13]/table/tbody/tr/td[2]') %>%
  html_table(fill = TRUE, header = TRUE)

df22 <- data.frame(get_data_22) %>% 
  mutate(season = "October 1971 – April 1972") %>% 
  mutate(decade = "1970s")

# October 1972 – April 1973
get_data_23 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[14]/table/tbody/tr/td[1]') %>%
  html_table(fill = TRUE, header = TRUE)

df23 <- data.frame(get_data_23) %>% 
  mutate(season = "October 1972 – April 1973") %>% 
  mutate(decade = "1970s")

# October 1973 – April 1974
get_data_24 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[14]/table/tbody/tr/td[2]') %>%
  html_table(fill = TRUE, header = TRUE)

df24 <- data.frame(get_data_24) %>% 
  mutate(season = "October 1973 – April 1974") %>% 
  mutate(decade = "1970s")

# October 1974 – April 1975
get_data_25 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[15]/table/tbody/tr/td[1]') %>%
  html_table(fill = TRUE, header = TRUE)

df25 <- data.frame(get_data_25) %>% 
  mutate(season = "October 1974 – April 1975") %>% 
  mutate(decade = "1970s")

# October 1975 – April 1976
get_data_26 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[15]/table/tbody/tr/td[2]') %>%
  html_table(fill = TRUE, header = TRUE)

df26 <- data.frame(get_data_26) %>% 
  mutate(season = "October 1975 – April 1976") %>% 
  mutate(decade = "1970s")

# October 1976 – April 1977
get_data_27 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[16]/table/tbody/tr/td[1]') %>%
  html_table(fill = TRUE, header = TRUE)

df27 <- data.frame(get_data_27) %>% 
  mutate(season = "October 1976 – April 1977") %>% 
  mutate(decade = "1970s")

# October 1977 – April 1978
get_data_28 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[16]/table/tbody/tr/td[2]') %>%
  html_table(fill = TRUE, header = TRUE)

df28 <- data.frame(get_data_28) %>% 
  mutate(season = "October 1977 – April 1978") %>% 
  mutate(decade = "1970s")

# October 1978 – April 1979
get_data_29 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[17]/table/tbody/tr/td[1]') %>%
  html_table(fill = TRUE, header = TRUE)

df29 <- data.frame(get_data_29) %>% 
  mutate(season = "October 1978 – April 1979") %>% 
  mutate(decade = "1970s")

# October 1979 – April 1980
get_data_30 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[17]/table/tbody/tr/td[2]') %>%
  html_table(fill = TRUE, header = TRUE)

df30 <- data.frame(get_data_30) %>% 
  mutate(season = "October 1979 – April 1980") %>% 
  mutate(decade = "1970s")


# October 1979 – April 1980
get_data_30 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[17]/table/tbody/tr/td[2]') %>%
  html_table(fill = TRUE, header = TRUE)

df30 <- data.frame(get_data_30) %>% 
  mutate(season = "October 1979 – April 1980") %>% 
  mutate(decade = "1970s")


# October 1979 – April 1980
get_data_30 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[17]/table/tbody/tr/td[2]') %>%
  html_table(fill = TRUE, header = TRUE)

df30 <- data.frame(get_data_30) %>% 
  mutate(season = "October 1979 – April 1980") %>% 
  mutate(decade = "1970s")


# October 1979 – April 1980
get_data_30 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[17]/table/tbody/tr/td[2]') %>%
  html_table(fill = TRUE, header = TRUE)

df30 <- data.frame(get_data_30) %>% 
  mutate(season = "October 1979 – April 1980") %>% 
  mutate(decade = "1970s")


# October 1979 – April 1980
get_data_30 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[17]/table/tbody/tr/td[2]') %>%
  html_table(fill = TRUE, header = TRUE)

df30 <- data.frame(get_data_30) %>% 
  mutate(season = "October 1979 – April 1980") %>% 
  mutate(decade = "1970s")


# October 1979 – April 1980
get_data_30 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[17]/table/tbody/tr/td[2]') %>%
  html_table(fill = TRUE, header = TRUE)

df30 <- data.frame(get_data_30) %>% 
  mutate(season = "October 1979 – April 1980") %>% 
  mutate(decade = "1970s")

###################

# October 1980 – April 1981
get_data_31 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[18]/table/tbody/tr/td[1]') %>%
  html_table(fill = TRUE, header = TRUE)

df31 <- data.frame(get_data_31) %>% 
  mutate(season = "October 1980 – April 1981") %>% 
  mutate(decade = "1980s")

# October 1981 – April 1982
get_data_32 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[18]/table/tbody/tr/td[2]') %>%
  html_table(fill = TRUE, header = TRUE)

df32 <- data.frame(get_data_32) %>% 
  mutate(season = "October 1981 – April 1982") %>% 
  mutate(decade = "1980s")

# October 1982 – April 1983
get_data_33 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[19]/table/tbody/tr/td[1]') %>%
  html_table(fill = TRUE, header = TRUE)

df33 <- data.frame(get_data_33) %>% 
  mutate(season = "October 1982 – April 1983") %>% 
  mutate(decade = "1980s")

# October 1983 – April 1984
get_data_34 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[19]/table/tbody/tr/td[2]') %>%
  html_table(fill = TRUE, header = TRUE)

df34 <- data.frame(get_data_34) %>% 
  mutate(season = "October 1983 – April 1984") %>% 
  mutate(decade = "1980s")

# October 1984 – April 1985
get_data_35 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[20]/table/tbody/tr/td[1]') %>%
  html_table(fill = TRUE, header = TRUE)

df35 <- data.frame(get_data_35) %>% 
  mutate(season = "October 1984 – April 1985") %>% 
  mutate(decade = "1980s")

# October 1985 – April 1986
get_data_36 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[20]/table/tbody/tr/td[2]') %>%
  html_table(fill = TRUE, header = TRUE)

df36 <- data.frame(get_data_36) %>% 
  mutate(season = "October 1985 – April 1986") %>% 
  mutate(decade = "1980s")

# October 1986 – April 1987
get_data_37 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[21]/table/tbody/tr/td[1]') %>%
  html_table(fill = TRUE, header = TRUE)

df37 <- data.frame(get_data_37) %>% 
  mutate(season = "October 1986 – April 1987") %>% 
  mutate(decade = "1980s")

# October 1987 – April 1988
get_data_38 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[21]/table/tbody/tr/td[2]') %>%
  html_table(fill = TRUE, header = TRUE)

df38 <- data.frame(get_data_38) %>% 
  mutate(season = "October 1987 – April 1988") %>% 
  mutate(decade = "1980s")

# October 1988 – April 1989
get_data_39 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[22]/table/tbody/tr/td[1]') %>%
  html_table(fill = TRUE, header = TRUE)

df39 <- data.frame(get_data_39) %>% 
  mutate(season = "October 1988 – April 1989") %>% 
  mutate(decade = "1980s")

# October 1989 – April 1990
get_data_40 <- web_data %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[22]/table/tbody/tr/td[2]') %>%
  html_table(fill = TRUE, header = TRUE)

df40 <- data.frame(get_data_40) %>% 
  mutate(season = "October 1989 – April 1990") %>% 
  mutate(decade = "1980s")

# create list -------------------------------------------------------------
list <- mget(ls(pattern = "df."))

# wrangle data and create data frame --------------------------------------
df <- do.call(rbind, list) %>% 
  # filter(Rank <= 30) %>%
  mutate(Network_d = paste0(Network, " - ", season)) %>% 
  group_by(decade, season, Network_d) %>%
  mutate(n = n_distinct(Program)) %>% 
  mutate(center = "Network TV") %>% 
  ungroup()

# create edges ------------------------------------------------------------
edge_list1 <- df %>% 
  select(center, decade) %>% 
  unique %>% 
  rename(from = center, to = decade)

edge_list2 <- df %>% 
  select(decade, season) %>% 
  unique %>% 
  rename(from = decade, to = season) 

edge_list3 <- df %>%
  select(season, Network_d) %>%
  unique %>%
  rename(from = season, to = Network_d)

# combine edges -----------------------------------------------------------
edge_list <- rbind(edge_list1, edge_list2, edge_list3)

# create vertices for color group -----------------------------------------
vertices <- data.frame(
  name = unique(c(as.character(edge_list$from), as.character(edge_list$to))), 
  value = runif(165))

vertices$size = df$n[match(vertices$name, df$Network_d)]

vertices$group = df$Network[match(vertices$name, df$Network_d)] %>% 
  replace_na("none")

# create data frame -------------------------------------------------------
df_network <- graph_from_data_frame(edge_list, directed = TRUE, vertices = vertices) 

# create plot -------------------------------------------------------------
set.seed(27)

df_network %>% 
  ggraph(layout = "circlepack") + 
  geom_edge_link(color = "#272324") +
  geom_node_point(size = 0.75) +
  geom_node_point(aes(size = size, color = group), alpha = 0.90) +
  scale_size_continuous(name = "Number of Shows in Top 30") +
  scale_color_manual(values = c("#672fe6", "#f6ac0f", "#0fcdf6", "#f60f5a", "black"), guide = "none") +
  annotate("text", x = -5, y = 11, label = "1970s", color = "#000000", family = font, fontface = "bold", size = 3) +
  annotate("text", x = 6, y = 7, label = "1950s", color = "#000000", family = font, fontface = "bold", size = 3) +
  annotate("text", x = -4, y = -7, label = "1960s", color = "#000000", family = font, fontface = "bold", size = 3) +
  annotate("text", x = 0, y = -11, label = "1980s", color = "#000000", family = font, fontface = "bold", size = 3) +
  annotate("curve", x = 6, y = -12, xend = 4.7, yend = -9.25, linewidth = 0.5,  curvature = -0.15, arrow = arrow(length = unit(1.25, "mm")), color = "#0fcdf6") +
  annotate("text", x = 6.5, y = -13, label = "The Simpsons", color = "#0fcdf6", family = font, fontface = "bold", size = 3) +
  theme_void() +
  theme(plot.title = element_text(family = font_t, size = 30, hjust = 0, face = "bold", color = "#000000"),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(family = font, size = 10, hjust = 0, color = "#000000", lineheight = 1.1),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 8, family = font, color = "#000000", hjust = 0.5),
        legend.position = "top",
        legend.margin = margin(t = 10),
        legend.text = element_text(size = 8, family = font, color = "#272324", hjust = 0.5),
        legend.title = element_text(size = 8, family = font, color = "#272324", hjust = 0.5),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "TELEVISION NETWORKS",
       subtitle = "Three networks — <span style='color: #672fe6;'><b>ABC</b></span>, <span style='color: #f6ac0f;'><b>CBS</b></span>, and <span style='color: #f60f5a;'><b>NBC</b></span> — dominated American primetime network<br>television for decades until <span style='color: #0fcdf6;'><b>FOX</b></span> landed in the top 30 rated shows with a little show<br>called The Simpsons in 1989.",
       caption = "\n\n\n#30DayChartChallenge | Data: wikipedia | Design: Ryan Hart")


# save plot
ggsave(paste0("day_17_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


