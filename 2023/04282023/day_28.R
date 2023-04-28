# #30DayChartChallenge | April 2023 - Day 28 | trend  
# Data Source is FRED
# https://fred.stlouisfed.org/series/APU000072610


# libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(stringr)
library(janitor)
library(tsibble)
library(ggtext)
library(bsts)
library(tidybayes)
library(ggfx)
# library(transformr)

# add font ----------------------------------------------------------------
font_add_google(name = "Righteous", family = "Righteous")
font_add_google(name = "Open Sans", family = "Open Sans")
font_t <- "Righteous"
font <- "Open Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999)

# load data ---------------------------------------------------------------
data <- readr::read_csv("APU000072610.csv")

# wrangle data and create data frame --------------------------------------
df <- data %>%
  clean_names() %>% 
  mutate(date = ymd(date)) %>%
  rename(avg_price = apu000072610) %>% 
  filter(date > "2006-12-01") %>% 
  mutate(avg_price = as.numeric(avg_price)) %>% 
  mutate(year = year(date), month = month(date), day = day(date)) %>%
  mutate(date = floor_date(as_date(date), "month")) %>% 
  mutate(date = yearmonth(date))


# starting reference for model and forecast code
# https://github.com/mjskay/uncertainty-examples/blob/master/arima.md

# create model ------------------------------------------------------------
set.seed(74)

ss <- AddLocalLinearTrend(list(), df$avg_price)
ss <- AddSeasonal(ss, df$avg_price, nseasons = 12)

model <- bsts(df$avg_price, state.specification = ss, niter = 1000)

# set time horizon for forecast -------------------------------------------
horizon <- 48

df_forecast <- data.frame(date = max(df$date) + 1:horizon) %>% 
  add_draws(predict(model, horizon = horizon, burn = SuggestBurn(0.1, model), quantiles = c(0.25, 0.75))$distribution) %>% 
  sample_draws(100)

# create plot -------------------------------------------------------------
ggplot() +
  geom_line(data = df, aes(x = as.Date(date), y = avg_price), color = "#FFFFFF", linewidth = 0.8) +
  with_outer_glow(
    geom_line(data = df_forecast, aes(x = as.Date(date), y = .value, group = .draw), linewidth = 0.3, color = "#69F0AE", alpha = 0.5),
    color = "#69F0AE", sigma = 7) +
  geom_vline(xintercept = 03-2023, color = "red", size = 10) +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 0.3, by = 0.04)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal() +
  theme(plot.title = element_text(family = font_t, size = 32, hjust = 0.5, color = "#FFFFFF"),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(family = font, size = 14, hjust = 0.5, color = "#FFFFFF", margin = margin(b = 30)),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 9, family = font, color = "#FFFFFF", hjust = 0.5, margin = margin(t = 30)),
        legend.position = "none",
        axis.text = element_text(size = 8, family = font, color = "#FFFFFF"),
        axis.title.y = element_text(size = 8, family = font, color = "#FFFFFF", margin  = margin(r = 5)),
        axis.title.x = element_text(size = 8, family = font, color = "#FFFFFF", margin = margin(t = 20)),
        # axis.title.x = element_blank(),
        axis.line = element_line(color = "#FFFFFF", linewidth = 0.3),
        panel.grid = element_line(color = "#616161", linewidth = 0.2, linetype = "dashed"),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(color = NA, fill = "#000000")) +
  labs(title = "ELECTRICITY PRICES",
       subtitle = "<span style='color: #69F0AE;'><b>Forecasting</b></span> U.S. City Average Prices",
       y = "Electricity per Kilowatt-Hour (U.S. Dollars)\n",
       x = "Forecast uses a Bayesian Structural Time Series model",
       caption = "#30DayChartChallenge | Historical Prices: fred.stlouisfed.org | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("day_28_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)
 
