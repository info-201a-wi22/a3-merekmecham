library(ggplot2)
library(dplyr)
library(tidyr)
install.packages("usdata")
library(usdata)

setwd("~/Documents/INFO201/a3-merekmecham/source/")

filename <- "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"
data <- read.csv(filename)

by_race <- data %>%
  select(year, aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop) %>%
  filter(year >= 2010) %>%
  gather(key = race, value = population, -year)
  
chart_one <- ggplot(by_race) +
  geom_col(
    mapping = aes(x = year, y = population, fill = race), position = "dodge",
    alpha = .5
  ) +
  labs(
    title = "Jail Population Rates Among Different Races",
    x = "Year",
    y = "Jail Population Rate",
    fill = "Race"
  )
comparison <- data %>%
  select(year, total_jail_pop, black_jail_pop) %>%
  filter(year >= 1984) %>%
  gather(key = pop_type, value = jail_pop, -year)
chart_two <- ggplot(comparison) +
  geom_col(
    mapping = aes(x = year, y = jail_pop, fill = pop_type), position = "dodge",
  ) + 
  labs(
    title = "Total Jail Populations vs. Black Jail Populations",
    x = "Year",
    y = "Population",
    fill = "Population Type"
  )

black_rate <- data %>%
  filter(year == 2018) %>%
  select(state, black_jail_pop) 
  

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
  )
state_shape <- map_data("state") %>%
  rename(state = region) %>%
  mutate(state = state2abbr(state))

map <- state_shape %>%
  left_join(black_rate, by = "state")

black_map <- ggplot(map) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop),
    color = "white",
    size = .3
  ) +
  coord_map() +
  scale_fill_continuous(low = "#fee8c8", high = "#de2d26") +
  labs(title = "Black Jail Population in 2018", fill = "Black Jail Population") +
  blank_theme 


