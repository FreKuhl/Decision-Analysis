# Data Wrangling ####
library(ggplot2)
library(tidyverse)


x <- diamonds %>%
  select(price, carat) %>%
  filter(carat > 0.5) %>%
  rename(cost = price) %>%
  mutate(cheap_expensive = ifelse(cost > mean(cost), "expensive", "cheap")) %>%
  group_by(cheap_expensive) %>%
  summarize(min(carat), mean(carat), max(carat),
            min(cost), mean(cost), max(cost))

x
