# Section 1 ####

library(tidyverse)

# ?read.csv
participants_data <- read.csv("participants_data.csv")

# shows first 6 rows of the dataset
head(participants_data)

# shows the names of the variables
names(participants_data)

# shows the structure of the dataset
str(participants_data)

# shows all ages in the data
participants_data$age


# dplyr ####
library(dplyr)
library(magrittr)
library(tidyr)

# Change the selection to batch and age
select(participants_data, 
       academic_parents,
       working_hours_per_day)

# Change the selection without batch and age
select(participants_data,
       -academic_parents,
       -working_hours_per_day)

# Change the selection to 
# those who work more than 5 hours a day
filter(participants_data, 
       working_hours_per_day >10)

# Change the filter to those who 
# work more than 5 hours a day and 
# names are longer than three letters
filter(participants_data, 
       working_hours_per_day >10 & 
         letters_in_first_name >6)

# Rename the variable km_home_to_office as commute
rename(participants_data, 
       commute = km_home_to_office)

# mutate a new column named age_mean that is a function of the 
# age multiplied by the mean of all ages in the group
mutate(participants_data, 
       age_mean = age*
         mean(age))

# Mutate new column named response_speed 
# populated by 'slow' if it took you 
# more than a day to answer my email and 
# 'fast' for others
mutate(participants_data, 
       response_speed = ifelse(days_to_email_response > 1, "fast", "slow"))

# Create a summary of the participants_mutate data 
# with the mean number of siblings 
# and median years of study
summarize(participants_data,
          mean(number_of_siblings),
          median(years_of_study))

# magrittr ####
# Use the magrittr pipe to summarize 
# the mean days to email response, 
# median letters in first name, 
# and maximum years of study by gender
participants_data %>% 
  group_by(research_continent) %>% 
  summarize(mean(days_to_email_response), 
            median(letters_in_first_name), 
            max(years_of_study))

# Use the magrittr pipe to create a new column 
# called commute, where those who travel 
# more than 10km to get to the office 
# are called "commuter" and others are "local". 
# Summarize the mean days to email response, 
# median letters in first name, 
# and maximum years of study. 
participants_data %>% 
  mutate(commute = ifelse(
    km_home_to_office > 10, 
    "commuter", "local")) %>% 
  group_by(commute) %>% 
  summarize(mean(days_to_email_response), 
            median(letters_in_first_name), 
            max(years_of_study))

# purrr ####
# Split the data frame by batch, 
# fit a linear model formula 
# (days to email response as dependent 
# and working hours as independent) 
# to each batch, compute the summary, 
# then extract the R^2.
participants_data %>%
  split(.$batch) %>%
  map(~ 
        lm(days_to_email_response ~ 
             working_hours_per_day, 
           data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")
