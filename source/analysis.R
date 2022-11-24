library(tidyverse)
library("dplyr")
library("ggplot2")
library("ggrepel")


# The functions might be useful for A4
source("./source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num = 6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ----
#----------------------------------------------------------------------------#
# Reading data
orig_incarceration_trends <- read.csv("./data/incarceration_trends.csv")

# Filtering columns
incarceration_trends <- orig_incarceration_trends[, -c(
  1, 3, 7:9, 15:20, 22:27, 34:45, 48:53, 55:56, 63:74, 76:77, 84:95, 97:98, 108:109, 112:113, 115:116
)]
incarceration_trends <- incarceration_trends[, -c(
  5:9, 32:49
)]

# Counties with most jail and prison population in each state from 1970-2018

# Jail Population
jail_pop <- incarceration_trends %>%
  mutate(location = paste0(county_name, sep = ", ", state)) %>%
  select(county_name, state, location, total_jail_pop, aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop, other_race_jail_pop)

jail_pop <- jail_pop %>%
  group_by(county_name, state, location) %>%
  summarise(across(everything(), sum, na.rm = TRUE))

max_jail_pop <- jail_pop %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>%
  group_by(state) %>%
  select(state, location, total_jail_pop, aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop, other_race_jail_pop) %>%
  slice(which.max(total_jail_pop))

# Prison population
prison_pop <- incarceration_trends %>%
  mutate(location = paste0(county_name, sep = ", ", state)) %>%
  select(county_name, state, location, total_prison_pop, aapi_prison_pop, black_prison_pop, latinx_prison_pop, native_prison_pop, white_prison_pop, other_race_prison_pop)

prison_pop <- prison_pop %>%
  group_by(county_name, state, location) %>%
  summarise(across(everything(), sum, na.rm = TRUE))

max_prison_pop <- prison_pop %>%
  filter(total_prison_pop == max(total_prison_pop, na.rm = TRUE)) %>%
  group_by(state) %>%
  select(state, location, total_prison_pop, aapi_prison_pop, black_prison_pop, latinx_prison_pop, native_prison_pop, white_prison_pop, other_race_prison_pop) %>%
  slice(which.max(total_prison_pop))

# Which year did each state experienced the most jail population spike
jail_incar_over_time <- incarceration_trends %>%
  select(year, state, total_jail_pop, aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop, other_race_jail_pop)

jail_incar_over_time <- jail_incar_over_time %>%
  group_by(year, state) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  arrange(by = state, year)

jail_incar_over_time <- jail_incar_over_time %>%
  group_by(state) %>%
  mutate(pop_diff = total_jail_pop - lag(total_jail_pop))

most_jail_incarcerations <- jail_incar_over_time %>%
  filter(pop_diff == max(pop_diff, na.rm = TRUE)) %>%
  select(year, state, pop_diff) %>%
  slice(which.max(pop_diff))

# Which year did each state experienced the largest decrease in jail population
decreased_jail_incarcerations <- jail_incar_over_time %>%
  filter(pop_diff == min(pop_diff, na.rm = TRUE)) %>%
  select(year, state, pop_diff) %>%
  slice(which.min(pop_diff))

# Which year did each state experienced the most prison population spike
prison_incar_over_time <- incarceration_trends %>%
  select(year, state, total_prison_pop, aapi_prison_pop, black_prison_pop, latinx_prison_pop, native_prison_pop, white_prison_pop, other_race_prison_pop)

prison_incar_over_time <- prison_incar_over_time %>%
  group_by(year, state) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  arrange(by = state, year)

prison_incar_over_time <- prison_incar_over_time %>%
  group_by(state) %>%
  mutate(pop_diff = total_prison_pop - lag(total_prison_pop))

most_prison_incarcerations <- prison_incar_over_time %>%
  filter(pop_diff == max(pop_diff, na.rm = TRUE)) %>%
  select(year, state, pop_diff) %>%
  slice(which.max(pop_diff))

# Which year did each state experienced the largest decrease in prison population
decreased_prison_incarcerations <- prison_incar_over_time %>%
  filter(pop_diff == min(pop_diff, na.rm = TRUE)) %>%
  select(year, state, pop_diff) %>%
  slice(which.min(pop_diff))

# Jail population differences as of 2018 in each state
jail_pop_2018 <- jail_incar_over_time %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  group_by(state) %>%
  select(year, state, total_jail_pop, pop_diff, aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop, other_race_jail_pop) %>%
  slice(which.max(pop_diff))

# Prison population differences as of 2018 in each state
prison_pop_2018 <- prison_incar_over_time %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  select(year, state, total_prison_pop, pop_diff, aapi_prison_pop, black_prison_pop, latinx_prison_pop, native_prison_pop, white_prison_pop, other_race_prison_pop) %>%
  slice(which.max(pop_diff))
#----------------------------------------------------------------------------#

## Section 3  ----
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# This function shows the yearly total jail population from 1970 to 2018
get_year_jail_pop <- function() {
  yearly_jail_pop <- incarceration_trends %>%
    select(year, total_jail_pop) %>%
    group_by(year) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>%
    arrange(by = year)
  return(yearly_jail_pop)
}

# This function is a bar chart that shows the yearly jail population
plot_jail_pop_for_us <- function() {
  yearly_jail_plot <- ggplot(get_year_jail_pop()) +
    geom_col(mapping = aes(x = year, y = total_jail_pop)) +
    scale_y_continuous(breaks = c(0, 200000, 400000, 600000, 800000), labels = scales::comma) +
    labs(
      title = "Increase of Jail Population in U.S. (1970-2018)",
      x = "Year",
      y = "Total Jail Population"
    )
  return(yearly_jail_plot)
}
#----------------------------------------------------------------------------#


## Section 4  ----
#----------------------------------------------------------------------------#
# Growth of Prison Population by State
# This function shows the yearly jail population by each state from 1970 to 2018
get_jail_pop_by_states <- function(states) {
  states <- incarceration_trends$state
  jail_pop_states <- incarceration_trends %>%
    filter(state == states) %>%
    select(year, state, total_jail_pop) %>%
    group_by(year, state) %>%
    summarise(across(everything(), sum, na.rm = TRUE))
  return(jail_pop_states)
}

# This function is a line chart that shows yearly jail population by state
plot_jail_pop_by_states <- function(states) {
  jail_states_plot <- ggplot(get_jail_pop_by_states()) +
    geom_line(mapping = aes(x = year, y = total_jail_pop, group = state, color = state)) +
    geom_text(data = subset(get_jail_pop_by_states(), year == "2018"),
              aes(label = state, color = state, x = year, y = total_jail_pop),
              hjust = -1) +
    ggtitle("Growth of Jail Population by State (1970-2018)") +
    xlab("Year") +
    ylab("Total Jail Population")
  return(jail_states_plot)
}
print(plot_jail_pop_by_states())

#----------------------------------------------------------------------------#

## Section 5  ----
#----------------------------------------------------------------------------#
# Race population per state and 
# Your functions might go here ... <todo:  update comment>
state_race_pop <- incarceration_trends %>% 
  select(state, total_jail_pop, aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop, other_race_jail_pop) %>%
  

#----------------------------------------------------------------------------#

## Section 6  ----
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ----
