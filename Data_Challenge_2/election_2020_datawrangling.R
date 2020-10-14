# Author: Pratik Mistry
# Date: 14 October 2020
# Data Challenge 2

# Description: This r script is used to wrangle election data from fivethirtyeight into CSVs in order to plot poll 
# trends over 2020, #an election map, and historical polling percentages. All of the plots will be displayed on a shiny 
# web app.

# Shiny App: https://pratikmistry96.shinyapps.io/election_2020/
# GitHub: https://github.com/pratikmistry96/Data-Science-I-Data-Challenges/tree/master/Data_Challenge_1


rm(list = ls()) # Remove any variables from the environment

###########################
## Set Working Directory ##
###########################

setwd(
  "~/Weill Cornell Medicine/Fall 2020/Data Science I/Data Challenges/Data_Challenge_2/"
)

####################
## Load Libraries ##
####################
# Add any libraries necessary to perform all necessary functions

library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(lubridate)


#####################
## Define Function ##
#####################

## Following function will be used to average the polling values of the three electoral districts of Maine and Nebraska

get_state_avg <- function(data, name, state_vec) {
  state_df <- data %>% ## Start the results in state_df
    filter(state %in% state_vec) %>% ## Filter the data by the names of the districts
    group_by(cycle, ## Group by election cycle year
             candidate_name, ## Group by candidate name
             modeldate, ## Group by model date or "polling date"
             party) %>% ## Group by political party
    summarize(state = name, ## Summarize and add the entire state name
              pct_trend_adjusted = mean(pct_trend_adjusted)) ## Add the mean of all districts
  return(state_df)
}


#####################
## Load Data Files ##
#####################

# The following chunks of code load the initial CSVs and perform some basic manipulations 

## Load data of historical polling information dating from 2016 all the way back to 1968
historical_polls <-
  read.csv("polls/pres_pollaverages_1968-2016.csv") %>% # Read the CSV file
  # Select columns of interest that will match the following dataset that gets loaded in
  select(cycle, # Select election year
         modeldate, # Select poll date
         state, # Select state
         candidate_name, # Select candidate name
         pct_trend_adjusted) %>% # Adjusted polling percent
  mutate(modeldate = as.Date(modeldate, format = "%m/%d/%Y")) %>% ## Turn string date into date object
  filter(months(modeldate) == "October",  # Filter to specific poll date of October 14th to match most recent data from 
                                          # fivethirtyeight 
         day(modeldate) == 14,
         state == "National") # Filter data to include only national polls


## Load polling data of all states from February to October from 2020
polls_2020 <-
  read.csv("polls/presidential_poll_averages_2020.csv") %>%
  mutate(modeldate = as.Date(modeldate, format = "%m/%d/%Y")) %>% # Turn date into date object
  filter(candidate_name %in% c("Joseph R. Biden Jr.", # Filter by candidate to remove convention bounce for simplicity
                               "Donald Trump")) %>%
  select(!pct_estimate) # Remove percent estimate and only include percent trend adjusted

## Load data of political affiliation of each candidate from 2020 back to 1968 of candidates included in this analysis.
## That means we are also including some third party independent candidates for some elections
political_aff <-
  read.csv("polls/pres_parties_1968-2020.csv")


## Load that includes the number of electoral college votes by state
electoral_college <-
  read.csv("polls/electoral_college.csv")


####################
## Data Wrangling ##
####################


# Extract polling values from October 14, 2020 from all continental US states, excluding National polls, Alaska, 
# and Hawaii

polls_2020_states <- polls_2020 %>%
  filter(months(modeldate) == "October", # Filter data to be polls from October 14
         day(modeldate) == 14,
         state != "National") %>% # Filter data to remove national polls
  # Use an if_else statement to add political affiliations, if Biden = Democratic ; if Trump = Republican
  mutate(party = if_else( # Use mutate to update data frame
    candidate_name == "Joseph R. Biden Jr.",
    "Democratic",
    "Republican"
  ))

## Add national poll values to the historical data and store it into a new data frame titled polls_1968_2020
polls_1968_2020 <-
  historical_polls %>%
  rbind(polls_2020 %>% # Use rbind to add the new row of values after manipulating data from the polls_2020 data frame
          filter(
            months(modeldate) == "October", # Filter to October 14th
            day(modeldate) == 14,
            state == "National" # Filter only National polls
          )) %>%
  arrange(cycle) %>% # Arrange the data frame by election year from 1968 to 2020
  inner_join(political_aff, by = c("cycle",
                                   "candidate_name")) # Use inner_join to add the political affiliation of all 
                                                      # candidates in the data frame


## Average all three Nebraska and Maine Districts to get an average poll for each state
me_dist <- c("ME-1", "ME-2", "Maine") # Create a vector for the name of Maine districts
ne_dist <- c("NE-1", "NE-2", "Nebraska") # Create a vector the name of Nebraska districts

## Use the function defined earlier and the recently created vectors to get a row containing the average value for NE
nebraska <- get_state_avg(polls_2020_states,
                          "Nebraska",
                          ne_dist)

## Repeat above process for ME
maine <- get_state_avg(polls_2020_states,
                       "Maine",
                       me_dist)

## Now adjust the polls_2020_states data frame to remove all NE and ME districts and rejoin NE and ME with average polls
## Also, the following code removes Alaska and Hawaii because we will be using this data to create an election map
## of the continental USA

polls_2020_states <- polls_2020_states %>%
  # Use filter to remove NE districts, ME districts, Hawaii, and Alaska
  filter(!(state %in% c(ne_dist,
                        me_dist,
                        "Alaska",
                        "Hawaii"))) %>%
  rbind(maine, nebraska) %>% # Join the average poll data for NE and ME back into the data frame
  mutate(state = stringr::str_to_title(state)) # Capitalize all data


## Determine who the winner is for each state by separating the data frame into democrats and republicans (dems and rep)
# Extract results for Biden
dems <- polls_2020_states %>%
  filter(candidate_name == "Joseph R. Biden Jr.")

# Extract results for Trump
rep <- polls_2020_states %>%
  filter(candidate_name == "Donald Trump")

# Determine for which state Biden wins = TRUE and Trump wins = FALSE
results <- dems$pct_trend_adjusted > rep$pct_trend_adjusted

# Create a new data frame of election results with each state and DC as a row
polls_2020_results <- dems %>%
  mutate(dems = pct_trend_adjusted) %>% # Add Democratic poll values to a new column called dems
  select(state, dems) %>% # Select only the state and dems column
  mutate(rep = rep$pct_trend_adjusted) %>% # Add republican poll values
  cbind(results) %>% # Add a column of the previous calculated results
  # Use an if_else statement to switch from logical vector to a new column stating who the winner is
  mutate(winner = if_else(results,
                          "Joseph R. Biden Jr.",
                          "Donald Trump")) %>%
  select(!results) %>% # Remove logical vector
  inner_join(electoral_college,
             by = "state")

## Create a new data frame containing total electoral results for each candidate
electoral_results <- polls_2020_results %>%
  group_by(winner) %>%
  summarize(total_votes = sum(electoral_votes))

## Create a data frame containing coordinates for a map of continental US similar to class and add election result
## by state
us_states <- map_data("state") %>%
  mutate(region = stringr::str_to_title(region))

## Add election results to map
election_map <- us_states %>%
  inner_join(polls_2020_results, by = c("region" = "state"))

##################################
## Remove Unnecessary Variables ##
##################################

rm(
  maine,
  nebraska,
  political_aff,
  rep,
  dems,
  us_states,
  electoral_college,
  historical_polls,
  polls_2020_states,
  polls_2020_results,
  me_dist,
  ne_dist,
  results
)


##############################
## Write CSVs for Shiny App ##
##############################

write.csv(electoral_results, "election_2020/electoral_results.csv")
write.csv(election_map, "election_2020/election_map.csv")
write.csv(polls_1968_2020, "election_2020/polls_1968_2020.csv")
write.csv(polls_2020, "election_2020/polls_2020.csv")