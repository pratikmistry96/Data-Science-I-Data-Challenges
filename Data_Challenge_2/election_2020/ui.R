#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

####################
## Load Libraries ##
####################

library(dplyr)
library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(lubridate)

## Load polls 2020 csv for a drop-down menu of region/states for the trend plot
polls_2020 <- read.csv("polls_2020.csv")


###############
## Define UI ##
###############

## Define a navbarpage with multiple tabPanels with different types of inputs

shinyUI(navbarPage(
  "2020 Election Poll Trends",
  ## Define tab panel for 2020 poll trends by region
  tabPanel(
    "2020 Poll Trends",
    ## Set tab title
    sidebarPanel(
      h4("Comparing 2020 Poll Trends by Region"),# Create an HTML heading
      # Create a paragraph describing the plot
      p(
        "Joe Biden became the unofficial presidential nominee of the Democratic party around February. In this tab, you can compare
        polling percentages of the candidates from February to October to see how their favorability flucuated over
        the past couple of months. Within this tab, you can visualize trends in polling percentage from February to 
        October 14th, 2020. Within this tab, you can select the region/state and restrict the polls within a range
        of months. A few things to note, the national polls have remained in favor in Biden with
        a slowly increasing gap, but this plot allows you to see how state polls have changed over time. The other tabs
        visualize the electoral map of continental US with results from the most recent poll on October 14th, 2020. And
        the last tab allows you to see how the 2020 poll compared with historical polls (from the same day) of their
        respective year. However, it's important to realize there are 20 days till the election from October 14th,
        so anything can happen. This app allows you to visualize trends of polling."
      ),
      p("This interface allows you to select the region and restrict the months between February and March, inclusive,
        to observe polling trends."),
      selectInput(
        # Add a section to include a drop down for selecting the state of interest for the 2020 poll trend
        "state",
        "Select a Region",
        choices = sort(unique(polls_2020$state)),
        # The choices are unique states from the csv file
        selected = "National"
      ),
      numericInput(
        # Add a numeric input if one is interested in restricting the data by month range
        "start",
        # Variable name is start
        "Select a Start Month",
        value = 2,
        # Current value is 2
        min = 2,
        # The minimum is 2 (February)
        max = 10,
        # Max is 10 (October)
        step = 1
      ),
      numericInput(
        "end",
        # Variable name is end
        "Select an End Month",
        value = 10,
        min = 2,
        max = 10,
        step = 1
      )
    ),
    mainPanel(plotlyOutput("trendPlot", height = 600),# Plot the data and set the height
              p("All data used in this visualization was pulled from the FiveThirtyEight GitHub page 
                for election poll results https://github.com/fivethirtyeight/data/tree/master/polls"),
              p("FiveThirtyEight provides data for 2020 polling, as well as, results from historical polls
                from 1968 to 2020")) 
  ),
  tabPanel(
    # Create a tab panel of the election map if the election happened on October 14th 2020
    "2020 Election Map",
    sidebarPanel(
      h4("Electoral Results of Continental USA"),
      # Set the header
      # Add a paragraph describing the graph and electoral results if the election were to happen on October 14th, 2020
      p(
        "Based on polling results from October 14th, 2020, this maps shows who will win the state and the associated number
        of electoral votes of only the continental USA. As of October 14th and according to election results from the 
        continental US, it seems that Biden will win the election by a landslide; however, the results may differ on 
        election day depending on the actual number of people who vote, and the results of undecided voters."
      )
    ),
    mainPanel(plotlyOutput("map", height = "auto"),# Plot the data and set the height
              p("\n\nAll data used in this visualization was pulled from the FiveThirtyEight GitHub page 
                for election poll results https://github.com/fivethirtyeight/data/tree/master/polls"),
              p("FiveThirtyEight provides data for 2020 polling, as well as, results from historical polls
                from 1968 to 2020"))
  ),
  tabPanel(
    # Create a tab panel of historical election comparisons on October 1th across all years
    "Historical Poll Comparisons",
    sidebarPanel(
      h4("Historical Comparison of Polling Percentages"),
      p(
        "With this graph, you can compare polling percentages of candidates from historical elections between 1968 to
        2016 with 2020. The polling percentages of all of these models were dated on October 14th of their respective years.
        You can use this chart to infer results, but recently, we've seen that polls are not definite measures of election
        results."
      ),
      selectInput(
        # Ask for year inputs
        "comp_year",
        "Select Election Year for Comparison",
        choices = seq(1968, 2016, 4),
        selected = 2016
      )
    ),
    mainPanel(plotlyOutput("compPlot", height = 800),# Plot the data and set the height
              p("\n\nAll data used in this visualization was pulled from the FiveThirtyEight GitHub page 
                for election poll results https://github.com/fivethirtyeight/data/tree/master/polls"),
              p("FiveThirtyEight provides data for 2020 polling, as well as, results from historical polls
                from 1968 to 2020"))
  )
))