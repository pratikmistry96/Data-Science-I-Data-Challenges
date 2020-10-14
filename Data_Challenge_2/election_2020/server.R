#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Author: Pratik Mistry
# Date: 14 October 2020
# Data Challenge 2 - Server Side R files

# Description: This r script is to define server logic, such as plotting functions for the three plots in the app

# Shiny App: https://pratikmistry96.shinyapps.io/election_2020/
# GitHub: https://github.com/pratikmistry96/Data-Science-I-Data-Challenges/tree/master/Data_Challenge_1

####################
## Load Libraries ##
####################

library(dplyr)
library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(lubridate)


###############
## Functions ##
###############

## Create a function to create a bar plot that compares polling percentages from historical elections to 2020

election_comp <- function(data, year) {
    ## Filter data to years of interest
    data <- data %>%
        filter(cycle == 2020 | cycle == year) %>% # Filter election year to 2020 and year of interest
        mutate(cycle = factor(cycle), # Turn election year into factors
               party = factor(party, # Turn parties into factors and order them to ensure Independent is last
                                     # this is important because not all years have a third party
                              levels = c(
                                  "Democratic",
                                  "Republican",
                                  "Independent"
                              )))
    ## Create plot object
    ggplot.comp <- data %>%
        ggplot(aes(x = cycle, # x-axis is cycle
                   y = pct_trend_adjusted, # y-axis is the poll percentage
                   group = party)) + # Group the plot by political party
        geom_col(aes( # Create column chart
            fill = party, # Separate the fill by political party
            text = paste0( # Add text so the hoverinfo will be clean
                "Election Year: ", # Include election year
                cycle,
                "\nCandidate: ",
                candidate_name, # Include candidate name
                "\nPolitical Party: ",
                party,
                "\nPoll Percentage: ",
                round(pct_trend_adjusted, 1), # Include the polling percentage
                "%"
            )
        ),
        position = "dodge") + # Use dodge to separate the columns
        # Use geom_text to add the candidate name above the bar
        geom_text(
            aes(
                x = cycle, # Set the x-axis to be the election year
                y = pct_trend_adjusted + 9, # Set the y-axis to be above the height of the bar
                label = str_wrap(candidate_name, 8), # Wrap the candidate names 
                text = NULL
            ),
            position = position_dodge(0.9), # Use position dodge to separate the text
            size = 3
        ) +
        # Set the color of the bars by political affiliation Blue = Dems, Red = Rep, Green = Independent
        scale_fill_manual(values = c("blue",
                                     "red",
                                     "green")) +
        ggtitle(sprintf("Election Polls: %d vs 2020", year)) + ## Add plot title
        xlab("Election Cycle") + ## Set x-label
        ylab("Polling %") + ## Set y label
        ylim(0, 100) + ## Set y limits
        labs(fill = "Political Affliation:") + ## Set fill title
        theme(
            panel.grid = element_blank(), ## Turn off the grid
            plot.background = element_rect(fill = "gray92"), # Blend background to plot background
            legend.background = element_rect(fill = "gray92") # Blend legened background
        )
    ggplot.comp <- ggplotly(ggplot.comp, tooltip = "text") ## Turn plot into a plotly plot
    
    return(ggplot.comp) # Return plot object
}

## Create a function to plot the poll trends across 2020 from a specified region(state), start month, and end month
trends <- function(data, region, start, end) {
    if (start > end) # If the start number is greater than end, set it equal to end
        start <- end
    trend.plot <- data %>%
        mutate(modeldate = as.Date(modeldate, # Transform date to a date object
                                   format = "%Y-%m-%d")) %>%
        filter(state == region, # Filter data to a specified region and month range
               months(modeldate) %in% month.name[start:end]) %>%
        arrange(modeldate) %>% # Arrange trends by model date (poll date)
        group_by(candidate_name) %>% # Group data by candidate name
        ggplot(
            aes(
                x = modeldate, # Set x value to be model date
                y = pct_trend_adjusted, # Set y value to be poll percentage
                group = candidate_name, # Group data by candidate name
                color = candidate_name, # Color by candidate name
                # Use text to create clean hoverinfo for the plotly 
                text = paste0( 
                    "Candidate: ",
                    candidate_name, # Add candidate name
                    "\nPoll Date: ",
                    modeldate, # Add poll date / model date
                    "\nPoll Percentage: ",
                    pct_trend_adjusted # All poll percentage
                )
            )
        ) +
        geom_line(size = 1) +
        labs(
            title = "2020 Election Polling Trend", # Add Title
            x = "Poll Date", # Set x-label
            y = "Polling %", # Set y-label
            color = "Candidate Name" # Label color / legened
        ) +
        theme(
            plot.background = element_rect(fill = "gray92"),
            legend.background = element_rect(fill = "gray92")
        ) +
        ylim(0, 100) +
        scale_color_manual(values = c("red",
                                      "blue"))
    trend.plot <- ggplotly(trend.plot, tooltip = "text")
    return(trend.plot)
}

# Create a function to create an election map of continental US color coded by who poll percentages from 
# October 7th 2020
plot_state_results <- function(data) {
    election_map_plot <- ggplot(data,
                                aes(x = long, # Plot longitude on x axis
                                    y = lat)) + # Plot latitude on y axis
        geom_polygon( # Use geom_polygon to create the plot
            aes(
                group = group, # Group the data to appropriately create the map
                fill = winner, # Fill the plot by winner
                # Use text to create clean hoverinfo for the plotly 
                text = paste0(
                    "State: ",
                    region, # Add state
                    "\nWinner: ",
                    winner, # Add winner
                    "\nElectoral votes: ",
                    electoral_votes # Add number of electoral votes by state
                )
            ),
            alpha = 0.6,
            color = "black" # use color to outline the states
        ) +
        # Color code the state by winner and democratic party; same convention as above
        scale_fill_manual(values = c("Red",
                                     "Blue")) +
        # Create the title and add the total electoral votes in the title
        ggtitle(
            paste0(
                "Continental US Results - October 7th\n",
                "Results: Joseph R. Biden Jr. - ",
                electoral_results %>%
                    filter(winner == "Joseph R. Biden Jr.") %>%
                    select(total_votes),
                " and Donald Trump. - ",
                electoral_results %>%
                    filter(winner == "Donald Trump") %>%
                    select(total_votes)
            )
        ) +
        labs(fill = "Winner") + # Name the legend
        theme(
            title = element_text(vjust = 1), # 
            axis.title = element_blank(), # Turn off axis titles
            axis.text = element_blank(), # Turn off axis text
            axis.ticks = element_blank(), # Turn off axis ticks
            panel.grid = element_blank(), # Turn off Panel Grid
            plot.background = element_rect(fill = "gray92"),
            legend.background = element_rect(fill = "gray92")
        )
    return(ggplotly(election_map_plot, tooltip = "text"))
}

###############
## Load CSVs ##
###############

# Load wrangled data from CSVs into the app

polls_1968_2020 <- read.csv("polls_1968_2020.csv")
polls_2020 <- read.csv("polls_2020.csv")
election_map <- read.csv("election_map.csv")
electoral_results <- read.csv("electoral_votes.csv")


#########################
## Define server logic ##
#########################

# Set up the plot outputs so they can be accessible on the UI side
# The input variable pulls values from the UI side

shinyServer(function(input, output) {
    output$compPlot <- renderPlotly({
        # Return the comparison plot as compPlot
        # Set the inputs into the function and ensure the years are integers
        return(election_comp(polls_1968_2020,
                             as.integer(input$comp_year)))
    })
    output$trendPlot <- renderPlotly({
        # Return the trend plot as trendPlot
        # Set the inputs into the function and ensure the months are integers
        return(trends(
            polls_2020,
            input$state,
            as.integer(input$start),
            as.integer(input$end)
        ))
    })
    output$map <- renderPlotly({
        # Return the election map as map
        return(plot_state_results(election_map))
    })
})
