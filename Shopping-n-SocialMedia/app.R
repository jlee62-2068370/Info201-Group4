## INFO 201 FINAL PROJECT
# Names:
# Jordan Lee
# Kyra Diaz
# Zareen Tasnim
# Dylan Johnson

library(shiny)
library(tidyverse)

## LOADING DATA
data <- read_delim("WhatsgoodlyData-6.csv")


## DATA CLEANING

# 1. Check to see if there are NA's in the data and exclude those.

# 2. Do any other filtering if needed!


## SHINY APP

ui <- fluidPage(
  tabsetPanel(
    
    ## INTRODUCTION (TAB 1)
    tabPanel("Home"
      
    ),
    
    ## QUESTION 1 (TAB 2)
    tabPanel("Most Influential Social Media Platform"
      
    ),
    
    ## QUESTION 2 (TAB 3)
    tabPanel("General Trends",
             titlePanel("How does social media influence over shopping behavior
                        differ across various demographics?"),
             p("This research question aims to find any significant trends across
               three different demographical categories:",
                strong("Race, Socioeconomic Status, and Marital Status."),
               "Below is a bar graph comparing different segments of people
               and the frequency at which they shop on social media platforms.
               You may use the widgets to compare specific groups."
               )
      
    ),
    
    ## QUESTION 3 (TAB 4)
    tabPanel("Gender and Social Media Influence"
      
    ),
    
    ## CONCLUSION (TAB 5)
    tabPanel("Conclusion"
      
    )
    
  ) # end of tabsetPanel section
) # end of fluidPage section


server <- function(input, output) {
  
  ## INTRODUCTION
  
  ## QUESTION 1
  
  ## QUESTION 2
  
  ## QUESTION 3
  
  ## CONCLUSION
}


shinyApp(ui = ui, server = server)

