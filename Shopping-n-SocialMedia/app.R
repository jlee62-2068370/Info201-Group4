## INFO 201 FINAL PROJECT
# Names:
# Jordan Lee
# Kyra Diaz
# Zareen Tasnim
# Dylan Johnson

library(shiny)
library(tidyverse)
library(plotly)
library(ggplot2)
library(colourpicker)

# Let's hope that this works...

## LOADING DATA
data <- read_delim("WhatsgoodlyData-6.csv")


## DATA CLEANING

# 1. Check to see if there are NA's in the data and exclude those.

# 2. Created a new column that displays the size of the demographics.
data$`Number of Voters` <- floor(data$Count/data$Percentage)
data$`Number of Voters` <- data$`Number of Voters` %>% 
  replace(is.na(.), 0)

shopping_gender <- data %>% 
  filter(`Segment Type` == "Gender")
  
## SHINY APP

ui <- fluidPage(
  tabsetPanel(
    
    ## INTRODUCTION (TAB 1)
    tabPanel("Home"
      
    ),
    
    ## QUESTION 1 (TAB 2)
    tabPanel(
      "Most Influential Social Media Platform",
      sidebarLayout(
        sidebarPanel(
          wellPanel(
            p("You can analyze how each demograhic viewed social media advertising
            as having affected what they purchase. Each bar represents a specific
            social media platform (or NONE) and the y-axis represents the total
            number of users."),
            fluidRow(
              # Couldn't get a second button with an option to select all
              # data working, nor a way to disable all the other buttons.
              
              column(6, radioButtons("All", "Select All", choices = c("Off", "On"))),
              column(6, 
                uiOutput("uniqueDemographics")
              )
            )
          )
        ),
        mainPanel(
          wellPanel(
            plotlyOutput("barPlot")
          )
        )
      )
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
    tabPanel("Gender and Social Media Influence",
             titlePanel("Frequency of Shoppers According to Social Media"),
             p("Here you can see the frequency of users who spend by their
               gender identity. You can also choose the color of the graph!"),
             
             mainPanel(plotOutput("barplot"),
                       textOutput("sentence1")
             ),
             
             sidebarPanel(
               fluidRow(
                 column(6,
                        radioButtons("color", "Choose color:",
                                     choices = c("purple3", "pink2", "lightgreen",
                                                          "skyblue"))),
                column(6,
                        radioButtons("gender", "Choose gender:",
                                     choices = c(unique(shopping_gender$`Segment Description`), 
                                     "Both")))
               )
    ),
    
    ## CONCLUSION (TAB 5)
    tabPanel("Conclusion"
      
    )
    
  ) # end of tabsetPanel section
)
)# end of fluidPage section


server <- function(input, output) {
  
  ## INTRODUCTION
  
  ## QUESTION 1
  output$uniqueDemographics <- renderUI({
    checkboxGroupInput("specification", "Select demographics",
                       choices = sort(unique(data$`Segment Description`)))
  })
  
  # Find out what was selected on the "Select All" radio button
  # CODE GOES HERE
  
  # Proper way to implement changing demographics without errors.
  sample <- reactive({
    # Using s1 allows for us to get around an Error if no demographic
    # is selected.
    s1 <- data %>% 
      filter(`Segment Description` %in% input$specification)
  })
  
  # Bar plot filtered by demographic categories for number of pollers who view
  # a certain social media platform's ads influenced their purchases.
  # Want to plot the complete, unedited graph when nothing is selected,
  # then only data found in selected demographics if one or more are selected.
  
  
  # Do an If/Else function to find out if a "total" button is selected, and if so
  # display just the code WTIHOUT the filter. If it IS NOT selected, then use this
  # code WITH the filter.
  
  output$barPlot = renderPlotly({
    manipulated_data <- data %>% 
      select(Question, Answer, Count, `Segment Description`, `Number of Voters`) %>% 
      group_by(Answer) %>% 
      # Removed this to have the plot display all demographics. Currently
      # the "select demographics" will not work as this is commented out.
      # filter(`Segment Description` %in% input$specification) %>% 
      summarise(total_votes = sum(unique(Count))) %>% 
      arrange(rank(desc(total_votes)))
    
    plot_ly(data = sample(),
            x = manipulated_data$Answer,
            y = manipulated_data$total_votes,
            marker = list(size = 10),
            type = "bar")
  })
  ## QUESTION 2
  
  ## QUESTION 3
  output$barplot <- renderPlot({
    shopping_gender %>%
      filter(input$gender == "Both" | `Segment Description` == input$gender) %>%
      ggplot() +
      geom_bar(mapping = aes(x = Answer, y = Count), 
               stat = 'identity',
               fill = input$color) +
      labs(x = "Social Media", y = "Number of Shoppers")
  })
  
  output$sentence1 <- renderText({
    if(input$gender == "Both"){
      paste("Overall, most people either didn't shop from social media
            or used Instagram to shop!")
    }
    
    else if(input$gender == "Female voters"){
      paste("Instagram was the most influential for women!")
    }
    
    else if(input$gender == "Male voters"){
      paste("There was no social media influence on shopping
            for most men.")
    }
  })
  
  ## CONCLUSION
}


shinyApp(ui = ui, server = server)

