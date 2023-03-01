## INFO 201 FINAL PROJECT
# Names:
# Jordan Lee
# Kyra Diaz
# Zareen Tasnim
# Dylan Johnson

library(shiny)
library(tidyverse)

data <- read_delim("WhatsgoodlyData-6.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
}

# Run the application 
shinyApp(ui = ui, server = server)
