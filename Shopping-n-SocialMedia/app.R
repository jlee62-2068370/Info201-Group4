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

## LOADING DATA
data <- read_delim("WhatsgoodlyData-6.csv")


## DATA CLEANING
3
# 1. Created a new column that displays the size of the demographics.
data$`Number of Voters` <- floor(data$Count/data$Percentage)
data$`Number of Voters` <- data$`Number of Voters` %>% 
  replace(is.na(.), 0)

## QUESTION 2 - AVERAGE GENDER DATA CLEANING 

# Gender Subset
shopping_gender <- data %>% 
  filter(`Segment Type` == "Gender")

# Calculate average amount of people who shopped from social media apps
social <- data %>% 
  filter(`Segment Type` == "Gender") %>% 
  group_by(`Segment Description`) %>%
  filter(!Answer == "None") %>% 
  summarize(avg = mean(Count))

# Calculate average amount of people who DIDN'T shop from social media apps
none <- data %>% 
  filter(`Segment Type` == "Gender") %>% 
  group_by(`Segment Description`) %>% 
  filter(Answer == "None") %>% 
  summarize(avg = mean(Count))

# Create a new column that ranks whether the average is "social" or "none"
social <- cbind(social, status = c("social", "social"))
none <- cbind(none, status = c("none", "none"))

# Create new dataframe that shows averages
new_gender <- rbind(social, none)
  

# Create options for race inputs
race_selection <- data %>%
  select(`Segment Description`) %>%
  filter(`Segment Description` %in% c("closely identify as? Asian", "closely identify as? Black",
      "closely identify as? White","closely identify as? Native American","closely identify as? Other",
      "closely identify as? Hispanic")) %>%
  unique() %>%
  reframe(race = `Segment Description`)

## SHINY APP

ui <- fluidPage(
  titlePanel("Social Media Influences on Shopping Analysis"),
  tabsetPanel(
    
    ## INTRODUCTION (TAB 1)
    tabPanel("Introduction",
             h3(strong("Creators:"),"Jordan Lee, Kyra Gaile Diaz, Zareen Tasnim, Dylan Johnson"),
             h3(strong("Project Overview")),
             imageOutput("renderedImage"),
             p("This report provides a broad overview of how online advertising on social media affects consumers of varying gender, age, status, and more."),
             h3(strong("Audience")),
             p("Our target audience are mainly young people (particularly from Gen-Z and Millennial generations) who use social media platforms daily. 
             The focus on these two generations is due to the fact that the dataset excludes other generations, so we cannot report trends for them (e.g. Baby Boomers, Gen x, etc.). 
             These age groups also seem to be more relevant in terms of social media influence on shopping behavior due to their increased exposure to
             apps such as Instagram, Facebook, Twitter, and Snapchat. This may be valuable to them by creating more awareness on how these platforms affect the way they spend."),
             h3(strong("Data Set")),
             p("The dataset we’re working with is ",
               tags$a(href="https://www.kaggle.com/datasets/thedevastator/uncovering-millennials-shopping-habits-and-socia",
                      "'Social Influence on Shopping'"),
               "by Adam Halper on Kaggle. The dataset specifically surveyed both 
               Millennials and Gen-Z and asked what social media app seemed to influence their shopping habits the most. The dataset also provides information 
               on individual demographics, such as gender, educational status, university attending, etc., allowing us to look into the trends of different subgroups.
               Using 50,770 self-reported responses from users of Whatsgoodly, this app hopes to find trends between which social media platforms more effectively
               advertise to younger generations and which ones fail to effectively entice their users."),
             h3(strong("Questions")),
             h5("Which social media platform has the most influence on shopping?"),
             h5("Is there a particular demographic that values or uses certain types of social influence on their shopping habits more than others?"),
             h5("Which gender is more susceptible to social media influencing?")
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
          ),
          wellPanel(
            p("Common trends can be noticed across many demographics is that
              many Millennial and Generation Z members feel that either
              social media does not have any impact on their purchases or
              Instagram has the largest impact of all the platforms, with very
              few consistently selecting Snapchat as a source of advertisments
              working. ", em("Meta Platforms Inc."), " has done the best job of
              any major tech company in getting young users to purchase
              advertisments through both of their major social media platforms,",
              strong("Facebook"), " and ", strong("Instagram."))
          ),
          p("These numbers were calculated through separating every calculated
            vote by a specified demographic question. Although not specified
            within the dataset itself, we can safely assume that overlap does
            exist for many demographics and that one voter may have replied to
            multiple demographic questions. Strangely, however, it can be 
            observed that certain categories such as your perceived race had
            many more responses than relationship status, both fields that 
            should theoretically cover all of the participants.")
        )
      )
    ),
    
    ## QUESTION 2 (TAB 3)
    tabPanel("General Trends",
       sidebarLayout(
         mainPanel(
           plotOutput("plot"),
           h1("Observations"),
           p("When looking at the data for social media influence in race demographics,
        White people have far more recorded data compared to the rest of the other
        races oberserved. Additionally, the counts for each social media, 
        including none, White also lead in the respective oberservations."),
           p("Another general trend depicted from the plot, is that the Native American
        race has not only the least recorded data in general, but the lowest count
        for each of the social media influences"),
           h2("General Trends Across Each Race:"),
           tags$ul(
             tags$li(strong("Asian: "), "the data recorded for Asian people show the highest
           count for social media, actually being none. However, if we do not 
           consider the 'none' option, Instagram comes in first for the highest count,
           Facebook as second, and lastly Snapchat/Twitter in third and fourth respectively"),
             tags$li(strong("Other: "), "the data recorded for Other people show a tie between
           Instagram and 'none' for the highest count. Facebook follows for second, then afterwards
           Snapchat and Twitter come third and fourth respectively."),
             tags$li(strong("Native American: "), "the data recorded for Native American people
           illustrate the highest count for social media influence in Instagram. 
           'None' follows this data as the second highest, with Twitter and Facebook
           tied for the third highest. Surprisingly, there is no count for Snapchat
           when considering the data for Native American people."),
             tags$li(strong("Black: "), "the data recorded for Black people observe that Instagram
           have the highest influence on the Black community's shopping habits. The
           next heights is 'none', with Facebook falling in third, Twitter in fourth, 
           and Snapchat coming in last."),
             tags$li(strong("Hispanic: "), "the data trends from observing Hispanic people
                show the heighest social media influence on shopping habit as 'none'. 
                However, if we look at this data and do not consider the 'none' section,
                Instagram comes in first highest across the social media platforms, Facebook
                in second, Twitter in third, and Snapchat as the last."),
             tags$li(strong("White: "), "the data trends from observing white people
                show the heighest social media influence on shopping habit as 'none'. 
                However, if we look at this data and do not consider the 'none' section,
                Instagram comes in first highest across the social media platforms, Facebook
                in second, Twitter in third, and Snapchat as the last.")
           ),
           h2("Overall Trends:"),
           tags$blockquote("\"Is there a particular race demographic that values or uses certain types of social
         influence on their shopping habits more than others?\""),
           p("After considering the data from each subset of races there are some clear
        indications for trends:"),
           tags$ul(
             tags$li("Across each race, the highest social media influence on shopping
                habits was either 'none' or Instagram. From this oberservation alone it 
                can be correct to assume that Instagram or using no social media 
                influences shopping habits the most in each race demographic"),
             tags$li("Snapchat has overall the least influence on shopping habits as it came in
                the lowest for each race demographic"),
             tags$li("Facebook consistently came in as the third result for social media
                influence on shopping habits")
           )
         ),
         sidebarPanel(
           fluidRow(
             selectInput("Race1", "Race Choice 1", str_sub(race_selection$race, 22)),
             selectInput("Race2", "Race Choice 2", c("None", str_sub(race_selection$race, 22)))
           ),
           fluidRow(
             colourInput("color1", "Color of Race Choice 1", 
                         value = "#7FFFD4", palette = "limited"),
             colourInput("color2", "Color of Race Choice 2", 
                         value = "#E066FF", palette = "limited"),
             textOutput("plotText")
           )
         )
       )
    ),
    
    ## QUESTION 3 (TAB 4)
    tabPanel("Gender and Social Media Shopping",
             
             titlePanel("Gender and Social Media Shopping"),
             
             h3("Research Question"),
             p("Here we aim to answer the question,", 
               strong("which gender is more susceptible to 
                  social media influencing on shopping behavior?")),
             
             mainPanel(
               h3("Individual Trends of Gender and Social Media Shopping"),
               plotOutput("barplot"), # interactive plot to see general trends
               textOutput("sentence1"), # short summary statements of key features
               
               h3("Who's More Suseceptible?"),
               plotOutput("average"), # summary plot that answers research question
               p("On average, both genders seem to be equally influenced
                      by social media apps--specifically Instagram across both
                      groups. However, it seems that a higher proportion of men
                      on average do not shop on social media apps. This observation
                      may be due to a higher sample size of male responses (n = 1562)
                      than female responses (n = 1114). Thus, we cannot conclusively
                      determine if women or men are more susceptible to social media
                      influence on shopping.")
             ),
             
             sidebarPanel(
               fluidRow(
                 p("This plot shows the different frequencies at which people 
                       shopped on social media apps according to gender. You can 
                       manipulate which group (male/female) you're looking at, 
                       and can also compare individual frequencies with the 'Both'
                       option."),
                 column(6,
                        radioButtons("gender", "Choose gender:",
                                     choices = c(unique(shopping_gender$`Segment Description`), 
                                                 "Both"))),
               )
             )
    ),
    
    ## CONCLUSION (TAB 5)
    tabPanel(
      "Conclusion",
      ## Takeaways
                 h2("Takeaways"),
                 h3("Takeaway 1: Men and women both self-reported similar influences on purchases through social media advertisements."),
             p("The gender gap between male and female respondents in our survey has made it challenging to identify trends in terms of the influence advertisements have. 
               With 1562 male respondents and 1114 female respondents, it is hard to create a conclusion on the different genders' perspectives. It was interesting to see that a larger proportion 
               of male respondents felt that no social media platform influenced their purchasing decisions compared to female respondents. On the other hand, both genders had similar 
               averages in social media influence, with an average score of around 211 for females and 221 for males. However, there was a significant average difference between males and females
               regarding their feelings about the impact of social media on their purchases. While 678 men reported feeling that social media did not influence their purchases, only 271 women 
               reported the same feeling. These gender-specific differences in responses highlight the importance of obtaining more gender-balanced survey samples to achieve reliable and accurate results."),
             
      ## Plot
      wellPanel(
        plotOutput("average2")
      ),
      
             h3("Takeaway 2: Meta Platforms Inc. is the most successful major tech company in successfully targeting ads to younger generations."),
             p("One other takeaway from our research is that social media apps 
               owned by ", em("Meta Platforms Inc."), " (Instagram and Facebook) had the most 
               success in targeting ads to younger generations. 
               This finding goes against the perceived notion that Meta does not 
               effectively advertise to younger audiences, due to their predominantly 
               older user base. For example, ", 
               tags$a(href="https://www.statista.com/statistics/376128/facebook-global-user-age-distribution/",
                      "only 26.3% of their users"),
               " are within the Gen-Z and Millennial age range (ages 13-24) on
               Facebook (a Meta-owned platform), whereas ", 
               tags$a(href="https://www.statista.com/statistics/933948/snapchat-global-user-age-distribution/",
                      "60.1% of Snapchat’s user base"), " are 
               between the ages of 13-24 worldwide. Despite these demographics, our data 
               shows that Meta’s advertising seems to effectively target younger 
               audiences, especially through Instagram. Instagram is the clear winner in effective
               younger generation advertising in almost every demographic and has the most
               responces claiming that pollers purchased a product after seeing an ad on
               Instagram."
             ),
  
      ## Limitations
             h2("Limitations"),
             p("One of the main drawbacks of our dataset was that even though it focused on Millenials and Gen-Z, we could not differentiate their responses.
               The absence of this difference means that we were unable to determine the attitudes or opinions of either generation individually. Another significant factor
               affecting the dataset's reliability was the unequal gender representation. The dataset had more male respondents than female, which could have biased our results.
               And our final limitation was that the data was scattered in terms of categories such as race, sexual orientation, student status, and more. These variables were grouped 
               under the same category,  making it challenging to identify specific groups of interest due to the unorganized structure of the dataset."),
             p("The broader implications of this insight are that researchers and analysts need to be mindful of the limitations
               of the dataset and carefully consider the representativeness of the data when drawing conclusions. 
               This also highlights the need for larger and more diverse datasets to ensure that our analyses accurately reflect the broader population."),
             ## Data Quality
             h2("Data Quality"),
             p("The quality of the dataset was not as reasonable as desired, and the information was scattered and varied. 
               As a result, it's unclear whether the dataset gives unbiased results or could potentially harm certain population groups. For example,
               categories such as racial identity, sexual orientation, student status (undergraduate/graduate), and more fell under the same Segment 
               Type of 'Custom'. This made it difficult to parse out specific groups of interests due to the unorganized structure of the dataset."),
             ## Future Ideas
             h2("Future Ideas"),
             p("Future ideas that we have for advancing the project include looking into how answers may vary region to region. This can reveal other
             patterns that we may have not seen previously. Organizing the trends by location can show new patterns."),
      
    )
  ) # end of tabsetPanel section

) # end of fluidPage section


server <- function(input, output, session) {
  
  # Rendering the Intro image
  output$renderedImage <- renderImage({
    list(src = "shopping-2.png",
         width = "50%", height = "100%")
  }, deleteFile = FALSE)
  
  ## INTRODUCTION
  
  ## QUESTION 1
  output$uniqueDemographics <- renderUI({
    checkboxGroupInput("specification", "Select demographics",
                       choices = sort(unique(data$`Segment Description`)))
  })
  
  # Function that powers the "Select All" radio button
  observe({
    updateCheckboxGroupInput(
      session, "specification", "Select demographics",
      choices = sort(unique(data$`Segment Description`)),
      selected = if(input$All == "On") sort(unique(data$`Segment Description`))
    )
  })
  
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
      filter(`Segment Description` %in% input$specification) %>% 
      
      summarise(total_votes = sum(unique(Count))) %>% 
      arrange(rank(desc(total_votes)))
    
    plot_ly(data = sample(),
            x = manipulated_data$Answer,
            y = manipulated_data$total_votes,
            marker = list(size = 10),
            type = "bar")
  })
  ## QUESTION 2
  output$plot <- renderPlot({
    input1 <- paste("closely identify as?", input$Race1, sep = " ")
    input2 <- paste("closely identify as?", input$Race2, sep = " ")
    plot_data <- data %>%
      select(`Segment Description`, Answer, Count ) %>%
      filter(`Segment Description` %in% c(input1, input2)) %>%
      group_by(`Segment Description`)
    
    ggplot(plot_data, aes(Answer, Count, fill = factor(str_sub(`Segment Description`, 22)))) +
      geom_col(position = "dodge") +
      labs(title = "Number of People's Shopping Habits Influenced by Social Media Platforms",
           x = "Social Media Platform",
           y = "Count",
           fill = "Race(s)") +
      scale_fill_manual(values = c(input$color1, input$color2))
  })
  
  ## QUESTION 3
  output$barplot <- renderPlot({
    if(input$gender == "Both"){
      data %>% 
        filter(`Segment Type` == "Gender") %>% 
        group_by(`Segment Description`) %>% 
        ggplot(., mapping = aes(x = Answer, y = Count, 
                                fill = factor(shopping_gender$`Segment Description`))) +
        geom_bar(stat = 'identity', position = "dodge2") +
        labs(x = "Social Media", y = "Number of Shoppers", fill = "Gender")
    }
    else {
      data %>% 
        filter(`Segment Type` == "Gender") %>%
        filter(`Segment Description` == input$gender) %>%
        ggplot() +
        geom_bar(mapping = aes(x = Answer, y = Count), 
                 stat = 'identity',
                 fill = "orangered") +
        labs(x = "Social Media", y = "Number of Shoppers")
    }
  })
  
  # A reactive sentence that quickly summarizes the trends according to gender
  output$sentence1 <- renderText(
    if(input$gender == "Both"){
      paste("While social media influence on shopping differed across
            different platforms, it seems that both Snapchat and Twitter
            had the least influence on both genders.")
    }
    
    else if(input$gender == "Female voters"){
      paste("Instagram seemed to be the most influential social media app
            for female shoppers.")
    }
    
    else if(input$gender == "Male voters"){
      paste("While most men surveyed didn't shop from social media apps,
            Instagram seemed to be the most influential social media app.")
    }
  )
  
  # Average table that answers research question
  output$average <- renderPlot(
    ggplot(new_gender, mapping = aes(x = `Segment Description`, y = avg, 
                                     fill = factor(status))) +
      geom_bar(stat = 'identity', position = "dodge2") +
      labs(x = "Gender", y = "Average Influence", fill = "Social Media vs None")
  )
  
  # Second plot to allow for the Conclusion plot render
  output$average2 <- renderPlot(
    ggplot(new_gender, mapping = aes(x = `Segment Description`, y = avg, 
                                     fill = factor(status))) +
      geom_bar(stat = 'identity', position = "dodge2") +
      labs(x = "Gender", y = "Average Influence", fill = "Social Media vs None")
  )
  
  ## CONCLUSION
  female_voters <- subset(data, `Segment Description` == "Female voters")
  output$table <- renderTable({
    data.frame(female_voters)
  })

  uw_students <- subset(data, `Segment Description` == "University of Washington")
  output$table2 <- renderTable({
    data.frame(uw_students)
  })
}


shinyApp(ui = ui, server = server)

