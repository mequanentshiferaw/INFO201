 # link 
# https://mequanent-shiferaw.shinyapps.io/mequanent/

library(shiny)
library(dplyr)
data_set_midwest <- ggplot2::midwest
data <- data_set_midwest %>% select(state,popwhite,popblack,popamerindian,popasian,
                                    popother)
race_pop <- data %>% group_by(state) %>% summarize(num_white = sum(popwhite), num_black = sum(popblack),
                       num_amerindian = sum(popamerindian), 
                         num_asian =sum(popasian), num_other = sum(popother))
data_second_bar <- data_set_midwest %>% select(state,poptotal, popadults, popdensity)
total_pop <- data_second_bar %>%  
  group_by(state) %>% summarize(Total_population = sum(poptotal), Population_density = sum(popdensity), 
                                Num_Adults = sum(popadults))

# Define UI for application that draws a histogram
shinyUI(navbarPage(
  "midwest",
  tabPanel("Race bar graph",

  # Sidebar with a slider input for number of bins 
  pageWithSidebar(
    headerPanel("Midwest demographics"),
    sidebarPanel(
      selectInput("state", "State", race_pop$state[2:5]),
     
   selectInput(
        "color",
    label = "Color", 
    choices = list("Red" = "red", "Blue" = "blue", "Green" = "green")
     )),
    mainPanel(
      plotOutput("plot456")
    )
  )
  ),

  tabPanel("Midwest demographics of poplution",
  pageWithSidebar(
    headerPanel("Midwest demographics of population "),
    sidebarPanel(
      selectInput("name_state", "name_state", total_pop$state[2:5]),

      selectInput(
        "colors",
        label = "Colors", 
        choices = list("Red" = "red", "Blue" = "blue", "Green" = "green")
       
      )),
    mainPanel(
      plotOutput("plot")
)
)
)
)
)

