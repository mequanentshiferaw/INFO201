#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)
library(colourpicker)
data_set_midwest <- ggplot2::midwest 
data <- data_set_midwest %>% select(state,popwhite,popblack,popamerindian,popasian,
                                                                   popother)
data_second_bar <- data_set_midwest %>% select(state,poptotal, popadults, popdensity)

server <- function(input, output) {
   

  output$plot456 <- renderPlot({
  race_pop <- data %>% group_by(state) %>% summarize(num_white = sum(popwhite), num_black = sum(popblack),
                  num_amerindian = sum(popamerindian), num_asian =sum(popasian), num_other = sum(popother))
  race_pop <- race_pop %>% filter(state == input$state)
  dat <- data.frame(
    time = factor(names(race_pop[2:5])),
    total_bill = c(race_pop$num_white, race_pop$num_black, race_pop$num_amerindian, race_pop$num_asian)
  )
   ggplot(data=dat, aes(x=time, y=total_bill)) +
    geom_bar(stat="identity",fill= input$color, colour= input$color) +  
     labs(x = "Race", y ="Number of people" , title = "Race demographics by state") 
   
     
  

  
  })
  
  
  # sum of population of people, density, and adult
   output$plot <- renderPlot({
     
  total_pop <- data_second_bar %>% 
    group_by(state) %>% summarize(Total_population = sum(poptotal), Population_density = sum(popdensity), 
                              Num_Adults = sum(popadults))
  total_pop <- total_pop %>% filter(state == input$name_state)
  data_total_pop <- data.frame(
    time_h = factor(names(total_pop[2:4])),
    total_u = c(total_pop$Total_population, total_pop$Population_density,total_pop$Num_Adults)
  )
  ggplot(data=data_total_pop, aes(x=time_h, y=total_u)) +
    geom_bar(stat="identity",fill= input$colors, colour= input$colors) +  
    labs(x = "Name of Population", y ="Number of Population" , title = " demographics by state of Total_population, Density and adult") 
   }) 
}


