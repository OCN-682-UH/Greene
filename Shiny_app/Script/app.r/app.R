

### Shiny app Introduction
### Created on: 2024-11-18



# Packages
# install.packages('rsconnect')

# Libraries
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(rsconnect)
library(tidyverse)

ui<-fluidPage(
  sliderInput(inputId = "num", # ID name for the input
              label = "Choose a number", # Label above the input
              value = 25, min = 1, max = 100 # values for the slider
  ),
  plotOutput("hist") #creates space for a plot called hist  
)
server<-function(input,output){
  output$hist <- renderPlot({
    # {} allows us to put all our R code in one nice chunck
    data<-tibble(x = rnorm(100)) # 100 random normal points
    ggplot(data, aes(x = x))+ # make a histogram
      geom_histogram()
  })
}

shinyApp(ui, server)

deployApp()

