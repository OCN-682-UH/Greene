

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
library(shinythemes)


ui <- fluidPage(
  sliderInput(inputId = "num", # input name: ID name for the input
              label = "Select a value", # label: label to display for render
              value = 50, min = 0, max = 100), # argument: values for the slider
  textInput(inputId = "title", # new Id is title
            label = "Create your own title",
            value = "Histogram of Random Normal Values"), # starting title
  plotOutput("hist"), # type of output # "_" name if output object! 
  verbatimTextOutput("stats") # create a space for stats
  )

server <- function(input,output){
  data<-reactive({ 
    tibble(x = rnorm(input$num)) # 100 random normal points
  }) 
  output$hist <- renderPlot({
    ggplot(data(), aes(x = x))+ # make a histogram
      geom_histogram()+
      labs(title = input$title) #Add a new title
  })
  output$stats <- renderPrint({
    summary(data()) # calculate summary stats based on the numbers
  })
}
shinyApp(ui = ui, server = server)

deployApp()
