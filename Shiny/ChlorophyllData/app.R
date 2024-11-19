
# Libraries
library(shiny)
library(tidyverse)
library(here)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggExtra)

# Data upload

df <- read_csv(here("Shiny", "Data", "mean_data.csv"))
glimpse(df)

clean_df <- df %>% 
  select(Week, Treatment, Population, mean_chl)
glimpse(clean_df)

# Data

ui <- page_sidebar(
  sidebar = sidebar(
    selectInput(
      inputId = "select_population", 
      label = "Select Population", 
      choices = unique(clean_df$Population)
    )
  ),
  mainPanel(
  plotOutput("scatter")
)
)

server <- function(input, output, session) {
  subsetted <- reactive({
    clean_df |> filter(Population == input$select_population)
  })
  
  output$scatter <- renderPlot({
    plot <- ggplot(subsetted(), aes(x = Week, 
                            y = mean_chl, 
                            color = Treatment)) + 
      geom_point() + 
      geom_line() + 
      labs(title = paste("Chlorophyll Content at", input$select_population), 
           x = "Time (Week)", 
           y = "Mean Chlorophyll Content", 
           color = "Treatment") + 
      theme(legend.position = "bottom")
    
    plot + scale_color_manual(values = c("lightblue", "brown"))
  })
}

shinyApp(ui = ui, server = server)
