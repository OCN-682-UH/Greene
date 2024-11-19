
# Libraries
library(shiny)
library(tidyverse)
library(here)
library(bslib)
library(ggExtra)
library(rsconnect)

# Data upload

df <- read_csv(here("Data", "mean_data.csv")) # read in data

clean_df <- df %>% # clean data  
  select(Week, Treatment, Population, mean_chl) # select focal columns  
 

# Data

ui <- page_sidebar( # page layout
  sidebar = sidebar( # bar layout
    selectInput( # input object  
      inputId = "select_population", # input id name  
      label = "Select Population", # label name  
      choices = unique(clean_df$Population) # options  
    )
  ),
  mainPanel(
  plotOutput("scatter") # type of plot  
)
)

server <- function(input, output, session) { # server object  
  subsetted <- reactive({ # make it reactive  
    clean_df |> filter(Population == input$select_population) # population data  
  })
  
  output$scatter <- renderPlot({ # make plot  
    plot <- ggplot(subsetted(), aes(x = Week, # x axis  
                            y = mean_chl, # y axis  
                            color = Treatment)) + # treatment will be differentiated by colors
      geom_point() + # plot data points  
      geom_line() + # connect points to see trends 
      labs(title = paste("Chlorophyll Content of ʻAʻaliʻi from", input$select_population), # plot title  
           x = "Time (Week)", # x axis title  
           y = "Mean Chlorophyll Content", # y axis title  
           color = "Treatment") + # legend title  
      theme(legend.position = "bottom") # legend position  
    
    plot + scale_color_manual(values = c("lightblue", "brown")) # manually set treatment colors  
  })
}

shinyApp(ui = ui, server = server) # shiny app 

# publish shiny app!
