
### INTRO ###
### homework 04B using chemistry data from flipped lecture ###
### Created by: Kauanoe Greene ###
### Updated on: 2024-09-24 ###


### LIBRARIES ###
library(tidyverse)
library(here)
library(ggthemes)
library(wsjplot)


### DATA UPLOAD ###
ChemData<-read_csv(here("Week_04", "Data", "chemicaldata_maunalua.csv"))
view(ChemData)
glimpse(ChemData)

### DATA ANALYSES ###
# tasks:
  # remove all NAs
  # separate Tide_time
  # filter out a subset of data (any of your choice)
  # pivot_longer or pivot_wider
  # summary statistics (any of your choice) export to CSV
  # make a plot


# CHONK OF DATA FOR SUMMARY STATISTICS

ChemData_hw<-ChemData %>% 
  drop_na() %>% #filters out everything that is not a complete row.
  separate(col = Tide_time, #choose the tide and time column. 
           into = c("Tide","Time"), # separate it into 2 columns Tide and Time.
           sep = "_", # separate by "_" 
           remove = FALSE) %>% #keep the "tide_time" column
  select(c(Site, Season, Tide, Time, Salinity, pH)) %>% # select variables of interest
  pivot_longer(cols = Salinity:pH, # the cols you want to pivot. 
               names_to = "Salinity", # the names of the new cols.
               values_to = "Values") %>% # the names of the new cols with all the values.
  filter(Salinity == "Salinity") %>% # filter to remove pH values
  group_by(Site, Tide, Season, Time) %>% # select groups for summary stats
  summarise(mean_sal = mean(Values), # get mean salinity
            var_sal = var(Values)) %>% # get variance salinity
  write_csv(here("Week_04", "Output", "Week_04B_hw.summary.csv")) #export as CSV.


# MAKE A PLOT

ggplot(ChemData_hw, 
         mapping = aes(x = Site, # my x value
                       y = mean_sal, # my y value
                       color = Time, # how I want to designate colors
                       fill = Time)) + # I want the violins to be filled with color
    scale_color_viridis_d() + # make it color blind friendly
    geom_violin() + labs(x = "Sample Site", # name of my x axis
                         y = "Mean Salinity", # name of my y axis
                         title = "Temporal Variability in Salinity Measures Across Maunalua Bay", # plot title
                         subtitle = "Comparing salinity measures collected at BP and W over time", # plot subtitle
                         caption = "Source: Dr. Nyssa Silber's Maunalua Bay Chemical Dataset") +  # plot caption/source
    theme_wsj() + # make it look anything by plain lol
    theme(axis.title = element_text(size = 20)) # adjust the font size


# SAVE YOUR PLOT!
ggsave(here("Week_04", "Output", "Week_04B_plot.png")) # save my plot into my output folder!


# adjust the width and height of the plot (dimension of plot in inches)
ggsave(here("Week_04", "Output", "Week_04B_plot.png"), 
       width = 18, height = 10)


# SAVE PLOT AS AN OBJECT (assign it a name)
Week_04B_plot<-ggplot(ChemData_hw, 
                       mapping = aes(x = Site, # my x value
                                     y = mean_sal, # my y value
                                     color = Time, # how I want to designate colors
                                     fill = Time)) + # I want the violins to be filled with color
  scale_color_viridis_d() + # make it color blind friendly
  geom_violin() + labs(x = "Sample Site", # name of my x axis
                       y = "Mean Salinity", # name of my y axis
                       title = "Temporal Variability in Salinity Measures Across Maunalua Bay", # plot title
                       subtitle = "Comparing salinity measures collected at BP and W over time", # plot subtitle
                       caption = "Source: Dr. Nyssa Silber's Maunalua Bay Chemical Dataset") +  # plot caption/source
  theme_wsj() + # make it look anything by plain lol
  theme(axis.title = element_text(size = 20)) # adjust the font size
        

# view your plot
Week_04B_plot
