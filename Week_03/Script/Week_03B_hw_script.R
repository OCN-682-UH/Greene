
### INTRODUCTION ###

### Homework Week_03: Build a plot! ###
### Created by: Kauanoe Greene ###
### Updated on: 2024-09-16 ###


### LIBRARIES ###
library(palmerpenguins)
library(tidyverse)
library(here)
library(beyonce)
library(ggthemes)
library(wsjplot)


### DATA UPLOAD ###
# I am choosing to build a box plot using the Palmer Penguins dataset!
# I will compare flipper length (mm) across species

# let's get quick view of the dataset.
glimpse(penguins)


### DATA ANALYSIS ###

# now, let's plot!
ggplot(data = penguins, mapping = aes( x = species, 
                                       y = flipper_length_mm, 
                                       color = species, 
                                       fill = species)) + geom_boxplot() + 
  labs(x = "Species", 
       y = "Flipper Length (mm)", 
       title = "Variability in Flipper Length Across Species", 
       caption = "Source: Palmer Station LTER/ palmerpenguins package") + 
  scale_color_manual(values = beyonce_palette(10)) + 
  theme_wsj() + 
  theme(axis.title = element_text(size = 10, 
                                  color = "black")) + 
  theme(title = element_text(size = 30))


# now that we assigned this plot a name, let's save it!
ggsave(here("Week_03", "Output", "Week_03B_hw.png"))


# adjust the width and height of the plot (dimension of plot in inches)
ggsave(here("Week_03", "Output", "Week_03B_hw.png"), 
       width = 7, height = 5)

# note: you can also save it as a pdf if you replace ".png"


# now that we are happy with what we have built...
# we shall assign a name to this plot to "plot3B"

plot3B<- ggplot(data = penguins, mapping = aes( x = species, 
                                                y = flipper_length_mm, 
                                                color = species, 
                                                fill = species)) + geom_boxplot() + 
  labs(x = "Species", 
       y = "Flipper Length (mm)", 
       title = "Variability in Flipper Length Across Species", 
       caption = "Source: Palmer Station LTER/ palmerpenguins package") + 
  scale_color_manual(values = beyonce_palette(10)) + 
  theme_wsj() + 
  theme(axis.title = element_text(size = 10, 
                                  color = "black")) + 
  theme(title = element_text(size = 30)) 

plot3B
