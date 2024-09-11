### Week_03_script: Plotting practice with GGplot2! ###
### Created by: Kauanoe Greene ###
### Updated on: 2024-09-10 ###

### PACKAGES ###
install.packages("palmerpenguins")

### LIBRARIES ###
library(palmerpenguins)
library(tidyverse)
glimpse(penguins)

### DATA ###

# ggplot = graphing!
# data = data sheet you will use
# aes = aesthetics
# labs = labels
# use "+" to link functions
# use "," to add to attributes within functions

ggplot(data=penguins, mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm, 
                     color = species, 
                     shape = species, 
                     size = body_mass_g, 
                     alpha = flipper_length_mm)) + 
  geom_point() + 
  labs(title = "Bill depth and length", 
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins", 
       x = "Bill depth (mm)", y = "Bill length (mm)", 
       color = "Species", ## name of data associated with color legend
       shape = "Species", ## name of data associated with shape legend
       size = "Body Mass (g)", ## name of data associated with size legend
       alpha = "Flipper length (mm)", ## name of data associated with alpha legend
       caption = "Source: Palmer Station LTER/ palmerpenguins package") + 
  scale_color_viridis_d() + 
  facet_grid(species~sex) + 
  guides(color = FALSE)

# faceting! cannot determine grid

ggplot(penguins, 
       aes(x = bill_depth_mm, 
           y = bill_length_mm)) + 
  geom_point() + 
  facet_grid(species~sex)

# faceting! wraps like a ribbon allowing me to determine grid values? 
ggplot(penguins, 
       aes(x = bill_depth_mm, 
           y = bill_length_mm)) + 
  geom_point() + 
  facet_wrap(~ species, ncol=2) # ncol = number of columns

