### we be plotting penguin data! ###
### Created by: Kauanoe Greene ###
### Updated on: 2024-09-16 ###

### PACKAGES ###
install.packages(praise)
install.packages(devtools)

### LIBRARIES ###
library(palmerpenguins)
library(tidyverse)
library(here)
library(beyonce)
library(ggthemes)
library(wsjplot)

### Data ###
# this data is from a package called penguins
# let's use "glimpse" to view the data. What other ways can we view out data?

glimpse(penguins)

### MAKE A PLOT ###

ggplot(data=penguins,
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm)) + 
  geom_point()+labs(x = "Bill depth (mm)", 
                    y = "Bill length (mm)")

# add a line of regression
# to smooth out the line, we used "method = "lm"" in the geom_smooth func!

ggplot(data=penguins,
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Bill depth (mm)", 
                    y = "Bill length (mm)")

# we added "group = species" to group by species

ggplot(data=penguins,
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm, 
                     group = species)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)")

# we added "color = species" to color each group of species

ggplot(data=penguins,
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm, 
                     group = species, 
                     color = species)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)")

# change the color scale to virdis

ggplot(data=penguins,
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm, 
                     group = species, 
                     color = species)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") + 
  scale_color_viridis_d()

# Scales
# naming scheme for scale has 3 parts:
# the name of the primary aesthetic (color, shape, or x)
# the name of the scale (continuous, discrete, manual)

#Change scales (x and y scale limit)

ggplot(data=penguins,
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm, 
                     group = species, 
                     color = species)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") + 
  scale_color_viridis_d() + 
  scale_x_continuous(limits = c(0,20)) + 
  scale_y_continuous(limit = c(0,50))

# i want to add "breaks" to my x scale limit

ggplot(data=penguins,
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm, 
                     group = species, 
                     color = species)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") + 
  scale_color_viridis_d() + 
  scale_x_continuous(breaks = c(0,14,17,21)) 
 
# change x break labels

ggplot(data=penguins,
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm, 
                     group = species, 
                     color = species)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") + 
  scale_color_viridis_d() + 
  scale_x_continuous(breaks = c(14,17,21), 
                     labels = c("low", "medium", "high"))

# manually change color scale

ggplot(data=penguins,
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm, 
                     group = species, 
                     color = species)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") + 
  scale_color_manual(values = c("orange", "purple", "green"))

# Custom color palettes!
# most popular color palettes - Beyonce lol... which i just installed hehe.
# function is called "beyonce_palette_"

# let's try to use the beyonce palette haha.
ggplot(data=penguins,
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm, 
                     group = species, 
                     color = species)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") + 
  scale_color_manual(values = beyonce_palette(2))

# Coordinates!
# default coordinates for ggplot is "cartesian"
# cartesian --> where 2D position of an element is given by the x and y position in aes().
# let's manipulate coordinates!
# coord_flip(): cartesian coordinate system with x and y axes flip
# coord_fixed(): cartesian coordinate sys with a fixed aspect ratio
# coord_trans(): apply arbitrary transformations to x and y positions, after the data has been processed by the stat
# coord_polar(): polar coordinates
# coord_map()/coord_sf(): map projections


# 1
#let's FLIP the axes so x and y switch axes

ggplot(data=penguins,
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm, 
                     group = species, 
                     color = species)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") + 
  scale_color_manual(values = beyonce_palette(2)) + 
  coord_flip()


# 2
#let's FIX the axes, it is compressed

ggplot(data=penguins,
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm, 
                     group = species, 
                     color = species)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") + 
  scale_color_manual(values = beyonce_palette(2)) + 
  coord_fixed()


# 3
# let's TRANSFORM the axes (log10)
# use diamond data set

glimpse(diamonds)

ggplot(data=diamonds,
       mapping = aes(carat, price)) + 
  geom_point() + 
  coord_trans(x = "log10", y = "log10")


# 4
# let's make them polar

ggplot(data=penguins,
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm, 
                     group = species, 
                     color = species)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") + 
  scale_color_manual(values = beyonce_palette(2)) + 
  coord_polar("x")


# Themes!
# the visual of the plots. 4 components of theme system:
# elements: non-data element we can control
# element functions: describes the visual properties
# "theme()" override the default theme
# complete themes --> sets all the elements that work nicely together
# complete themes: "theme_bw()" "theme_minimal()" "theme_classic()" ...


# 1
# make it theme_classic()

ggplot(data=penguins,
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm, 
                     group = species, 
                     color = species)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") + 
  scale_color_manual(values = beyonce_palette(2)) + 
  theme_classic()

# 2
# make it theme_bw()

ggplot(data=penguins,
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm, 
                     group = species, 
                     color = species)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") + 
  scale_color_manual(values = beyonce_palette(2)) + 
  theme_bw()

# 3
# wall street journal theme ^-^

ggplot(data=penguins,
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm, 
                     group = species, 
                     color = species)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") + 
  scale_color_viridis_d() + 
  scale_color_manual(values = beyonce_palette(20)) + 
  theme_wsj()

# customize it further (axis text size)

ggplot(data=penguins,
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm, 
                     group = species, 
                     color = species)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") + 
  scale_color_viridis_d() + 
  scale_color_manual(values = beyonce_palette(20)) + 
  theme_bw() + 
  theme(axis.title = element_text(size = 20))
                            
# customize it further (axis text size)

ggplot(data=penguins,
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm, 
                     group = species, 
                     color = species)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") + 
  scale_color_viridis_d() + 
  scale_color_manual(values = beyonce_palette(20)) + 
  theme_bw() + 
  theme(axis.title = element_text(size = 20))


# customize it further (axis text color)

ggplot(data=penguins,
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm, 
                     group = species, 
                     color = species)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") + 
  scale_color_viridis_d() + 
  scale_color_manual(values = beyonce_palette(20)) + 
  theme_bw() + 
  theme(axis.title = element_text(size = 20, 
        color = "red"))

# customize it further (change panel background)

ggplot(data=penguins,
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm, 
                     group = species, 
                     color = species)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") + 
  scale_color_viridis_d() + 
  scale_color_manual(values = beyonce_palette(10)) + 
  theme_bw() + 
  theme(axis.title = element_text(size = 20, 
                                  color = "red"), 
                                  panel.background = element_rect(fill = "linen"))

?theme()

# try to test out some new theme parameters on your own now!

ggplot(data=penguins,
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm, 
                     group = species, 
                     color = species)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") + 
  scale_color_viridis_d() + 
  scale_color_manual(values = beyonce_palette(18)) + 
  theme_wsj() + 
  theme(axis.title = element_text(size = 20, 
                                  color = "maroon"), 
        panel.background = element_rect(fill = "linen"))



  
# SAVE YOUR PLOT!
ggsave(here("Week_03", "Output", "penguin.png"))


# adjust the width and height of the plot (dimension of plot in inches)
ggsave(here("Week_03", "Output", "penguin.png"), 
width = 7, height = 5)

# note: you can also save it as a pdf if you replace ".png"


# SAVE PLOT AS AN OBJECT (assign it a name)
plot1<- ggplot(data=penguins,
               mapping = aes(x = bill_depth_mm, 
                             y = bill_length_mm, 
                             group = species, 
                             color = species)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)") + 
  scale_color_viridis_d() + 
  scale_color_manual(values = beyonce_palette(18)) + 
  theme_wsj() + 
  theme(axis.title = element_text(size = 20, 
                                  color = "maroon"), 
        panel.background = element_rect(fill = "linen"))

# view your plot
plot1        

# notes
# lots of ggplot extensions for different needs! check it out :)

#
#


### Homework... make a penguin plot! ###

# build a box plot!
# compare flipper length (mm) across species

# quick view of the dataset
glimpse(penguins)

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



# Name plot to "plot3B"

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
                                  

        