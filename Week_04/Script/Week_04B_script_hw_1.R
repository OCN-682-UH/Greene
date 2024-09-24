
### INTRO ###
### Homework: Week_04B: Practicing data wrangling with dplyr! ###
### Created by: Kauanoe Greene ###
### Updated on: 2024-09-17 ###

### LIBRARIES ###
library(palmerpenguins)
library(tidyverse)
library(here)
library(dadjoke)
library(praise)
library(wsjplot)
library(ggthemes)


### DATA UPLOAD ###

glimpse(penguins) #view data

### DATA ANALYSES ###

# PT 1 DIRECTIONS
# calculate mean and variance for body mass by species, islands, and sex.
# without NAs

# PT 2 DIRECTIONS
# (a) filter out! male penguins
# (b) calculate log body mass
# (c) select only columns for species, island, sex, and log body mass
# (d) MAKE A PLOT ^-^



# PT 1 ANALYSIS
view(penguins)

part1<-penguins %>%
  drop_na(.) %>%
  group_by(species, island, sex) %>%
 summarise(mean_mass = mean(body_mass_g), 
                             var_mass = var(body_mass_g))
view(part1)

# PT 2 ANALYSIS
part2<- penguins %>% 
  filter(sex != "male") %>% 
  mutate (log_mass = log(body_mass_g)) %>% 
  select(c(species, island, sex, log_mass))

view(part2)         



# PLOT IT!

ggplot(part2, 
       mapping = aes(x = species, 
                     y = log_mass, 
                     color = species,
                     shape = island, 
                     fill = species)) + 
  scale_color_viridis_d() + 
  geom_violin() + labs(x = "Species", 
                    y = "Log Body Mass (g)", 
                    title = "Variability in Body Mass Across Female Penguin Species", 
                    subtitle = "Comparing log body mass across Adelie, Chinstrap, and Gentoo Species", 
                    caption = "Source: Palmer Station LTER/ palmerpenguins package") +  
  theme_wsj() + 
  theme(axis.title = element_text(size = 20), 
        legend.position = "none")



#save my plot!

ggsave(here("Week_04", "Output", "Week_04B_hw.png"))

  
