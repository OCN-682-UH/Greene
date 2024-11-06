
# INTRODUCTION
# Project: ITV Functional Trait Data for ʻAʻaliʻi
# Created by: Kauanoe Greene
# Updated on: 2024-10-22


# LIBRARIES
library(tidyverse)
library(here)
library(patchwork)
library(ggrepel)
library(gganimate)
library(magick)

# DATA UPLOAD
# here is my continuous trait data


# DATA ANALYSES

means <- cleandata %>% 
  group_by(Population, Week, Treatment) %>% 
  summarise(mean.cond = mean(Conductance, na.rm = TRUE), 
            mean.chl = mean(Chlorophyll, na.rm = TRUE), 
            mean.alpha = mean(Alpha, na.rm = TRUE), 
            mean.etrm= mean(ETRmax, na.rm = TRUE), 
            mean.ek = mean(Ek, na.rm = TRUE), 
            mean.npq = mean(NPQmax, na.rm = TRUE), 
            mean.fvfm = mean(Fv.Fm, na.rm = TRUE))

# DATA PLOT - Conductance !

means %>% 
  ggplot(aes(x = Week, 
             y = mean.cond, 
             # fill = Treatment, 
             color = Treatment)) + 
  geom_point() + 
  geom_line() + 
  labs(subtitle = "Detecting trends in leaf stomatal conductance of control and drought groups over time", 
       x = "Time (Weeks)", 
       y = "Mean Stomatal Conductance (mmol m^(-2) s^(-1)") + 
  # theme(legend.position="none") +
  ggtitle("Plasticity in Leaf Stomatal Conductance Across Populations") +
  #theme(panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 8)) +
  facet_wrap(~Population)
  


  

# Change the name on graph
# dataset$columntitle[dataset$columntitle == "Current Name"] <- "New name"
# means$mean.cond[means$mean.cond == "mean.cond"] <- "Mean Leaf Stomatal Conductance (mmol m^(-2) s^(-1))"
