### Hi! here is my first script for OCN-682! ###
### created by: Kauanoe Greene ###
### created on: 2024-09-09 ###
### updated on: 2024-09-09 ###

### LIBRARIES ###
library(tidyverse)
library(here) #tells me where I am working from

### DATA ###

#read in my data
weightdata<-read_csv(here("Week_02", "Data", "weightdata.csv"))
