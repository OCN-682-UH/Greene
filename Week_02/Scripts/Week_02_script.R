### Hi! here is my first script for OCN-682! ###
### created by: Kauanoe Greene ###
### created on: 2024-09-09 ###
### updated on: 2024-09-09 ###

### LIBRARIES ###
library(tidyverse)
library(here) #tells me where I am working from

### DATA ###

# read in my data
weightdata<-read_csv(here("Week_02", "Data", "weightdata.csv"))

# data analysis
head(weightdata) # shows me the top 6 lines of data frame
tail(weightdata) # shows me the tailing 6 lines of data frame
view(weightdata) # opens data frame into a new window!

### Notes: Homework ###
# (graded) push Week_02 script to github
# (not graded) check out markdown tutorial
# (graded) revise README file "README.md" can edit in Rstudio, just add somethign useful 
