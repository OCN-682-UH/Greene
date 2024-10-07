
### INTRO ###
### Week_05A classwork: Data Wrangling: joins (Becker and Silbiger (2020)) ###
### Created by: Kauanoe Greene ###
### Updated on: 2024-09-25 ###


### LIBRARIES ###
library(tidyverse)
library(here)


### DATA UPLOAD ###
# read csv
EnviroData<- read.csv(here("Week_05", "Data", "site.characteristics.data.csv"))
TPCData<- read.csv(here("Week_05", "Data", "Topt_data.csv"))

glimpse(EnviroData)
glimpse(TPCData)


### DATA ANALYSIS ###

# let's format this data to be consistent
# view afterwards

EnviroData_wide<- EnviroData %>% 
  pivot_wider(names_from = parameter.measured, 
              values_from = values)

# now let us arrange it alphabetically using "arrange()"

EnviroData_wide<- EnviroData %>% 
  pivot_wider(names_from = parameter.measured, 
              values_from = values) %>% 
  arrange(site.letter)

# LEFT_JOIN() "left_join(x, y)
# bring together data frames
# order (layers) of joins really do matter!
# left is more common than right join

FullData_left<- left_join(TPCData, EnviroData_wide)

# now "join_by(site.letter)"

join_by(site.letter)
head(FullData_left)

# relocate all the numeric data after the character data

FullData_left<- left_join(TPCData, EnviroData_wide) %>% 
  relocate(where(is.numeric), .after = where(is.character))



# think, pair, share
# calculate the mean and variance of all collected data by site

FullData_left<- left_join(TPCData, EnviroData_wide) %>% 
  relocate(where(is.numeric), .after = where(is.character)) %>% 
pivot_longer(cols = E:substrate.cover, # the cols you want to pivot. 
             names_to = "Parameters", # the names of the new cols.
             values_to = "Values") 

  FullData_left_summary<- FullData_left %>%
  group_by(site.letter, Parameters) %>% 
  summarise(mean_values = mean(Values), # get mean 
            var_values = var(Values)) # get variance
  
# angela's version

  FullData_mean_var <- FullData_left %>% 
    group_by(site.letter) %>% 
    summarise(across(where(is.numeric), 
                     list(mean = mean, var = var), na.rm = TRUE))
  
# MAKE A TIBBLE!!
# tibble() it is a dataframe, but simpler.
  
T1 <- tibble(Site.ID = c("A", "B", "C", "D"),
             Temperature = c(14.1, 16.7, 15.3, 12.8 ))
T2 <- tibble(Site.ID = c("A", "B", "C", "E"),
             pH = c(7.3, 7.8, 8.1, 7.9))


# LEFT_JOIN VS. RIGHT_JOIN

left_join(T1, T2)

right_join(T1, T2)


#INNER_JOIN VS. FULL_JOIN
# inner_join: only keeps dataframes that overlap
# full_join: keeps everything

inner_join(T1, T2)

full_join(T1, T2)


#SEMI_JOIN VS. ANTI_JOIN
# semi_join(): keeps all rows from te first data set where there are matching values in the second data set.
# anti_join(): saves all the rows in the first dataset that do not match or overlap in the second dataset
# order matters!
# anti_join helps you pin point what variable does not overlap!

semi_join(T1, T2)

anti_join(T1, T2)
anti_join(T2, T1)




