
### INTRO ###
### Week_05B homework: lubridate dates and times ###
### Created by: Kauanoe Greene ###
### Updated on: 2024-10-04 ###




### PACKAGES ###
install.packages("lubridate") #this package deals with dates and times!

### LIBRARIES ###
library(tidyverse)
library(here)
library(lubridate)
library(ggthemes)
library(ggplot2)


### DATA UPLOAD: CLASSWORK ###

# read csv
CondData<- read.csv(here("Week_05", "Data", "CondData.csv"))
DepthData<- read.csv(here("Week_05", "Data", "DepthData.csv"))

glimpse(CondData)
glimpse(DepthData)

### DATA ANALYSES: CLASSWORK ###
# PRACTICE LUBRIDATE FUNCS
# what time is it now?
# now() automatically gives the time
# now(tzone = "GMT")

now()
now(tzone = "GMT")

# what is today's date?
# today()
# today(tzone = "")

today()
today(tzone = "GMT")

# you can ask if it is morning or night, too!
# morning
# am(now())
# ask if it is a leap year
# leap_year(now())

am(now()) #false
leap_year(now()) #true

# date and time specifications with lubridate

ymd_hms("2021-02-24 10:22:20 PM")
mdy_hms("02/24/2021 22:22:20")
mdy_hm("February 24 2021 10:22 PM")

# vector of dates!
datetimes<-c("02/24/2021 22:22:20", 
             "02/25/2021 11:21:10", 
             "02/26/2021 8:01:52")

# convert to datetimes
datetimes <- mdy_hms(datetimes)

#extract which month were these date entries from
month(datetimes)

# change the months into words instead of numbers
# setting labels = TRUE makes the month a factor
month(datetimes, label = TRUE)

# if you don't want the month to be abbreviated, try this!
month(datetimes, label = TRUE, abbr = FALSE) #spell it out!

#extract the day or weekday
day(datetimes) # day
wday(datetimes, label = TRUE) # weekday

# extract hours, minutes, seconds...
hour(datetimes) # hours
minute(datetimes) #minutes
seconds(datetimes) #seconds

# let's add 4 hours to all the datetimes
datetimes + hours(4) #this adds 4 hours

# notice the "s" in hours() function. hours() is used to add hours, not extract.
# not using the "s" and using hour() extracts the hour component from a time.

# you can do the same with days, minutes, and seconds!
datetimes + days(2) # days

# rounding dates!
round_date(datetimes, "minute") # round to the nearest minute
round_date(datetimes, "5 mins") # round to the nearest 5 minute


# CHALLENGE FOR TUESDAY!
# CondData (temp and salinity date)
# convert date column to a datetime
# use pipe %>% to keep it clean
# don't forget to use mutate if you need to add/modify columns.
# hint: always look at your data in R after you read it in!

# read in data for challenge!
CondData<- read.csv(here("Week_05", "Data", "CondData.csv"))
DepthData<- read.csv(here("Week_05", "Data", "DepthData.csv"))

glimpse(CondData)
glimpse(DepthData)

CondData_datetime<-CondData %>%
  mutate(datetime = mdy_hm(date)) %>% 
  drop_na() %>% 
  select(datetime, Temperature, Serial, Salinity)

glimpse(CondData_datetime)
view(CondData_datetime)



# HOMEWORK INSTRUCTIONS
# read in cond and depth data in
# convert date columns appropriately
# round cond data to nearest 10 secs
# join two data frames so there are no NAs 
  #join in a way where only exact matches between the two data frame are kept
# take averages by minute of:
  # date
  # depth
  # temp
  # sal


### DATA UPLOAD  ###

CondData<- read.csv(here("Week_05", "Data", "CondData.csv")) #read in cond data!
DepthData<- read.csv(here("Week_05", "Data", "DepthData.csv")) # read in depth data!

glimpse(CondData) # check it out :)
glimpse(DepthData) # check it out ;)


### DATA ANALYSES ###

# formatting dataset 1: cond data
CondData_datetime<-CondData %>% # modify cond dataset so the datetime is in character format and matches depth dataset
  mutate(datetime = mdy_hm(date), # modify column name - date column to be "datetime" column
         datetime=round_date(datetime, "10 secs")) %>% # round the datetime format to round up to nearest 10 secs.
  drop_na() %>% # drop the NAs
  select(datetime, Temperature, Serial, Salinity) # select which parameters you want to include in dataset.

view(CondData_datetime) # always check out your dataset after pipeing!

# formatting dataset 2: depth data
DepthData_datetime<-DepthData %>%
  mutate(datetime = mdy_hm(date), 
         datetime=round_date(datetime, "10 secs")) %>% 
  drop_na() %>% 
  select(datetime, AbsPressure, Depth)

view(DepthData_datetime) # always check out your data after pipeing!

# lubridate func task! join datasets 1 and dataset 2!

datajoin<- inner_join(CondData_datetime, DepthData_datetime) 

#left join cond and depth datasets! 
# join_by datetime column since they overlap ;)
 

# stats task! calculate the mean values at each minute for depth, temp, and salinity!

  datajoin_mean <- datajoin %>% # time to calculate the stats
    select(datetime, Depth, Temperature, Salinity) %>% # choose which parameters you want to keep in your dataset to summarise.
    group_by(datetime) %>% # choose which how you want to group the mean by (we want it grouped by each minute)
    summarise(across(where(is.numeric), # calculate the mean of each numeric parameter listed and group by each minute
                   list(mean =mean))) %>% 
    pivot_longer(cols = Depth_mean:Salinity_mean, 
                 names_to = "Parameters", 
                 values_to = "Values")
  
view(datajoin_mean) # check your data every time you pipe!

# plot data!

ggplot(data = datajoin_mean, 
       mapping = aes(x = datetime, 
                     y = Values)) + 
  geom_line() + 
  facet_wrap(vars(Parameters), scales = "free") +
  labs(title = "Variability in Salinity and Temperature Over Time", 
       x = "Time", 
       y = "Mean Values") + 
  scale_color_viridis_d()





