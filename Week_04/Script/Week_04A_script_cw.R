
### INTRO ###
### Week_04A Script: Data Wrangling Palmer Penguin data with dplyr! ###
### Created by: Kauanoe Greene ###
### Updated on: 2024-09-17 ###



### LIBRARIES ###
library(palmerpenguins)
library(tidyverse)
library(here)
library(dadjoke)
library(praise)


### DATA UPLOAD ###

#check out the data :) data frame name is "penguins"
glimpse(penguins) 
head(penguins)


### DATA ANALYSES ###


# filter()! 
# double = "==" means "exactly equal to".
# biggest mistake is only one = sign.

filter(.data = penguins, 
       year == "2008") #ideally good practice to using quotes for explicit characters to identify what you are looking for.

filter(.data = penguins, 
       body_mass_g > 5000) #you don't really need the quotes if it is a numeric value and you aren't using ==.

#combine them into a single line
filter(.data = penguins, sex == "female", body_mass_g > 5000)

#penguins that were collected in either 2008 or 2009
filter(.data = penguins, year == "2008" | year == "2009")
filter(.data = penguins, year %in% c("2008","2009"))

#penguins not from the island dream
filter(.data = penguins, island != "Dream")

#penguins in the species Adelie and Gentoo
filter(.data = penguins, species %in% c("Adeline","Gentoo"))

# mutate!
data2<-mutate(.data = penguins, body_mass_kg = body_mass_g/1000)
view(data2) # you msut name the data frame to view it!

#ifelse(): do conditional tests within
# categorize data type and name a column based on conditions you set!
data2<- mutate(.data = penguins, after_2008 = ifelse(year>2008, "After 2008", "Before 2008"))
view(data2)

# create a new column to add flipper length and body mass together
data2<-mutate(.data = penguins, flipper_body = body_mass_g, flipper_length_mm)
view(data2)

# create a new column where body mass grater than 4000 is labeled as big and everything else is small.
data2<- mutate(.data = penguins, chubs = ifelse(body_mass_g>4000, "chonks", "smol"))
view(data2)

# The Pipe! we link verbs!

penguins %>% #use penguin data set
  filter(sex == "female") %>% 
  mutate (log_mass = log(body_mass_g)) %>% 
  select(species, island, sex, log_mass) #selects out the columns I want to keep.
select (Species = species, island, sex, log_mass) #rename species to "Species"

# Summarize!

penguins %>% 
  summarise(mean_flipper = mean(flipper_length_mm, na.rm = TRUE), 
min_flipper = min(flipper_length_mm, na.rm = TRUE))

# Group_by: summarize values by certain groups!

penguins %>% 
  group_by(island) %>% 
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE), max_bill_length = max(bill_length_mm, na.rm = TRUE))

# Remove NAs: drop rows with NAs from a specific column.

penguins %>% 
  drop_na(sex)

# Pipe straight to ggplot!

penguins %>% 
  drop_na (sex) %>% 
  ggplot(aes(x = sex, y = flipper_length_mm)) + 
  geom_boxplot()

# link functions tgthr with a "+" sign when using ggplot
