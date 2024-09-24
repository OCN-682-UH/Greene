
### INTRO ###
### Week_04B flipped lecture script: data wrangling with tidyr (using Dr. Silbiger's Maunalua data) ###
### Created by: Kauanoe Greene ###
### Updated on: 2024-09-23 ###


### LIBRARIES ###
library(tidyverse)
library(here)

### DATA UPLOAD ###
ChemData<-read_csv(here("Week_04", "Data", "chemicaldata_maunalua.csv"))
view(ChemData)

glimpse(ChemData)

### DATA ANALYSES ###

# filter out everything that is not a complete row!
ChemData_clean<-ChemData %>%
  filter(complete.cases(.)) 
view(ChemData_clean)

# SEPARATE COLUMNS
# we want to separate different parameters!
# separate()
  #data = [data frame that you are using]
  #col = [column that you want to separate]
  #into = [name of the new columns]
  #sep = [what are you separating by?]

?separate()

# let's do it!

ChemData_clean<- ChemData %>%
  drop_na() %>% #filters out everything that is not a complete row!
  separate(col = Tide_time, 
         into = c("Tide", "Time"), 
         sep = "_")

head(ChemData_clean)


# what this means:
  #chose the tide time column
  #separated it into 2 columns "Tide" and "Time"
  #separated it by "_"

# This actually deleted the old column and creates new ones,
# but if we want to keep it,
  # add "remove = FALSE"

ChemData_clean<- ChemData %>%
  drop_na() %>% #filters out everything that is not a complete row!
  separate(col = Tide_time, 
           into = c("Tide", "Time"), 
           sep = "_", 
           remove = FALSE)

head(ChemData_clean)

# UNITE COLUMNS

ChemData_clean<- ChemData %>%
  drop_na() %>% #filters out everything that is not a complete row!
  separate(col = Tide_time, 
           into = c("Tide", "Time"), 
           sep = "_", 
           remove = FALSE) %>% 
  unite(col = "Site_Zone", 
      c(Site, Zone), 
      sep = ".", 
      remove = FALSE)

head(ChemData_clean)

# what this means:
  # the name of the new column
  # the columns to unite
  # let us add a "." in the middle
  # keep the original column

# PIVOTING DATA WIDE TO LONG
  # Wide to long: pivot_longer()
  # Long to wide: pivot_wider()

ChemData_long <-ChemData_clean %>% 
  pivot_longer(cols = Temp_in:percent_sgd, 
               names_to = "Variables", 
               values_to = "Values")
view(ChemData_long)

# what this means:
  # the columns you want to pivot. this says to select the "temp" to "percent SGD" columns.
  # the names of the new columns with all the column names.
  # names of the new column with all the values.


# CALCULATE MEAN AND VARIANCE FOR ALL VARIABLES AT EACH SITE!

ChemData_long %>% 
  group_by(Variables, Site) %>% 
  summarise(Param_means = mean(Values, na.rm = TRUE), 
            Param_vars = var(Values, na.rm = TRUE))

head(ChemData_long)  

# what this means:
  # group everything we want
  # get mean
  # get variance

# summarise() has grouped output by "Variables"
  # you can override using the ".groups" argument.



# CALCULATE MEAN, VARIANCE, AND STANDARD DEVIATION
  # for all variables by SITE, ZONE, AND TIDE.

ChemData_long %>% 
  group_by(Variables, Site, Zone, Tide) %>% 
  summarise(Param_means = mean(Values, na.rm = TRUE), 
            Param_vars = var(Values, na.rm = TRUE), 
            Param_sd = sd(Values, na.rm = TRUE))
head(ChemData_long)


# USE FACET_WRAP WITH LONG DATA
  # create a boxplot of every parameter by site

ChemData_long %>% 
  ggplot(aes(x = Site, y = Values)) + 
  geom_boxplot() + 
  facet_wrap(~Variables)


# create boxplots of every parameter by site, but fix the axes.
  # scales = "free" releases both the x and y axes.

ChemData_long %>% 
  ggplot(aes(x = Site, y = Values)) + 
  geom_boxplot() + 
  facet_wrap(~Variables, scales = "free")

# CONVERT LONG FORMAT TO WIDE
  # use pivot_wider()

ChemData_wide <-ChemData_long %>%
  pivot_wider(names_from = Variables, 
              values_from = Values)

view(ChemData_wide)  

# what this means:
  # column with the names for the new column
  # column with the values

view(ChemData_wide)


# WRAPPIN IT UP

# CALCULATE SOME SUMMARY STATISTICS AND EXPORT THE CSV FILE
# start from the beginning and work through the entire flow again, ending with data export.

ChemData_clean<-ChemData %>% 
  drop_na() %>% #filters out everything that is not a complete row.
separate(col = Tide_time, #choose the tide and time column. 
         into = c("Tide","Time"), # separate it into 2 columns Tide and Time.
         sep = "_", # separate by "_" 
         remove = FALSE) %>% 
  pivot_longer(cols = Temp_in:percent_sgd, # the cols you want to pivot. 
               names_to = "Variables", # the names of the new cols. 
               values_to = "Values") %>%  # the names of the new cols with all the values. 
group_by(Variables, Site, Time) %>% 
  summarise(mean_vals = mean(Values, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Variables, 
              values_from = mean_vals) %>% # notice it is now "mean_vals"
  write_csv(here("Week_04", "Output", "Week_04B_summary.csv")) #export as CSV!!
view(ChemData_clean) # always VIEW DATA AFTER PIPE (in your console)!!!


# WE DID IT! OUR FIRST DATA ANALYSIS!

# today's fun R package: Put a Bernie on it.
# devtools::install_github("R-CoderDotCom/ggbernie@main")
# library(ggbern)

