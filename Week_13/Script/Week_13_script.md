Week 13 (Lectures & Homework)
================
Kauanoe Greene
2024-12-03

# Libraries

``` r
library(tidyverse)
library(here)
library(dplyr)
```

# Lecture: For Loops!

``` r
# for loops!  
# one way to run an iterative process  
# index in statement  
# command looks like:
# for( index in sequence) {command to repeat}
```

``` r
# using loops to read in multiple .csv files  
testdata<-read_csv(here("Week_13", "data", "cond_data","011521_CT316_1pcal.csv"))
glimpse(testdata)
```

    ## Rows: 1,474
    ## Columns: 3
    ## $ date        <dttm> 2021-01-15 08:24:40, 2021-01-15 08:24:50, 2021-01-15 08:2…
    ## $ Temperature <dbl> 23.28, 23.28, 23.28, 23.28, 23.28, 23.27, 23.28, 23.28, 23…
    ## $ Salinity    <dbl> 34.83656, 34.59268, 34.90039, 34.72214, 34.53604, 34.42168…

``` r
# list files in a directory  
# point to the location on the computer of the folder
CondPath<-here("Week_13", "data", "cond_data")
# list all the files in that path with a specific pattern
# In this case we are looking for everything that has a .csv in the filename
# you can use regex to be more specific if you are looking for certain patterns in filenames
files <- dir(path = CondPath,pattern = ".csv")
files
```

    ## [1] "011521_CT316_1pcal.csv" "011621_CT316_1pcal.csv" "011721_CT354_1pcal.csv"

``` r
# pre-allocate space for the loop  
# make an empty dataframe that has one row for each file and 3 columns
 cond_data<-tibble(filename =  rep(NA, length(files)),  # column name for year
                   mean_temp = rep(NA, length(files)), # column name for the mean temperature
                   mean_sal = rep(NA, length(files)), # column name for the mean salinity
                   ) # column name for the year name
cond_data
```

    ## # A tibble: 3 × 3
    ##   filename mean_temp mean_sal
    ##   <lgl>    <lgl>     <lgl>   
    ## 1 NA       NA        NA      
    ## 2 NA       NA        NA      
    ## 3 NA       NA        NA

``` r
# for loop  
raw_data<-read_csv(paste0(CondPath,"/",files[1])) # test by reading in the first file and see if it works
head(raw_data)
```

    ## # A tibble: 6 × 3
    ##   date                Temperature Salinity
    ##   <dttm>                    <dbl>    <dbl>
    ## 1 2021-01-15 08:24:40        23.3     34.8
    ## 2 2021-01-15 08:24:50        23.3     34.6
    ## 3 2021-01-15 08:25:00        23.3     34.9
    ## 4 2021-01-15 08:25:10        23.3     34.7
    ## 5 2021-01-15 08:25:20        23.3     34.5
    ## 6 2021-01-15 08:25:30        23.3     34.4

``` r
# turn it into a for loop  
for (i in 1:length(files)){ # loop over 1:3 the number of files
}

# add in the loop over the raw data  

for (i in 1:length(files)){ # loop over 1:3 the number of files 
raw_data<-read_csv(paste0(CondPath,"/",files[i]))
glimpse(raw_data)
}
```

    ## Rows: 1,474
    ## Columns: 3
    ## $ date        <dttm> 2021-01-15 08:24:40, 2021-01-15 08:24:50, 2021-01-15 08:2…
    ## $ Temperature <dbl> 23.28, 23.28, 23.28, 23.28, 23.28, 23.27, 23.28, 23.28, 23…
    ## $ Salinity    <dbl> 34.83656, 34.59268, 34.90039, 34.72214, 34.53604, 34.42168…
    ## Rows: 874
    ## Columns: 3
    ## $ date        <dttm> 2021-01-16 08:16:00, 2021-01-16 08:16:10, 2021-01-16 08:1…
    ## $ Temperature <dbl> 23.59, 23.60, 23.61, 23.60, 23.60, 23.59, 23.59, 23.59, 23…
    ## $ Salinity    <dbl> 34.04744, 33.96974, 33.93468, 33.91809, 33.87572, 33.85311…
    ## Rows: 1,004
    ## Columns: 3
    ## $ date        <dttm> 2021-01-17 08:20:00, 2021-01-17 08:20:10, 2021-01-17 08:2…
    ## $ Temperature <dbl> 23.73000, 23.72900, 23.73000, 23.73000, 23.73000, 23.73000…
    ## $ Salinity    <dbl> NA, NA, NA, NA, NA, NA, NA, 33.48506, 33.47455, 33.45649, …

``` r
# add in the columns  
for (i in 1:length(files)){ # loop over 1:3 the number of files 
raw_data<-read_csv(paste0(CondPath,"/",files[i]))
#glimpse(raw_data)
cond_data$filename[i]<-files[i]
} 
cond_data
```

    ## # A tibble: 3 × 3
    ##   filename               mean_temp mean_sal
    ##   <chr>                  <lgl>     <lgl>   
    ## 1 011521_CT316_1pcal.csv NA        NA      
    ## 2 011621_CT316_1pcal.csv NA        NA      
    ## 3 011721_CT354_1pcal.csv NA        NA

``` r
# add in means  
for (i in 1:length(files)){ # loop over 1:3 the number of files 
raw_data<-read_csv(paste0(CondPath,"/",files[i]))
#glimpse(raw_data)
cond_data$filename[i]<-files[i]
cond_data$mean_temp[i]<-mean(raw_data$Temperature, na.rm =TRUE)
cond_data$mean_sal[i]<-mean(raw_data$Salinity, na.rm =TRUE)
} 
cond_data
```

    ## # A tibble: 3 × 3
    ##   filename               mean_temp mean_sal
    ##   <chr>                      <dbl>    <dbl>
    ## 1 011521_CT316_1pcal.csv      29.2     33.7
    ## 2 011621_CT316_1pcal.csv      29.7     33.3
    ## 3 011721_CT354_1pcal.csv      29.2     33.0

# Lecture: Maps!

``` r
# the pattern of looping over a vector.  
# map() makes a list.
# map_lgl() makes a logical vector.
# map_int() makes an integer vector.
# map_dbl() makes a double vector.
# map_chr() makes a character vector.
# map_df() makes a dataframe

# Each function takes a vector as input, applies a function to each piece, and then returns a new vector that’s the same length (and has the same names) as the input.
```

``` r
# use a canned function that already exists


1:10 %>% # a vector from 1 to 10 (we are going to do this 10 times) %>% # the vector to iterate over
  map(rnorm, n = 15) # calculate 15 random numbers based on a normal distribution in a list
```

    ## [[1]]
    ##  [1]  0.50611601 -1.19722162  0.45164209  2.33861491  0.13617146  2.59573754
    ##  [7]  0.04879799  0.47032167  0.28929751  0.63527238 -0.96523345  1.34894212
    ## [13]  0.59267373 -0.52737173 -0.30962494
    ## 
    ## [[2]]
    ##  [1] 2.4207582 1.6434834 1.7833741 0.2901378 2.4718606 1.7987145 1.9033279
    ##  [8] 2.8267606 2.4859630 0.9329304 4.7789485 3.8157040 2.3700129 1.4861301
    ## [15] 1.8518715
    ## 
    ## [[3]]
    ##  [1] 3.059191 3.522711 2.796085 2.095657 3.326378 3.868783 3.558733 3.680654
    ##  [9] 3.463549 2.668519 1.329303 2.715459 2.415825 2.602821 2.175998
    ## 
    ## [[4]]
    ##  [1] 3.210498 2.612556 4.374383 4.757767 6.580331 3.093299 4.121100 3.976747
    ##  [9] 4.804007 3.008844 4.186805 1.307123 2.658871 5.196300 2.252337
    ## 
    ## [[5]]
    ##  [1] 6.120231 3.799897 6.091161 5.148890 5.896065 3.864539 5.406000 2.726096
    ##  [9] 7.062595 3.889806 6.664695 5.309642 5.501781 3.604654 6.094135
    ## 
    ## [[6]]
    ##  [1] 5.978566 5.997253 4.649839 5.594715 6.270790 5.185591 5.260684 6.205423
    ##  [9] 6.560791 7.165524 4.585199 5.233838 6.821944 6.376654 5.817991
    ## 
    ## [[7]]
    ##  [1] 7.486564 7.483445 6.664782 5.419264 7.248669 5.768008 5.706910 6.784983
    ##  [9] 8.063623 8.131867 6.074932 6.565157 6.835595 7.114795 7.956243
    ## 
    ## [[8]]
    ##  [1] 7.551359 8.743274 6.964419 7.237403 8.485928 7.515307 8.087554 7.955231
    ##  [9] 9.935069 9.050394 8.483083 6.624060 7.573111 7.796821 7.812256
    ## 
    ## [[9]]
    ##  [1]  8.792653  9.686399  8.663031 10.313960  8.120427  9.514026  9.603520
    ##  [8]  8.145837  9.157992  8.152887  8.055531  9.726358 10.448222 10.005721
    ## [15]  9.434564
    ## 
    ## [[10]]
    ##  [1] 10.224925 10.397414  9.260378  8.067477 10.746286  8.797074  9.762501
    ##  [8]  9.285664  9.687471  9.819295  9.605010  9.678855 12.094662 10.125655
    ## [15] 10.836648

``` r
# calculate the mean from each list  

1:10 %>% # a vector from 1 to 10 (we are going to do this 10 times) %>% # the vector to iterate over
  map(rnorm, n = 15)  %>% # calculate 15 random numbers based on a normal distribution in a list 
  map_dbl(mean) # calculate the mean. It is now a vector which is type "double"
```

    ##  [1]  1.132157  1.782719  2.515741  4.024966  4.613531  6.208839  6.693932
    ##  [8]  8.251151  9.697571 10.002831

``` r
# make your own function  

1:10 %>% # list 1:10
  map(function(x) rnorm(15, x)) %>% # make your own function
  map_dbl(mean)
```

    ##  [1]  1.087049  2.230903  3.046101  3.705612  5.139758  6.181200  6.654779
    ##  [8]  7.747163  8.716060 10.292470

``` r
# Use a formula when you want to change the arguments within the function  

1:10 %>%
  map(~ rnorm(15, .x)) %>% # changes the arguments inside the function
  map_dbl(mean)
```

    ##  [1]  1.310334  2.066733  2.644004  3.787410  4.407388  5.822779  6.283430
    ##  [8]  8.138688  8.667082 10.024837

``` r
# bring in files using purrr instead of a for loop 
# point to the location on the computer of the folder

CondPath<-here("Week_13", "data", "cond_data")
files <- dir(path = CondPath,pattern = ".csv")
files
```

    ## [1] "011521_CT316_1pcal.csv" "011621_CT316_1pcal.csv" "011721_CT354_1pcal.csv"

``` r
# Or, we can get the full file names in one less step by doing this...  

files <- dir(path = CondPath,pattern = ".csv", full.names = TRUE)
#save the entire path name
files
```

    ## [1] "C:/Users/18088/OneDrive/Desktop/Repositories/Greene/Week_13/data/cond_data/011521_CT316_1pcal.csv"
    ## [2] "C:/Users/18088/OneDrive/Desktop/Repositories/Greene/Week_13/data/cond_data/011621_CT316_1pcal.csv"
    ## [3] "C:/Users/18088/OneDrive/Desktop/Repositories/Greene/Week_13/data/cond_data/011721_CT354_1pcal.csv"

``` r
# read in the files  

data<-files %>%
  set_names()%>% # set's the id of each list to the file name
  map_df(read_csv,.id = "filename") # map everything to a dataframe and put the id in a column called filename
data
```

    ## # A tibble: 3,352 × 4
    ##    filename                             date                Temperature Salinity
    ##    <chr>                                <dttm>                    <dbl>    <dbl>
    ##  1 C:/Users/18088/OneDrive/Desktop/Rep… 2021-01-15 08:24:40        23.3     34.8
    ##  2 C:/Users/18088/OneDrive/Desktop/Rep… 2021-01-15 08:24:50        23.3     34.6
    ##  3 C:/Users/18088/OneDrive/Desktop/Rep… 2021-01-15 08:25:00        23.3     34.9
    ##  4 C:/Users/18088/OneDrive/Desktop/Rep… 2021-01-15 08:25:10        23.3     34.7
    ##  5 C:/Users/18088/OneDrive/Desktop/Rep… 2021-01-15 08:25:20        23.3     34.5
    ##  6 C:/Users/18088/OneDrive/Desktop/Rep… 2021-01-15 08:25:30        23.3     34.4
    ##  7 C:/Users/18088/OneDrive/Desktop/Rep… 2021-01-15 08:25:40        23.3     34.3
    ##  8 C:/Users/18088/OneDrive/Desktop/Rep… 2021-01-15 08:25:50        23.3     34.3
    ##  9 C:/Users/18088/OneDrive/Desktop/Rep… 2021-01-15 08:26:00        23.3     34.2
    ## 10 C:/Users/18088/OneDrive/Desktop/Rep… 2021-01-15 08:26:10        23.3     34.1
    ## # ℹ 3,342 more rows

``` r
# calculate means  

data<-files %>%
  set_names()%>% # set's the id of each list to the file name
  map_df(read_csv,.id = "filename") %>% # map everything to a dataframe and put the id in a column called filename
  group_by(filename) %>%
  summarise(mean_temp = mean(Temperature, na.rm = TRUE),
            mean_sal = mean(Salinity,na.rm = TRUE))
```

# Homework

``` r
# Homework  

# You have a set of 4 .csv files in data/homework. Each of these files is a timeseries of temperature and light data collected in tide pools in Oregon by Jenn Fields. 
# Your goal is to bring in all 4 files and calculate the mean and standard deviation of both temperature (Temp.C) and light (Intensity.lux) for each tide pool. 

# Use both a for loop and map() functions in your script. (Basically, do it twice). Due Tuesday at 1pm.
```

``` r
# data upload all .csv files below  

TP1<-read_csv(here("Week_13", "Data", "homework", "TP1.csv"))
TP2<-read_csv(here("Week_13", "Data", "homework", "TP2.csv"))
TP3<-read_csv(here("Week_13", "Data", "homework", "TP3.csv"))
TP4<-read_csv(here("Week_13", "Data", "homework", "TP4.csv"))
```

``` r
# set up for loops

HW<-here("Week_13", "Data", "homework") # data  
files <- dir(path = HW,pattern = ".csv") # name  

# make column space for loop  
# make an empty dataframe 
 
TP_data<-tibble(filename =  rep(NA, length(files)),  # column name for year
                   mean_temp = rep(NA, length(files)), # mean temperature column
                   mean_light = rep(NA, length(files)), # mean light  
                std_temp = rep(NA, length(files)), # sd temp column  
                std_light = rep(NA, length(files)) # sd light column  
                   )

raw_data<-read_csv(paste0(HW,"/",files[1])) # test run  
head(raw_data)
```

    ## # A tibble: 6 × 7
    ##   PoolID Foundation_spp Removal_Control Date.Time    Temp.C Intensity.lux
    ##    <dbl> <chr>          <chr>           <chr>         <dbl>         <dbl>
    ## 1      1 Phyllospadix   Control         6/16/19 0:01   10.2             0
    ## 2      1 Phyllospadix   Control         6/16/19 0:16   10.1             0
    ## 3      1 Phyllospadix   Control         6/16/19 0:31   10.2             0
    ## 4      1 Phyllospadix   Control         6/16/19 0:46   10.1             0
    ## 5      1 Phyllospadix   Control         6/16/19 1:01   10.1             0
    ## 6      1 Phyllospadix   Control         6/16/19 1:16   10.1             0
    ## # ℹ 1 more variable: LoggerDepth <dbl>

``` r
mean_temp<-mean(raw_data$Temp.C, na.rm = TRUE) # mean temp  
mean_light<-mean(raw_data$Intensity.lux, na.rm = TRUE) # mean light  
std_temp<-sd(raw_data$Temp.C, na.rm = TRUE) # sd temp  
std_light<-sd(raw_data$Intensity.lux, na.rm = TRUE) # sd light
```

``` r
# Loop!

for (i in 1:length(files)){ # loop over 1:3 the number of files 
raw_data<-read_csv(paste0(HW,"/",files[i]))
TP_data$filename[i]<-files[i] # caluclate means and sd from data files  
TP_data$mean_temp[i]<-mean(raw_data$Temp.C, na.rm =TRUE) # mean temp  
TP_data$mean_light[i]<-mean(raw_data$Intensity.lux, na.rm =TRUE) # mean light 
TP_data$std_temp[i]<-sd(raw_data$Temp.C, na.rm =TRUE) # sd temp  
TP_data$std_light[i]<-sd(raw_data$Intensity.lux, na.rm =TRUE) # sd light  
} 

TP_data
```

    ## # A tibble: 4 × 5
    ##   filename mean_temp mean_light std_temp std_light
    ##   <chr>        <dbl>      <dbl>    <dbl>     <dbl>
    ## 1 TP1.csv       13.3       427.     2.32     1661.
    ## 2 TP2.csv       13.2      5603.     2.31    11929.
    ## 3 TP3.csv       13.1      5605.     2.32    12101.
    ## 4 TP4.csv       13.2       655.     2.27     2089.

``` r
# maps! 

HW<-here("Week_13", "Data", "homework") # file  
files <- dir(path = HW,pattern = ".csv") # rename 
files <- dir(path = HW,pattern = ".csv", full.names = TRUE)
# or save the entire path name
```

``` r
data<-files %>%
  set_names()%>% # set's the id of each list to the file name
  map_df(read_csv,.id = "filename") %>% # map everything to a dataframe and put the id in a column called filename
  group_by(filename) %>% # group  
  summarise(mean_temp = mean(Temp.C, na.rm = TRUE), # mean temp  
            mean_light = mean(Intensity.lux,na.rm = TRUE), # mean light  
            std_temp = sd(Temp.C, na.rm = TRUE), # sd temp  
            std_light = sd(Intensity.lux,na.rm = TRUE)) # sd light  
data
```

    ## # A tibble: 4 × 5
    ##   filename                               mean_temp mean_light std_temp std_light
    ##   <chr>                                      <dbl>      <dbl>    <dbl>     <dbl>
    ## 1 C:/Users/18088/OneDrive/Desktop/Repos…      13.3       427.     2.32     1661.
    ## 2 C:/Users/18088/OneDrive/Desktop/Repos…      13.2      5603.     2.31    11929.
    ## 3 C:/Users/18088/OneDrive/Desktop/Repos…      13.1      5605.     2.32    12101.
    ## 4 C:/Users/18088/OneDrive/Desktop/Repos…      13.2       655.     2.27     2089.
