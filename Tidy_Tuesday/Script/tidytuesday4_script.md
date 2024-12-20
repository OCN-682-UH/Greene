Tidy Tuesday!
================
Kauanoe Greene
2024-11-26

# Libraries

``` r
library(here)
library(tidyverse)
library(plotly)
library(viridis)
library(hrbrthemes)
```

# Data

``` r
# Data upload

episode_metrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-19/episode_metrics.csv')
```

# Data clean

``` r
# data cleaning via Tidy Tuesday

words <- episode_metrics %>% # rename data set  
  select(season, unique_words, avg_length) %>% # select focal columns  
  group_by(season) %>% # select parameter to group by  
  summarise(mean.word = mean(unique_words, na.rm = TRUE), # calculate mean words for each season  
            mean.length = mean(avg_length, na.rm = TRUE)) # calculate mean length for each season  
```

# Plot

``` r
# let's make a time series plot and area chart

ggplot(words, aes(x=season, y=mean.word, color = season)) + # identify axis variables
  geom_segment(aes(x=season, xend=season, y=0, yend=mean.word), color="skyblue") + # aesthetics for segments
  geom_point(size=4, alpha=0.6) + # points
  theme_light() + # theme
  coord_flip() + # orientation
  theme(
    panel.grid.major.y = element_blank(), # no major panel
    panel.border = element_blank(), # no border
    axis.ticks.y = element_blank(), # no axis ticks
    legend.position = "none" # no legend
  ) +
  xlab("Bob's Burgers Season") + # x axis title  
  ylab("Mean Unique Words") + # y axis title  
  scale_x_continuous(breaks = seq(0,15,by=1)) + # modify axis scale values
  scale_y_continuous(breaks = seq(0, 1200, by = 100)) + # modify axis scale values  
  labs(title = "Mean Unique Words used per Season of Bob's Burgers") + # plot title  
  scale_color_viridis() # plot color theme  
```

![](../Output/unnamed-chunk-4-1.png)<!-- -->

# Wrap up

``` r
# save plot to my output folder
ggsave(here("Tidy_Tuesday", "Output", "tidyplot4.png")) 

# save csv file to data folder
write.csv(words, here("Tidy_Tuesday", "Data", "tidydata4.csv"))

# Tidy Tuesday Takeaways:  
# made a lollipop map!  
# practiced customizing the plot aesthetic and themes
# modified the axis scales
```
