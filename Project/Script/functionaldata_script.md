Functional Trait Data!
================
Kauanoe Greene
2024-10-24

- [Data](#data)
- [Data Visualization](#data-visualization)
- [Statistic Modeling: Chlorophyll Content
  Data](#statistic-modeling-chlorophyll-content-data)

``` r
# LIBRARIES

# visuals
library(tidyverse)
library(tidytext)
library(here)

# models (stats)
library(lmerTest)
library (RLRsim)
library(Matrix)
# library(lme4)
library(lattice)
library(car)
library(nlme)
```

``` r
cleandata <- read_csv(here("Project", "Data", "cleandata.csv"))
view(cleandata)
```

# Data

``` r
# DATA ANALYSES: Means

means <- cleandata %>% 
  group_by(Population, Week, Treatment) %>% 
  summarise(mean.cond = mean(Conductance, na.rm = TRUE), 
            mean.chl = mean(Chlorophyll, na.rm = TRUE), 
            mean.alpha = mean(Alpha, na.rm = TRUE), 
            mean.etrm= mean(ETRmax, na.rm = TRUE), 
            mean.ek = mean(Ek, na.rm = TRUE), 
            mean.npq = mean(NPQmax, na.rm = TRUE), 
            mean.fvfm = mean(FvFm, na.rm = TRUE))
```

# Data Visualization

``` r
# CONDUCTANCE PLOT
# Let us create a plot that will communicate our key takeaway
# Main question: how does drought affect the performance of plants across populations, measured via stomatal conductance?
# Key finding: stomatal conductance varies amongst control and drought treatment groups across populations over time.
# Stomatal conductance data was collected on a weekly basis

conductanceplot <- means %>% # datasheet
  ggplot(aes(x = Week, # x-axis
             y = mean.cond, # y-axis
             color = Treatment)) + # colors
  geom_point() +  # data points
  geom_line() +  # plot
  labs(subtitle = "Detecting intraspecific variation in plasticity of rates of stomatal conductance in response to drought stress", # plot subtitle
       caption = "Data sourced from: Greene 2023", # plot caption
       x = "Time (Weeks)", # x-axis label
       y = "Mean Stomatal Conductance (mmol m^(-2) s^(-1)") + # y-axis label
  ggtitle("Effects of Drought on Leaf Stomatal Conductance in 'A'ali'i") + # plot title
  facet_wrap(~Population) + # create panels for each population!
  scale_color_manual(breaks= c("C", "PD"), labels = c("Control Group", "Pulse Drought Group"), values = c("darkblue", "maroon")) + # rename legend variables
  theme(plot.title = element_text(face = "bold", color = "black", hjust = 0.5), # bold title
        axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10), # adjust x-axis labels
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),   # adjust y-axis labels
        legend.position = "top", 
        plot.subtitle = element_text(size = 10, hjust = 0.5), 
        legend.title = element_blank())

# save plot to my output folder
ggsave(here("Project", "Output", "conductanceplot.png")) 

# view plot
conductanceplot
```

![](../Output/conductance%20plot-1.png)<!-- -->

``` r
# CHLOROPHYLL CONTENT

chlplot <- means %>% # datasheet
  ggplot(aes(x = Week, # x-axis
             y = mean.chl, # y-axis
             color = Treatment)) + # colors
  geom_point() +  # data points
  geom_line() +  # plot
  labs(subtitle = "Detecting intraspecific variation in plasticity of rates of chlorophyll content in response to drought stress", # plot subtitle
       caption = "Data sourced from: Greene 2023", # plot caption
       x = "Time (Weeks)", # x-axis label
       y = "Mean Chlorophyll Content") + # y-axis label
  ggtitle("Effects of Drought on Leaf Chlorophyll Content in 'A'ali'i") + # plot title
  facet_wrap(~Population) + # create panels for each population!
  scale_color_manual(breaks= c("C", "PD"), labels = c("Control Group", "Pulse Drought Group"), values = c("darkgreen", "brown")) + # rename legend variables
  theme(plot.title = element_text(face = "bold", color = "black", hjust = 0.5), # bold title
        axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10), # adjust x-axis labels
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),   # adjust y-axis labels
        legend.position = "top", 
        plot.subtitle = element_text(size = 10, hjust = 0.5), 
        legend.title = element_blank())

# save plot to my output folder
ggsave(here("Project", "Output", "chlplot.png")) 

# view plot
chlplot
```

![](../Output/chlorophyll%20plot-1.png)<!-- -->

``` r
# FV/FM Plot

fvfmplot <- means %>% # datasheet
  ggplot(aes(x = Week, # x-axis
             y = mean.fvfm, # y-axis
             color = Treatment)) + # colors
  geom_point() +  # data points
  geom_line() +  # plot
  labs(subtitle = "Detecting intraspecific variation in plasticity of photsynthetic yield in response to drought stress", # plot subtitle
       caption = "Data sourced from: Greene 2023", # plot caption
       x = "Time (Weeks)", # x-axis label
       y = "Mean Fv/Fm") + # y-axis label
  ggtitle("Effects of Drought on Leaf Stomatal Conductance in 'A'ali'i") + # plot title
  facet_wrap(~Population) + # create panels for each population!
  scale_color_manual(breaks= c("C", "PD"), labels = c("Control Group", "Pulse Drought Group"), values = c("orange", "maroon")) + # rename legend variables
  theme(plot.title = element_text(face = "bold", color = "black", hjust = 0.5), # bold title
        axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10), # adjust x-axis labels
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),   # adjust y-axis labels
        legend.position = "top", 
        plot.subtitle = element_text(size = 10, hjust = 0.5), 
        legend.title = element_blank())

# save plot to my output folder
ggsave(here("Project", "Output", "fvfmplot.png")) 

# view plot
fvfmplot
```

![](../Output/fvfm%20plot-1.png)<!-- -->

``` r
# NQPmax Plot

npqplot <- means %>% # datasheet
  ggplot(aes(x = Week, # x-axis
             y = mean.npq, # y-axis
             color = Treatment)) + # colors
  geom_point() +  # data points
  geom_line() +  # plot
  labs(subtitle = "Detecting intraspecific variation in plasticity of rates of maximum non-photochemical quenching values in response to drought stress", # plot subtitle
       caption = "Data sourced from: Greene 2023", # plot caption
       x = "Time (Weeks)", # x-axis label
       y = "Mean NPQ Max") + # y-axis label
  ggtitle("Effects of Drought on Leaf Stomatal Conductance in 'A'ali'i") + # plot title
  facet_wrap(~Population) + # create panels for each population!
  scale_color_manual(breaks= c("C", "PD"), labels = c("Control Group", "Pulse Drought Group"), values = c("violet", "maroon")) + # rename legend variables
  theme(plot.title = element_text(face = "bold", color = "black", hjust = 0.5), # bold title
        axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10), # adjust x-axis labels
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),   # adjust y-axis labels
        legend.position = "top", 
        plot.subtitle = element_text(size = 10, hjust = 0.5), 
        legend.title = element_blank())

# save plot to my output folder
ggsave(here("Project", "Output", "npqplot.png")) 

# view plot
npqplot
```

![](../Output/npq%20plot-1.png)<!-- -->

# Statistic Modeling: Chlorophyll Content Data

- PT.I: Primary interaction treatment  
- PT.II: Reduced models  
- PT.III: Comparison models

Data notes:  
- Effects of Drought Treatment on Chlorophyll Content (F-stat in drought
column)  
- Code developed with Kasey (with advice from Amber and Kyle):
2024-10-25.  
- Running the code with the (1\|Week:Treatment) interaction and anova
stuff.  
- Name of model: chl.primary.A/B (untransformed/log-transformed data for
primary interaction) - Chlorophyll: trait of focus  
- Treatment: fixed factor (drought)  
- Population: random factor  
- Population \* Treatment: random interaction  
- Week \* Treatment: fixed?? idk. Must ask Kasey.  
- Questions for Kasey: how do we determine whether the
log-transformed/untransformed data is ideal?

``` r
# PT.I  

# PRIMARY INTERACTION TREATMENT: UNTRANSFORMED DATA!  
# The F-statistic will be the final value for our Drought column in our results table.
# Reported value: F-stat
# RESULT: (F-stat) 3.5275, (p-val) 0.09882

chl.primary.A = lmer(Chlorophyll ~ Treatment + (1|Week) + (1|ID) + (1|Population) + (1|Population:Treatment) + (1|Population:Treatment:Week), data = cleandata, na.action = na.omit)

Anova(chl.primary.A, test.statistic = "F", type = 3)
```

    ## Analysis of Deviance Table (Type III Wald F tests with Kenward-Roger df)
    ## 
    ## Response: Chlorophyll
    ##                    F Df  Df.res    Pr(>F)    
    ## (Intercept) 775.2496  1 10.5566 3.181e-11 ***
    ## Treatment     3.5275  1  7.6588   0.09882 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# ANOVA RESULTS (POP + TREATMENT INTERACTION)
# Analysis of Deviance Table (Type III Wald F tests with Kenward-Roger df)
# Response: Chlorophyll
                   # F Df  Df.res    Pr(>F)    
# (Intercept) 775.2496  1 10.5566 3.181e-11 ***
# Treatment     3.5275  1  7.6588   0.09882 .   
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

``` r
# NORMALITY CHECK: UNTRANSFORMED  
  
# Shapiro-Wilk Normality Test
# Checking normality of residuals in untransformed data
# RESULT: (p-value) 1.124e-09

shapiro.test(resid(chl.primary.A)) 
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  resid(chl.primary.A)
    ## W = 0.98397, p-value = 1.124e-09

``` r
# Shapiro-Wilk normality test
# data:  resid(chl.primary.A)
# W = 0.98397, p-value = 1.124e-09
```

``` r
# Untransformed data: QQPlot  
qqnorm(resid(chl.primary.A))
```

![](../Output/chl%20qqnorm%20A-1.png)<!-- -->

``` r
# Log-transformed data
# This will be used in the next steps when we look at it as log-transformed data.

log.chl <- log(cleandata$Chlorophyll)
glimpse(log.chl)
```

    ##  num [1:1107] 3.66 3.28 3.26 3.56 3.53 ...

``` r
# PRIMARY INTERACTION: TRANSFORMED DATA!
# Reported value: F-stat
# RESULTS: (F-stat) 0.3662 +, (p-val) 0.5626

chl.primary.B = lmer(log.chl ~ Treatment + (1|Week) + (1|ID) + (1|Population) + (1|Population:Treatment) + (1|Population:Treatment:Week), data = cleandata, na.action = na.omit)

Anova(chl.primary.B, test.statistic = "F", type=3) 
```

    ## Analysis of Deviance Table (Type III Wald F tests with Kenward-Roger df)
    ## 
    ## Response: log.chl
    ##                     F Df  Df.res    Pr(>F)    
    ## (Intercept) 5012.0816  1 10.5148 1.976e-15 ***
    ## Treatment      0.3662  1  7.6474    0.5626    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Response: log.chl
                   # F Df  Df.res    Pr(>F)    
# (Intercept) 5012.0816  1 10.5148 1.976e-15 ***
# Treatment      0.3662  1  7.6474    0.5626    
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

``` r
# NORMALITY CHECK: TRANSFORMED DATA  

# Shapiro-Wilk Normality Test  
# Checking normality of residuals in log-transformed data
# RESULT: (p-value) < 2.2e-16

shapiro.test(resid(chl.primary.B))
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  resid(chl.primary.B)
    ## W = 0.8098, p-value < 2.2e-16

``` r
# Shapiro-Wilk normality test
# data:  resid(chl.primary.B)
# W = 0.8098, p-value < 2.2e-16
```

``` r
# Transformed data: QQPlot 
qqnorm(resid(chl.primary.B))
```

![](../Output/chl%20qqnorm%20B-1.png)<!-- -->

``` r
# PT.II   

# REDUCED MODELS!   
# This only tests the effect of Population type on the chlorophyll content results.  

chl.reduced.pop = lmer(Chlorophyll ~ Treatment + (1|Week) + (1|ID) + (1|Population), data = cleandata) 
# i removed the interaction and treatment in this code so we can just see the population effect

Anova(chl.reduced.pop)
```

    ## Analysis of Deviance Table (Type II Wald chisquare tests)
    ## 
    ## Response: Chlorophyll
    ##           Chisq Df Pr(>Chisq)  
    ## Treatment  6.58  1    0.01031 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Analysis of Deviance Table (Type II Wald chisquare tests)
# Response: Chlorophyll
          # Chisq Df Pr(>Chisq)  
# Treatment  6.58  1    0.01031 *
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

``` r
# This only tests the effect of pulse drought treatment on the chlorophyll content results
# we use glm because when we remove the "population" factor, we no longer have a random effect specified in the formula.

chl.reduced.dr = glm(Chlorophyll ~ Treatment, data = cleandata)
# i removed everything but the treatment data so we can see the treatment effect on chlorophyll content results

Anova(chl.reduced.dr)
```

    ## Analysis of Deviance Table (Type II tests)
    ## 
    ## Response: Chlorophyll
    ##           LR Chisq Df Pr(>Chisq)    
    ## Treatment   17.769  1  2.494e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Analysis of Deviance Table (Type II tests)
# Response: Chlorophyll
          # LR Chisq Df Pr(>Chisq)    
# Treatment   17.769  1  2.494e-05 ***
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

``` r
# PT.III

# COMPARISON MODELS!
# This compares the primary interaction model and population effect reduced model.
# Reported value: chisq
# The chisq value will be final result for our P X D column in our table.
# RESULT: (chisq) 67.711 ***, (p-val) 1.981e-15

anova(chl.primary.A, chl.reduced.pop)
```

    ## Data: cleandata
    ## Models:
    ## chl.reduced.pop: Chlorophyll ~ Treatment + (1 | Week) + (1 | ID) + (1 | Population)
    ## chl.primary.A: Chlorophyll ~ Treatment + (1 | Week) + (1 | ID) + (1 | Population) + (1 | Population:Treatment) + (1 | Population:Treatment:Week)
    ##                 npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
    ## chl.reduced.pop    6 7441.4 7471.5 -3714.7   7429.4                         
    ## chl.primary.A      8 7377.7 7417.8 -3680.9   7361.7 67.711  2  1.981e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Data: cleandata
# Models:
# chl.reduced.pop: Chlorophyll ~ Treatment + (1 | Week) + (1 | ID) + (1 | Population)
# chl.primary.A: Chlorophyll ~ Treatment + (1 | Week) + (1 | ID) + (1 | Population) + (1 | Population:Treatment) + (1 | Population:Treatment:Week)

                # npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# chl.reduced.pop    6 7441.4 7471.5 -3714.7   7429.4                         
# chl.primary.A      8 7377.7 7417.8 -3680.9   7361.7 67.711  2  1.981e-15 ***
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

``` r
# This compares the population effect reduced model (glm) and the treatment effect reduced model (lmer).
# Reported value: chisq
# The chisq value will be final result for our Population column in our table.
# RESULT: (chisq) 176.41***, (p-val) < 2.2e-16

anova(chl.reduced.pop,chl.reduced.dr)
```

    ## Data: cleandata
    ## Models:
    ## chl.reduced.dr: Chlorophyll ~ Treatment
    ## chl.reduced.pop: Chlorophyll ~ Treatment + (1 | Week) + (1 | ID) + (1 | Population)
    ##                 npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
    ## chl.reduced.dr     3 7611.8 7626.9 -3802.9   7605.8                         
    ## chl.reduced.pop    6 7441.4 7471.5 -3714.7   7429.4 176.41  3  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
