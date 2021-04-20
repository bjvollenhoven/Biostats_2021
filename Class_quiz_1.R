#Bevan Vollenhoven (3840572)
#Class Quiz 1
#20 April 2021
library(tidyverse)
library(lubridate)
library(ggpubr)
library(dplyr)
library(dslabs)
library(ggplot2)
library(e1071)

# Question 1 --------------------------------------------------------------

#List the various data classes
#Quantitative data includes:
#Discrete Data
#Continuous Data
#Dates

#Qualitative data includes:
#Categorical data
#Ordinal Data
#Binary Data
#Character Data

#List some of the functions used to view your data in R
#view()
#glimpse()
#head()
#tail()
#str()
#names()
#summary()

#Discuss skewness and kurtosis
#Skewness indicates the symmetry of your data. If your data is skewed positively
#then majority fo your data lies above the median. IF your data is skewed negatively
#then majority of your data lies lower than the mediun.

#If your data is positive then kurtosis=0 and your data is mesokurtic. IF kurtosis 
#is negative then your data is polykurtic. If kurtosis is posertive then your data 


# Question 2 --------------------------------------------------------------
orange <- datasets::Orange

str(orange)
glimpse(orange)
head(orange)

#Explain what type of data class orange belongs to
#This would belong to numerical data class (continuous data)

#Apply correct function to show first 6 and last 6 rows, column names and summary 
#stats of the data
head(orange)
tail(orange)
colnames(orange)
summary(orange)

#Determine mean, median, and standard deviation of the age and circumference of the 
#oranges for each of the trees. 
org_stats_circumference <- orange %>% 
  group_by(Tree) %>% 
  summarise(org_mean = mean(circumference),
            org_median = mean(circumference),
            org_sd = sd(circumference))

org_stats_circumference 
head(org_stats_circumference)
colnames(org_stats_circumference)

org_stats_age <- orange %>% 
  group_by(Tree) %>% 
  summarise(org_mean = mean(age),
            org_median = mean(age),
            org_sd = sd(age))

org_stats_age
summary(org_stats_age)
head(org_stats_age)
glimpse(org_stats_age)

#Determine skewness and kurtosis of orange data.
kurtosis(orange$age)
#kurtosis is negative signifying that the data is platykurtic. 
kurtosis(orange$circumference)
#kurtosis is negative signifying that the data is platykurtic. 

skewness(orange$age)
#The negative skewness value indicates that majority of the data for age is lower
#than the median age of orange trees.
skewness(orange$circumference)
#the positive skewness value indicates that majority of the data for circumference 
#is high than the median circumference of orange trees.


# use summary to generate min, max, etc.  ---------------------------------

org_stats1 <- orange %>% 
  select(-Tree, -age) %>% 
  summarise(org_min = min(circumference),
            org_q1 = quantile(circumference, p = 0.25),
            org_q3 = quantile(circumference, p = 0.75),
            org_max = max(circumference))
org_stats1
head(org_stats1)

#plots
org_line <- ggplot(data = orange, aes(x = age, y = circumference, fill = Tree)) +
  geom_point(aes(colour = Tree))+
  geom_smooth(method = "lm") +
  labs(x = "Age (Years)", y = "Circumference", title = "Relationship between age and tree circumference") +
  facet_wrap(~Tree, ncol = 2) +
  theme_bw()
org_line
#there is a linear relationship between age and circumference of orange tree. The
#older a tree is the larger its Circumference

org_box <- ggplot(data = orange, aes(x = circumference, fill = Tree)) 


# Question 3 --------------------------------------------------------------

#mutate()
#This function is used when creating and editing a new column

#select()
#this function is used to only filter in data belonging to a specific column.

#group_by
#this function groups data according to a specific variable. 

#filter()
#this function is used to filter out specific data belonging to specific rows.

#separate()
#this function is used when tidying data. It is used to seperate data found in rows
#that have more than one observation within them.







