#DAy1 - Biostatistics
#19 April 2021

library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(scales)
library(ggsn)
library(maps)

data()
data(package = .packages(all.available = TRUE)) #shows all the information about the available packages

str() #gives information about the type of data contained in the dataset fround in R.
#num = numerical/floating point
summary(BOD) #gives you the mean, median, max and various quantiles
#since you can get these values the data must be numerical. 

str(InsectSprays)
#contains factors - this would be catergorial data)
view(InsectSprays)
unique(InsectSprays$spray)

str(Loblolly)
?Loblollyone 
#one observation per row
#seed is an ordered factor = this is an ordinal factor

#Pick 5 data and see what they are 
str(Orange)

str(airquality)
view(airquality)

str(rivers)
view(rivers)

str(beaver1)
str(beaver2)

str(faithful)
?faithful
view(faithful)

str(Nile)
view(Nile)
?Nile


# loblolly ----------------------------------------------------------------
#load built-in data o loblolly
pine <- Loblolly
str(pine) #structure of the data
head(pine)
view(pine)
class(pine$height) #gives you the class of the data within a particular data set

# descriptive statistics --------------------------------------------------

# calculate sample size ---------------------------------------------------
chicks <- as_tibble(ChickWeight)
nrow(chicks)
view(chicks)

unique(chicks$Chick) #unique counts the amount of unique characters in the column
#note the distinction between 'nrow()' and the 
#'true' sample size
#n = 50

#Above, what is the difference between the two?

#normal distribution, mean = median
#we strive to have data that is normally distributed. 

chick_mean <- chicks %>% 
  filter(Time == 20) %>% 
  group_by(Diet) %>% #finding mean per diet
  summarise(chick_mean = mean(weight))

view(chick_mean)

#mean and median of whole group and std
chick_1 <- chicks %>% 
  filter(Time == 20) %>% 
  summarise(chick_mean = mean(weight),
  chick_sd = sd(weight),
  chick_median = median(weight))
view(chick_1)

#mean, median, and std per diet
chick_2 <- chicks %>% 
  filter(Time == 20) %>% 
  group_by(Diet) %>% 
  summarise(chick_mean = mean(weight),
            chick_sd = sd(weight),
            chick_median = median(weight))

kurtosis(chicks$weight) #leptokurtic

library(e1071)

#mean calulcutaed manually 
chick_man <- chicks %>% 
  filter(Time == 20) %>% 
  summarise(chicks = mean(weight),
            chicks_St = sd(weight),
            chicks_med = median(weight), 
            mean_wt = sum(weight) / n())

#quartile 
chick_qrt <- chicks %>% 
  filter(Time == 20) %>% 
  group_by(Diet) %>% 
  summarise(chicks = mean(weight),
            chicks_St = sd(weight),
            chicks_med = median(weight), 
            mean_wt = sum(weight) / n(),
            min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25),
            med_wt = median(weight),
            qrt3_wt = quantile(weight, p = 0.75),
            max_wt = max(weight))
           
view(chick_qrt)
chick_min_max <- chicks %>% 
  filter(Time == 20) %>% 
  group_by(Diet) %>% 
  summarise(chicks = mean(weight), 
            chicks_St = sd(weight),
            chicks_med = median(weight), 
            lower_wt = range(weight)[1],
            upper_wt = range(weight)[2])

view(chick_min_max)            
           
#find min and max


dat1 <- c(NA, 12, 76, 34, 23)
mean(dat1) #gives and error as you have a missing value
mean(dat1, na.rm = TRUE)




#skewness tells us whether the tails of distribution are long tail or short tail
#kurtosis tells you if its left or right skewed.

#firts 5 masses of group 2

