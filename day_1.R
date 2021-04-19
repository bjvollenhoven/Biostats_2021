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


