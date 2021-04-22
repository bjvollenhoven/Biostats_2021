#Biostats 
#Quiz 2
#Bevan Vollenhoven 
#3840572
#22 April 2021

library(tidyverse)
library(lubridate)
library(ggplot2)
library(Rmisc)
library(dplyr)
library(ggplot2)
library(corrplot)
library(ggpubr)
library(reshape)
library(plyr)
library(plotly)
library(rcompanion)

# question 1 --------------------------------------------------------------
orange <- datasets::Orange
tooth <- datasets::ToothGrowth
warp <- datasets::warpbreaks

# orange ------------------------------------------------------------------
orange
head(orange)
glimpse(orange)
str(orange)

orange %>% 
  group_by(Tree) %>% 
  summarise(tree_var = var(circumference))

orange_plot <- ggplot(data = orange, x = age, y = circumference) +
  geom_bar(stat = "identity")
orange_plot

orange2 <- aov(circumference ~ age + Tree, data  = filter(orange, Tree %in% c(1:5)))
summary(orange2)
#p value < 0.05 for age, therefore age does have significance on circumference
#p < 0.05 for Tree, therefore the tree itself also has significance on circumference

# toothgrowth -------------------------------------------------------------
tooth
glimpse(tooth)
head(tooth)
tail(tooth)
str(tooth)

t_plot <- ggplot(data = tooth, aes(x = dose, y = len, fill = supp)) +
  geom_bar(stat = "identity")
t_plot

tooth2 <- aov(len ~ supp + dose, data = filter(tooth, supp %in% c("OJ", "VC")))
summary(tooth2)

#p value < 0.05 for supp, therefore supp does have significance on tooth length.
#p < 0.05 for dose, therefore the dosage of supp has significance on tooth length.

# warpbreaks --------------------------------------------------------------

warp
summary(warp)
#mean = 28.15

warp_n %>% 
  group_by(wool) %>% 
  summarise(norm_breaks = as.numeric(shapiro.test(breaks)[2]))

warp_h %>% 
  group_by(wool) %>% 
  summarise(wool_var = var(breaks))

t.test(warp$breaks, mu = 28.15)
#p > 0.05, therefore the data is normaly distributed 

# question2 ---------------------------------------------------------------

load("data/SACTN_daily_v4.2.RData")
view(SACTN_daily_v4.2)
head(SACTN_daily_v4.2)
tail(SACTN_daily_v4.2)
glimpse(SACTN_daily_v4.2)

sactn_tidy <- SACTN_daily_v4.2 %>% 
  separate(col = date, 
           into = c("year", "month", "day"),
           sep = "-")
view(sactn_tidy)

sactn_tidy2 <- sactn_tidy %>% 
  separate(col = index,
           into = c("Site", "Src"),
           sep = "/")
view(sactn_tidy2)

sactn_plot <- ggplot(data = sactn_tidy2, aes(x = temp, fill = Site)) +
  geom_bar() +
  group_by(Site, Src) 
