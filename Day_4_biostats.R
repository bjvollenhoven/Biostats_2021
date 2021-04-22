#Bevan Vollenhoven (3840572)
#Day_4
#Basic Biostats 
#22 APril 2021

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



# confidence intervals ----------------------------------------------------


Input <- ("
Student  Sex     Teacher  Steps  Rating
a        female  Jacob    8000   7
b        female  Jacob    9000  10
c        female  Jacob   10000   9
d        female  Jacob    7000   5
e        female  Jacob    6000   4
f        female  Jacob    8000   8
g        male    Jacob    7000   6
h        male    Jacob    5000   5
i        male    Jacob    9000  10
j        male    Jacob    7000   8
k        female  Sadam    8000   7
l        female  Sadam    9000   8
m        female  Sadam    9000   8
n        female  Sadam    8000   9
o        male    Sadam    6000   5
p        male    Sadam    8000   9
q        male    Sadam    7000   6
r        female  Donald   10000  10
s        female  Donald    9000  10
t        female  Donald    8000   8
u        female  Donald    8000   7
v        female  Donald    6000   7
w        male    Donald    6000   8
x        male    Donald    8000  10
y        male    Donald    7000   7
z        male    Donald    7000   7
")

data <- read.table(textConnection(Input),header = TRUE)
summary(data)
str(data)

#boostraping - the method used for CI.
#mimicks the sample process multiple times

groupwiseMean(Steps ~ 1,data = data, conf = 0.95, digits = 3)
#steps ~ 1 tell the program to calculate the overall mean 
#is calculates the confidence intervals and gives the upper and lower CI
#trad. low/upper - using an equation of a t-distribution. 
#mean is 7690 is half and it was between lower and upper CI.

groupwiseMean(Steps ~ Sex, data = data, conf = 0.95,digits = 3)
#calculating the mean and CI of steps as a function of sex. 


groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)
#calculating the mean and CI of steps as a function of sex and teacher.
gender

#creating appropriate graph
sex <- groupwiseMean(Steps ~ Sex, data = data, conf = 0.95,digits = 3) 
data2 <- ggplot(data = sex, aes(x = Sex, y = Mean)) +
  geom_boxplot()
 
data2

# -------------------------------------------------------------------------

out <- groupwiseMean(Steps ~ Sex, data = data, conf = 0.95,digits = 3)

sex <- ggplot(data = out) +
  geom_col(aes(x = Sex, y = Mean), fill = "red", col = "black") +
  geom_errorbar(aes(ymin = Trad.lower,
                    ymax = Trad.upper,
                    x = Sex),
                col = "black",
                width = 0.2) +
  theme_bw()

sex

#students are equally lazy
#error bars slight overlap, not that much of a difference

teach <- groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)
teacher <- ggplot(data = teach) +
  #  group_by(Teacher) +
  geom_col(aes(x = Sex, y = Mean), fill = "purple", col = "black", alpha = 0.5) +
  geom_errorbar(aes(ymin = Trad.lower,
                    ymax = Trad.upper,
                    x = Sex),
                col = "black",
                width = 0.2) +
  ylab("Mean number of steps") +
  xlab("Sex") +
  labs(title = "Mean number of steps", subtitle = "Steps per gender for each teacher") +
  facet_wrap(~Teacher, ncol = 3) +#add this line to separate by teacher
  theme_cleveland()
teacher



#teacher1 <- ggplot(teach,aes(x = Sex, y = Mean, group = Teacher))+
 #geom_col(aes(x = Sex, y = Mean)) 
#teacher1
ggplot(data = teach) +
  geom_col(aes(x = Sex, y = Mean), fill = "blue", col = "black") +
  geom_errorbar(aes(ymin = Trad.lower,
                    ymax = Trad.upper,
                    x = Sex),
                col = "black",
                width = 0.2) +
  facet_wrap(~Teacher, ncol = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Sex") + ylab("Steps")
  

#CI is a good way to visualize a broad way about what your data says

# -------------------------------------------------------------------------

# by bootstrapping - repeats all your data
#one-way
groupwiseMean(Steps ~ Sex,
              data = data,
              conf = 0.95,
              digits = 3,
              R = 10000,
              boot = TRUE,
              traditional = FALSE,
              normal = FALSE,
              basic = FALSE,
              percentile = FALSE,
              bca = TRUE)
anova <- aov(Steps~Sex*Teacher, data = data)
summary(anova)

anova_Tukey <- TukeyHSD(anova)
plot(anova_Tukey)
#there is no significant difference between the data


#two way
groupwiseMean(Steps ~ Teacher + Sex,
              data = data,
              conf = 0.95,
              digits = 3,
              R = 10000,
              boot = TRUE,
              traditional = FALSE,
              normal = FALSE,
              basic = FALSE,
              percentile = FALSE,
              bca = TRUE)
