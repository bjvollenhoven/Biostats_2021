#Bevan Vollenhoven 
#Snakes - understanding ANOVA
#Homework
#04 April 2021

library(tidyverse)
library(lubridate)
library(ggplot2)
library(Rmisc)
library(dplyr)

snakes <- read.csv("data/snakes.csv")
snakes$day = as.factor(snakes$day) #converting day to a factor as it is a continuous variable
#as.factor changes it to factorial date

snakes.aov <- aov(openings ~ as.factor(day) + snake, data = snakes)
summary(snakes.aov) #p values if greater than 
head(snakes.aov)
str(snakes.aov)
glimpse(snakes.aov)

snakes.summary <- snakes %>% 
  group_by(snake, day) %>% #this is it not get the summarized values of all data in a column combined 
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings)) %>% 
  ungroup()
snakes.summary
tail(snakes.summary)
#it is not able to to calcualte the standard deviation of openings.
#grouping by both day and snake causes this error.

snakes.summary <- snakes %>% 
  group_by(day) %>% #grouping data just according  to the day.
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings)) %>% 
  ungroup()
snakes.summary
head(snakes.summary)
tail(snakes.summary)
glimpse(snakes.summary)

# -------------------------------------------------------------------------

snakes.summary2 <- summarySE(data = snakes, measurevar = "openings", groupvars = c("day"))
#this will give the count, mean, standard deviation, standard error of the mean, 
#and confidence interval (default 95%). 
head(snakes.summary2)
glimpse(snakes.summary2)
snakes.summary2


# -------------------------------------------------------------------------



ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, aes(x = day, xend = day, y = openings - ci, yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

# -------------------------------------------------------------------------

plot_2 <- ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, aes(x = day, xend = day,y = openings - ci, yend = openings + ci, colour = day),
               #setting the data being inputed into each segment
               #x, xend,y, and yend signify the ending points and starting points of the segment.
               size = 2, linetype = "solid") +
  geom_boxplot(aes(fill = day), alpha = 0.5) +
  labs(x = "Day", y = "Number of openings", title = "Mean number of opening of the lid per day") +
  geom_jitter(width = 0.05) +
  theme_bw() +
  theme(legend.position = "none")
plot_2 

# -------------------------------------------------------------------------
snake_pnt <- ggplot(data = snakes, aes(x = snake, y = openings, fill = snake)) +
  geom_bar(stat = "identity") +
  labs(x = "Snake", y = "Number of openings", title = "Number of openings per snake") +
  theme_grey() +
  theme(legend.position = "none")

snake_pnt
# -------------------------------------------------------------------------
snake_pnt2 <- ggplot(data = snakes, aes(x = day, y = openings, fill = snake)) +
  geom_bar(position = "dodge", stat = "identity", col = "black") +
  #scale_fill_brewer(pallete = "Pastel2") +
  scale_fill_manual(values = c("orangered4", "orange4", "olivedrab4", "palegreen3", "plum2", "tomato2")) +
  labs(x = "Day", 
       y = "Openings",
       title = "Comparitive bar graph",
       subtitle = "Number of openings per snake for each day") +
  theme_bw() 
 
snake_pnt2

# -------------------------------------------------------------------------

snakes.aov <- aov(openings ~ day + snake, data = snakes) #"~" signifies that you
#are explaining anova of openings with reference to day and snake as variables.
summary(snakes.aov) 

par(mfrow = c(2, 2))
# Checking assumptions...
# make a histogram of the residuals;
# they must be normal
snakes.res <- residuals(snakes.aov)
hist(snakes.res, col = "red")

plot(fitted(snakes.aov), residuals(snakes.aov), col = "red")

snakes.tukey <- TukeyHSD(snakes.aov, which = "day", conf.level = 0.90)
plot(snakes.tukey, las = 1, col = "red")






