#Bevan Vollenhoven (3840572)
#Biostatistic and Intro_R recap assignment: 
#Data Manipulation, Analyses and Visualization
#19 April 2021

library(tidyverse)
library(lubridate)
library(ggpubr)
library(dplyr)
library(dslabs)
library(ggplot2)
options(scipen = 100)
# section 1 ---------------------------------------------------------------
data(package = .packages(all.available = TRUE))
view(BOD)
#C BOD is tidy: each row is an observation with two values (time and demand)


# section 2 ---------------------------------------------------------------
data(package = .packages(all.available = TRUE))

data("murders")
head(murders)
tail(murders)
glimpse(murders)
view(murders)
str(murders)

#This data set indicates the the number of gun related deaths per state in 
#various regions of the USA. The data comprises of both nominal and qualitative
#data. 

murders_pop_size <- murders %>% 
  select(population, state)
head(murders_pop_size)
tail(murders_pop_size)
view(murders_pop_size)
str(murders)

murders_flo <- murders %>% 
  filter(state != "Florida")
tail(murders_flo)
view(murders_flo)

no_south <- murders %>% 
  filter(region != "South")
head(no_south)
str(no_south)
view(no_south)

south <- murders %>% 
  filter(region == "South")
head(south)
tail(south)
nrow(south)
view(south)
unique(south$state)
#there are seventeen states found in the southern region

comb_pop <- murders %>% 
  filter(region %in% c("South", "West")) %>% 
  arrange(region)

pop_south <- murders %>% 
  filter(region == "South")
sum(pop_south$population)
  
pop_south
#the population of the southern region consists of 115 674 434 people

pop_west <- murders %>% 
  filter(region == "West")
sum(pop_west$population)
#the population in the western region consists of 71 945 553 people.

only_north <- murders %>% 
  filter(region == "Northeast")
head(only_north)
tail(only_north)
glimpse(only_north)
view(only_north)

ne <- only_north %>% 
  select(region, population)
ne  
head(ne)
tail(ne)


# plot 1 ------------------------------------------------------------------

south <- murders %>% 
  filter(region == "South")
  
  
mur_plot1 <- ggplot(data = south, aes(x = state, y = total, fill = state)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = population), vjust = 0) +
  labs(x = "State", y = "Total gun murders") +
  ggtitle("Gun murders State in the southern region") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) 
  
mur_plot1 
#states that have the most population have the most gun murders


# plot 2 ------------------------------------------------------------------


library(ggrepel)

north_plot2 <- ggplot(data = only_north, aes(x = population, y = total)) +
  geom_text_repel(aes(label = state), size = 3.5) +
  geom_point(aes(colour = state)) +
  geom_smooth(method = "lm") +
  labs(x = "Population size", y = "Total gun murders", title = "Gun murders per state in the northeast region") +
  theme(axis.text.x = element_text(angle = 90)) 
 
  
north_plot2
#larger populations within the north east region. The linear model show the linear
#relationship between population size and total number of gun murders


# Compare with population size of the South with the population si --------

south_west <- murders %>% 
  filter(region %in% c("South", "West")) %>% 
  group_by(region) %>% 
  summarise(mean_pop = mean(population))
 
sw_plot <- ggplot(data = south_west, aes(x = region, y = mean_pop, fill = region)) +
  geom_col() +
  labs(x = "Region", y = "Mean Population", title = "Comparitve bar graph: South vs West mean population") +
  theme_bw() +
  theme(legend.position = "none")
  
sw_plot
# Create a new data frame where the total>20 but <100 and to exclu --------

new_data <- murders %>% 
  filter(total %in% (20:100)) %>% #filer data >20 and <100
  arrange(total) #arrange it numerically
head(new_data)
tail(new_data)
#120 is automatically omitted as it fall out of the range
#of desired data.

slc <- murders %>% 
  slice(10:24, 26) #index rows 10 to 24 and row 26.
slc
view(slc)
view(murders)

murders_tibble <- as_tibble(murders)
view(murders_tibble)
view(murders)

murders_tibble %>% 
  group_by(region) %>% 
  arrange(region)

view(murders_tibble)

# section 3 ---------------------------------------------------------------

data(heights) 
head(heights)
tail(heights)
str(heights)
view(heights)
glimpse(heights)

#The data is tidy as each row is an observation. The matrix contains both numerical
#(heieght) and categorial (sex) data. 
?height
heights_stats <- heights %>% 
  group_by(sex) %>% 
  summarise(height_ave = mean(height),
            height_st = sd(height),
            height_median = median(height),
            height_min = min(height),
            hieght_max = max(height))
heights_stats    
glimpse(heights_stats)
head(heights_stats)
tail(heights_stats)
str(heights_stats)
view(heights_stats)


# section 4 ---------------------------------------------------------------

#question A
x <-  c( 1, 6, 21, 19 , NA, 73, NA) 
sum(is.na(x))

y <- c(NA, NA, 3, NA, 13, 24, NA)
sum(is.na(y))

#question B
my_son <- function(x){
  x <- (sum(is.na(x)))
 # y <- (sum(is.na(y)))
  return(x)
 
}

my_son(x) 
my_son(y)

#question C
cat <- c(3,NA, 4, 5, 7, NA, NA, NA)
my_son(cat)

dog <- c(NA, 4, 5, 6 ,7, NA)
my_son(dog)


# section 5 ---------------------------------------------------------------

seasonal_data <- data.frame(year = c(2015, 2016, 2017, 2018),
                            winter = c(41, 39, 47, 40),
                            spring = c(41, 46, 57, 45),
                            summer = c(75, 52, 85, 66),
                            Autumn = c(57, 66, 52, 56))
seasonal_data
#Hypothesis
#Summer average temperature will be high than the rest of the seasons across all 
#four years.Winter average temperature will be the lowest in all four years. 

seasonal_data_tidy <- seasonal_data %>% 
  gather(winter, spring, summer, Autumn, key = "season", value = "Temp") %>% 
  group_by(season) %>% 
  summarise(mean_temp = mean(Temp))
seasonal_data_tidy

# plot 1 ------------------------------------------------------------------

season_1 <- ggplot(data = seasonal_data_tidy, aes(x = season, y = mean_temp, fill = season)) +
  geom_bar(stat = "identity") +
  labs(x = "Season", y = "Temp (°F)", title = "Mean temperature (°F) over four years per season") +
  theme_bw() + 
  theme(legend.position = "none")
  
season_1

# plot 2 ------------------------------------------------------------------
seasonal_data_tidy2 <- seasonal_data %>% 
  gather(winter, spring, summer, Autumn, key = "season", value = "Temp") %>% 
  group_by(season)

seasonal_data_tidy2  

season_2 <- ggplot(data = seasonal_data_tidy2, aes(x = season, y = Temp, fill = season)) +
  geom_bar(stat = "identity") +
  labs(x = "Season", y = "Temp (°F)", title = "Average seasonal temperature per year") +
  facet_wrap(~year, ncol = 2) +
  theme_bw() +
  theme(legend.position = "none")
  

season_2

#PLot 1 shows how summer has the highest seasonal temperature over an average of
#four years. Plot 2 indicates how in each year summer has the highest seasonal temperature

# seperate() function split the position column into new three column
cats_data<- tibble(cats = c("A", "B", "C"),
                   position = c("1-2-3", "3-1-2", "2-3-1"),
                   minutes = c(3, 3, 3),
                   seconds = c(12, 44, 15))
cats_data

cats_tidy <- cats_data %>% 
  separate(col = position, into = c("first_place", "second_place", "third_place"), sep = "-")
cats_tidy
head(cats_tidy)
glimpse(cats_tidy)
str(cats_tidy)

cats_tidy
cats_tidy_2 <- cats_tidy %>% 
  mutate(total_time = seconds*0.0166667+minutes) %>% #converts th seconds to minutes and then joins the columns into the new column 
  select(-minutes, -seconds)

cats_tidy_2

# section 6 ---------------------------------------------------------------

data("faithful")
faithful 

faithful_gather <- faithful %>% 
  gather(eruptions, waiting, #select the columns i want to gather
         key = "untidy", #assign a new column heading for the old column names that will now be variables
         value = "values") #name the new column where my gathered data will be found
faithful_gather
head(faithful_gather)
glimpse(faithful_gather)

#Spread
f_spread <- faithful %>% 
  spread(key = eruptions, #choose the column that contains more than one variable
             value = val) #indicate the name of the column that contains the values that need to be spread
#will not work for my chosen data as it is already neat.

#separate()
f_separate <- faithful %>% 
  separate(col = eruptions, #the name of the column that needs to be separated
           into = c("now", "then"), #what the values are separated into
           sep = ".")#what the values are separated by in the column.
#will not work for my chosen data as it is already neat.

#joining()
f_join <- faithful %>% 
  left_join(faithful_gather, f_spread) #joines data fromt two seperate datasets
#will not work for my chosen data as it is already neat.

#arrange
f_arrange <- faithful %>% 
  arrange(eruptions)#arrange rows in ascending order based on the values in the eruptions column
head(f_arrange)
tail(f_arrange)

#select
f_select <- faithful %>% 
  select(eruptions) #selected to only show the eruptions column
head(f_select)
glimpse(f_select)

#group_by
f_gb <- faithful %>% 
  group_by(waiting). #grouped according to rows that say waiting. Mainly used for 
#grouping things according to qualitative data 

#mutate
f_mut <- faithful %>% 
mutate(double_erruptions = eruptions*2) #used mutate to add a new column containing the 
#modified data
f_mut
