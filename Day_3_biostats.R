#Day_3_biostats
#Simple linear regressions
#21 APril 2021
#Bevan Vollenhoven 

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


#comparing the influence of one variable to another
data(faithful)
head(faithful)

eruption.lm <- lm(eruptions ~ waiting, data = faithful)
summary(eruption.lm)

slope <- round(eruption.lm$coef[2], 3)
# p.val <- round(coefficients(summary(eruption.lm))[2, 4], 3) # it approx. 0, so...
p.val = 0.001
r2 <- round(summary(eruption.lm)$r.squared, 3)

ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "salmon") +
  labs(title = "Old Faithful eruption data",
       subtitle = "Linear regression",
       x = "Waiting time (minutes)",
       y = "Eruption duration (minutes)") +
  theme_bw()
#if you can use anova or t-test you can linear regression


# correlations  -----------------------------------------------------------

 #does a correlation plot
#reading in data
ecklonia <- read_csv("data/ecklonia.csv")


ecklonia_sub <- ecklonia %>% 
  select(-species, - site, - ID) #not select these variables
#do this to remove the words


# pearson -----------------------------------------------------------------
#used for continuous data
# Perform correlation analysis on two specific variables
# Note that we do not need the final two arguments in this function to be stated
# as they are the defaut settings.
# They are only shown here to illustrate that they exist.
cor.test(x = ecklonia$stipe_length, ecklonia$frond_length,
         use = "everything", method = "pearson")

#correlation is 0.6524911
#closer the value is to one, the stronger the correlation

ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson


# spearman ----------------------------------------------------------------
#used for ordinal data
cor.test(x = ecklonia$stipe_length, ecklonia$frond_length,
         use = "everything", method = "spearman")
ecklonia$length <- as.numeric(cut((ecklonia$stipe_length+ecklonia$frond_length), 
                                  breaks = 3))

cor.test(ecklonia$length, ecklonia$digits)


# kendall  ----------------------------------------------------------------
#will work for both
#allows us to perform correlation on non-normally distributed data
ecklonia_norm <- ecklonia_sub %>% 
  gather(key = "variable") %>% 
  group_by(variable) %>% 
  summarise(variable_norm = as.numeric(shapiro.test(value)[2]))
ecklonia_norm

cor.test(ecklonia$primary_blade_length, 
         ecklonia$primary_blade_width, 
         method = "kendall")

# one panel vision --------------------------------------------------------

# Calculate Pearson r beforehand for plotting
r_print <- paste0("r = ", #r_print <- : stores the function into an object
                  round(cor(x = ecklonia$stipe_length, ecklonia$frond_length),2))
# Calculate Pearson r beforehand for plotting
ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length)) +
  geom_smooth(method = "lm", colour = "grey90", se = F) + #se = f, removes standard error bar
  geom_point(colour = "mediumorchid4") +
  geom_label(x = 300, y = 240, label = r_print) +
  labs(x = "Stipe length (cm)", y = "Frond length (cm)") +
  theme_pubclean()


# Multiple panel visual ---------------------------------------------------
#clear plot history before running this 
c_plt <- corrplot(ecklonia_pearson, method = "circle") 
c_plt


# excercise 1 -------------------------------------------------------------

ht <- heatmap(ecklonia_pearson, scale = "none",
              Rowv = NA, Colv = NA,#this removes dendrogram
              ) 
#ggplot methods

data_melt <- melt(ecklonia_pearson)

ht_map <- ggplot(data_melt, aes(X1, X2)) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient(low = "black", high = "peru") +
  labs(title = "Heatmap", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 90)) 
  
?geom_tile  
ht_map
# RowSideColors = rep(c("blue", "pink"), each = 16),
# ColSideColors = c(rep("purple", 5), rep("orange", 6)))


# dlply -------------------------------------------------------------------

data(package = .packages(all.available = TRUE))

data("faithful")
faithful

faith <- ddply(faithful, c)

#bnames <- ddply(bnames, c("sex", "year"), transform, 
#rank = rank(-percent, ties.method = "first"))
