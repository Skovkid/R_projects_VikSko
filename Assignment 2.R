library(tidyverse)
library(ggplot2) 

# Assignment instructions
#1. Calculate mean and median for all variables that are numeric. Explain what they show.


#Start with reading the file into R

life_exp <- readxl::read_xlsx("hu2_lifeexp.xlsx") #Using the xlsx function to read into R

summary(life_exp) #Brief overview of the values we should expect to recieve from calculation.

#Viewed the relevant variables (columns) for this excersise.
life_exp <- life_exp %>% 
  select(life_expectancy,gdp,women_econ_op,gini) -> variables_are #Then made a new table for the next step

variables_are  #Displaying the new table

Mean_val<- sapply(variables_are, mean)  #Calculating the mean value
Mean_val #Displaying mean

Median_val<-sapply(variables_are, median)  #Calculating the *median* value
Median_val #Displaying *median*

joint_table <- rbind(Mean_val, Median_val) #Using rbind to merge the two tables together.
joint_table




#2. Create a histogram for each of the numeric variables (in total four histograms).

#Save each histogram as a .png file on your computer.

#Histogram life expectancy
life_exp %>%
  ggplot(aes(x=life_expectancy)) + geom_histogram(bins=100, color = "black", fill="gray40") -> lifeExp_hist


#Histogram GDP
life_exp %>%
  ggplot(aes(x=gdp)) + geom_histogram(bins=100, color = "blue", fill="gray30") -> GDP_hist

#Histogram Women_Econ

life_exp %>%
  ggplot(aes(x=women_econ_op)) + geom_histogram(bins=100, color = "pink", fill="gray70" ) -> Women_hist


Women_hist %>% 
  ggsave(filename = "women.png", device = "png")

#Histogram Gini
life_exp %>%
  ggplot(aes(x=gini)) + geom_histogram(bins=100, color = "yellow", fill="gray30") -> Gini_hist

histograms<- list(lifeExp_hist, GDP_hist, Women_hist, Gini_hist)
histograms #Saved all of my histograms in a list. The idea here is to loop through each element and making saving as png

#I have tried every way possible to find an adequate for loop to convert every element of the list histograms
#I had no success in doing so. Therefore I have opted to do the manual solution of repeating the saving process

#The issue I found was that I wasn't sucessfull in looping through each element of the list that I had created.
#The idea I had was to loop through each element in a for-loop and concatinanting each filename element with ".png".
#I was not successful in achieving this beacause I don't know how to perform actions on single elements in the loops.

lifeExp_hist%>% 
  ggsave(filename = "lifeExp.png", device = "png")

GDP_hist %>% 
  ggsave(filename = "GDP_hist.png", device = "png")

Women_hist %>% 
  ggsave(filename = "women.png", device = "png")

Gini_hist %>% 
  ggsave(filename = "Gini_hist.png", device = "png")




#3.

# a) Estimate three regression models. In all three, chose life_expectancy as the dependent variable(y).
#Independent variables(x) should for each model be one of the remaining hree numeric variables.
#In other words, the following three models should be estimated:

# life_expectancy ~ gdp

# life_expectancy ~ women_econ

# life_expectancy ~ gini.

# b) save the results in a table 

