library(tidyverse)
library(ggplot2) 
library(stargazer)

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



#This is the kind of funciton I was trying to implement but didn't manage to due because of my limited knowledge and know-how.

#for (i in 1:length(histograms)) {
 # histograms[i] <- ggsave(filename = as_string(i), device = "png)
#}




#3.

# a) Estimate three regression models. In all three, chose life_expectancy as the dependent variable(y).
#Independent variables(x) should for each model be one of the remaining hree numeric variables.
#In other words, the following three models should be estimated:

# life_expectancy ~ gdp

# life_expectancy ~ women_econ

# life_expectancy ~ gini.

# b) save the results in a table 




#Regression model

# Y = a+b*X*u



#Saving the model in an object

#I hate my own code and feel really incompetent doing this but I am not aware capable of finding another solution :´(

lg_model_1 <- life_exp$life_expectancy ~ log(gdp)
lg_model_2 <- life_exp$life_expectancy ~log(women_econ_op)
lg_model_3 <- life_exp$life_expectancy ~log(gini)


#Calculating the model with the lm-model (I hate this :( )

lg_estimation_1 <- lm(lg_model_1, data = life_exp)#gdp
lg_estimation_2 <- lm(lg_model_2, data = life_exp) #women
lg_estimation_3 <- lm(lg_model_3, data = life_exp)#gini



#Solving part B

Reg_table <- stargazer(lg_estimation_1,lg_estimation_2,lg_estimation_3, type = "text", digits=1)
Reg_table
