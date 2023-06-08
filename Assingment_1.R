library(tidyverse)
library(ggplot2)

#load the rds file

hu1_lifeexp.rds <- file.choose()
life_exp <- readRDS(hu1_lifeexp.rds)


# 1. Calculate the average life expectancy for all countries. 

summary(life_exp)

mean(life_exp$life_expectancy)


# 2. Identify the top and bottom 3 

#Bottom list

life_exp %>%
  select(country, life_expectancy)%>%
  arrange(life_expectancy)%>%
  head(3)  -> bottom_list


#Top list

life_exp%>%
  select(country, life_expectancy)%>%
  arrange(desc(life_expectancy))%>%
  head(3) -> top_list


#Making a bar chart for each list

#Bottom three
bottom_list%>%
  ggplot(aes(
    x = reorder(country,life_expectancy), 
    y = life_expectancy)) + geom_col()


#Top three
top_list%>%
  ggplot(aes(
    x=reorder(country,life_expectancy),
    y= life_expectancy)) + geom_col()




#Mergin together the graphs of top and bottom

top_list <- 
  top_list%>%
  mutate(category="top")


bottom_list<-
  bottom_list%>%
  mutate(category ="bottom")

graph_table <- 
  bind_rows(top_list,
            bottom_list)

graph_table    #Creates the table requested


#Creating the visuals of the bar chart

graph_table %>%
  ggplot(aes(x = reorder(country,life_expectancy), y = life_expectancy))+ geom_col()



#Bringing in the continent variable to merge.

hu1_continents.rds <- file.choose()
continents <- readRDS(hu1_continents.rds)

colnames(continents) <- c("country","continent")

inner_join(life_exp, continents) -> mer_cont  #This is number 4
group_by(continent)
#This is attempt at nr 5

mer_cont %>%
  arrange(continent)%>%
  select(life_expectancy,continent)
  
africa_data <- mer_cont %>%
  filter(continent == "Africa")
  mean(africa_data$life_expectancy) -> mean_afr

europe_data <- mer_cont %>%
  filter(continent == "Europe")
  mean(europe_data$life_expectancy) -> mean_eur
  
asia_data <- mer_cont %>%
    filter(continent == "Asia")
    mean(asia_data$life_expectancy) -> mean_asia

americas_data <- mer_cont%>%
  filter(continent == "Americas")
  mean(americas_data$life_expectancy) -> mean_amer

oceania_data <- mer_cont%>%
  filter(continent == "Oceania")
  mean(oceania_data$life_expectancy) -> mean_oce
  
tibble(
  Continent = c("Africa","America", "Asia", "Europe", "Oceania"),
  Average = c(mean_afr, mean_amer,mean_asia,mean_eur,mean_oce)
  )-> avg

#Hard coded solution to the problem but at least there's a table with the values of the different continents.

view(avg)



avg %>%
  ggplot(aes(x = reorder(Continent, Average), y = Average))+ geom_col()
