library("tidyverse")
#library("ggpubr")#
#library("maps")#
library("conflicted")


conflict_prefer("recode", "dplyr")
conflict_prefer("filter", "dplyr")

cm_data_raw <- read.csv("https://raw.githubusercontent.com/owid/owid-datasets/master/datasets/Child%20mortality%20-%20Gapminder%20(2013)/Child%20mortality%20-%20Gapminder%20(2013).csv")

cm_data <-  cm_data_raw%>%
  
rename(
  country = Entity,
  year = Year,
  child_mort = 'Child.mortality..Gapminder..2013..'
)


summary(cm_data)
summary(cm_data$child_mort)

#saving the year 2013 for future references

query_year <- 2013

#Creating a new table for the year of query, this can be altered

cm_sample <- filter(cm_data, year == query_year)

cm_sample <- cm_data %>%
  filter(year == query_year)

head(cm_sample)


#Calculating key-values

mean(cm_sample$child_mort)
sd(cm_sample$child_mort)


#Introducing weighted statistics in relation to population

pop_data <- read_csv("https://raw.githubusercontent.com/owid/owid-datasets/master/datasets/Population%20by%20country%2C%201800%20to%202100%20(Gapminder%20%26%20UN)/Population%20by%20country%2C%201800%20to%202100%20(Gapminder%20%26%20UN).csv")

pop_data <- pop_data %>%
  set_names(c("country","year", "pop_proj","population"))

glimpse(pop_data)


#Joining two different tables

country_data <- inner_join(cm_sample, pop_data)

#Removing projection
country_data <- country_data %>%
  select(country, year, population, child_mort)

country_data <- country_data %>% filter(year == 2013)

summary(country_data)

#Weighted mean

weighted.mean(country_data$child_mort,
              w = country_data$population)



#Rangordna en tabell i stigande/ fallande ordning

#10 länder med lägst dödlighet
country_data %>%
  select(country, child_mort)%>%
  arrange(child_mort) %>%
  head(10)


#10 länder i fallande ordning

country_data %>%
  select(country, child_mort)%>%
  arrange(desc(child_mort)) %>%
  head(10)


#Introducing a graphic component withr ggplot2 from the Tidyverse package

bottom_list <- country_data %>%
  select(country, child_mort)%>%
  arrange(child_mort)%>%
  head(10)

#Bar-chart w ggplot, ascending order

bottom_list %>%
  ggplot(aes(x = reorder(country, child_mort), 
             y = child_mort)) +
  geom_col()


#Bar-chart with highest mortality
top_list <- country_data%>%
  select(country, child_mort)%>%
  arrange(desc(child_mort))%>%
  head(10)

#Plot in ggplot

  ggplot(top_list,
         aes(x = reorder(country,
                         -child_mort),
         y = child_mort))+
    
    geom_col() +
  
  #Aesthetic changes
  labs(
    title = "Countries with the highest child mortality rate 2013",
    subtitle = "Mortalities per 1000 children 0-5 years old",
    caption = "Source, www.ourworldindata.org",
    y = "Child mortality",
    x = "Country")+ 
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95))

  
  
  
  #Showing two diagrams at the same time:
  
  install.packages("ggpubr")
  library("ggpubr")  

  
  #Bottom list
  bottom_cm_graph <- ggplot(bottom_list,
                              aes(x = reorder(country, child_mort), 
                                         y = child_mort)) +
                              geom_col() +
                            theme(axis.text = element_text(angle=90, hjust=0.95))

  #Top list
  top_cm_graph <- top_list %>%
    ggplot(aes(x = reorder(country, -child_mort),
                           y=child_mort))+
    geom_col() +
    labs(y=NULL)+
    theme(axis.text.x = element_text(angle=90, hjust=0.95))

  #Putting it all together'
  ggarrange(bottom_cm_graph, top_cm_graph, align = "h")
  