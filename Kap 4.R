#1. Hur mycket har barndödligheten minskat sedan år 1950?
#2 Har barnadödligheten minskat i alla delar av världen?
#3 Var har barnadödligheten förändrats mest respektive minst under denna tidsperiod
#4 Hur har utvecklingen sett ut i repsektive land i Asien?


# Ladda data

library(tidyverse)
library(conflicted)
conflict_prefer("filter", "dplyr")

#Hämtade Raw data
mortality_data <- read_csv("https://git.io/JJPrq")


#Tittar på tabellen 

mortality_data


#byter namn på variablerna, #redan bytt tydligen


#Hur mycket har barnadödligheten minskat?

summary(mortality_data)

#Första år tillgängligt är ju 1916, vi ska bara titta på 1950 och framåt. Dags att filtrera bort dessa år

mortality_data%>%
  filter(year >= 1950)

#Använda group by functionen för att sektionera enskilda värden beroende på variabler

yearly_mortality_data<-mortality_data%>%
  group_by(year)%>%
  summarise(yearly_mean_child_mort = mean(child_mort))



#Ovan är oviktat medelvärde för de olika åren, mao, populationen är inte tagit i hänsyn i beräkningen


#Dags för grafisk representation

ggplot(yearly_mortality_data,
       aes(
         x = year,
         y = yearly_mean_child_mort
       ))+
  geom_line() + 
  labs(
           title = "Genomsnittligt barnadödlighet per år, världen 1950",
           subtitle = "Oviktat medelvärde"
         )

#Ovanför spottar ut en graf med en linje över barnadödligheten i världen 


#Nedanför får vi alla individuella länder möjliga att beskåda

ggplot(mortality_data,
       aes(
         y = child_mort,
         x = year,
         color = country
         )) +
  geom_line()+
  theme(legend.position = "none")


#Nu skapar vi en kontinent lista för att förtydliga skiten för oss själva

continent_list <- read_csv("https://git.io/JTZJq")

#döper om kolumner
continent_list <- continent_list%>%       #Viktigt att spara i sig själv här, annars kommer inte i left_join ske någonting eftersom data förändingen inte sparas
  select(Entity, 'Countries Continents')%>%
  rename(country = Entity,
         continent = 'Countries Continents')

continent_list
#Sammanfogar kontinentlistan och barnadödllighetsdatan

mortality_continents <- left_join(
  continent_list, mortality_data,
  by = "country"
)

#Filtrerar igen efter eftersträvad år.
mortality_continents <- mortality_continents%>%
  filter(year >= 1950)

mortality_continents <- mortality_continents %>% drop_na()

summary(mortality_continents)


#Nu är datan så upplagd att vi kan gruppera efter kontinent och år

continent_means <- mortality_continents%>%
  group_by(year,continent)%>%
  summarise(continent_mean_child_mort = mean(child_mort))
summary(continent_means)


continent_means %>%
  group_by(continent)%>%
  ggplot(aes(x=year,
         y = continent_mean_child_mort,
         linetype = continent
         )) +
  geom_line()


#Voila, ett linje diagram av oviktade kontinenter :)



#Nu ska vi titta på förändringen i variablerna

continent_means <- continent_means%>%
  group_by(continent)%>%
  mutate(
    continent_diff = continent_mean_child_mort[year == 2016] -
      continent_mean_child_mort
  )

continent_means%>%
  select(continent, continent_diff, year) %>%
  filter(year == 1950)


#Vi skippar att räkna ut alla länders skillnader men det kan vara bra att veta att det går att gå åt andra hållet utifrån dessa principer.



#Nu tittar vi på länderna i en kontinent, Afrika

africa_data <- mortality_continents%>%
  filter(continent == "Africa")

africa_data%>% ggplot(aes(x = year,
                           y = child_mort,
                           color = country)) +
  geom_line() +
  theme(legend.position = "none") +
  labs(title = "Barnadödlighet, Länder i Afrika")


#Loopar! Gud vad kul

for(i in 1:4){
  paste("Detta är ett varv", i) %>% print()
}


#Nu ska vi använda loopen på "africa_data"

#Loopen ska visa barnadödligheten i Mali för olika år

for(index_year in 1960:1962){
  #hämta bara data från mali
  one_result<-africa_data$child_mort[ #Här säger vi, hämta från kolumnen och spara som one result
    africa_data$country == "Mali" & 
    africa_data$year == index_year #specifierar att det är index-året
  ]


#Skeiver ut for-loopen

paste(
  "Barnadödligheten i Mali var år",
  index_year,
  "lika med",
   one_result
  ) %>% print()
}

