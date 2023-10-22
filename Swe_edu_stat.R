library(tidyverse)
library(ggplot2)
library(dplyr)


#Load in the data from SCB about education

Swe_edu <- readxl::read_xlsx("C:/Users/Viktor/Documents/Programering/R projekt/Edu_stat_Swe/tab10_2022.xlsx")


#browse
Swe_edu


#locate column names
colnames(Swe_edu)

#Change column names

colnames(Swe_edu)[1] = "År"
colnames(Swe_edu)[2] = "Kön"
colnames(Swe_edu)[3] = "Befolkning"
colnames(Swe_edu)[4] = "Under grundskola"
colnames(Swe_edu)[5] = "Grundskola"
colnames(Swe_edu)[6] = "Under gymnasie"
colnames(Swe_edu)[7] = "Gymnasieexamen"
colnames(Swe_edu)[8] = "Under kandidat"
colnames(Swe_edu)[9] = "Kandidat - Master"
colnames(Swe_edu)[10] = "Forskare"

#Remove unneccesarry columns

Swe_edu <- Swe_edu%>% 
  select(c(-...11,-...12,-...13,-...14))

#Remove row number 1

Swe_edu <- Swe_edu [-c(1),]
  

#browse again
Swe_edu



#drop unnecessary na

Swe_Edu_Pop <- na.omit(Swe_edu)

help("ggplot2-package")

Swe_Edu_Pop
#Let's plot out the historical data


ggplot(data = Swe_Edu_Pop, aes(x=Swe_Edu_Pop))+
  geom_line()+
  geom_point()
       
       
       