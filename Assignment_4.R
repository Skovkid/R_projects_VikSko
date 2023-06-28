#*************** Relevant packages****************


#install.packages("tidyverse")
#install.packages("gutenbergr")
#install.packages("tidytext")
#install.packages("stringr")
#install.packages("reshape2")
#install.packages("wordcloud")
install.packages("ggpubr")

#Loading Libraries*************************


library(tidyverse)
library(gutenbergr)
library(tidytext)
library(stringr)
library(reshape2)
library(wordcloud)
library(ggpubr)



# In order to do this excersise we have to define the relevant variables of strings to a dataframe

#Downloading the relevant books

help(package = "gutenbergr") #Finding out relevant information about the package

#gutenberg_works() 
#Found the command to filter for Swedish works.

gw <- gutenberg_works(languages = "sv")

gw_StrBrg <- gw%>% 
  filter(author == "Strindberg, August")  #Filter for only August Strindberg works

#After looking at the data we fortunately have no doubles or preexisting values that could meddle with our resutls.

#Now we download the first book as a test trial

gut_id_StrBrg<-c(gw_StrBrg$gutenberg_id)

August_Strindberg <- gutenberg_download( 
  gutenberg_id = gut_id_StrBrg,
  meta_fields = c("title"),
  mirror = "http://www.gutenberg.org/dirs/"
)


#Since there are carachters outside the american language we must fix the data 

Encoding(August_Strindberg$text) <- "latin1"

August_Strindberg <- August_Strindberg %>%  slice(21:n())
August_Strindberg$text %>%  head(10)

#Now we will split the column 'text' in the dataframe August_Strindberg in order to be able to analyze every single word

August_Strindberg_words <- unnest_tokens(
  August_Strindberg,
  input = text, output = word,
  token = "words", to_lower = TRUE
)

August_Strindberg_words %>%  head(10)


#Loading the book to R is completet here**************************************************************************************



#Below this line is the sentiment analysis************************************************************************************



#Now we start our sentiment analysis for August_Strindberg by calling a git lexicon for sentiment analysis.
#I was not able to follow the books instructions since the parsing of the table was unsuccessful in loading the values.
#Therefore I manually downloaded the JSON file from the developers GitHub
# LINK: https://github.com/AlexGustafsson/sentiment-swedish/blob/develop/build/AFINN-sv-165.txt


sentiments_swedish_raw <- read_table("AFINN-sv-165.txt") 

#Upon loading we realized that there were errors in the read.table function
#Because there were many string values that contained spaces between them. 
#The read_table made it possible to store data in a df however, very messy data

colnames(sentiments_swedish_raw) <- c("word", "score") 

#After a couple of hours trying to fix this error of the table we finally 
#at this solution were now there are many words in a df combined with scores.

#There are however values that are non numeric in the rows and we therefore have to clean the table of such instances.

glimpse(sentiments_swedish_raw)

sentiments_swedish <- sentiments_swedish_raw%>%
  mutate(
    word=str_trim(word),
    score = as.integer(str_trim(score)))

#All NA-values removed from the data. We can now use the sentiments values for analysis. 

sentiments_swedish<-drop_na(sentiments_swedish) 


#Now we procure doubles and delete them from our sentiment analysis

sentiments_swedish <- sentiments_swedish %>% 
  arrange(word) %>%
  mutate(is_duplicate=
           if_else(word == lag(word, 1), TRUE, FALSE)) %>% #Here we created a new variable to check if there is a duplicate value 
  filter(is_duplicate == FALSE) %>% 
  select(-is_duplicate)


#No we have gotten rid of the duplicate text values and NA score values. Therefore the data cleaning is done and we are ready
#To apply our analysis of the book August_Strindberg. 
  
  
#Now we will join he table created for the book August_Strindberg with the table of sentiment analysis


#Below this line WE JOIN THE SENTIMENT ANALYSIS WITH THE BOOKS************************** 


August_Strindberg_words <- August_Strindberg_words %>% 
  left_join(sentiments_swedish, by = "word")

August_Strindberg_words    #This is a simple left join in all honesty.

August_Strindberg_words <- August_Strindberg_words %>% 
  mutate(score = replace_na(score, 0)) #Replacing words that are out-of-bounds for our analysis in the 0 score category.

#Now we simply remove the neutral words (score == 0) from our analysis

August_Strindberg_words %>% 
  filter(score == 0) %>% 
  nrow() #Looking at this we can see that we have out of 45,360 words analyzed 42,476 which is 93% of the words in the entire book
 

#Let's represent this in a histogram

August_Strindberg_words %>% 
  filter(!score == 0) %>% #removing 0-value words
  ggplot(aes(x=score)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(-5,5,1)) %>% 
  labs(
    x = "Sentiment points", y="Quantity",
    title = "August Strindberg",
    subtitle = "Distribution August Strindberg"
  ) -> Strindberg_Chart




#HERE WE START BY ADDING OTHER AUTHORS WORK*************************************


#SELMA LAGERLÖF ****************************************************************


#Now we find the 4 authors we want tp have an analysis of the five different books
#I decided to opt for Selma Lagerlöf, Viktor Rydberg, Sigge Strömberg, , Hjalmar Söderberg

gw_Selm_Lag <- gw%>% 
  filter(author == "Lagerlöf, Selma") 

gut_id_Selm_Lag<-c(gw_Selm_Lag$gutenberg_id)

Selma_Lagerlof <- gutenberg_download( 
  gutenberg_id = gut_id_Selm_Lag,
  meta_fields = c("title"),
  mirror = "http://www.gutenberg.org/dirs/"
)


#Since there are carachters outside the american language we must fix the data 

Encoding(Selma_Lagerlof$text) <- "latin1"

Selma_Lagerlof <- Selma_Lagerlof %>%  slice(21:n())
Selma_Lagerlof$text %>%  head(10)

#Now we will split the column 'text' in the dataframe Selma_Lagerlof in order to be able to analyze every single word

Selma_Lagerlof_words <- unnest_tokens(
  Selma_Lagerlof,
  input = text, output = word,
  token = "words", to_lower = TRUE
)

Selma_Lagerlof_words %>%  head(10)


Selma_Lagerlof_words<- Selma_Lagerlof_words %>% 
  left_join(sentiments_swedish, by = "word")

Selma_Lagerlof_words    #This is a simple left join in all honesty.

Selma_Lagerlof_words <- Selma_Lagerlof_words %>% 
  mutate(score = replace_na(score, 0)) #Replacing words that are out-of-bounds for our analysis in the 0 score category.

#Now we simply remove the neutral words (score == 0) from our analysis

Selma_Lagerlof_words %>% 
  filter(score == 0) %>% 
  nrow() #Looking at this we can see that we have out of 45,360 words analyzed 42,476 which is 93% of the words in the entire book


#Let's represent this in a histogram

Selma_Lagerlof_words %>% 
  filter(!score == 0) %>% #removing 0-value words
  ggplot(aes(x=score)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(-5,5,1)) %>% 
  labs(
    x = "Sentiment points", y="Quantity",
    title = "Selma Lagerlof",
    subtitle = "Distribution of sentimentpoints of Selma Lagerlof's works"
  ) -> Selma_Chart





# Viktor Rydberg,***************************************************************

gw_Vik_Ryd <- gw%>% 
  filter(author == "Rydberg, Viktor") 

gut_id_Vik_Ryd<-c(gw_Vik_Ryd$gutenberg_id)

Viktor_Rydberg <- gutenberg_download( 
  gutenberg_id = gut_id_Vik_Ryd,
  meta_fields = c("title"),
  mirror = "http://www.gutenberg.org/dirs/"
)

#Since there are carachters outside the american language we must fix the data 

Encoding(Viktor_Rydberg$text) <- "latin1"

Viktor_Rydberg <- Viktor_Rydberg %>%  slice(21:n())
Viktor_Rydberg$text %>%  head(10)

#Now we will split the column 'text' in the dataframe Viktor_Rydberg in order to be able to analyze every single word

Viktor_Rydberg_words <- unnest_tokens(
  Viktor_Rydberg,
  input = text, output = word,
  token = "words", to_lower = TRUE
)

Viktor_Rydberg_words %>%  head(10)


Viktor_Rydberg_words<- Viktor_Rydberg_words %>% 
  left_join(sentiments_swedish, by = "word")

Viktor_Rydberg_words    #This is a simple left join in all honesty.

Viktor_Rydberg_words <- Viktor_Rydberg_words %>% 
  mutate(score = replace_na(score, 0)) #Replacing words that are out-of-bounds for our analysis in the 0 score category.

#Now we simply remove the neutral words (score == 0) from our analysis

Viktor_Rydberg_words %>% 
  filter(score == 0) %>% 
  nrow() #Looking at this we can see that we have out of 45,360 words analyzed 42,476 which is 93% of the words in the entire book


#Let's represent this in a histogram

Viktor_Rydberg_words %>% 
  filter(!score == 0) %>% #removing 0-value words
  ggplot(aes(x=score)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(-5,5,1)) %>% 
  labs(
    x = "Sentiment points", y="Quantity",
    title = "Viktor Rydberg",
    subtitle = "Distribution Viktor Rydberg"
  ) -> Rydberg_Chart
  

















#Sigge Strömberg****************************************************************


gw_Sig_Str <- gw%>% 
  filter(author == "Strömberg, Sigge") 

gut_id_Sig_Str<-c(gw_Sig_Str$gutenberg_id)

Sigge_Stromberg <- gutenberg_download( 
  gutenberg_id = gut_id_Selm_Lag,
  meta_fields = c("title"),
  mirror = "http://www.gutenberg.org/dirs/"
)


#Since there are carachters outside the american language we must fix the data 

Encoding(Sigge_Stromberg$text) <- "latin1"

Sigge_Stromberg <- Sigge_Stromberg %>%  slice(21:n())
Sigge_Stromberg$text %>%  head(10)

#Now we will split the column 'text' in the dataframe Sigge_Stromberg in order to be able to analyze every single word

Sigge_Stromberg_words <- unnest_tokens(
  Sigge_Stromberg,
  input = text, output = word,
  token = "words", to_lower = TRUE
)

Sigge_Stromberg_words %>%  head(10)


Sigge_Stromberg_words<- Sigge_Stromberg_words %>% 
  left_join(sentiments_swedish, by = "word")

Sigge_Stromberg_words    #This is a simple left join in all honesty.

Sigge_Stromberg_words <- Sigge_Stromberg_words %>% 
  mutate(score = replace_na(score, 0)) #Replacing words that are out-of-bounds for our analysis in the 0 score category.

#Now we simply remove the neutral words (score == 0) from our analysis

Sigge_Stromberg_words %>% 
  filter(score == 0) %>% 
  nrow() #Looking at this we can see that we have out of 45,360 words analyzed 42,476 which is 93% of the words in the entire book


#Let's represent this in a histogram

Sigge_Stromberg_words %>% 
  filter(!score == 0) %>% #removing 0-value words
  ggplot(aes(x=score)) +
  geom_bar(fill = 'blue') +
  scale_x_continuous(breaks = seq(-5,5,1)) %>% 
  labs(
    x = "Sentiment points Sigge", y="Quantity",
    title = "Sigge Söderström",
    subtitle = "Distribution of Sigge Söderström"
  ) -> Sigge_Chart
Sigge_Chart













#Hjalmar Söderberg*************************************************************


gw_Hjal_Soder <- gw%>% 
  filter(author == "Söderberg, Hjalmar") 

gut_id_Hjal_Soder<-c(gw_Hjal_Soder$gutenberg_id)

Hjalmar_Soderberg <- gutenberg_download( 
  gutenberg_id = gut_id_Hjal_Soder,
  meta_fields = c("title"),
  mirror = "http://www.gutenberg.org/dirs/"
)


#Since there are carachters outside the american language we must fix the data 

Encoding(Hjalmar_Soderberg$text) <- "latin1"

Hjalmar_Soderberg <- Hjalmar_Soderberg %>%  slice(21:n())
Hjalmar_Soderberg$text %>%  head(10)

#Now we will split the column 'text' in the dataframe Hjalmar_Soderberg in order to be able to analyze every single word

Hjalmar_Soderberg_words <- unnest_tokens(
  Hjalmar_Soderberg,
  input = text, output = word,
  token = "words", to_lower = TRUE
)

Hjalmar_Soderberg_words %>%  head(10)


Hjalmar_Soderberg_words<- Hjalmar_Soderberg_words %>% 
  left_join(sentiments_swedish, by = "word")

Hjalmar_Soderberg_words    #This is a simple left join in all honesty.

Hjalmar_Soderberg_words <- Hjalmar_Soderberg_words %>% 
  mutate(score = replace_na(score, 0)) #Replacing words that are out-of-bounds for our analysis in the 0 score category.

#Now we simply remove the neutral words (score == 0) from our analysis

Hjalmar_Soderberg_words %>% 
  filter(score == 0) %>% 
  nrow() #Looking at this we can see that we have out of 45,360 words analyzed 42,476 which is 93% of the words in the entire book


#Let's represent this in a histogram

Hjalmar_Soderberg_words %>% 
  filter(!score == 0) %>% #removing 0-value words
  ggplot(aes(x=score)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(-5,5,1)) %>% 
  labs(
    x = "Sentiment points", y="Quantity",
    title = "Hjalmar Söderberg",
    subtitle = "Distribution of Hjalmar Söderberg"
  )-> Hjalmar_Chart

#Trying to plot multiple bar charts next to eachother

ggarrange(Strindberg_Chart,Selma_Chart,Hjalmar_Chart,Sigge_Chart,Rydberg_Chart)

