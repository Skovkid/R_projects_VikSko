#*************** Relevant packages****************


#install.packages("tidyverse")
#install.packages("gutenbergr")
#install.packages("tidytext")
#install.packages("stringr")
#install.packages("reshape2")
#install.packages("wordcloud")

#*************** Loading Libraries*************************


library(tidyverse)
library(gutenbergr)
library(tidytext)
library(stringr)
library(reshape2)
library(wordcloud)
library(jsonlite)


# In order to do this excersise we have to define the relevant variables of strings to a dataframe

#Downloading the relevant books

help(package = "gutenbergr") #Finding out relevant information about the package

#gutenberg_works() 
#Found the command to filter for Swedish works.

gw <- gutenberg_works(languages = "sv")

gw <- gw%>% 
  filter(author == "Strindberg, August")  #Filter for only August Strindberg works

#After looking at the data we fortunately have no doubles or preexisting values that could meddle with our resutls.

#Now we download the first book as a test trial

inferno <- gutenberg_download(
  gutenberg_id = 29935,
)


#Since there are carachters outside the american language we must fix the data 

Encoding(inferno$text) <- "latin1"

inferno <- inferno %>%  slice(21:n())
inferno$text %>%  head(10)

#Now we will split the column 'text' in the dataframe inferno in order to be able to analyze every single word

inferno_words <- unnest_tokens(
  inferno,
  input = text, output = word,
  token = "words", to_lower = TRUE
)

inferno_words %>%  head(10)

#Now we start our sentiment analysis for Inferno by calling a git lexicon for sentiment analysis.
#I was not able to follow the books instructions since the parsing of the table was unsuccessful in loading the values.
#Therefore I manually downloaded the JSON file from the developers GitHub
# LINK: https://github.com/AlexGustafsson/sentiment-swedish/blob/develop/build/AFINN-sv-165.txt


sentiments_swedish_raw <- read_table("AFINN-sv-165.txt") #Upon loading we realized that there were errors in the read.table function
#Because there were many string values that contained spaces between them. 
#The read_table made it possible to store data in a df however, very messy data

colnames(sentiments_swedish_raw) <- c("word", "score") #After a couple of hours trying to fix this error of the table we finally 
#at this solution were now there are many words in a df combined with scores.

#There are however values that are non numeric in the rows and we therefore have to clean the table of such instances.

glimpse(sentiments_swedish_raw)

sentiments_swedish <- sentiments_swedish_raw%>%
  mutate(
    word=str_trim(word),
    score = as.integer(str_trim(score)))

sentiments_swedish<-drop_na(sentiments_swedish) #All NA-values removed from the data. We can now use the sentiments values for analysis. 


#Now we procure doubles and delete them from our sentiment analysis

sentiments_swedish <- sentiments_swedish %>% 
  arrange(word) %>%
  mutate(is_duplicate=
           if_else(word == lag(word, 1), TRUE, FALSE)) %>% #Here we created a new variable to check if there is a duplicate value 
  filter(is_duplicate == FALSE) %>% 
  select(-is_duplicate)


#No we have gotten rid of the duplicate text values and NA score values. Therefore the data cleaning is done and we are ready
#To apply our analysis of the book inferno. 
  
  
#Now we will join he table created for the book inferno with the table of sentiment analysis


inferno_words<- inferno_words %>% 
  left_join(sentiments_swedish, by = "word")

inferno_words    #This is a simple left join in all honesty.

inferno_words <- inferno_words %>% 
  mutate(score = replace_na(score, 0)) #Replacing words that are out-of-bounds for our analysis in the 0 score category.

#Now we simply remove the neutral words (score == 0) from our analysis

inferno_words %>% 
  filter(score == 0) %>% 
  nrow() #Looking at this we can see that we have out of 45,360 words analyzed 42,476 which is 93% of the words in the entire book
  42476/45360*100 -> percentage_inferno
percentage_inferno


#Let's represent this in a histogram

inferno_words %>% 
  filter(!score == 0) %>% #removing 0-value words
  ggplot(aes(x=score)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(-5,5,1)) %>% 
  labs(
    x = "Sentiment points", y=NULL,
    title = "Mood of the book Inferno",
    subtitle = "Distribution of sentimentpoints in August Strindberg's 'Inferno'"
  )

  