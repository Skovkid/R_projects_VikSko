#Assignment nr 3

library(tidyverse)

# Q:Create an object that is a string (containing 5 sentences).

# A:
str_obj <- c("This is a string.",
             "It contains 5 senteences.",
             "I wish I could write longer.",
             "However I don't have time.",
             "Sorry about that")

# Q: How many characters and words does it contain (i.e., the entire document)?

# A: 
str_length(str_obj) -> char_str_obj
sum(char_str_obj) #getting the amount of characthers used

str_obj_df <- tibble(sentence = 1:5, text = str_obj) #Defining tibble to count words

str_obj_df$text %>%
  str_count("\\w+") -> wrd_df #This right here calculates the words 

sum(wrd_df) #These are all the words. 


# Q:Which command did you use to calculate the number of characters/words?

# A: For charachters I used the sum of the str_length command and the words I created a tibble df and summed it. 

# Q: How many characters does each of the sentences have? Provide the code.

# A: It is also given by the list

str_length(str_obj) -> str_data

str_data

#Q: Plot the number of characters for each of the 5 sentences using ggplot package.

#A: Here is the end result

str_obj_df %>%
  mutate(count = str_length(text)) %>%  
  ggplot(aes(x = sentence, y = count)) +
  geom_col() +
  coord_flip()



#Part B.

# 1. Using rtweet package, retrieve tweets that contain #climatestrike.


#Installing the package

updateR()

install.packages("rtweet")
install.packages("installr")
library(rtweet)
library(jsonlite)
library(installr)


# 2. Save the data in a JSON file.

library(tidyverse)
library(jsonlite)
library(tidytext)
library(SnowballC)
library(dplyr)
library(tm)
library(ggplot2)


df <- fromJSON(paste(readLines('tweets_1week_climate_str.json'), collapse=""))


# 3. How many observations are in your data?

#The df variable is listed as having 143 observations. 

# 4. What is the location from which the largest number of tweets has been posted?

df %>% 
  count(place) %>% 
  arrange(desc(n))

# 5. How many observations contain the word “crisis”? Provide the code.

df$text %>%
  str_detect("crisis") %>%
  sum()

# 6. Split the data into tokens with unnest_tokens.

df %>% 
  unnest_tokens(word, text)

# 7. Remove the stop words. Provide the code.

freq <- df %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>%  # anti_join = to return all rows in one data frame that do not have matching values in another data frame
  count(word, sort=T) %>% 
  top_n(10, n)

ggplot(freq, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  labs(title="Twitter ", x = NULL, y = "Frequency") +
  coord_flip()


# 8. Customize the list of stop words that the output makes sense to you.

custom_stopwords <- add_row(stop_words, word="t.co", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="https", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="t.co", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="change", lexicon="custom")

freq <- df %>% 
  unnest_tokens(word, text) %>% 
  anti_join(custom_stopwords) %>% 
  count(word, sort=T) %>% 
  top_n(10, n)
ggplot(freq, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  labs(title="Twitter ", x = NULL, y = "Frequency") +
  coord_flip()

# 9. Plot a histogram of 10 most frequent words used in tweets. Describe what you see.

#Here's the plot

ggplot(freq, aes(x = n)) +
  geom_histogram()


#The frequency differs significantly between the different categories.
#The words that were used the most (climatestrike) was to a large extent just the 

# 10. Apply stemming to your data and build a histogram of the most frequent words. What do you see?

freq <- df %>% 
  unnest_tokens(word, text) %>% 
  anti_join(custom_stopwords) %>% 
  mutate(word=wordStem(word)) %>% 
  count(word, sort=T) %>% 
  top_n(20, n)

ggplot(freq, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  labs(title="Twitter ", x = NULL, y = "Frequency") +
  coord_flip()

# 11. Remove number from the data. Describe what you see.

freq <- df %>% 
  mutate(text=removeNumbers(text)) %>% # remove numbers
  unnest_tokens(word, text) %>% 
  anti_join(custom_stopwords) %>% 
  mutate(word=wordStem(word)) %>% 
  count(word, sort=T) %>% 
  top_n(30, n)

# Plot histogram
ggplot(freq, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  labs(title="Twitter ", x = NULL, y = "Frequency") +
  coord_flip()