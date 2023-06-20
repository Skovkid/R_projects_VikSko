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
# 2. Save the data in a JSON file.
# 3. How many observations are in your data?
# 4. What is the location from which the largest number of tweets has been posted?
# 5. How many observations contain the word “crisis”? Provide the code.
# 6. Split the data into tokens with unnest_tokens.
# 7. Remove the stop words. Provide the code.
# 8. Customize the list of stop words that the output makes sense to you.
# 9. Plot a histogram of 10 most frequent words used in tweets. Describe what you see.
# 10. Apply stemming to your data and build a histogram of the most frequent words. What do you see?
# 11. Remove number from the data. Describe what you see.