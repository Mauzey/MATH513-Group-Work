# 
#   Analysis of Twitter Data
# 

# IMPORT DEPENDENCIES -------------------------------------------------------------------------------------------------------

library(readr)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(dplyr)
library(modelr)
library(rtweet)

# SET WORKING DIRECTORY -----------------------------------------------------------------------------------------------------

# Will
#setwd("C:/Users/willo/SynologyDrive/Documents - Will/Qualifications/!MSc Data Science/!Courses/1 MATH513 Big Data and Social Network/Assessment/MATH513-Group-Work/practical")

# Tania
#setwd("")

# Alex
#setwd("~/Development/Repositories/MATH513-Group-Work/practical")
#setwd("D:/Development/MATH513-Group-Work/practical")

# IMPORT DATA ---------------------------------------------------------------------------------------------------------------

tweets <- read_csv("./data/cleaned-tweets.csv")
users <- read_csv("./data/cleaned-users.csv")

# DATA SUMMARY --------------------------------------------------------------------------------------------------------------

# Print the columns of the dataframes
names(tweets)
names(users)

# Print the shape of the data (rows/columns)
dim(tweets)
dim(users)

# TIME SERIES PLOTS ---------------------------------------------------------------------------------------------------------
S20tweets <- tweets %>% filter(product == 'Galaxy S20')
S20FEtweets <- tweets %>% filter(product == 'Galaxy S20 FE')
iPhone12tweets <- tweets %>% filter(product == 'iPhone12')



# SLIDE 'ANALYSIS OF DATA OVER TIME' --------------------------------------


#for  all the data use time series with 'days'

#TO DO:
#do a facet grid: factorize the product column and make it a facet grid
#3 grids go horizontally across



ts_plot(tweets, "days", trim = 1) + 
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold")) +
  labs(x = NULL, y = NULL,
      title = "Frequency of Twitter statuses",
      subtitle = "Twitter status (tweet) counts aggregated using 1-mins intervals",
      caption = "Source: Data collected from Twitter's REST API via rtweet"
  ) 


# SLIDE 'SENTIMENT ANALYSIS'-------------------------------------------------------------------

# TO DO:
# first we're filtering the data for 3 separate models by 5 categories
# - display
# - camera
# - price
# - battery
# - 5G
# 
# foreach 


#tweets -> split by 5 categories -> sentiment analysis -> group by model in facet grid

tweets$stripped_text <- gsub("http.*","",  tweets$text)
tweets$stripped_text <- gsub("https.*","", tweets$stripped_text)
tweets$stripped_text <- gsub("amp","", tweets$stripped_text)

tweets_clean <- tweets %>%
  select(stripped_text) %>%
  mutate(tweetnumber = row_number()) %>%
  unnest_tokens(word, stripped_text)

data("stop_words")

cleaned_tweet_words <- tweets_clean %>%
  anti_join(stop_words) 

my_stop_words <- data.frame(word = c("galaxys20fe", "iphone12", "iphone", '12', 'win', 'galaxys20', 'giveaway',
                                     'galaxys20', 'galaxy', 's20', 'apple', 'samsung', 'iphone12pro',
                                     'competition', '1', 'fe0f', '00a0', 'winners'))

cleaned_tweet_words_2 <- cleaned_tweet_words %>%
  anti_join(my_stop_words)

cleaned_tweet_words_2 %>%
  count(word, sort = TRUE) %>%
  head(50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "pink", color = "red") +
  coord_flip() +
  labs(x = "Unique Words",
       y = "Frequency",
       title = "Top 10 most popular words found in tweets with #climatechange")


# what do we expect to see from the sentiment analysis?
# 
# let's concentrate on s20fe and iphone12 
# we want to see what features were good and bad
# 
# stage 1
# perform sentiment analysis on new features from the list in miro
# for each model separately
# 
# stage 2
# for the shared features (5 in total) facet grid sentiment for s20fe and iphone12
# 
# stage 3
#
#this is the change for s20fe, this change was perceived more positively compared to how
#*this change* was perceived for iphone12
# for shared features in s20 and s20fe facet grid sentiment analysis 
#<- why you want to listen to your fans 
#
#stage 4
#plot s20 against iphone12 (see stage 2)


# SLIDE WITH LOCATION PLOT (GLOBE) ----------------------------------------

#got the tweets set -> group by the product -> plot on the globe with diff. colors



