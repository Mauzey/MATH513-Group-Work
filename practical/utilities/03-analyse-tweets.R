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
library(tidyr)
library(rtweet)
library(stringr)
library(tidytext)
library(ggthemes)
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
#3 grids go horizontally on top of each other

#GRIDS WITH THE SAME MONTHS SCALES

tweets %>% group_by(product, is_retweet) %>%
  ts_plot("days", trim = 1) + 
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold")) +
  labs(x = NULL, y = NULL,
       title = "Frequency of Twitter statuses",
       subtitle = "Twitter status (tweet) counts aggregated using 1-day intervals",
       caption = "Source: Data collected from Twitter's REST API via rtweet") +
  facet_grid(product ~ .)

#ONLY S20FE VS IPHONE12:

tweets%>% 
  filter(product == 'Galaxy S20 FE' | product == 'iPhone12') %>% 
  group_by(product, is_retweet) %>%
  ts_plot("days", trim = 1) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  labs(x = NULL, y = NULL,
       title = "Frequency of Twitter statuses",
       subtitle = "Twitter status (tweet) counts aggregated using 1-day intervals",
       caption = "Source: Data collected from Twitter's REST API via rtweet") +
  facet_grid(product ~ .)


# iPhone12tweets %>%
#   ts_plot("days", trim = 1) +
#   theme_minimal() +
#   theme(plot.title = element_text(face = "bold")) +
#   labs(x = NULL, y = NULL,
#        title = "Frequency of Twitter statuses for iPhone12",
#        subtitle = "Twitter status (tweet) counts aggregated using 1-hour intervals",
#        caption = "Source: Data collected from Twitter's REST API via rtweet")


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

dataNames = c('S20FEtweets', 'S20tweets', 'iPhone12tweets')

#THE NEXT CODE CAN BE MOST PROBABLY MODIFIED WITH A SIMPLE FOR LOOP

#remove links from S20FE tweets
S20FEtweets$stripped_text <- gsub("http.*","",  S20FEtweets$text)
S20FEtweets$stripped_text <- gsub("https.*","", S20FEtweets$stripped_text)
S20FEtweets$stripped_text <- gsub("amp","", S20FEtweets$stripped_text)
S20FEtweets$stripped_text <- gsub("[^\x01-\x7F]","", S20FEtweets$stripped_text)
S20FEtweets$stripped_text <- gsub("[^[:alnum:][:blank:]?&/\\-]","", S20FEtweets$stripped_text)
S20FEtweets$stripped_text <- gsub("00..","", S20FEtweets$stripped_text)
S20FEtweets$stripped_text <- gsub(".uf.*","", S20FEtweets$stripped_text)


#do the same with S20...
S20tweets$stripped_text <- gsub("http.*","",  S20tweets$text)
S20tweets$stripped_text <- gsub("https.*","", S20tweets$stripped_text)
S20tweets$stripped_text <- gsub("amp","", S20tweets$stripped_text)
S20tweets$stripped_text <- gsub("[^\x01-\x7F]","", S20tweets$stripped_text)
S20tweets$stripped_text <- gsub("[^[:alnum:][:blank:]?&/\\-]","", S20tweets$stripped_text)
S20tweets$stripped_text <- gsub("00..","", S20tweets$stripped_text)
S20tweets$stripped_text <- gsub(".uf.*","", S20tweets$stripped_text)

#... and iPhone12
iPhone12tweets$stripped_text <- gsub("http.*","",  iPhone12tweets$text)
iPhone12tweets$stripped_text <- gsub("https.*","", iPhone12tweets$stripped_text)
iPhone12tweets$stripped_text <- gsub("amp","", iPhone12tweets$stripped_text)
iPhone12tweets$stripped_text <- gsub("[^[:alnum:][:blank:]?&/\\-]","", iPhone12tweets$stripped_text)
iPhone12tweets$stripped_text <- gsub("[^\x01-\x7F]","", iPhone12tweets$stripped_text)
iPhone12tweets$stripped_text <- gsub("00..","", iPhone12tweets$stripped_text)
iPhone12tweets$stripped_text <- gsub(".uf.*","", iPhone12tweets$stripped_text)
# #now splitting up the datasets to the ones related to 5 categories
# S20displayTweets <- S20tweets %>% 
#   filter(str_detect(stripped_text, regex('display', ignore_case = T)))
# 
# S20cameraTweets <- S20tweets %>% 
#   filter(str_detect(stripped_text, regex('camera', ignore_case = T)))
# 
# S20priceTweets <- S20tweets %>% 
#   filter(str_detect(stripped_text, regex('price', ignore_case = T)))
# 
# S20batteryTweets <- S20tweets %>% 
#   filter(str_detect(stripped_text, regex('battery', ignore_case = T)))
# 
# S205GTweets <- S20tweets %>% 
#   filter(str_detect(stripped_text, regex('5G', ignore_case = T)))
# 
# S20otherTweets <- S20tweets %>% 
#   anti_join(S20batteryTweets) %>%
#   anti_join(S20displayTweets) %>%
#   anti_join(S205GTweets) %>% 
#   anti_join(S20priceTweets) %>% 
#   anti_join(S20cameraTweets)
# 
# #do the same for s20 and for iphone 12
# 
# S20FEdisplayTweets <- S20FEtweets %>% 
#   filter(str_detect(stripped_text, regex('display', ignore_case = T)))
# 
# S20FEcameraTweets <- S20FEtweets %>% 
#   filter(str_detect(stripped_text, regex('camera', ignore_case = T)))
# 
# S20FEpriceTweets <- S20FEtweets %>% 
#   filter(str_detect(stripped_text, regex('price', ignore_case = T)))
# 
# S20FEbatteryTweets <- S20FEtweets %>% 
#   filter(str_detect(stripped_text, regex('battery', ignore_case = T)))
# 
# S20FE5GTweets <- S20FEtweets %>% 
#   filter(str_detect(stripped_text, regex('5G', ignore_case = T)))
# 
# #for iphone12
# iPhone12displayTweets <- iPhone12tweets %>% 
#   filter(str_detect(stripped_text, regex('display', ignore_case = T)))
# 
# iPhone12cameraTweets <- iPhone12tweets %>% 
#   filter(str_detect(stripped_text, regex('camera', ignore_case = T)))
# 
# iPhone12priceTweets <- iPhone12tweets %>% 
#   filter(str_detect(stripped_text, regex('price', ignore_case = T)))
# 
# iPhone12batteryTweets <- iPhone12tweets %>% 
#   filter(str_detect(stripped_text, regex('battery', ignore_case = T)))
# 
# iPhone125GTweets <- iPhone12tweets %>% 
#   filter(str_detect(stripped_text, regex('5G', ignore_case = T)))

#tweets -> split by 5 categories -> sentiment analysis -> group by model in facet grid


S20tweets_clean <- S20tweets %>%
  filter(is_bot != TRUE & is_spam != TRUE) %>%
  select(stripped_text) %>%
  mutate(tweetnumber = row_number()) %>%
  unnest_tokens(word, stripped_text)

S20FEtweets_clean <- S20FEtweets %>%
  filter(is_bot != TRUE & is_spam != TRUE) %>%
  select(stripped_text) %>%
  mutate(tweetnumber = row_number()) %>%
  unnest_tokens(word, stripped_text)

iPhone12tweets_clean <- iPhone12tweets %>%
  filter(is_bot != TRUE & is_spam != TRUE) %>%
  select(stripped_text) %>%
  mutate(tweetnumber = row_number()) %>%
  unnest_tokens(word, stripped_text)

data("stop_words")

#IPHONE12 MOST POPULAR WORDS

clean_iPhone12 <- iPhone12tweets_clean %>%
  anti_join(stop_words) 

clean_iPhone12 %>%
  count(word, sort = TRUE) %>%
  head(50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "lightgoldenrod3", color = "white") +
  theme_minimal() +
  coord_flip() +
  labs(x = "Unique Words",
       y = "Frequency",
       title = "Top 10 most popular words found in tweets with iPhone12 (raw)") +
  theme(plot.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 10))


iphone12_stopWords <- data.frame(word = c("iphone12", "iphone", 
                                          "apple", "12", 
                                          "iphone12pro", "iphone12promax", 
                                          "max", "iphone12mini", "min", 
                                          "series", "ios", "tim_cook",
                                          'uf449', 'appleihone12', '5544', 'uf525uf525uf525',
                                          'tgfamilyuf64cwhats', 'stevewoz','18iphone12',
                                          'timcook', 'ios14', 'pro', 'appleevent',
                                          'id', 'mini', "phone", "amazon", "pick",
                                          "buy", "shop", "iphone11", "20",
                                          "appleiphone12", "11",
                                          "free", "cashback",
                                          "limited"))

clean_iPhone12 <- clean_iPhone12 %>% 
  anti_join(iphone12_stopWords)

clean_iPhone12 %>%
  count(word, sort = TRUE) %>%
  head(30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "lightgoldenrod3", color = "white") +
  theme_minimal() +
  coord_flip() +
  labs(x = "Unique Words",
       y = "Frequency",
       title = "Top 10 most popular words found in tweets with iPhone12") +
  theme(plot.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 12))
#margin = margin(t = 10,r= 0, b=10, l = 0, unit = "pt")))

sentiments <- sentiments %>% filter(sentiments$word !="matte")
#get_sentiments("bing")

iPhone_bing_count <- clean_iPhone12 %>%
  inner_join(sentiments) %>%
  count(word, sentiment, sort = TRUE) %>%
  mutate(word = reorder(word, n)) 
#head(bing_word_counts)

iPhone_bing_count %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~sentiment, scales = "free_y") +
  theme_minimal() +
  labs(title = "Most common Positive and Negative words in tweets on iPhone12",
       y = "Sentiment",
       x = NULL) +
  theme(axis.text = element_text(size = 11, color = "black"), 
        axis.title = element_text(size = 11, color = "black"),
        title = element_text(size = 12))

#GALAXY S20
clean_S20 <- S20tweets_clean %>%
  anti_join(stop_words) 

clean_S20 %>%
  count(word, sort = TRUE) %>%
  head(50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "turquoise", color = "white") +
  coord_flip() +
  theme_minimal() +
  labs(x = "Unique Words",
       y = "Frequency",
       title = "Top 10 most popular words found in tweets with Galaxy S20 (raw)") +
  theme(plot.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 10))

s20_stopWords <- data.frame(word = c("galaxys20", "galaxy", "samsung", 
                                     "s20", "di", "ini", "ultra", 
                                     "indonesia", "pake", "uf60dsemua", 
                                     "timur", "kok", "java", 
                                     "djambil", "bukan", "bondowoso", 
                                     "samsungmobilesa", "expressoshow", 
                                     "series", "galaxys20ultra", "pre", 
                                     "uf31fget", "introducing", "renders", 
                                     "samsungindia", "official", "smartphone", 
                                     "unpacked2020", "galaxys20fe", 
                                     "phone", "swiss", "jawa", "diambil", 
                                     "galaxys20plus", "mobile", "galaxys20ultra5g", 
                                     "uf60d", "samsungevent", "fe", 
                                     "2020", "pro", "samsunggalaxy",
                                     "unpacked", "galaxyzflip", "day", "check", 
                                     "galaxyunpacked", "leaked", "de", "en", 
                                     "samsungmobile", "3"))

clean_S20 <- clean_S20 %>% anti_join(s20_stopWords)

clean_S20 %>%
  count(word, sort = TRUE) %>%
  head(30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "turquoise", color = "white") +
  coord_flip() +
  theme_minimal() +
  labs(x = "Unique Words",
       y = "Frequency",
       title = "Top 10 most popular words found in tweets with Galaxy S20") +
  theme(plot.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 12))


s20_bing_count <- clean_S20 %>%
  inner_join(sentiments) %>%
  count(word, sentiment, sort = TRUE) %>%
  mutate(word = reorder(word, n)) 
#head(bing_word_counts)

s20_bing_count %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~sentiment, scales = "free_y") +
  theme_minimal() +
  labs(title = "Most common Positive and Negative words in tweets on S20",
       y = "Sentiment",
       x = NULL) +
  theme(axis.text = element_text(size = 11, color = "black"), 
        axis.title = element_text(size = 11, color = "black"),
        title = element_text(size = 12))


#S20FE
clean_S20FE <- S20FEtweets_clean %>%
  anti_join(stop_words) 

clean_S20FE %>%
  count(word, sort = TRUE) %>%
  head(50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "mediumpurple1", color = "white") +
  coord_flip() +
  theme_minimal() +
  labs(x = "Unique Words",
       y = "Frequency",
       title = "Top 10 most popular words found in tweets with Galaxy S20FE (raw)") +
  theme(plot.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 10))


s20FE_stopWords <- data.frame(word = c("galaxys20fe", "samsungindia",
                                       "samsungmobilesa", "samsung", "galaxy", 
                                       "phone", "contestalert", "s20", "season", 
                                       "mintknow", "btsus", "moreu", 
                                       "withugalaxys20feuin", "jiminukeeps", 
                                       "bts", "galaxys20fesuperfan", 
                                       "withgalaxy", "fe", "galaxybuds",
                                       "madeforfans", "s20fecamerafan", 
                                       "uf49a", "1", "teamgalaxy", "pro",
                                       "galaxyxbts"))
clean_S20FE <- clean_S20FE %>%
  anti_join(s20FE_stopWords) 

clean_S20FE %>%
  count(word, sort = TRUE) %>%
  head(30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "mediumpurple1", color = "white") +
  coord_flip() +
  theme_minimal()+
  labs(x = "Unique Words",
       y = "Frequency",
       title = "Top 10 most popular words found in tweets with Galaxy S20FE") +
  theme(plot.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 12))

s20fe_bing_count <- clean_S20FE %>%
  inner_join(sentiments) %>%
  count(word, sentiment, sort = TRUE) %>%
  mutate(word = reorder(word, n)) 

s20fe_bing_count %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~sentiment, scales = "free_y") +
  theme_minimal() +
  labs(title = "Most common Positive and Negative words in tweets on S20FE",
       y = "Sentiment",
       x = NULL) +
  theme(axis.text = element_text(size = 11, color = "black"), 
        axis.title = element_text(size = 11, color = "black"),
        title = element_text(size = 12))




# what do we expect to see from the sentiment analysis?
# 
# let's concentrate on s20fe and iphone12 
# we want to see what features were good and bad
# 
# stage 1
# perform sentiment analysis on new features from the list in miro
# for each model separately - CANCELLED
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



