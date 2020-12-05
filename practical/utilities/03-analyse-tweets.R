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

# Factorize product column
tweets$product <- as.factor(tweets$product)

# All products
tweets %>% group_by(product, is_retweet) %>%
  ts_plot('days', trim = 1) + theme_minimal() +
  facet_grid(product ~ .) +
  labs(x = NULL, y = NULL,
       title = "Frequency of Twitter Statuses",
       subtitle = "Twitter status counts aggregated using 1-day intervals",
       caption = "Source: Data collected from Twitter's REST API via rtweet") +
  theme(plot.title = element_text(face = 'bold'))

# Galaxy S20 FE vs. iPhone12
tweets %>% filter(product == 'Galaxy S20 FE' | product == 'iPhone12') %>%
  group_by(product, is_retweet) %>%
  ts_plot('days', trim = 1) + theme_minimal() +
    facet_grid(product ~ .) +
    labs(x = NULL, y = NULL,
         title = "Frequency of Twitter Statuses",
         subtitle = "Twitter status counts aggregated using 1-day intervals",
         caption = "Source: Data collected from Twitter's REST API via rtweet") +
    theme(plot.title = element_text(face = 'bold'))

# SENTIMENT ANALYSIS - PREPARATION ------------------------------------------------------------------------------------------

# Filter out spam
tweets_no_spam <- tweets %>% filter(potential_spam == F)

# ...
extract_words <- function(data) {
  return(data %>%
           select(stripped_text) %>%
           mutate(tweet_number = row_number()) %>%
           unnest_tokens(word, stripped_text)
  )
}
iPhone12_words<- extract_words(tweets_no_spam %>% filter(product == 'Galaxy S20'))
S20_words <- extract_words(tweets_no_spam %>% filter(product == 'Galaxy S20 FE'))
S20FE_words <- extract_words(tweets_no_spam %>% filter(product == 'iPhone12'))
rm(tweets_no_spam, extract_words)  # Remove vars from memory to keep the environment tidy

# Remove stop words
data('stop_words')
iPhone12_words <- iPhone12_words %>% anti_join(stop_words)
S20_words <- S20_words %>% anti_join(stop_words)
S20FE_words <- S20FE_words %>% anti_join(stop_words)
rm(stop_words)  # Remove vars from memory to keep the environment tidy

# Remove custom stop words
custom_stop_words <- data.frame(word = c('iphone12', 'iphone', 'apple', '12', 'iphone12pro', 'iphone12promax', 'max', 'min',
                                         'iphone12mini', 'series', 'ios', 'tim_cook', 'uf449', '5544', 'uf525uf525uf525',
                                         'tgfamilyuf64whats', 'stevewoz', '18iphone12', 'timcook', 'ios14', 'pro', 'appleevent',
                                         'id', 'mini', 'phone', 'amazon', 'pick', 'buy', 'shop', 'iphone11', '20',
                                         'appleiphone12', '11', 'free', 'cashback', 'limited', '0001f525', 'giveaway', 'amp',
                                         'bauxyvcr2z', 'fe0f', '393c', '3e32', '0001f449', '0001f64c', '2063', '0001f64f', 'win',
                                         '5', '4', '3', '1', '20b9', '2764', '18', '0001f447', '2796', 'pqsilf3aj0',
                                         'galaxys20', 'galaxy', 'samsung', 's20', 'di', 'ini', 'ultra', 'indonesia', 'pake',
                                         'uf60dsemua', 'timur', 'kok', 'java', 'djambil', 'bukan', 'bondowoso', 'samsungmobilesa',
                                         'expressoshow', 'galaxys20ultra', 'pre', 'uf32fget', 'introducing', 'renders',
                                         'samsungindia', 'official', 'smartphone', 'unpacked2020', 'galaxys20fe', 'phone',
                                         'swiss', 'jawa', 'diambil', 'galaxys20plus', 'mobile', 'galaxys20fultra5g', 'uf60d',
                                         'samsungevent', 'fe', '2020', 'samsunggalaxy', 'unpacked', 'galaxyzflip', 'day', 'check',
                                         'galaxyunpacked', 'leaked', 'de', 'en', 'samsungmobile', '0001f60d', '0001f49c',
                                         '0001f31f', 'contestalert', 'season', 'mintknow', 'btsus', 'moreu', '0001f4b0',
                                         'withugalaxys20feuin', 'jiminukeeps', 'bts', 'galaxys20fesuperfan', 'withgalaxy', 'fe',
                                         'galaxybuds', 'madeforfans', 's20fecamerafan', 'uf49a', 'teamgalaxy', 'galaxyxbts',
                                         '0001f49a', '3e30', '613c', '15', 'semua', 'invebuybmf', 'winner', 'san', 'francisco',
                                         'makes', 'learn', 'apply', 'wfvkmdrti2', 'active2', 'tlch6y0bsp', '30x', 'buds', '0e2d',
                                         '0001f48e', '3x', '383c', 'foto', '0001f499', 'india', 'retweet', 'password', 'usdt',
                                         '_cook', 'newly', '_problem', '0e01', '2', '0001f3fb', 'variant', 'unlock', 'locked',
                                         'reset', 'recovery', 'passcode', 'review', 'launched', 'past', 'time', 'join', 'drop',
                                         'pair', 'w7gnpkkvxi', 'shout', '0e32', 'nov', 'collection', 'winter', 'draw', 'dec',
                                         '3kwjjkvz8o', '2744', '4pm', '27', '0001f606', 'vip', '23f0', '58', '3am', '88', '26c4',
                                         'utc', 'tmsvspi6pw', 'okb', '50,000', '6000', 'gt', 'black', '0001f4f1', 'ft', 'cosmo',
                                         '0001f92f', 'blue', 'ares', 'unboxing', 'blason', 'fwn7ozovs7', '0001f6a8', 'follow',
                                         'devices', 'isn', 'link', '10', 'rt', 'friday', 'store', 'close', 'space', 'glow',
                                         'formats', 'grade', 'mode', 'butterfly\'s'))
iPhone12_words <- iPhone12_words %>% anti_join(custom_stop_words)
S20_words <- S20_words %>% anti_join(custom_stop_words)
S20FE_words <- S20FE_words %>% anti_join(custom_stop_words)

rm(custom_stop_words, word)  # Remove vars from memory to keep the environment tidy

# SENTIMENT ANALYSIS - iPhone12 ---------------------------------------------------------------------------------------------

# Plot most frequent words in iPhone 12 tweets
iPhone12_words %>%
  count(word, sort = T) %>%
  head(30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) + theme_minimal() + coord_flip() +
    geom_col(fill = 'lightgoldenrod3', colour = 'white') +
    labs(x = "Unique Words", y = "Frequency",
         title = "Top 30 Most Popular Words in iPhone12 Tweets") +
    theme(axis.title = element_text(face = 'bold'),
          axis.text.y = element_text(size = 12))

# Get sentiment scores for iPhone12 words
iPhone12_bing_count <- iPhone12_words %>%
  inner_join(sentiments %>% filter(sentiments$word != 'matte')) %>%
  count(word, sentiment, sort = T) %>%
  mutate(word = reorder(word, n))

# Plot sentiment scores for iPhone12 words
iPhone12_bing_count %>% group_by(sentiment) %>%
  top_n(10) %>% ungroup() %>%
  ggplot(aes(x = word, y = n, fill = sentiment)) + theme_minimal() + coord_flip() +
    geom_col(show.legend = F) +
    facet_wrap(~ sentiment, scales = 'free_y') +
    labs(x = NULL, y = "Sentiment",
         title = "Most Common Positive and Negative Words in iPhone12 Tweets") +
    theme(axis.text = element_text(size = 11, colour = 'black'),
          axis.title = element_text(size = 11, colour = 'black'),
          title = element_text(size = 12))

# SENTIMENT ANALYSIS - Galaxy S20 -------------------------------------------------------------------------------------------

# Plot most frequent words in Galaxy S20 tweets
S20_words %>%
  count(word, sort = T) %>%
  head(30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) + theme_minimal() + coord_flip() +
    geom_col(fill = 'turquoise', colour = 'white') +
    labs(x = "Unique Words", y = "Frequency",
         title = "Top 30 Most Popular Words in Galaxy S20 Tweets") +
    theme(axis.title = element_text(face = 'bold'),
          axis.text.y = element_text(size = 12))

# Get sentiment scores for Galaxy S20 words
S20_bing_count <- S20_words %>%
  inner_join(sentiments) %>%
  count(word, sentiment, sort = T) %>%
  mutate(word = reorder(word, n))

# Plot sentiment scores for Galaxy S20 words
S20_bing_count %>% group_by(sentiment) %>%
  top_n(10) %>% ungroup() %>%
  ggplot(aes(x = word, y = n, fill = sentiment)) + theme_minimal() + coord_flip() +
    geom_col(show.legend = F) +
    facet_wrap(~ sentiment, scales = 'free_y') +
    labs(x = NULL, y = "Sentiment",
         title = "Most Common Positive and Negative Words in Galaxy S20 Tweets") +
    theme(axis.text = element_text(size = 11, colour = 'black'),
          axis.title = element_text(size = 11, colour = 'black'),
          title = element_text(size = 12))

# SENTIMENT ANALYSIS - Galaxy S20 FE ----------------------------------------------------------------------------------------

# Plot most frequent words in Galaxy S20 FE tweets
S20FE_words %>%
  count(word, sort = T) %>%
  head(30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) + theme_minimal() + coord_flip() +
    geom_col(fill = 'mediumpurple1', colour = 'white') +
    labs(x = "Unique Words", y = "Frequency",
         title = "Top 30 Most Popular Words in Galaxy S20 FE Tweets") +
    theme(axis.title = element_text(face = 'bold'),
          axis.text.y = element_text(size = 12))

# Get sentiment scores for Galaxy S20 FE words
S20FE_bing_count <- S20FE_words %>%
  inner_join(sentiments) %>%
  count(word, sentiment, sort = T) %>%
  mutate(word = reorder(word, n))

# Plot sentiment scores for Galaxy S20FE words
S20FE_bing_count %>% group_by(sentiment) %>%
  top_n(10) %>% ungroup() %>%
  ggplot(aes(x = word, y = n, fill = sentiment)) + theme_minimal() + coord_flip() +
    geom_col(show.legend = F) +
    facet_wrap(~ sentiment, scales = 'free_y') +
    labs(x = NULL, y = "Sentiment",
         title = "Most Common Positive and Negative Words in Galaxy S20 FE Tweets") +
    theme(axis.text = element_text(size = 11, colour = 'black'),
          axis.title = element_text(size = 11, colour = 'black'),
          title = element_text(size = 12))

# SENTIMENT ANALYSIS - Product Features -------------------------------------------------------------------------------------


test <- select(tweets, mentioned_features, product)
test$sentiment_score <- 10

test2 <- separate(test, col = mentioned_features, into = paste0('feature', 1:5), sep = ', ')
test3 <- pivot_longer(test2, cols = names(test2)[which(grepl('feature', names(test2)))])
test4 <- test3 %>%
  mutate(value = trimws(value)) %>%
  filter(!is.na(value))

ggplot(test4, aes(x = product, y = sentiment_score)) +
  geom_point() +
  facet_wrap(. ~ value)


# TODO:
#   - Filter data for the three products by 5 features (tweets$mentioned_features)
#       - Display
#       - Battery
#       - Camera
#       - Price
#       - 5G
#   - Perform sentiment analysis on these features - CANCELLED
#   - For these features, facet grid sentiment for the Galaxy S20 FE and iPhone12
#   - Draw conclusions from these plots

# ---------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------

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
