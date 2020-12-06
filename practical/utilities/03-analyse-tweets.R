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
library(sentimentr) #for 'SENTIMENT ANALYSIS - Product Features' section

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
                                         'formats', 'grade', 'mode', 'cloud', 'butterfly\'s'))
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



# We count up how many positive and negative words there are in each tweet
# associate sentiment scores to each tweet

iPhone12_sentiment <- iPhone12_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(tweet_number, sentiment) %>%
  spread(sentiment, n, fill = 0) %>% # negative and positive sentiment in separate columns
  mutate(score = positive - negative) # score = net sentiment (positive - negative)

head(iPhone12_sentiment)
#
# Add a variable to indicate the phone model

iPhone12_sentiment <- iPhone12_sentiment %>% mutate(phone_model = "iPhone12")

# Tabulate the scores
iPhone12_sentiment %>% count(score)

# Let's work out the mean score 
# We'll include it as a line and as a numerical value to our plot
#
sentiment_mean_iPhone12 <- iPhone12_sentiment %>% 
  summarise(mean_score = mean(score))

# Barplot
iPhone12_sentiment %>%
  ggplot(aes(x = score)) + # Sentiment score on x-axis
  geom_bar(fill = "lightgoldenrod3", colour = "brown") + # geom_bar will do the tabulation for you :-)
  geom_vline(aes(xintercept = mean_score), data = sentiment_mean_iPhone12) +
  # Add a vertical line at the mean score, calculated and stored in sentiment_mean_climate above
  geom_text(aes(x = mean_score, 
                y = Inf, 
                label = signif(mean_score, 3)), # Show to three significant figures
            vjust = 2, 
            data = sentiment_mean_iPhone12) + 
  theme_minimal() +
  # Add the mean as a number; vjust moves it down from the top of the plot
  scale_x_continuous(breaks = -10:10,  # Specify a suitable integer range for the x-axis
                     minor_breaks = NULL) + # Show integers; set this to a suitably large range
  labs(title = paste("Sentiments towards iPhone12 give a mean of", signif(sentiment_mean_iPhone12$mean_score, 3)),
       # Title that gives page name and mean sentiment score, to three significant figures
       x = "Sentiment Score", 
       y = "Number of tweets") 


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


# We count up how many positive and negative words there are in each tweet
# associate sentiment scores to each tweet

S20_sentiment <- S20_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(tweet_number, sentiment) %>%
  spread(sentiment, n, fill = 0) %>% # negative and positive sentiment in separate columns
  mutate(score = positive - negative) # score = net sentiment (positive - negative)

head(S20_sentiment)
#
# Add a variable to indicate the topic

S20_sentiment <- S20_sentiment %>% mutate(phone_model = "S20")

# Tabulate the scores
S20_sentiment %>% count(score)

# Let's work out the mean score 
# We'll include it as a line and as a numerical value to our plot
#
sentiment_mean_S20 <- S20_sentiment %>% 
  summarise(mean_score = mean(score))

# Barplot
S20_sentiment %>%
  ggplot(aes(x = score)) + # Sentiment score on x-axis
  geom_bar(fill = "turquoise", colour = "cyan") + # geom_bar will do the tabulation for you :-)
  geom_vline(aes(xintercept = mean_score), data = sentiment_mean_S20) +
  # Add a vertical line at the mean score, calculated and stored in sentiment_mean_climate above
  geom_text(aes(x = mean_score, 
                y = Inf, 
                label = signif(mean_score, 3)), # Show to three significant figures
            vjust = 2, 
            data = sentiment_mean_S20) + 
  theme_minimal() +
  # Add the mean as a number; vjust moves it down from the top of the plot
  scale_x_continuous(breaks = -10:10,  # Specify a suitable integer range for the x-axis
                     minor_breaks = NULL) + # Show integers; set this to a suitably large range
  labs(title = paste("Sentiments towards S20 give a mean of", signif(sentiment_mean_S20$mean_score, 3)),
       # Title that gives page name and mean sentiment score, to three significant figures
       x = "Sentiment Score", 
       y = "Number of tweets") 




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


# We count up how many positive and negative words there are in each tweet
# associate sentiment scores to each tweet

S20FE_sentiment <- S20FE_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(tweet_number, sentiment) %>%
  spread(sentiment, n, fill = 0) %>% # negative and positive sentiment in separate columns
  mutate(score = positive - negative) # score = net sentiment (positive - negative)

head(S20FE_sentiment)
#
# Add a variable to indicate the topic

S20FE_sentiment <- S20FE_sentiment %>% mutate(phone_model = "S20FE")

# Tabulate the scores
S20FE_sentiment %>% count(score)

# Let's work out the mean score 
# We'll include it as a line and as a numerical value to our plot
#
sentiment_mean_S20FE <- S20FE_sentiment %>% 
  summarise(mean_score = mean(score))

# Barplot
S20FE_sentiment %>%
  ggplot(aes(x = score)) + # Sentiment score on x-axis
  geom_bar(fill = "mediumpurple1", colour = "purple") + # geom_bar will do the tabulation for you :-)
  geom_vline(aes(xintercept = mean_score), data = sentiment_mean_S20FE) +
  # Add a vertical line at the mean score, calculated and stored in sentiment_mean_climate above
  geom_text(aes(x = mean_score, 
                y = Inf, 
                label = signif(mean_score, 3)), # Show to three significant figures
            vjust = 2, 
            data = sentiment_mean_S20FE) + 
  theme_minimal() +
  # Add the mean as a number; vjust moves it down from the top of the plot
  scale_x_continuous(breaks = -10:10,  # Specify a suitable integer range for the x-axis
                     minor_breaks = NULL) + # Show integers; set this to a suitably large range
  labs(title = paste("Sentiments towards S20 give a mean of", signif(sentiment_mean_S20FE$mean_score, 3)),
       # Title that gives page name and mean sentiment score, to three significant figures
       x = "Sentiment Score", 
       y = "Number of tweets") 


#PLOTTING IPHONE12 AND S20FE

iPhone12_S20FE_sentiment <-rbind(S20FE_sentiment, iPhone12_sentiment)

sentiment_mean_iPhone12_S20FE <- iPhone12_S20FE_sentiment %>% 
  group_by(phone_model) %>% 
  summarize(mean_score = mean(score)) 


# Perform the plot
iPhone12_S20FE_sentiment %>%
  ggplot(aes(x = score, # Sentiment score on x-axis
           fill = phone_model)) + # Fill bars with a colour according to the topic
  geom_bar() + # geom_bar will do the tabulation for you :-)
  geom_vline(aes(xintercept = mean_score), 
             data = sentiment_mean_iPhone12_S20FE) +
  # Add a vertical line at the mean scores, calculated and stored in sentiment_mean_both above
  geom_text(aes(x = mean_score, 
                y = Inf, 
                label = signif(mean_score, 3)), 
            vjust = 2, 
            data = sentiment_mean_iPhone12_S20FE) + 
  # Add the mean as a number; vjust moves it down from the top of the plot
  scale_x_continuous(breaks = -15:15, 
                     minor_breaks = NULL) + # Show integers; set this to a suitably large range
  scale_fill_manual(values = c("iPhone12" = "lightgoldenrod3", 
                               "S20FE" = "mediumpurple1")) + # Specify your own colours
  labs(x = "Sentiment Score" , 
       y = "Number of tweets", 
       fill = "Phone Model") +
  facet_grid(phone_model ~ .) + 
  theme_minimal() +
  theme(legend.position = "bottom") # Legend on the bottom


#CLEANING AFTER OURSELVES
rm(iPhone12_bing_count, iPhone12_sentiment, 
   iphone12_stopWords, iPhone12_S20FE_sentiment,
            S20_bing_count, S20_sentiment, 
            S20FE_sentiment, S20FE_bing_count)

# SENTIMENT ANALYSIS - Product Features -------------------------------------------------------------------------------------

#For features analysis we'll be using a different package called "sentimentr"

#Find more info about it here:
#https://towardsdatascience.com/sentiment-analysis-in-r-good-vs-not-good-handling-negations-2404ec9ff2ae

# install.packages('sentimentr')
# #or
# library(devtools)
# install_github('trinker/sentimentr')

# “sentimentr attempts to take into account valence shifters 
# (i.e., negators, amplifiers (intensifiers), de-amplifiers (downtoners), 
# and adversative conjunctions) while maintaining speed. Simply put, 
# sentimentr is an augmented dictionary lookup.”


# Compute overall sentiment score for each tweet using sentimentr

tweets_features <- tweets %>% filter(potential_spam != T)

tweets_features$sentiment_score <- 10

tweets_features_length <- 1: dim(tweets_features)[1]

for (i in tweets_features_length){
  temp_sentiment[i] <- sentiment_by(tweets_features$stripped_text[i])
}

temp_sentiment <- sapply(tweets_features$stripped_text, sentiment_by)

# Extract relevant information
feature_sentiment_data <- select(tweets_features, product, mentioned_features, sentiment_score)

# Separate 'mentioned_features' column
feature_sentiment_data <- separate(feature_sentiment_data, col = mentioned_features, into = paste0('feature', 1:5), sep = ', ')

# Pivot feature columns
feature_sentiment <- pivot_longer(feature_sentiment_data,
                                  cols = names(feature_sentiment_data)[which(grepl('feature', names(feature_sentiment_data)))])

# Remove NAs and whitespace
feature_sentiment <- feature_sentiment %>%
  mutate(value = trimws(value)) %>% filter(!is.na(value))

feature_sentiment_stat <- feature_sentiment %>% 
  group_by(product, value) %>% 
  summarise(mean_sentiment = mean(sentiment_score),
            sum_sentiment = sum(sentiment_score))

#AVERAGE SENTIMENT

# Plot feature avg sentiment - point
feature_sentiment_stat %>% 
  group_by(product) %>%
  ggplot(aes(x = product, y = mean_sentiment, color = product)) +
  geom_point(size = 5) +
  facet_wrap(. ~ value) +
  theme_bw() +
  scale_color_manual(values = c("iPhone12" = "lightgoldenrod3", 
                               "Galaxy S20" = "turquoise",
                               "Galaxy S20 FE" = "mediumpurple1"),) +
  scale_y_continuous(name = "Average Sentiment Score", limits = c(0,10)) +
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(title = "Average Sentiment Score of 5 Features for Each of the Phone Models",
       color = "Phone Model")


# Plot feature avg sentiment - bars
feature_sentiment_stat %>% 
  group_by(product) %>%
  ggplot(aes(x = product, y = mean_sentiment, fill = product)) +
  geom_col() +
  facet_wrap(. ~ value) +
  theme_bw() +
  scale_fill_manual(values = c("iPhone12" = "lightgoldenrod3", 
                                "Galaxy S20" = "turquoise",
                                "Galaxy S20 FE" = "mediumpurple1"),) +
  scale_y_continuous(name = "Average Sentiment Score", limits = c(0,10)) +
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(title = "Average Sentiment Score of 5 Features for Each of the Phone Models",
       color = "Phone Model")


#SUM SENTIMENT

# Plot feature avg sentiment - point
feature_sentiment_stat %>% 
  group_by(product) %>%
  ggplot(aes(x = product, y = sum_sentiment, color = product)) +
  geom_point(size = 5) +
  facet_wrap(. ~ value) +
  theme_bw() +
  scale_color_manual(values = c("iPhone12" = "lightgoldenrod3", 
                                "Galaxy S20" = "turquoise",
                                "Galaxy S20 FE" = "mediumpurple1"),) +
  scale_y_continuous(name = "Sentiment Score") +
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(title = "Sentiment Score of 5 Features for Each of the Phone Models",
       color = "Phone Model")


# Plot feature avg sentiment - bars
feature_sentiment_stat %>% 
  group_by(product) %>%
  ggplot(aes(x = product, y = sum_sentiment, fill = product)) +
  geom_col() +
  facet_wrap(. ~ value) +
  theme_bw() +
  scale_fill_manual(values = c("iPhone12" = "lightgoldenrod3", 
                               "Galaxy S20" = "turquoise",
                               "Galaxy S20 FE" = "mediumpurple1"),) +
  scale_y_continuous(name = "Sentiment Score") +
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(title = "Sentiment Score of 5 Features for Each of the Phone Models",
       color = "Phone Model")


#BOXPLOT
feature_sentiment %>% 
  group_by(product) %>%
  ggplot(aes(x = product, y = sentiment_score, color = product)) +
  geom_boxplot() +
  facet_wrap(. ~ value) +
  theme_bw() +
  scale_color_manual(values = c("iPhone12" = "lightgoldenrod3", 
                               "Galaxy S20" = "turquoise",
                               "Galaxy S20 FE" = "mediumpurple1"),) +
  scale_y_continuous(name = "Sentiment Score") +
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(title = "Sentiment Score of 5 Features for Each of the Phone Models",
       color = "Phone Model")




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
#plot s20 against iphone12 (see stage 2) <- DONE


# SLIDE WITH LOCATION PLOT (GLOBE) ----------------------------------------

#got the tweets set -> group by the product -> plot on the globe with diff. colors
