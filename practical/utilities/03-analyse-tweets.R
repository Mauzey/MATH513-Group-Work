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
library(RColorBrewer)
library(tidytext)
library(ggthemes)
library(sentimentr) #for 'SENTIMENT ANALYSIS - Product Features' section
library(rworldmap)
library(nortest)

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

# All products timeseries

  tweets %>% group_by(product, is_retweet) %>%
  ts_plot('days', trim = 1) +
  labs(x = NULL, y = NULL,
       title = "Frequency of Twitter Statuses",
       subtitle = "Twitter status counts aggregated using 1-day intervals",
       caption = "Source: Data collected from Twitter's REST API via rtweet",
       color = "Product",
       linetype = "Retweet") +
  theme_bw() +
  theme(plot.title = element_text(face = 'bold'),
        strip.text.y = element_blank()) + 
  geom_vline(xintercept = as.POSIXct(as.Date("2020-03-06")), colour = "turquoise") +
  geom_vline(xintercept = as.POSIXct(as.Date("2020-10-02")), colour = "mediumpurple1") +
  geom_vline(xintercept = as.POSIXct(as.Date("2020-10-23")), colour = "lightgoldenrod3") +
  facet_grid(product ~ .) +
  scale_color_manual(values = c("Galaxy S20" = "turquoise",
                               "Galaxy S20 FE" = "mediumpurple1",
                               "iPhone12" = "lightgoldenrod3"))

# Galaxy S20 FE vs. iPhone12
tweets %>% filter(product == 'Galaxy S20 FE' | product == 'iPhone12') %>%
  group_by(product, is_retweet) %>%
  ts_plot('days', trim = 1) + theme_minimal() +
    facet_grid(product ~ .) +
    labs(x = NULL, y = NULL,
         title = "Frequency of Twitter Statuses",
         subtitle = "Twitter status counts aggregated using 1-day intervals",
         caption = "Source: Data collected from Twitter's REST API via rtweet",
         color = "Product",
         linetype = "Retweet") +
    theme(plot.title = element_text(face = 'bold'),
          strip.text.y = element_blank()) +
    scale_color_manual(values = c("Galaxy S20 FE" = "mediumpurple1",
                               "iPhone12" = "lightgoldenrod3"))

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
    geom_col(fill = 'lightgoldenrod3', colour = 'darkgoldenrod4') +
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

rm(iPhone12_bing_count)  # Remove vars from memory to keep the environment tidy


# Calculate the mean sentiment for tweets regarding the iPhone12
mean_iPhone12_sentiment <- tweets %>%
  filter(potential_spam == F & product == 'iPhone12') %>%
  pull(avg_sentiment) %>%
  mean() %>%
  signif(6)

# Plot the distribution of sentiment for tweets regarding the iPhone12
tweets %>%
  filter(potential_spam == F & product == 'iPhone12') %>%
  ggplot(aes(x = avg_sentiment)) + theme_minimal() +
    geom_histogram(aes(y = ..density..), fill = 'lightgoldenrod3', colour = 'darkgoldenrod4') +
    geom_vline(xintercept = mean_iPhone12_sentiment) +
    geom_text(aes(x = mean_iPhone12_sentiment, y = Inf, label = paste('Mean: ', mean_iPhone12_sentiment)),
              vjust = 2, hjust = -0.1) +
    labs(x = "Sentiment Score", y = "Density",
         title = "Distribution of Sentiment Scores Across iPhone12 Tweets",
         caption = "Source: Data collected from Twitter's REST API via rtweet") +
    theme(plot.title = element_text(face = 'bold'))

rm(mean_iPhone12_sentiment)  # Remove vars from memory to keep the environment tidy

# SENTIMENT ANALYSIS - Galaxy S20 -------------------------------------------------------------------------------------------

# Plot most frequent words in Galaxy S20 tweets
S20_words %>%
  count(word, sort = T) %>%
  head(30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) + theme_minimal() + coord_flip() +
    geom_col(fill = 'turquoise', colour = 'turquoise4') +
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


# Calculate the mean sentiment for tweets regarding the Galaxy S20
mean_S20_sentiment <- tweets %>%
  filter(potential_spam == F & product == 'Galaxy S20') %>%
  pull(avg_sentiment) %>%
  mean() %>%
  signif(6)

# Plot the distribution of sentiment for tweets regarding the Galaxy S20
tweets %>%
  filter(potential_spam == F & product == 'Galaxy S20') %>%
  ggplot(aes(x = avg_sentiment)) + theme_minimal() +
  geom_histogram(aes(y = ..density..), fill = 'turquoise', colour = 'turquoise4') +
  geom_vline(xintercept = mean_S20_sentiment) +
  geom_text(aes(x = mean_S20_sentiment, y = Inf, label = paste('Mean: ', mean_S20_sentiment)),
            vjust = 2, hjust = -0.1) +
  labs(x = "Sentiment Score", y = "Density",
       title = "Distribution of Sentiment Scores Across Galaxy S20 Tweets",
       caption = "Source: Data collected from Twitter's REST API via rtweet") +
  theme(plot.title = element_text(face = 'bold'))

rm(mean_S20_sentiment)  # Remove vars from memory to keep the environment tidy

# SENTIMENT ANALYSIS - Galaxy S20 FE ----------------------------------------------------------------------------------------

# Plot most frequent words in Galaxy S20 FE tweets
S20FE_words %>%
  count(word, sort = T) %>%
  head(30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) + theme_minimal() + coord_flip() +
    geom_col(fill = 'mediumpurple1', colour = 'mediumpurple4') +
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


# Calculate the mean sentiment for tweets regarding the Galaxy S20 FE
mean_S20FE_sentiment <- tweets %>%
  filter(potential_spam == F & product == 'Galaxy S20 FE') %>%
  pull(avg_sentiment) %>%
  mean() %>%
  signif(6)

# Plot the distribution of sentiment for tweets regarding the Galaxy S20 FE
tweets %>%
  filter(potential_spam == F & product == 'Galaxy S20 FE') %>%
  ggplot(aes(x = avg_sentiment)) + theme_minimal() +
  geom_histogram(aes(y = ..density..), fill = 'mediumpurple1', colour = 'mediumpurple4') +
  geom_vline(xintercept = mean_S20FE_sentiment) +
  geom_text(aes(x = mean_S20FE_sentiment, y = Inf, label = paste('Mean: ', mean_S20FE_sentiment)),
            vjust = 2, hjust = -0.1) +
  labs(x = "Sentiment Score", y = "Density",
       title = "Distribution of Sentiment Scores Across Galaxy S20 FE Tweets",
       caption = "Source: Data collected from Twitter's REST API via rtweet") +
  theme(plot.title = element_text(face = 'bold'))

rm(mean_S20FE_sentiment)  # Remove vars from memory to keep the environment tidy

# SENTIMENT ANALYSIS - iPhone12 vs. Galaxy S20 FE ---------------------------------------------------------------------------

# Calculate the mean sentiment for each product
mean_sentiment <- tweets %>%
  group_by(product) %>%
  summarize(mean_sentiment = signif(mean(avg_sentiment), 6))

# Plot the distribution of sentiment for tweets for all models
tweets %>%
  filter(potential_spam == F) %>%
  ggplot(aes(x = avg_sentiment, fill = product)) + theme_minimal() +
    geom_histogram(aes(y = ..density..), colour = 'black') +
  
    # Vertical lines for each product
    geom_vline(data = filter(mean_sentiment, product == 'iPhone12'), aes(xintercept = mean_sentiment)) +
    geom_vline(data = filter(mean_sentiment, product == 'Galaxy S20 FE'), aes(xintercept = mean_sentiment)) +
  geom_vline(data = filter(mean_sentiment, product == 'Galaxy S20'), aes(xintercept = mean_sentiment)) +
    # Mean sentiment text for each product
    geom_text(data = filter(mean_sentiment, product == 'iPhone12'),
              aes(x = mean_sentiment, y = Inf, label = paste('Mean: ', mean_sentiment), vjust = 2, hjust = -0.1)) +
    geom_text(data = filter(mean_sentiment, product == 'Galaxy S20 FE'),
              aes(x = mean_sentiment, y = Inf, label = paste('Mean: ', mean_sentiment), vjust = 2, hjust = -0.1)) +
    geom_text(data = filter(mean_sentiment, product == 'Galaxy S20'),
            aes(x = mean_sentiment, y = Inf, label = paste('Mean: ', mean_sentiment), vjust = 2, hjust = -0.1)) +
  
    facet_grid(product ~ .) +
    scale_fill_manual(values = c('iPhone12' = 'lightgoldenrod3', 
                                 'Galaxy S20 FE' = 'mediumpurple1',
                                 'Galaxy S20' = 'turquoise')) +
    labs(x = "Sentiment Score", y = "Density",
         title = "Distribution of Sentiment Scores Across Tweets Related to All the Models",
         caption = "Source: Data collected from Twitter's REST API via rtweet",
         fill = "Product: ") +
    theme(legend.position = 'bottom',
          plot.title = element_text(face = 'bold'),
          strip.text.y = element_blank())

rm(mean_sentiment)  # Remove vars from memory to keep the environment tidy

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

# Extract relevant information
feature_sentiment_data <- select(tweets, product, mentioned_features, avg_sentiment, sd_sentiment)

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
  summarise(mean_sentiment = mean(avg_sentiment),
            sum_sentiment = sum(avg_sentiment))

#AVERAGE SENTIMENT
feature_sentiment %>% count(product, value)

feature_sentiment %>% count(product)


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
  scale_y_continuous(name = "Average Sentiment Score", limits = c(0,1)) +
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
  scale_y_continuous(name = "Average Sentiment Score", limits = c(0,1)) +
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(title = "Average Sentiment Score of 5 Features for Each of the Phone Models",
       color = "Phone Model")


#SUM SENTIMENT

# Plot feature accemulated sentiment - point
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


# Plot feature accumulated sentiment - bars
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
  ggplot(aes(x = product, y = avg_sentiment, color = product)) +
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

#DOING STANDARD SCALING: new_val = (val-mean)/sd for each model

stat_s20 <- feature_sentiment %>% 
  filter(product == "Galaxy S20") %>% 
  summarise(sd = sd(avg_sentiment),
            mean = mean(avg_sentiment)) 

stat_iphone12 <- feature_sentiment %>% 
  filter(product == "iPhone12") %>% 
  summarise(sd = sd(avg_sentiment),
            mean = mean(avg_sentiment))
  
stat_s20fe <- feature_sentiment %>% 
  filter(product == "Galaxy S20 FE")%>% 
  summarise(sd = sd(avg_sentiment),
            mean = mean(avg_sentiment))

feature_sentiment <- feature_sentiment %>%
  mutate(norm_sentiment = avg_sentiment)

feature_sentiment <- feature_sentiment %>% 
  mutate_at(vars(norm_sentiment), 
            ~case_when(product == 'Galaxy S20'~ (. - stat_s20$mean)/stat_s20$sd,
                       product == 'Galaxy S20 FE'~ (. - stat_s20fe$mean)/stat_s20fe$sd,
                       product == 'iPhone12'~ (. - stat_s20$mean)/stat_s20$sd),
            TRUE ~ as.numeric(.))





#NORMALIZED PLOTS:::!!!!!


feature_sentiment_stat <- feature_sentiment %>% 
  group_by(product, value) %>% 
  summarise(mean_sentiment = mean(norm_sentiment),
            sum_sentiment = sum(norm_sentiment))


# Plot feature avg sentiment - point -NOT WORKING!!!
feature_sentiment_stat %>%
  group_by(product) %>%
  ggplot(aes(x = product, y = mean_sentiment, color = product)) +
  geom_point(size = 5) +
  facet_wrap(. ~ value) +
  theme_bw() +
  scale_color_manual(values = c("iPhone12" = "lightgoldenrod3",
                                "Galaxy S20" = "turquoise",
                                "Galaxy S20 FE" = "mediumpurple1")) +
  scale_y_continuous(name = "Average Sentiment Score", limits = c(-1, 1)) +
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
  scale_y_continuous(name = "Average Sentiment Score", limits = c(-1, 1)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(title = "Average Sentiment Score of 5 Features for Each of the Phone Models",
       color = "Phone Model")


#SUM SENTIMENT

# Plot feature accumulated sentiment - point
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


# Plot feature accumulated sentiment - bars
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
  ggplot(aes(x = product, y = norm_sentiment, color = product)) +
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

# SENTIMENT ANALYSIS - Geographical Data ------------------------------------------------------------------------------------

# Get the 'user_id' and 'country' of users with valid country values
geo_users <- users %>%
  filter(potential_bot == F & !is.na(country)) %>%
  select(user_id, country)

# Get 'avg_sentiment' and 'product' of tweets posted by users with valid country values
geo_tweets <- tweets %>%
  filter(potential_spam == F & user_id %in% geo_users$user_id) %>%
  select(user_id, product, avg_sentiment)

# Merge the data frames
geo_sentiment <- merge(geo_users, geo_tweets, by = 'user_id')



# IPHONE 12

# Join data referenced by country codes to an internal map
iPhone12_matched <- geo_sentiment %>%
  filter(product == 'iPhone12') %>%
  joinCountryData2Map(joinCode = 'NAME', nameJoinColumn = 'country')
# Plot the average sentiment for tweets regarding the iPhone12, for each country
mapCountryData(iPhone12_matched,
               mapTitle = "iPhone12 Sentiment by Country",
               borderCol = 'gray40',
               colourPalette = c('lightgoldenrod', 'goldenrod2', 'darkgoldenrod4'),
               nameColumnToPlot = 'avg_sentiment', catMethod = 'pretty')



# GALAXY S20

# Join data referenced by country codes to an internal map
S20_matched <- geo_sentiment %>%
  filter(product == 'Galaxy S20') %>%
  joinCountryData2Map(joinCode = 'NAME', nameJoinColumn = 'country')
# Plot the average sentiment for tweets regarding the iPhone12, for each country
mapCountryData(S20_matched,
               mapTitle = "Galaxy S20 Sentiment by Country",
               borderCol = 'gray40',
               colourPalette = c('paleturquoise', 'mediumturquoise', 'turquoise4'),
               nameColumnToPlot = 'avg_sentiment', catMethod = 'pretty')



# GALAXY S20 FE

# Join data referenced by country codes to an internal map
S20FE_matched <- geo_sentiment %>%
  filter(product == 'Galaxy S20 FE') %>%
  joinCountryData2Map(joinCode = 'NAME', nameJoinColumn = 'country')
# Plot the average sentiment for tweets regarding the iPhone12, for each country
mapCountryData(S20FE_matched,
               mapTitle = "Galaxy S20 FE Sentiment by Country",
               borderCol = 'gray40',
               colourPalette = c('thistle', 'mediumpurple2', 'purple4'),
               nameColumnToPlot = 'avg_sentiment', catMethod = 'pretty')


#T-TEST

#we can run the t-test if :
# when the two groups of samples (A and B), being compared, are normally distributed. 
# This can be checked using Shapiro-Wilk test.
# 
# and when the variances of the two groups are equal. 
# This can be checked using F-test.

#If we were to perform t-test, we'd do it on sentiment values for tweets related to 
#each model we're using, regardless of whether it's related to any feature or not

#first let's plot them and see if the tweets sentiments for each model are normally 
#distributed. package for sentiments used - 'sentimentr'


feature_sentiment_data %>%
  ggplot(aes(x=avg_sentiment, group=product, fill=product)) +
  geom_density(adjust=1.5, alpha = .9) +
  theme_bw() +
  facet_wrap(~product) +
  scale_fill_manual(values = c("Galaxy S20" = "turquoise",
                               "Galaxy S20 FE" = "mediumpurple1",
                               "iPhone12" = "lightgoldenrod3")) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank()
  ) +
  labs(x = "Average Sentiment",
       y = "Density",
       title = "Density Plots for Overall Sentiments of 3 Products")


feature_sentiment_data %>%
  ggplot(aes(x=avg_sentiment, group=product, fill=product)) +
  geom_density(adjust=1.5, alpha=.3) +
  theme_bw() +
  scale_fill_manual(values = c("Galaxy S20" = "turquoise",
                               "Galaxy S20 FE" = "mediumpurple1",
                               "iPhone12" = "lightgoldenrod3")) +
  labs(fill = "Product")

#T-TEST

s20fe_vals <- feature_sentiment_data %>% 
  filter(product=="Galaxy S20 FE") %>% .$avg_sentiment

s20_vals <- feature_sentiment_data %>% 
  filter(product=="Galaxy S20") %>% .$avg_sentiment

iphone12_vals <- feature_sentiment_data %>% 
  filter(product=="iPhone12") %>% .$avg_sentiment

vals <- list(s20 = s20_vals, s20fe = s20fe_vals, iphone12 = iphone12_vals)


#KOLMOGOROV-SMIRNOV

ks.test(x=vals$s20, y='pnorm')
# data:  vals$s20
# D = 0.40721, p-value < 2.2e-16
# alternative hypothesis: two-sided

ks.test(x=vals$s20fe, y='pnorm')
# data:  vals$s20fe
# D = 0.45695, p-value < 2.2e-16
# alternative hypothesis: two-sided

ks.test(x=vals$iphone12, y='pnorm')
# data:  vals$iphone12
# D = 0.40272, p-value < 2.2e-16
# alternative hypothesis: two-sided

#ANDERSON-DARLING normality test

ad.test(vals$s20)
# data:  vals$s20
# A = 396.71, p-value < 2.2e-16

ad.test(vals$s20fe)
# data:  vals$s20fe
# A = 660.69, p-value < 2.2e-16

ad.test(vals$iphone12)
# data:  vals$iphone12
# A = 687.88, p-value < 2.2e-16

t.test(vals$s20fe, vals$iphone12)
t.test(vals$s20fe, vals$s20)


