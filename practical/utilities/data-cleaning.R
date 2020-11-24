# 
#   Import Tweet/User Data and Clean it
# 

# CHANGE LOG ----------------------------------------------------------------------------------------------------------------
## 2020-11-21 - AM  - Created file and set up structure with basic data cleaning

# IMPORT DEPENDENCIES / SETUP -----------------------------------------------------------------------------------------------

library(dplyr)
library(readr)

#setwd() # Will
#setwd() # Tania
#setwd("~/Development/Repositories/MATH513-Group-Work/practical")  # Alex
#setwd("D:/Development/MATH513-Group-Work/practical")              #

# IMPORT / COMBINE DATA -----------------------------------------------------------------------------------------------------

for (file in list.file(path = "./data/tweets", pattern = '*.csv', full.names = T)) {
  # if (tweets dataframe doesn't exist):
  #   tweets <- file
  # else:
  #   append file to tweets
}

for (file in list.file(path = "./data/users", pattern = '*.csv', full.names = T)) {
  # if (users dataframe doesn't exist):
  #   users <- file
  # else:
  #   append file to users
}

# PRE-CLEANING ANALYSIS -----------------------------------------------------------------------------------------------------

names(tweets)  # Column names
names(users)   #

dim(tweets)    # Dataset shape
dim(users)     #

# TIME SERIES PLOTS TO CHECK DATES ----------------------------------------------------------------------------------------

# Suggestion: add a column during data cleaning which specifies which product a tweet is referring to, then plot all three 
# ts_plots on a single graph with different colours (using 'filter()'). We'll also be able to use this column for facet_grids
# in the future

ts_plot(iphone12_tweets, "secs", trim = 1) + 
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold")) +
  labs(x = NULL, y = NULL,
       title = "Frequency of Twitter statuses for #iPhone12",
       subtitle = "Twitter status (tweet) counts aggregated using 1-seconds intervals",
       caption = "Source: Data collected from Twitter's REST API via rtweet"
  )

ts_plot(s20fe_tweets, "secs", trim = 1) + 
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold")) +
  labs(x = NULL, y = NULL,
       title = "Frequency of Twitter statuses for #GalaxyS20FE",
       subtitle = "Twitter status (tweet) counts aggregated using 1-seconds intervals",
       caption = "Source: Data collected from Twitter's REST API via rtweet"
  )

ts_plot(s20_tweets, "secs", trim = 1) + 
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold")) +
  labs(x = NULL, y = NULL,
       title = "Frequency of Twitter statuses for #GalaxyS20",
       subtitle = "Twitter status (tweet) counts aggregated using 1-seconds intervals",
       caption = "Source: Data collected from Twitter's REST API via rtweet"
  )

# DATA CLEANING -------------------------------------------------------------------------------------------------------------

# Remove unnecessary columns from the tweets data
cols_to_keep <- c('', '', '')
tweets <- tweets[cols_to_keep]

# Remove unnecessary columns from the user data
cols_to_keep <- c('', '', '')
users <- users[cols_to_keep]

# Remove duplicate rows from the user data; we only need one entry per user
users <- distinct(users, user_id, .keep_all = T)

# Convert blank location entries into 'NA'
users$location[users$location == ''] <- NA

# EXPORT CLEANED DATA -------------------------------------------------------------------------------------------------------

for (dir in c('./data/', './data/backup/')) {
  write.csv(tweets, paste0(dir, 'cleaned-tweets.csv'))
  write.csv(users, paste0(dir, 'cleaned-users.csv'))
}
