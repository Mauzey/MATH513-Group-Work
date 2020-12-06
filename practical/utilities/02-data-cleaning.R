# 
#   Import Tweet/User Data and Clean it
# 

# IMPORT DEPENDENCIES -------------------------------------------------------------------------------------------------------

library(readr)
library(dplyr)
library(stringr)
library(countrycode)

options(scipen = 999)  # Disable scientific notation (for tweet and user ids)


# SET WORKING DIRECTORY -----------------------------------------------------------------------------------------------------

# Will
#setwd("C:/Users/willo/SynologyDrive/Documents - Will/Qualifications/!MSc Data Science/!Courses/1 MATH513 Big Data and Social Network/Assessment/MATH513-Group-Work/practical")

# Tania
#setwd("")

# Alex
#setwd("~/Development/Repositories/MATH513-Group-Work/practical")
#setwd("D:/Development/MATH513-Group-Work/practical")


# IMPORT & COMBINE TWEET DATA -----------------------------------------------------------------------------------------------

# Define columns to keep from the raw tweet data
# The raw tweet data is not going to be overwritten - if you think more columns could be useful, add them to this vector
tweet_cols_to_keep = c('user_id', 'status_id', 'created_at', 'text', 'source', 'is_retweet', 'favorite_count', 'retweet_count',
                       'quote_count', 'reply_count', 'hashtags', 'lang')


# Iterate over each file in the 'tweets' directory
for (file in list.files(path = "./data/tweets", pattern = '*.csv', full.names = T)) {
  new_data <- read_csv(file)[tweet_cols_to_keep]  # read in the file
  
  # Determine which product the file is referring to, and add a column to mark the data as such
  if (grepl('iphone12-', file)) {
    new_data$product <- 'iPhone12'
  } else if (grepl('s20-', file)) {
    new_data$product <- 'Galaxy S20'
  } else if (grepl('s20fe-', file)) {
    new_data$product <- 'Galaxy S20 FE'
  }
  
  # If a dataframe containing all tweets doesn't exist, create it with the first file that's read in
  if (!exists('complete_tweet_data')) {
    complete_tweet_data <- new_data
  # Otherwise, join the file to the existing tweets
  } else {
    complete_tweet_data <- rbind(complete_tweet_data, new_data)
  }
}
rm(new_data, file, tweet_cols_to_keep)  # Remove vars from memory to keep the environment tidy


# IMPORT & COMBINE USER DATA ------------------------------------------------------------------------------------------------

# Define columns to keep from the raw user data
# The raw user data is not going to be overwritten - if you think more columns could be useful, add them to this vector
user_cols_to_keep = c('user_id', 'screen_name', 'name', 'location', 'description', 'followers_count', 'friends_count',
                      'listed_count', 'statuses_count', 'favourites_count', 'account_created_at', 'verified')

# Iterate over each file in the 'users' directory
for (file in list.files(path = "./data/users", pattern = '*.csv', full.names = T)) {
  new_data <- read_csv(file)[user_cols_to_keep]  # read in the file
  
  # If a dataframe containing all users doesn't exist, create it with the first file that's read in
  if (!exists('complete_user_data')) {
    complete_user_data <- new_data
  # Otherwise, join the file to the existing tweets
  } else {
    complete_user_data <- rbind(complete_user_data, new_data)
  }
}
rm(new_data, file, user_cols_to_keep)  # Remove vars from memory to keep the environment tidy


# DATA SUMMARY --------------------------------------------------------------------------------------------------------------

# List column names
names(complete_tweet_data)
names(complete_user_data)


# Show dataframe shape
dim(complete_tweet_data)
dim(complete_user_data)


# Show NA counts for each column
lapply(complete_tweet_data, function(x) {length(which(is.na(x)))})
lapply(complete_user_data, function(x) {length(which(is.na(x)))})


# DATA CLEANING - Tweet Data ------------------------------------------------------------------------------------------------

# Remove duplicate observations
complete_tweet_data <- distinct(complete_tweet_data, status_id, .keep_all = T)


# Remove links, hashtags, emojis, and user mentions from tweet text
complete_tweet_data$stripped_text <- gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", complete_tweet_data$text)  # Links
complete_tweet_data$stripped_text <- gsub("#[A-Za-z0-9]+|@[A-Za-z0-9]+|\\w+(?:\\.\\w+)*/\\S+",  # Hashtags and user mentions
                                          "", complete_tweet_data$stripped_text)
complete_tweet_data$stripped_text <- gsub("[^\x01-\x7F]", "", complete_tweet_data$stripped_text)  # Emojis


# DATA CLEANING - User Data -------------------------------------------------------------------------------------------------

# Remove duplicate observations
complete_user_data <- distinct(complete_user_data, user_id, .keep_all = T)


# Remove links, hashtags, and emojis from description text
complete_user_data$stripped_description <- gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", complete_user_data$description)  # Links
complete_user_data$stripped_description <- gsub("#[A-Za-z0-9]+|@[A-Za-z0-9]+|\\w+(?:\\.\\w+)*/\\S+",  # Hashtags and user mentions
                                          "", complete_user_data$stripped_description)
complete_user_data$stripped_description <- gsub("[^\x01-\x7F]", "", complete_user_data$stripped_description)  # Emojis


# FEATURE ENGINEERING - User Data -------------------------------------------------------------------------------------------

# Mark potential bots based on the source used to post tweets
bot_keywords <- 'ContestGuy|Cubi.so|zigmeme|bot'
bot_tweets <- complete_tweet_data %>%
  filter(grepl(regex(bot_keywords, ignore_case = T), source))  # Compile tweets containing bot keywords
# Mark users as potential bots if their 'user_id' appears in the above dataframe
complete_user_data$potential_bot <- ifelse(complete_user_data$user_id %in% bot_tweets$user_id, T, F)


# Mark potential bots based on the content of the user description
bot_users <- complete_user_data %>%
  filter(grepl(regex(bot_keywords, ignore_case = T), description))  # Compile users whose description contains bot keywords
# Mark users as potential bots if their 'user_id' appears in the above dataframe
complete_user_data$potential_bot <- ifelse(complete_user_data$user_id %in% bot_users | complete_user_data$potential_bot == T,
                                           T, complete_user_data$potential_bot)

rm(bot_keywords, bot_users, bot_tweets)  # Remove vars from memory to keep the environment tidy


# Create a 'country' column, based on the 'location' column
countries <- data.frame(codelist[, c('country.name.en')]) %>% rename(country_name = country.name.en)  # Get a list of countries
extract_country <- function(x) {
  for (country in countries$country_name) {
    if (grepl(regex(country, ignore_case = T), x)) {  # If the country appears in the 'location' column, return that country
      return(country)
    }
  }
}
complete_user_data$country <- lapply(complete_user_data$location, extract_country)
complete_user_data$country[complete_user_data$country == 'NULL'] <- NA  # Mark 'NULL' cells as NA
complete_user_data$country <- unlist(complete_user_data$country, recursive = T, use.names = T)  # Un-list the country column

rm(extract_country, countries)  # Remove vars from memory to keep the environment tidy

# FEATURE ENGINEERING - Tweet Data ------------------------------------------------------------------------------------------

# Extract hash-tags from the tweet text, and add to the 'hashtags' column
extract_hashtags <- function(x) {
  hashtags <- str_extract_all(x, '#\\S+')  # Get hash-tags from the tweet text
  unlist_hashtags <- unlist(hashtags, recursive = T, use.names = T)  # Un-list the hash-tags
  
  return(paste(unlist_hashtags, collapse = ', '))  # Return the hash-tags as a string, separated by commas
}
complete_tweet_data$hashtags <- lapply(complete_tweet_data$text, extract_hashtags)

rm(extract_hashtags)  # Remove vars from memory to keep the environment tidy


# Mark potential spam tweets based on a selection of keywords found in the tweet text
spam_keywords <- 'giveaway|contest|competition|giving away|register|subscribers|your pick'
spam_tweets <- complete_tweet_data %>%
  filter(grepl(regex(spam_keywords, ignore_case = T), text))  # Compile tweets whose text contains spam keywords
# Mark tweets as potential spam if their 'status_id' appears in the above dataframe
complete_tweet_data$potential_spam <- ifelse(complete_tweet_data$status_id %in% spam_tweets$status_id, T, F)


# Mark potential spam tweets based on whether the author is marked as a potential bot
bot_users <- complete_user_data %>%
  filter(potential_bot == T)  # Get users marked as potential bots
# Mark tweets as potential spam if their 'user_id' appears in the above dataframe
complete_tweet_data$potential_spam <- ifelse(complete_tweet_data$user_id %in% bot_users$user_id | complete_tweet_data$potential_spam == T, 
                                             T, complete_user_data$potential_bot)

rm(spam_tweets, spam_keywords, bot_users)  # Remove vars from memory to keep the environment tidy


# Extract mentioned product features for further analysis
features <- 'display|battery|camera|price|5g'
extract_features <- function(x) {
  mentioned_features <- str_extract_all(x, regex(features, ignore_case = T))  # Get features from tweet text
  unlist_mentioned_features <- unique(unlist(mentioned_features, recursive = T, use.names = T))  # Un-list the features
  
  return(tolower(paste(unlist_mentioned_features, collapse = ', ')))  # Return the features as a lower-case string, separated by commas
}
complete_tweet_data$mentioned_features <- lapply(complete_tweet_data$stripped_text, extract_features)
complete_tweet_data$mentioned_features[complete_tweet_data$mentioned_features == ''] <- NA  # Mark empty cells as NA

rm(extract_features, features)  # Remove vars from memory to keep the environment tidy

# EXPORT CLEANED DATA -------------------------------------------------------------------------------------------------------

for (dir in c("./data/", "./data/backup/")) {
  write.csv(complete_tweet_data, paste0(dir, "cleaned-tweets.csv"))
  write.csv(complete_user_data, paste0(dir, "cleaned-users.csv"))
}
