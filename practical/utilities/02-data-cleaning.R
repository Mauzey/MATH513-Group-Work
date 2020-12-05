# 
#   Import Tweet/User Data and Clean it
# 

# IMPORT DEPENDENCIES -------------------------------------------------------------------------------------------------------

library(readr)
library(dplyr)
library(stringr)
library(mgsub)

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

# DATA CLEANING -------------------------------------------------------------------------------------------------------------

# Remove duplicate observations
complete_tweet_data <- distinct(complete_tweet_data, status_id, .keep_all = T)
complete_user_data <- distinct(complete_user_data, user_id, .keep_all = T)

# FEATURE ENGINEERING -------------------------------------------------------------------------------------------------------

# Extract hashtags from the tweet text, and add to the 'hashtags' column
extract_hashtags <- function(row) {
  hashtags <- str_extract_all(row['text'], '#\\S+')
  unlist_hashtags <- unlist(hashtags, recursive = T, use.names = T)
  
  return(paste(unlist_hashtags, collapse = ', '))
}
complete_tweet_data$hashtags <- apply(complete_tweet_data, 1, extract_hashtags)

rm(extract_hashtags)  # Remove vars from memory to keep the environment tidy


# Mark potential spam tweets based on a selection of keywords found in the tweet text
spam_keywords <- 'giveaway|contest|competition|giving away|register'

identify_potential_spam <- function(row) {
  grepl(regex(spam_keywords, ignore_case = T), row['text'])
}
complete_tweet_data$potential_spam <- apply(complete_tweet_data, 1, identify_potential_spam)

rm(spam_keywords, identify_potential_spam)  # Remove vars from memory to keep the environment tidy


# Mark potential bots based on the source used to post tweets
bot_keywords <- 'ContestGuy|Cubi.so|zigmeme|bot|cloud'
bot_tweets <- complete_tweet_data %>%
  filter(grepl(regex(bot_keywords, ignore_case = T), source))  # Compile tweets containing bot keywords

# If a user_id appears in the 'bot_tweets' dataframe, mark that user as a potential bot
complete_user_data$potential_bot <- ifelse(complete_user_data$user_id %in% bot_tweets$user_id, T, F)

rm(bot_keywords, bot_tweets)  # Remove vars from memory to keep the environment tidy

# Remove links, hashtags, and user mentions from tweet text
complete_tweet_data$stripped_text <- gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", complete_tweet_data$text)
complete_tweet_data$stripped_text <- gsub("#[A-Za-z0-9]+|@[A-Za-z0-9]+|\\w+(?:\\.\\w+)*/\\S+",
                                          "", complete_tweet_data$stripped_text)


# Extract mentioned product features for further analysis
product_features <- 'display|battery|camera|price|5g'
extract_mentioned_product_features <- function(row) {
  mentioned_features <- str_extract_all(row['stripped_text'], regex(product_features, ignore_case = T))
  unlist_mentioned_features <- unique(unlist(mentioned_features, recursive = T, use.names = T))
  
  return(tolower(paste(unlist_mentioned_features, collapse = ', ')))
}
complete_tweet_data$mentioned_features <- apply(complete_tweet_data, 1, extract_mentioned_product_features)

complete_tweet_data$mentioned_features[complete_tweet_data$mentioned_features == ''] <- NA  # Mark empty cells as NA


# Move the extracted features into their own separate columns



# EXPORT CLEANED DATA -------------------------------------------------------------------------------------------------------

for (dir in c("./data/", "./data/backup/")) {
  write.csv(complete_tweet_data, paste0(dir, "cleaned-tweets.csv"))
  write.csv(complete_user_data, paste0(dir, "cleaned-users.csv"))
}
