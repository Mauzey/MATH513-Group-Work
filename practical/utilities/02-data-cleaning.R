# 
#   Import Tweet/User Data and Clean it
# 

# IMPORT DEPENDENCIES -------------------------------------------------------------------------------------------------------

library(readr)
library(dplyr)

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
                       'quote_count', 'reply_count', 'hashtags', 'media_type', 'lang')

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
  
  rm(new_data)  # delete the 'new_data' variable from memory to keep the environment tidy
}


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
  
  rm(new_data)  # delete the 'new_data' variable from memory to keep the environment tidy
}

# DATA CLEANING -------------------------------------------------------------------------------------------------------------

# Remove duplicate entries
complete_tweet_data <- distinct(complete_tweet_data, status_id, .keep_all = T)
complete_user_data <- distinct(complete_user_data, user_id, .keep_all = T)

# Convert blank location values to 'NA'
# ...

# EXPORT CLEANED DATA -------------------------------------------------------------------------------------------------------

for (dir in c("./data/", "./data/backup/")) {
  write.csv(complete_tweet_data, paste0(dir, "cleaned-tweets.csv"))
  write.csv(complete_user_data, paste0(dir, "cleaned-users.csv"))
}

# DATA SUMMARY --------------------------------------------------------------------------------------------------------------

# List column names
names(tweets)
names(users)

# Show dataframe shape
dim(tweets)
dim(users)

# EXPORT CLEANED DATA -------------------------------------------------------------------------------------------------------

for (dir in c('./data/', './data/backup/')) {
  write.csv(tweets, paste0(dir, 'cleaned-tweets.csv'))
  write.csv(users, paste0(dir, 'cleaned-users.csv'))
}
