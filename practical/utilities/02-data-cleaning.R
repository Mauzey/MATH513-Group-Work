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

# Remove duplicate entries
complete_tweet_data <- distinct(complete_tweet_data, status_id, .keep_all = T)
complete_user_data <- distinct(complete_user_data, user_id, .keep_all = T)

# Handle NA values

#***CLEANING TWEETS***

# Removing column 'media_type' since it got all NAs
complete_tweet_noNa <- complete_tweet_data %>% select(-media_type)

#after the next magic line you'll get the data cut in 3 - only 25k obs. left
#removing observations where source value is NA
#complete_tweet_noNa <- complete_tweet_noNa %>% na.omit(source)

#Show all the sources
complete_tweet_noNa %>% group_by(source) %>% summarise(n = n()) %>% arrange(desc(n))

#show the number of tweets that were posted using sources that have 'bot' in them
# complete_tweet_noNa %>%
#   filter(str_detect(source, regex(".bot.|.bot", ignore_case = TRUE))) %>%
#   group_by(source) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))

#filter the sources which have 'bot' in their name and save in the list
# botSource <- complete_tweet_noNa %>% 
#   filter(str_detect(source, regex(".bot.|.bot|bot.", ignore_case = TRUE))) %>% 
#   group_by(source) %>% 
#   summarise(n = n()) %>% 
#   select(source)
# Remove identified sources as bots

# Show list of all sources from data

data_with_bot_sources <- complete_tweet_noNa %>%
  filter(str_detect(source, regex(".bot.|.bot", ignore_case = TRUE)))

# Remove tweets with source in collection
sources_to_remove <- c('Bot libre!','ContestGuy', 'Cubi.so') #sources that Will wrote
#sources_to_remove <- c(sources_to_remove, botSource$source)


#searching for all the tweets with sources from Will's list
data_with_bot_sources_2 <- complete_tweet_noNa %>% 
  filter(source %in% sources_to_remove)

#adding the tweets to already existing tibble with tweets posted through bot-like sources
data_with_bot_sources <- rbind(data_with_bot_sources, data_with_bot_sources_2)


#removing the tweets posted through bot-like sources with 'anti_join'
complete_tweet_noNa <- complete_tweet_noNa %>%
  anti_join(data_with_bot_sources)

#searching if we've filtered all the 'win' 'competition' types of tweets
#and adding them to a tibble in order to disjoint them later

data_with_bs <- complete_tweet_noNa %>% 
  filter(str_detect(text, 
                    regex(".win|win.|competition|i want|give me", ignore_case = TRUE)))

#removing the BS tweets

#TANIA: I DIDN'T CLEAN THE USER DATA YET, CAUSE OBVIOUSLY SOME USERS WILL BE REMOVED
#AND SO I DIDN'T SAVE THE DATA TO NEW CSV FILES
#the following code wasn't performed yet
complete_tweet_noNa <- complete_tweet_noNa %>% 
  anti_join(data_with_bs)
# EXPORT CLEANED DATA -------------------------------------------------------------------------------------------------------

for (dir in c("./data/", "./data/backup/")) {
  write.csv(complete_tweet_data, paste0(dir, "cleaned-tweets.csv"))
  write.csv(complete_user_data, paste0(dir, "cleaned-users.csv"))
}
