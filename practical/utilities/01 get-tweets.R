#
#   Download Tweets using the Twitter REST API via rtweet
#

# CHANGE LOG ----------------------------------------------------------------------------------------------------------------
## 2020-11-20 - WON - Move any variables to the top of the file for easy editing
##                  - Added individual change log

## 2020-11-21 - AM  - Environment variables are now loaded from a text file rather than 'Sys.getenvs()'

# IMPORT DEPENDENCIES / SETUP -----------------------------------------------------------------------------------------------

library(rtweet)

#setwd("C:/Users/willo/SynologyDrive/Documents - Will/Qualifications/!MSc Data Science/!Courses/1 MATH513 Big Data and Social Network/Assessment/MATH513-Group-Work/practical") # Will
#setwd() # Tania
#setwd("~/Development/Repositories/MATH513-Group-Work/practical")  # Alex
#setwd("D:/Development/MATH513-Group-Work/practical")              # 

# IMPORT ENVIRONMENT VARIABLES ----------------------------------------------------------------------------------------------

env_file <- file('./twitter-keys.txt', open = 'r')  # Define env vars file
envs <- readLines(env_file)  # Read in the file

twitter_app <- strsplit(envs, ' ')[[1]][3]
twitter_token <- strsplit(envs, ' ')[[2]][3]
twitter_secret <- strsplit(envs, ' ')[[3]][3]

# DEFINE LOCAL VARIABLES ----------------------------------------------------------------------------------------------------

# Hashtags
iphone12_query <- '#iPhone12'
s20fe_query <- '#GalaxyS20FE'
s20_query <- '#GalaxyS20'

# Date Ranges for Tweet Searches (format: YYYYMMDDHHMM)
iphone_12_date_range <-c(NULL, NULL)
s20fe_date_range <- c(NULL, NULL)
s20_date_range <- c(NULL, NULL)

# Name of the user pulling tweets for use in file names (to avoid overwriting each others' data)
name <- 'alex'

# Initialize Twitter API token
twitter_token <- create_token(
  app <- twitter_app,
  consumer_key <- twitter_token,
  consumer_secret <- twitter_secret
)

# ACQUIRE TWEETS ------------------------------------------------------------------------------------------------------------

# Get iPhone 12 Tweets
iphone12_tweets <- search_30day(q = iphone12_query, n = 12500, token = twitter_token,
                                fromDate = iphone12_date_range[1], toDate = iphone12_date_range[2])

# Get Samsung Galaxy S20 FE Tweets
s20fe_tweets <- search_30day(q = s20fe_query, n = 12500, token = twitter_token,
                             fromDate = s20fe_date_range[1], toDate = s20fe_date_range[2])

# Get Samsung Galaxy S20 Tweets
s20_tweets <- search_fullarchive(q = s20_query, n = 5000, token = twitter_token,
                                 fromDate = s20_date_range[1], toDate = s20_date_range[2])

# Extract user data
iphone12_users <- user_data(iphone12_tweets)
s20fe_users <- user_data(s20fe_tweets)
s20_users <- user_data(s20_tweets)

# PARSE LIST COLUMNS --------------------------------------------------------------------------------------------------------

# DataFrame columns of class 'list' cannot be exported as a .csv file - The following function fixes this by parsing any 
# lists into a string containing all list values, separated by commas
list_to_char <- function(column) {
  if (class(column) == 'list') {
    new_column <- paste(unlist(column[1]), sep = '', collapse = ', ')
  } else {
    new_column <- column
  }
  return(new_column)
}

# Apply the above function to each set of Tweets
iphone12_tweets_parsed <- data.frame(lapply(iphone12_tweets, list_to_char), stringsAsFactors = F)
s20fe_tweets_parsed <- data.frame(lapply(s20fe_tweets, list_to_char), stringsAsFactors = F)
s20_tweets_parsed <- data.frame(lapply(s20_tweets, list_to_char), stringsAsFactors = F)

# Apply the above function to each set of users
iphone12_users_parsed <- data.frame(lapply(iphone12_users, list_to_char), stringsAsFactors = F)
s20fe_users_parsed <- data.frame(lapply(s20fe_users, list_to_char), stringsAsFactors = F)
s20_users_parsed <- data.frame(lapply(s20_users, list_to_char), stringsAsFactors = F)

# EXPORT DATA ---------------------------------------------------------------------------------------------------------------

# Export data to './data' and './data/backup'
for (dir in c('./data/', './data/backup/')) {
  write.csv(iphone12_tweets_parsed, paste0(dir, 'tweets/iphone12-tweets-', name, '.csv'))  # Export iPhone12 data
  write.csv(iphone12_users_parsed, paste0(dir, 'users/iphone12-users-', name, '.csv'))     #
  
  write.csv(s20fe_tweets_parsed, paste0(dir, 'tweets/s20fe-tweets-', name, '.csv'))        # Export Samsung Galaxy S20 FE data
  write.csv(s20fe_users_parsed, paste0(dir, 'users/s20fe-users-', name, '.csv'))           #
  
  write.csv(s20_tweets_parsed, paste0(dir, 'tweets/s20-tweets-', name, '.csv'))            # Export Samsung Galaxy S20 data
  write.csv(s20_users_parsed, paste0(dir, 'users/s20-users-', name, '.csv'))               #
}
