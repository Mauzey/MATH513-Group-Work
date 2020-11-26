#
#   Download Tweets using the Twitter REST API via rtweet
#

# IMPORT DEPENDENCIES -------------------------------------------------------------------------------------------------------

library(rtweet)

# SET WORKING DIRECTORY -----------------------------------------------------------------------------------------------------

# Will
#setwd("C:/Users/willo/SynologyDrive/Documents - Will/Qualifications/!MSc Data Science/!Courses/1 MATH513 Big Data and Social Network/Assessment/MATH513-Group-Work/practical")

# Tania
#setwd("")

# Alex
#setwd("~/Development/Repositories/MATH513-Group-Work/practical")
#setwd("D:/Development/MATH513-Group-Work/practical")

# IMPORT TWITTER API INFORMATION --------------------------------------------------------------------------------------------

api_file <- file('./twitter-keys.txt', open = 'r')  # define api information file
api_info <- readLines(api_file)  # read in the file

twitter_app <- strsplit(api_info, ' ')[[1]][3]
twitter_token <- strsplit(api_info, ' ')[[2]][3]
twitter_secret <- strsplit(api_info, ' ')[[3]][3]
twitter_access_token <- strsplit(api_info, ' ')[[4]][3]
twitter_access_secret <- strsplit(api_info, ' ')[[5]][3]


# Initialise token
twitter_token <- create_token(
  app <- twitter_app,
  consumer_key <- twitter_token,
  consumer_secret <- twitter_secret,
  access_token <- twitter_access_token,
  access_secret <- twitter_access_secret
)

# DEFINE LOCAL VARIABLES ----------------------------------------------------------------------------------------------------

# Names - this prevents us overwriting each others' data
#name <- 'will'
#name <- 'tania'
#name <- 'alex'

# Search queries
iphone12_query <- "#iPhone12 (lang:en)"
s20fe_query <- "#GalaxyS20FE (lang:en)"
s20_query <- "#GalaxyS20 (lang:en)"

# Date ranges (format: YYYYMMDDHHMM)
if (name == 'will') {
  iphone12_date_range <- c(202011092359, 202011030000)
  s20fe_date_range <- c(202011092359, 202011030000)
  s20_date_range <- c(202003062359, 202003060000)  # <-- this date range is just one day
  
} else if (name == 'tania') {
  iphone12_date_range <- c(202011100000, 202011162359)
  s20fe_date_range <- c(202011100000, 202011162359)
  s20_date_range <- c(202002120000, 202002122359)
  
} else if (name == 'alex') {
  iphone12_date_range <- c(202011022359, 202010270000)
  s20fe_date_range <- c(202010270000, 202011022359)
  s20_date_range <- c(202002292359, 202002070000)
}

# ACQUIRE TWEETS ------------------------------------------------------------------------------------------------------------

# * iPhone 12 ---------------------------------------------------------------------------------------------------------------

iphone12_tweets <- search_30day(q = iphone12_query, n = 12500, token = twitter_token,
                                fromDate = iphone12_date_range[1], toDate = iphone12_date_range[2], env_name = 'dev')
iphone12_tweets_recent <- search_tweets(q = iphone12_query, n = 18000, token = twitter_token, retryonratelimit = T)

iphone12_tweets <- rbind(iphone12_tweets, iphone12_tweets_recent)  # combine the two datasets

# * Galaxy S20 FE -----------------------------------------------------------------------------------------------------------

s20fe_tweets <- search_30day(q = s20fe_query, n = 12500, token = twitter_token,
                             fromDate = s20fe_date_range[1], toDate = s20fe_date_range[2], env_name = 'dev')
s20fe_tweets_recent <- search_tweets(q = s20fe_query, n = 18000, token = twitter_token, retryonratelimit = T)

s20fe_tweets <- rbind(s20fe_tweets, s20fe_tweets_recent)  # combine the two datasets

# * Galaxy S20 --------------------------------------------------------------------------------------------------------------

s20_tweets <- search_fullarchive(q = s20_query, n = 5000, token = twitter_token,
                           fromDate = s20_date_range[1], toDate = s20_date_range[2], env_name = 'dev')

# ACQUIRE USER DATA ---------------------------------------------------------------------------------------------------------

iphone12_users <- users_data(iphone12_tweets)
s20fe_users <- users_data(s20fe_tweets)
s20_users <- users_data(s20_tweets_2)


# PARSE LIST COLUMNS --------------------------------------------------------------------------------------------------------

# Dataframe columns of class 'list' cannot be exported as a .csv file - The following function fixes this by parsing any 
# lists into a string containing all list values, separated by columns

list_to_char <- function(column) {
  if (class(column) == 'list') {
    new_column <- paste(unlist(column[1]), sep = '', collapse = ', ')
  } else {
    new_column <- column
  }
  
  return(new_column)
}

# Apply the function to the tweet data
iphone12_tweets_parsed <- data.frame(lapply(iphone12_tweets, list_to_char), stringsAsFactors = F)
s20fe_tweets_parsed <- data.frame(lapply(s20fe_tweets, list_to_char), stringsAsFactors = F)
s20_tweets_parsed <- data.frame(lapply(s20_tweets, list_to_char), stringsAsFactors = F)

# Apply the function to the user data
iphone12_users_parsed <- data.frame(lapply(iphone12_users, list_to_char), stringsAsFactors = F)
s20fe_users_parsed <- data.frame(lapply(s20fe_users, list_to_char), stringsAsFactors = F)
s20_users_parsed <- data.frame(lapply(s20_users, list_to_char), stringsAsFactors = F)

# EXPORT DATA ---------------------------------------------------------------------------------------------------------------

# Export iphone12 data
for (dir in c("./data/", "./data/backup/")) {
  write.csv(iphone12_tweets_parsed, paste0(dir, "tweets/iphone12-tweets-", name, ".csv"))
  write.csv(iphone12_users_parsed, paste0(dir, "users/iphone12-users-", name, ".csv"))
}

# Export s20fe data
for (dir in c("./data/", "./data/backup/")) {
  write.csv(s20fe_tweets_parsed, paste0(dir, "tweets/s20fe-tweets-", name, ".csv"))
  write.csv(s20fe_users_parsed, paste0(dir, "users/s20fe-users-", name, ".csv"))
}

# Export s20 data
for (dir in c("./data/", "./data/backup/")) {
  write.csv(s20_tweets_parsed, paste0(dir, "tweets/s20-tweets-", name, ".csv"))
  write.csv(s20_users_parsed, paste0(dir, "users/s20-users-", name, ".csv"))
}
