# Import and Setup ----------------------------------------------------------------------------------------------------------

# Import dependencies
library(rtweet)

# rtweet Setup --------------------------------------------------------------------------------------------------------------

# In order to avoid the raw Twitter API keys from appearing on the public Github repository, enter the following command in the 
# RStudio terminal prior to executing the token initialization below:
#
# Sys.setenv(TWITTER_APP = '', TWITTER_TOKEN = '', TWITTER_SECRET = '')
#
# where:
#   TWITTER_APP...: The name of your app in the Twitter developer portal
#   TWITTER_TOKEN.: Your Twitter API key
#   TWITTER_SECRET: Your Twitter API private key

# Initialize Twitter API token
twitter_token <- create_token(
  app <- Sys.getenv('TWITTER_APP'),
  consumer_key <- Sys.getenv('TWITTER_TOKEN'),
  consumer_secret <- Sys.getenv('TWITTER_SECRET')
)

# Acquire Tweets ------------------------------------------------------------------------------------------------------------

# Get iPhone 12 Tweets
iphone12_query <- '#iPhone12'
iphone12_date_range <- c(NULL, NULL)  # start_date, end_date (format: YYYYMMDDHHMM) -- CHANGE THIS !!

iphone12_tweets <- search_30day(q = iphone12_query, n = 12500,
                                fromDate = iphone12_date_range[1],
                                toDate = iphone12_date_range[2],
                                token = twitter_token)

# Get Samsung Galaxy S20 FE Tweets
s20fe_query <- '#GalaxyS20FE'
s20fe_date_range <- c(NULL, NULL)  # start_date, end_date (format: YYYYMMDDHHMM) -- CHANGE THIS !!!

s20fe_tweets <- search_30day(q = s20fe_query, n = 12500,
                             fromDate = s20fe_date_range[1],
                             toDate = s20fe_date_range[2],
                             token = twitter_token)

# Get Samsung Galaxy S20 Tweets
s20_query <- '#GalaxyS20'
s20_date_range <- c(NULL, NULL)  # start_date, end_date (format: YYYYMMDDHHMM) -- CHANGE THIS !!

s20_tweets <- search_fullarchive(q = s20_query, n = 5000,
                                 fromDate = s20_date_range[1],
                                 toDate = s20_date_range[2],
                                 token = twitter_token)

# Extract user data
iphone12_users <- user_data(iphone12_tweets)
s20fe_users <- user_data(s20fe_tweets)
s20_users <- user_data(s20_tweets)

# Parse List Columns --------------------------------------------------------------------------------------------------------

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

# Export Data ---------------------------------------------------------------------------------------------------------------

# Define file names
name <- 'alex'  # change this to you name; this is to avoid overwriting each others' pulled data

# Export data to './data' and './data/backup'
for (dir in c('data/', 'data/backup/')) {
  write.csv(iphone12_tweets_parsed, paste0(dir, 'iphone12-tweets-', name, '.csv'))  # Export iPhone 12 tweet data
  write.csv(iphone12_users_parsed, paste0(dir, 'iphone12-users-', name, '.csv'))  # Export iPhone 12 user data
  
  write.csv(s20fe_tweets_parsed, paste0(dir, 's20fe-tweets-', name, '.csv'))  # Export Samsung Galaxy S20 FE tweet data
  write.csv(s20fe_users_parsed, paste0(dir, 's20fe-users-', name, '.csv'))  # Export Samsung Galaxy S20 FE user data
  
  write.csv(s20_tweets_parsed, paste0(dir, 's20-tweets-', name, '.csv'))  # Export Samsung Galaxy S20 tweet data
  write.csv(s20_users_parsed, paste0(dir, 's20-users-', name, '.csv'))  # Export Samsung Galaxy S20 user data
}


#Tania's test