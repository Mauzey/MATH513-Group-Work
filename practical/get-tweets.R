# Import and Setup --------------------------------------------------------

# Import dependencies
library(rtweet)

# Initialise twitter token
# In order for this function to work, enter the following command into the
#   RStudio terminal:
#
#   Sys.setenv(TWITTER_APP = '', TWITTER_TOKEN = '', TWITTER_SECRET = '')
#
#   Where:
#     TWITTER_APP: The name of your app in the Twitter developer portal
#     TWITTER_TOKEN: Your Twitter API key
#     TWITTER_SECRET: Your Twitter API secret key
#
# This is to ensure that our private keys don't appear in the public repo
twitter_token <- create_token(
  app = Sys.getenv('TWITTER_APP'),
  consumer_key = Sys.getenv('TWITTER_TOKEN'),
  consumer_secret = Sys.getenv('TWITTER_SECRET')
)

# Acquire Tweets ----------------------------------------------------------

# Get tweets containing the hasthag '#iPhone12', excluding quotes and replies
query <- '#iPhone12 -filter:quote -filter:replies'
tweets <- search_tweets(
  q = query,
  n = 10,
  token = twitter_token,
  geocode = '53.86, -4.03, 373mi'  # latitude, longitude, radius
)

# Prepare and Export Data -------------------------------------------------

# Extract user data
users <- user_data(tweets)

# For some reason, dataframes containing columns of type 'list' cannot be 
#   exported as CSV. The following function fixes this problem
list_to_char <- function(x){
  #' Converts a column of type 'list' to 'char'
  #' 
  #' @param x (obj) The column to convert
  
  if (class(x) == 'list'){
    y <- paste(unlist(x[1]), sep = '', collapse = ', ')
  } else {
    y <- x
  }

  return(y)
}
# Convert columns of type 'list' to 'char'
tweets <- data.frame(lapply(tweets, list_to_char), stringsAsFactors = F)

# Export data to .csv files
#   Rename these so that the file name is relevant to the query used
tweets_filename <- 'data/tweets_test.csv'
users_filename <- 'data/users_test.csv'
