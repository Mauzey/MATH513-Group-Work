# 
#   Import Tweet/User Data and Clean it
# 

# IMPORT DEPENDENCIES / SETUP -----------------------------------------------------------------------------------------------

library(dplyr)
library(readr)

#setwd() # Will
#setwd() # Tania
#setwd("~/Development/Repositories/MATH513-Group-Work/practical")  # Alex laptop
#setwd("D:/Development/MATH513-Group-Work/practical")  # Alex desktop

# IMPORT DATA ---------------------------------------------------------------------------------------------------------------

tweets_dir <- "./data/tweets"  # Directory of tweet data
users_dir <- "./data/users"  # Directory of user data

# Import tweet data
for (file in list.file(path = tweets_dir, pattern = '*.csv', full.names = T)) {
  # If 'tweets' dataframe doesn't exist:
    # Import file into 'tweets' dataframe
  
  # Elif 'tweets' dataframe does exist:
    # Append file to 'tweets' dataframe
}

# Import user data
for (file in list.file(path = users_dir, pattern = '*.csv', full.names = T)) {
  # If 'users' dataframe doesn't exist:
    # Import file into 'users' dataframe
  
  # Elif 'users' dataframe does exist:
    # Append file to 'users' dataframe
}

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
  write.csv(tweets, paste0(dir, 'combined-tweets.csv'))
  write.csv(users, paste0(dir, 'combined-users.csv'))
}
