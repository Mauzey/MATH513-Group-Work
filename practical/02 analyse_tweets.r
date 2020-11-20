#########################################################################################
#       ANALYSIS OF TWITTER DATA
#########################################################################################

#----------------------------------------------------------------------------------------
## CHANGES 
#----------------------------------------------------------------------------------------

### 2020-11-20 - WON - Setup of inital file. Added dependancies, variables section, 
###                    csv read, combination of data sets and export, quick analysis,
###                    clean data section, quick analysis post lang strip.

#----------------------------------------------------------------------------------------
## VARIABLES 
#----------------------------------------------------------------------------------------

### Working Directory Setup
  #### Alex
    # setwd("MATH513 Big Data and Social Network/Assessment")

  #### Tania
    # setwd("MATH513 Big Data and Social Network/Assessment")

  #### Will
    # setwd("MATH513 Big Data and Social Network/Assessment")

### Hashtags
iphone12_query <- '#iPhone12'
s20fe_query <- '#GalaxyS20FE'
s20_query <- '#GalaxyS20'


#----------------------------------------------------------------------------------------
## IMPORT DEPENDANCIES 
#----------------------------------------------------------------------------------------

library(readr)      # Read in data from CSV
library(ggplot2)    # Graphing data
library(ggthemes)   # Graphing themes
library(tidyverse)  # Manipulating data
library(dplyr)      # Manipulating data
library(alr4)       # Statistical analysis of data
library(modelr)     # 

#----------------------------------------------------------------------------------------
## READ IN CSV DATA  
#----------------------------------------------------------------------------------------

### Tweets
iphone12_tweets_alex <- read_csv("iphone12-tweets-alex.csv")
iphone12_tweets_tania <- read_csv("iphone12-tweets-tania.csv")
iphone12_tweets_will <- read_csv("iphone12-tweets-will.csv")

s20fe_tweets_alex <- read_csv("s20fe-tweets-alex.csv")
s20fe_tweets_tania <- read_csv("s20fe-tweets-tania.csv")
s20fe_tweets_will <- read_csv("s20fe-tweets-will.csv")

s20_tweets_alex <- read_csv("s20-tweets-alex.csv")
s20_tweets_tania <- read_csv("s20-tweets-tania.csv")
s20_tweets_will <- read_csv("s20-tweets-will.csv")

### Users
iphone12_users_alex <-read_csv("iphone12-users-alex.csv")
iphone12_users_tania <-read_csv("iphone12-users-tania.csv")
iphone12_users_will <-read_csv("iphone12-users-will.csv")

s20fe_users_alex <-read_csv("s20fe-users-alex.csv")
s20fe_users_tania <-read_csv("s20fe-users-tania.csv")
s20fe_users_will <-read_csv("s20fe-users-will.csv")

s20_users_alex <-read_csv("s20-users-alex.csv")
s20_users_tania <-read_csv("s20-users-tania.csv")
s20_users_will <-read_csv("s20-users-will.csv")


#----------------------------------------------------------------------------------------
## COMBINE DATA SETS FROM ALL USERS
#----------------------------------------------------------------------------------------

# Join the datasets from each user into one set removing duplicates

    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          #Unsure what join needs to go here to keep the data and remve duplicates?




# Export data to './data' and './data/backup'
for (dir in c('data/', 'data/backup/')) {
  write.csv(iphone12_tweets, paste0(dir, 'iphone12-tweets.csv'))  # Export iPhone 12 tweet data
  write.csv(iphone12_users, paste0(dir, 'iphone12-users.csv'))  # Export iPhone 12 user data
  
  write.csv(s20fe_tweets, paste0(dir, 's20fe-tweets.csv'))  # Export Samsung Galaxy S20 FE tweet data
  write.csv(s20fe_users, paste0(dir, 's20fe-users.csv'))  # Export Samsung Galaxy S20 FE user data
  
  write.csv(s20_tweets, paste0(dir, 's20-tweets.csv'))  # Export Samsung Galaxy S20 tweet data
  write.csv(s20_users, paste0(dir, 's20-users.csv'))  # Export Samsung Galaxy S20 user data
}


#----------------------------------------------------------------------------------------
## QUICK ANALYSIS - BEFORE DATA CLEAN UP
#----------------------------------------------------------------------------------------

names(iphone12_tweets)  # Column names
names(s20fe_tweets)     # Column names
names(s20_tweets)       # Column names

dim(iphone12_tweets)    # Dimensions of the data set
dim(s20fe_tweets)       # Dimensions of the data set
dim(s20_tweets)         # Dimensions of the data set

no_tweets_iphone12 <- nrow(iphone12_tweets)  # Number of rows of data
no_tweets_s20fe <-nrow(s20fe_tweets)         # Number of rows of data
no_tweets_s20_ <-nrow(s20_tweets)            # Number of rows of data

### Time series plot to check dates
ts_plot(iphone12_tweets, "secs", trim = 1) + 
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold")) +
  labs(x = NULL, y = NULL,
       title = "Frequency of Twitter statuses for " + iphone12_query,
       subtitle = "Twitter status (tweet) counts aggregated using 1-seconds intervals",
       caption = "Source: Data collected from Twitter's REST API via rtweet"
  )

ts_plot(s20fe_tweets, "secs", trim = 1) + 
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold")) +
  labs(x = NULL, y = NULL,
       title = "Frequency of Twitter statuses for " + s20fe_query,
       subtitle = "Twitter status (tweet) counts aggregated using 1-seconds intervals",
       caption = "Source: Data collected from Twitter's REST API via rtweet"
  )

ts_plot(s20_tweets, "secs", trim = 1) + 
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold")) +
  labs(x = NULL, y = NULL,
       title = "Frequency of Twitter statuses for " + s20_query,
       subtitle = "Twitter status (tweet) counts aggregated using 1-seconds intervals",
       caption = "Source: Data collected from Twitter's REST API via rtweet"
  )

#----------------------------------------------------------------------------------------
## CLEAN DATA BASED ON LANGUAGE
#----------------------------------------------------------------------------------------

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # Do we need to actually remove tweets for some of the data analysis and visualisations?
  # We will need to for sentiment analysis as this works on the English language but other stats should work fine like time series etc?




#----------------------------------------------------------------------------------------
## QUICK ANALYSIS - POST LANGUAGE STRIP FOR ENGLISH ONLY
#----------------------------------------------------------------------------------------

no_tweets_iphone12_en <- nrow(iphone12_tweets)       # Number of rows of data
no_tweets_s20fe_en <-nrow(s20fe_tweets)              # Number of rows of data
no_tweets_s20_en <-nrow(s20_tweets)                  # Number of rows of data