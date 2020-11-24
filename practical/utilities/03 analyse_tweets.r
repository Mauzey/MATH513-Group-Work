# 
#   Analysis of Twitter Data
# 

# CHANGE LOG ----------------------------------------------------------------------------------------------------------------
## 2020-11-20 - WON - Setup of initial file
##                  - Added dependencies, variables section, .csv read, combination of data sets and export, quick analysis,
##                    clean data section, quick analysis post lang strip

## 2020-11-21 - AM  - Refactored section headers so they appear in the navigation dropdown (margin column = 130)
##                  - Moved 'Combining Data' section to its own file: './data-cleaning.R'
##                  - Moved 'Quick Analysis' section to its own file: './data-cleaning.R'

# IMPORT DEPENDENCIES / SETUP -----------------------------------------------------------------------------------------------

library(readr)      # Read in data from .csv
library(ggplot2)    # Graphing data
library(ggthemes)   # Graphing themes
library(tidyverse)  # Manipulating data
library(dplyr)      # Manipulating data
library(alr4)       # Statistical analysis of data
library(modelr)     # Modeling data

#setwd() # Will
#setwd() # Tania
#setwd("~/Development/Repositories/MATH513-Group-Work/practical")  # Alex
#setwd("D:/Development/MATH513-Group-Work/practical")  # Alex

# DEFINE LOCAL VARIABLES ----------------------------------------------------------------------------------------------------

# ...

# IMPORT DATA ---------------------------------------------------------------------------------------------------------------

tweets <- read_csv("./data/cleaned-tweets.csv")
users <- read_csv("./data/cleaned-users.csv")

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