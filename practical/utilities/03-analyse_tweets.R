# 
#   Analysis of Twitter Data
# 

# IMPORT DEPENDENCIES -------------------------------------------------------------------------------------------------------

library(readr)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(dplyr)
library(modelr)

# SET WORKING DIRECTORY -----------------------------------------------------------------------------------------------------

# Will
#setwd("C:/Users/willo/SynologyDrive/Documents - Will/Qualifications/!MSc Data Science/!Courses/1 MATH513 Big Data and Social Network/Assessment/MATH513-Group-Work/practical")

# Tania
#setwd("")

# Alex
#setwd("~/Development/Repositories/MATH513-Group-Work/practical")
#setwd("D:/Development/MATH513-Group-Work/practical")

# IMPORT DATA ---------------------------------------------------------------------------------------------------------------

tweets <- read_csv("./data/cleaned-tweets.csv")
users <- read_csv("./data/cleaned-users.csv")

# DATA SUMMARY --------------------------------------------------------------------------------------------------------------

# Print the columns of the dataframes
names(tweets)
names(users)

# Print the shape of the data (rows/columns)
dim(tweets)
dim(users)

# TIME SERIES PLOTS ---------------------------------------------------------------------------------------------------------

ts_plot(iphone12_tweets, "secs", trim = 1) + 
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold")) +
  labs(x = NULL, y = NULL,
       title = "Frequency of Twitter statuses for #iPhone12",
       subtitle = "Twitter status (tweet) counts aggregated using 1-seconds intervals",
       caption = "Source: Data collected from Twitter's REST API via rtweet"
  )

ts_plot(s20fe_tweets, "secs", trim = 1) + 
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold")) +
  labs(x = NULL, y = NULL,
       title = "Frequency of Twitter statuses for #GalaxyS20FE",
       subtitle = "Twitter status (tweet) counts aggregated using 1-seconds intervals",
       caption = "Source: Data collected from Twitter's REST API via rtweet"
  )

ts_plot(s20_tweets, "secs", trim = 1) + 
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold")) +
  labs(x = NULL, y = NULL,
       title = "Frequency of Twitter statuses for #GalaxyS20",
       subtitle = "Twitter status (tweet) counts aggregated using 1-seconds intervals",
       caption = "Source: Data collected from Twitter's REST API via rtweet"
  )
