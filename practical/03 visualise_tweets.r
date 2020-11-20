#########################################################################################
#       VISUALISATION OF TWITTER DATA
#########################################################################################

#----------------------------------------------------------------------------------------
## CHANGES 
#----------------------------------------------------------------------------------------

### 2020-11-20 - WON - Setup of inital file. Added dependancies, variables section,
###                    csv read


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


#----------------------------------------------------------------------------------------
## IMPORT DEPENDANCIES 
#----------------------------------------------------------------------------------------

library(readr)
library(ggplot2)
library(dplyr)
library(alr4)
library(tidyverse)
library(modelr)

#----------------------------------------------------------------------------------------
## READ IN CSV DATA  
#----------------------------------------------------------------------------------------

iphone12_tweets <- read_csv("iphone12-tweets.csv")
iphone12_users <- read_csv("iphone12-users.csv")

s20fe_tweets <- read_csv("s20fe-tweets.csv")
s20fe_users <- read_csv("s20fe-users.csv")

s20_tweets <- read_csv("s20-tweets.csv")
s20_users <- read_csv("s20-users.csv")


#----------------------------------------------------------------------------------------
## TIME SERIES GRAPH FOR EACH TYPE
#----------------------------------------------------------------------------------------
