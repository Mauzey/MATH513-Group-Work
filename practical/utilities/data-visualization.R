# 
#   Visualization of Twitter Data
# 

# CHANGE LOG ----------------------------------------------------------------------------------------------------------------
## 2020-11-20 - WON - Setup of initial file
##                  - Added dependencies, variables section, .csv read

## 2020-11-21 - AM  - Refactored section headers so they appear in the navigation dropdown (margin column = 130)
##                  - Refactored data import section in line with data cleaning

# IMPORT DEPENDENCIES / SETUP -----------------------------------------------------------------------------------------------

library(readr)
library(ggplot2)
library(dplyr)
library(alr4)
library(tidyverse)
library(modelr)

#setwd() # Will
#setwd() # Tania
#setwd("~/Development/Repositories/MATH513-Group-Work/practical")  # Alex laptop
#setwd("D:/Development/MATH513-Group-Work/practical")  # Alex desktop

# DEFINE LOCAL VARIABLES ----------------------------------------------------------------------------------------------------

# ...

# IMPORT DATA ---------------------------------------------------------------------------------------------------------------

tweets <- read_csv("./data/cleaned-tweets.csv")
users <- read_csv("./data/cleaned-users.csv")

# TIME SERIES PLOTS ---------------------------------------------------------------------------------------------------------

# Time series plots for each type
# ...
