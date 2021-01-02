library(lubridate)
library(readr)
library(tibble)
library(dplyr)

for (file in list.files(path="./data/raw_data", pattern='*.txt', full.names=T)){
  filename <- strsplit(file, "[./]")[[1]][5]  # extract filename from path
  
  
  location <- strsplit(filename, split='Sep')[[1]][1]  # extract location from filename
  
  
  raw_date <- strsplit(filename, split='Sep')[[1]][2]  # extract raw date from filename
  raw_date <- paste("09_", raw_date, sep="", collapse=NULL)  # add september to the raw date
  date <- as.Date(raw_date, "%m_%d_%Y")  # format as datetime
  
  
  content <- readChar(file, file.info(file)$size)  # extract speech from file
  
  
  new_obs <- tibble_row(speech=content, location=location, date=date)  # create a new observation (row)
  
  
  # if a tibble of trump data doesn't exist, create one
  if (!exists('trump_data')) {
    trump_data <- new_obs
  # otherwise, append to the existing data
  } else {
    trump_data <- bind_rows(trump_data, new_obs)
  }
  
  rm(filename, location, raw_date, date, content, new_obs, file)  # cleanup
}

write.csv(trump_data, './data/trump-speech-data.csv')  # export data as .csv
rm(trump_data)
