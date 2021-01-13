library(qdap)
library(tidyverse)
library(tidytext)
library(tokenizers)

# Data Preparation --------------------------------------------------------

# Importing the data
trump_data <- read_csv('../data/trump-speech-data.csv') %>%
  subset(select = -c(X1))

sentences <- trump_data %>% unnest_tokens(sentence, speech, token = "sentences")

# Extract each sentence from the speechs and add the sentence length to each speech
sentences <- tokenize_sentences(trump_data$speech)
trump_data <- trump_data %>% mutate(length_sentences = sapply(sentences, length))

# Find the average sentence length of the speeches
total_sentcence_count <- count(trump_data$length_sentences)
total_speech_count <- nrow(trump_data)

average_sentence_length <- (sum(trump_data$length_sentences/nrow(trump_data))
)
average_sentence_length




trump_data_speech <- trump_data %>% subset(select = c(speech))

trump_data_speech <- iconv(trump_data$speech,"latin1","ASCII","")

trump_data_speech <- gsub(",([[:alpha:]])", ", \\1", trump_data_speech)

trump_data_speech <- data.frame(trump_data_speech)

trump_data_split <- sentSplit(trump_data_speech,'trump_data_speech')


# We will begin with the readability. The “automated_readbility_index” function will calculate this for us.
ari <- automated_readability_index(trump_data$speech, trump_data$location)
ari

# Next we will calcualte the formality of the speeches. The “formality” function is used for this.
form <- formality(trump_data$speech, trump_data$location)
form
form$form.prop.by
plot(form)

# Both of these analysis tools can provide suggestions that may be needed in order to enhance communication or compare different authors and writing styles.
# educational research techniques (2017)
# https://educationalresearchtechniques.com/2017/08/02/readability-and-formality-analysis-in-r/



#########################################
# SENTIMENT ANALYSIS
#########################################

# STAGE 1 - Prepare data --------------------------------------------------

# Tokenize the dialogue, splitting each sentence in separate words
trump_data_words <-  trump_data %>%
  select(speech, location, date) %>%
  unnest_tokens(word, speech) %>%
  count(location, date, word) %>%
  group_by(location)

# Remove stop words
trump_data_words <- trump_data_words %>% anti_join(tidytext::stop_words)

# Remove custom stop words
custom_stop_words <- data.frame(word = c(''))
trump_data_words <- trump_data_words %>% anti_join(custom_stop_words)

# Remove vars from memory to keep the environment tidy
rm(custom_stop_words)

# Convert all words to lower case for matching
trump_data_words <- trump_data_words %>% mutate(word = tolower(word))

get_sentiments("afinn")

library(dplyr)
library(stringr)
library(sentimentr) #for 'SENTIMENT ANALYSIS - Product Features' section

# Get sentiment scores
trump_data_words_bing_count <- trump_data_words %>%
  inner_join(sentiments) %>% 
  count(word, sentiment, sort = T) %>%
  mutate(word = reorder(word, n))

trump_data_words_bing_count %>%
  ggplot(aes(x = n, y = sentiment, fill = location)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~location, ncol = 2, scales = "free_x")


head(trump_data_words_bing_count)


# Plot sentiment scores for top 50 words words
trump_data_words_bing_count %>% 
  group_by(sentiment) %>%

  head(n = 50L) %>% 
  ungroup() %>%
  ggplot(aes(x = word, y = n, fill = sentiment)) + theme_minimal() + coord_flip() +
    geom_col(show.legend = F) +
    facet_wrap(~ sentiment, scales = 'free_y') +
    labs(x = NULL, y = "Sentiment",
         title = "Most Common Positive and Negative Words") +
    theme(axis.text = element_text(size = 11, colour = 'black'),
          axis.title = element_text(size = 11, colour = 'black'),
          title = element_text(size = 12))
