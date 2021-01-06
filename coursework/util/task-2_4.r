#
# Work out tf, idf and tf_idf indexes
#

lib <- c('readr', 'dplyr', 'tidytext', 'ggplot2')
lapply(lib, library, character.only = T)
rm(lib)

#importing the data
trump_data <- read_csv('../data/trump-speech-data.csv') %>%
  subset(select = -c(X1))

#tokenizing - creating a tibble with location, words
trump_words <- trump_data %>%
           select(speech, location)%>%
           unnest_tokens(word, speech)

#creating a tibble with tf, idf, tf_idf
law_data <- trump_words %>%
  count(location, word) %>%
  bind_tf_idf(word, location, n) %>%
  arrange(desc(tf)) %>%
  group_by(location) %>% 
  mutate(rank = row_number())


# PLOTTING THE ZIPF'S LAW
law_data %>% 
  ggplot(aes(x = rank, y = tf, color = location)) +
  geom_line() +
  labs(x = "Word rank", y = "Term frequencey (tf)", 
       title = "Zipf's Law for Donald Trump's Rallies Data",
       color = "Location") +
  scale_x_log10() +
  scale_y_log10() 

#ADDING THE LINEAR REGRESSION - NOT WORKING YET BUT I'LL DO MY MAGIC
law_data %>% 
  ggplot(aes(x = rank, y = tf, color = location)) +
  geom_line() +
  labs(x = "Word rank", y = "Term frequencey (tf)", 
       title = "Zipf's Law for Donald Trump's Rallies Data",
       color = "Location") +
  scale_x_log10() +
  scale_y_log10() 
  # +geom_smooth(aes(x = rank, y = tf), method='lm', se = FALSE)
