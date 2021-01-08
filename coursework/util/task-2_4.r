#
# Work out tf, idf and tf_idf indexes
#

lib <- c('readr', 'dplyr', 'tidytext', 'ggplot2', 'scales')
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
require(scales)
# law_data %>% 
#   ggplot(aes(x = rank, y = tf, color = location)) +
#   geom_line() +
#   labs(x = "Word rank", y = "Term frequencey (tf)", 
#        title = "Zipf's Law for Donald Trump's Rallies Data",
#        color = "Location") +
#   scale_x_log10() +
#   scale_y_log10(labels = comma)
# 
lin_reg = coef(lm(log(rank) ~ log(tf), data = law_data))

#ADDING THE LINEAR REGRESSION
law_data %>% 
  ggplot(aes(x = rank, y = tf, color = location)) +
  geom_line() +
  labs(x = "Word rank", y = "Term frequencey (tf)", 
       title = "Zipf's Law for Donald Trump's Rallies Data",
       color = "Location") +
  scale_x_log10() +
  scale_y_log10(labels = comma) +
  geom_abline(intercept = lin_reg[1], slope = round(lin_reg[2], digits = 0))

rm(lin_reg)

#CHECKING WITH REAL ZIPF'S LAW
word_count <- law_data %>%
  mutate(word = factor(word, levels = word),
         zipfs_freq = ifelse(rank == 1, count, first(count) / rank^alpha))

#CHECKING THE TOP FREQUENT WORDS FOR TRUMP'S SPEECH AND THEIR TF
data('stop_words')
new_law <- anti_join(law_data, stop_words)
new_land_2 <- new_land %>% group_by(rank) %>% arrange(rank) %>%group_by(location)
