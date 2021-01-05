
lib <- c('readr', 'dplyr', 'tidytext', 'ggplot2')
lapply(lib, library, character.only = T)
rm(lib)

# Plotting the Words with Highest tf-idf Value ------------------------------------------------------------------------------
#
# The tf-idf index computes the frequency of a term adjusted for how rarely it is used. It's calculated as the product of the 
#   term frequency (tf) and the inverse document frequency (idf):
#
#     tf-idf = tf * idf
#
# The term frequency (tf) identifies how frequently a word occurs in a document. The inverse document frequency (idf) decreases 
#   the weight for commonly used words, and increases the weight for words that are not used very much in a collection of 
#   documents.

trump_data <- read_csv('./data/trump-speech-data.csv') %>%
  subset(select = -c(X1))

data('stop_words')

# calculate the tf-idf value for each word, for each speech
plot_data <- trump_data %>%
  unnest_tokens(word, speech) %>%
  anti_join(stop_words) %>%
  count(location, word) %>%
  bind_tf_idf(word, location, n) %>%
  arrange(desc(tf_idf))


# plot the most frequently occurring words in donald trump speeches, according to tf-idf value
plot_data %>%
  group_by(location) %>%
  top_n(10) %>%
  ungroup() %>%
  
  mutate(location = as.factor(location),
         word = reorder_within(word, tf_idf, location)) %>%
  
  ggplot(aes(x = word, y = tf_idf, fill = location)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~location, scales = 'free_y') +
    coord_flip() +
    scale_x_reordered() +
    scale_y_continuous(expand = c(0,0)) +
    
    labs(x = NULL, y = "Frequency (according to tf-idf value)",
         title = "Most Frequent Words used in Speeches made by Donald Trump",
         subtitle = "Grouped by event location",
         caption = "Data sourced from transcripts of 10 speeches made during September 2020")
