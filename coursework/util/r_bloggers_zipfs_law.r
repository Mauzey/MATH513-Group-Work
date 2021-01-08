library(ggplot2)
library(dplyr)
library(themes)
library(gganimate)


read.delim(file, header = TRUE, sep = " ", dec = ".", ...)

word_count <- # Data frame containing words and their frequency 

colnames(word_count) <- c("word", "count")
alpha <- 1 # Change it needed
word_count <- word_count %>%
  mutate(word = factor(word, levels = word),
         rank = row_number(),
         zipfs_freq = ifelse(rank == 1, count, dplyr::first(count) / rank^alpha))
zipfs_plot <- ggplot(word_count, aes(x = rank, y = count)) + 
  geom_point(aes(color = "observed")) +
  theme_bw() + 
  geom_point(aes(y = zipfs_freq, color = "theoretical")) +
  transition_reveal(count, rank) + 
  labs(x = "rank", y = "count", title = "Zipf's law visualization") +
  scale_colour_manual(name = "Word count", values=c("theoretical" = "red", "observed" = "black")) +
  theme(legend.position = "top")
zipfs_animation <- animate(p)