library(ggplot2)
library(dplyr)
library(themes)
library(gganimate)

#add this when knitting the vignette
# library(devtools)
# library(usethis)
# library(roxygen2)
# document()
# install()
# load_all()

word_count <- read.delim('../data/pl_50k.txt', header = FALSE, sep = " ", dec = ".")# Data frame containing words and their frequency 

colnames(word_count) <- c("word", "count")
alpha <- 1 # Change it needed

word_count <- word_count %>%
  mutate(word = factor(word, levels = word),
         rank = row_number(),
         zipfs_freq = ifelse(rank == 1, count, dplyr::first(count) / rank^alpha))

#zipfs_plot <- 

ggplot(word_count, aes(x = rank, y = count)) + 
  geom_line(aes(color = "observed")) +
  theme_bw() + 
  geom_line(aes(y = zipfs_freq, color = "theoretical", alpha=.1)) +
  #transition_reveal(count, rank) + 
  labs(x = "rank", y = "count", title = "Zipf's law visualization") +
  scale_colour_manual(name = "Word count", values=c("theoretical" = "red", "observed" = "black")) +
  theme(legend.position = "top") +
  scale_x_log10() +
  scale_y_log10()
  

#zipfs_animation <- animate(zipfs_plot)
