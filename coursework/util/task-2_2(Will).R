# Import packages used

lib <- c('tidyverse', 'tidytext', 'scales', 'lubridate')
lapply(lib, library, character.only = T)
rm(lib)

words_to_graph <- c("america")
data_set_path <- '../data/trump-speech-data.csv'

#' Graph of word usage of a data set
#'
#' This function provides a graph using ggplt2 of a data set containing speeches.
#'
#' @param data_set_path the dataset in csv form with a column called speeches, location and date
#' @param words A collection of words for which the user wants graphs plotted
#'
#' @return A named vector of numerical summaries:
#' \describe{
#' \item{M}{The mean of the data.}
#' \item{M_TRIMMED}{The trimmed mean of the data.}
#' }
#'

word_plots <- function(data_set_path, words){

    # STAGE 1 - Prepare data --------------------------------------------------

  # Read in data
  trump_data <- read_csv(data_set_path) %>%
                subset(select = -c(X1))

  # Clean the data
    # Convert collection to lower case

    #

    #

  # Tokenize the dialogue, splitting each sentence in separate words
  trump_data_words <-  trump_data %>%
                        select(speech, location, date) %>%
                        unnest_tokens(word, speech) %>%
                        count(date, word) %>%
                        group_by(date) %>%
                        mutate(p = n / sum(n))

  # Remove stop words
  data('stop_words')
  trump_data_words <- trump_data_words %>% anti_join(stop_words)

  # Remove custom stop words
  custom_stop_words <- data.frame(word = c(''))
  trump_data_words <- trump_data_words %>% anti_join(custom_stop_words)

  rm(stop_words, custom_stop_words)  # Remove vars from memory to keep the environment tidy


  # Convert all words to lower case for matching
  trump_data_words <- trump_data_words %>% mutate(word = tolower(word))


  # STAGE 2 - Filter the data set by the words requested --------------------

  trump_data_words_filtered <- trump_data_words %>% filter(word %in% words_to_graph)


  # STAGE 3 - Plot graphs ---------------------------------------------------

  # Plot the contents of trump_data_words using ggplot2
  graph<- ggplot(trump_data_words_filtered,
          aes(x = date, y = p, colour = word)) +
          geom_point() +
          geom_line() +
          geom_text(label = word) +
          labs(y = "Percentage of words") +
          facet_wrap(~ word) +
          scale_x_date(labels = date_format("%d %b %y", tz = "UTC"),
                       limits = c(as_date("2020-09-02")+0.5,
                                  as_date("2020-09-23")-0.5)) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
          theme(legend.position = "none")


  # Return the graph
  return(list(plot = graph))
}

output <- word_plots(data_set_path, words_to_graph)
output

trump_data_words_filtered
trump_data_words_filtered <- trump_data_words_filtered %>%
    mutate(
      date_label = paste(day(date), month(date, label= TRUE, abbr = TRUE), sep="/")
    )
trump_data_words_filtered


ggplot(trump_data_words_filtered,
       aes(x = date, y = p, colour = word)) +
  geom_point() +
  geom_text(hjust = 0, nudge_x = 0.25, aes(label = date_label)) +
  geom_smooth(model=lm) +
  labs(x = "Rally Date",
       y = "Percentage of words",
       title = "Change of word frequency in Donald Trump's rallies in September 2020") +
  facet_wrap(~ word) +
  scale_x_date(labels = date_format("%d %b %y", tz = "UTC"),
               limits = c(as_date("2020-09-02")+0.5,
                          as_date("2020-09-24")-0.5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  theme(legend.position = "none")
