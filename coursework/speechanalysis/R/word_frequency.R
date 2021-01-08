#'
#' Graph of word usage of a data set
#'
#' This function provides a graph using ggplt2 of a data set containing speeches.
#'
#' @param df A dataframe with the following columns: 'speech' <chr>, 'location' <chr>, 'date' <date>
#' @param words A collection of words for which the user wants graphs plotted
#'
#' @author 10701983 \email{10701983}
#'
#' @import tidyverse tidytext scales lubridate
#'
#' @export
#'
#' @examples
#' word_frequency(trump_speeches, c("america", "biden", "China"))
#'
#' @return A named vector of a plot:
#' \describe{
#' \item{plot}{The data plotted in a graph.}
#' }
#'


# lib <- c('readr', 'dplyr', 'tidytext', 'ggplot2', 'scales', 'lubridate')
# lapply(lib, library, character.only = T)
# rm(lib)
#
# #importing the data
# trump_data <- read_csv('../data/trump-speech-data.csv') %>%
#   subset(select = -c(X1))
#
# words_to_graph = c("america")

word_frequency <- function(df, words_to_graph){

  # STAGE 1 - Prepare data --------------------------------------------------

  # Read in data
  trump_data <- trump_speeches

  # Tokenize the dialogue, splitting each sentence in separate words
  trump_data_words <-  trump_data %>%
    select(speech, location, date) %>%
    unnest_tokens(word, speech) %>%
    count(date, word) %>%
    group_by(date) %>%
    mutate(p = n / sum(n))

  # Remove stop words
  trump_data_words <- trump_data_words %>% anti_join(tidytext::stop_words)

  # Remove custom stop words
  custom_stop_words <- data.frame(word = c(''))
  trump_data_words <- trump_data_words %>% anti_join(custom_stop_words)

  rm(custom_stop_words)  # Remove vars from memory to keep the environment tidy


  # Convert all words to lower case for matching
  trump_data_words <- trump_data_words %>% mutate(word = tolower(word))

  words_to_graph <- lapply(words_to_graph, tolower)


  # STAGE 2 - Filter the data set by the words requested --------------------

  trump_data_words_filtered <- trump_data_words %>% filter(word %in% words_to_graph)

  trump_data_words_filtered <- trump_data_words_filtered %>%
    mutate(
      date_label = paste(day(date), month(date, label= TRUE, abbr = TRUE), sep="/")
    )

  lin_reg = coef(lm(date ~ p, data = trump_data_words_filtered))

  # STAGE 3 - Plot graphs ---------------------------------------------------

  # Plot the contents of trump_data_words using ggplot2
  plot <- ggplot(trump_data_words_filtered,
         aes(x = date, y = p, colour = word)) +
    geom_point() +
    geom_text(hjust = 0, nudge_x = 0.25, aes(label = date_label)) +
    geom_smooth() +
    geom_smooth(method=lm, se=FALSE, col="black") +
    labs(x = "Rally Date",
         y = "Percentage of words",
         title = "Change of word frequency in Donald Trump's rallies in September 2020") +
    facet_wrap(~ word) +
    scale_x_date(labels = date_format("%d %b %y", tz = "UTC"),
                 limits = c(as_date("2020-09-02")+0.5,
                            as_date("2020-09-24")-0.5)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    theme(legend.position = "none")

  plot


  # Return the graph
  return(plot)
}
