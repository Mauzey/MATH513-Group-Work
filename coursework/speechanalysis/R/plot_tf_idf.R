#'
#' Most Frequent Words per Speech, by tf-idf Index
#'
#' Produces a plot of the most frequent words used (in descending order) during a collection of speeches. The plot is faceted by
#' the location of the speech, and uses the tf-idf index to determine word frequencies.
#'
#' @param df A dataframe with the following columns: 'speech' <chr>, 'location' <chr>, 'date' <date>
#' @param n_words <int> The number of 'top' values to plot
#'
#' @author Alex Mounsey \email{alex.mounsey@@postgrad.plymouth.ac.uk}
#'
#' @import dplyr tidytext ggplot2
#'
#' @export
#'
#' @examples
#' plot_tf_idf(trump_speeches, 10)
#'

plot_tf_idf <- function(df, n_words = 10) {
  # check that the required columns exist
  for (col in c('speech', 'location', 'date')) {
    if ((col %in% colnames(df) == FALSE)) {
      # this could be expanded to include a dtype check
      stop(paste0("Column '", col, "' does not exist in dataframe 'df'"))
    }
  }

  # calculate the tf-idf value for each word, for each speech
  plot_data <- df %>%
    unnest_tokens(word, speech) %>%
    anti_join(tidytext::stop_words) %>%
    count(location, word) %>%
    bind_tf_idf(word, location, n) %>%
    arrange(desc(tf_idf))

  # plot the most frequently occurring words in each speech, according to the tf-idf value
  plot <- plot_data %>%
    group_by(location) %>%
    top_n(n_words) %>%
    ungroup() %>%
    mutate(location = as.factor(location),
           word = reorder_within(word, tf_idf, location)) %>%

    ggplot(aes(x = word, y = tf_idf, fill = location)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~location, scales = 'free_y') +
      coord_flip() +
      scale_x_reordered() +
      scale_y_continuous(breaks = c(0, 0.005, 0.01)) +

      labs(x = NULL, y = "Frequency (tf-idf Index)",
           title = "Word Frequencies by Speech",
           subtitle = paste0("Data grouped by location; showing top ", n_words, " words per speech")) +
      theme(axis.text = element_text(size = 12))

  return(plot)
}
