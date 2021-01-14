#' Graph of word frequencies against linear regression and against the theoretical Zipf's Law.
#' 
#' This function provides a graph using ggplt2 of a data set containing speeches.
#'
#' @param df A dataframe with the following columns: 'speech' <chr>, 'location' <chr>, 'date' <date>
#' @param plot_type A number 1, 2, or 3 which indicates of the type of plot to be displayed.
#' 1 stands for 'frequency plot', 2 for 'frequency + linear',
#' 3 for 'theoretical zipf's law and practical frequencies'
#'
#' @return <ggplot> Graph of word frequencies against linear regression and against the theoretical Zipf's Law.
#' 
#' @author 10696253 \email{10696253}
#'
#' @import tidyverse tidytext scales lubridate
#'
#' @export
#'
#' @examples
#' zipfs_law(trump_speeches, 1)
#'

zipfs_law <- function(df, plot_type){

  # check that the required columns exist
  for (col in c('speech', 'location', 'date')) {
    if ((col %in% colnames(df) == FALSE)) {
      # this could be expanded to include a dtype check
      stop(paste0("Column '", col, "' does not exist in dataframe 'df'"))
    }
  }



  #tokenizing - creating a tibble with location, words
  trump_words <- df %>%
    select(speech, location)%>%
    unnest_tokens(word, speech)

  #creating a tibble with tf, idf, tf_idf
  law_data <- trump_words %>%
    count(location, word) %>%
    bind_tf_idf(word, location, n) %>%
    arrange(desc(tf)) %>%
    group_by(location) %>%
    mutate(rank = row_number())

  require(scales)

  if (plot_type == 1){

    # PLOTTING THE ZIPF'S LAW
    plot <- law_data %>%
      ggplot(aes(x = rank, y = tf, color = location)) +
      geom_line() +
      labs(x = "Word rank", y = "Term frequencey (tf)",
           title = "Zipf's Law for the Data",
           color = "Location") +
      scale_x_log10() +
      scale_y_log10(labels = comma)

    # Return the graph
    return(plot)

  }

  else if (plot_type==2){

    lin_reg = coef(lm(log(rank) ~ log(tf), data = law_data))

    #ADDING THE LINEAR REGRESSION
    plot <- law_data %>%
      ggplot(aes(x = rank, y = tf, color = location)) +
      geom_line() +
      labs(x = "Word rank", y = "Term frequencey (tf)",
           title = "Zipf's Law for the Data",
           color = "Location") +
      scale_x_log10() +
      scale_y_log10(labels = comma) +
      geom_abline(intercept = lin_reg[1], slope = round(lin_reg[2], digits = 0))

    rm(lin_reg)

    # Return the graph
    return(plot)

  }

  else if (plot_type==3){

    # Creating a tibble with zipf's frequencies and
    # removing the location column since we just want a theoretical zipf's law
    # for Trump's speeches in general

    theor_data <- trump_words %>%
      subset(select = -location) %>%
      group_by(word) %>%
      count(word) %>%
      arrange(desc(n))

    theor_data$rank <- seq.int(nrow(theor_data))

    # adding a zipf's frequency calculated according to the zipf's law

    alpha <- 1 # this coeficient can be changed if needed

    theor_data_zipfs <- theor_data %>%
      mutate(word = factor(word, levels = word),
             zipfs_freq =  ifelse(rank == 1, n, dplyr::first(n) / rank^alpha))

    # PLOT THE THEORETICAL ZIPF'S LAW AGAINS THE WORD FREQUENCY
    plot <- ggplot(theor_data_zipfs, aes(x = rank, y = n)) +
      geom_line(aes(color = "observed")) +
      theme_bw() +
      geom_line(aes(y = zipfs_freq, color = "theoretical")) +
      labs(x = "Word's Rank", y = "Frequency", title = "Zipf's Law Visualization") +
      scale_colour_manual(name = "Word count", values=c("theoretical" = "red", "observed" = "black")) +
      scale_x_log10() +
      scale_y_log10(labels=comma) +
      geom_hline(yintercept = 0, linetype="dashed", color = "red", size=1)

    # Return the graph
    return(plot)

  }

  else {
    stop(paste0("Please select a number from 1 -3 to specify a graph type"))
  }
}