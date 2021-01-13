
 lib <- c('readr', 'dplyr', 'tidytext', 'ggplot2', 'scales', 'rlang')
 lapply(lib, library, character.only = T)
 rm(lib)

#importing the data
 trump_data <- read_csv('../data/trump-speech-data.csv') %>%
   subset(select = -c(X1))

#tokenizing - creating a tibble with location, words

zipfs_law <- function(trump_data, plot_type){
  
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
  
  require(scales)
  
  if (plot_type == 1){
    
    # PLOTTING THE ZIPF'S LAW
    plot <- law_data %>%
      ggplot(aes(x = rank, y = tf, color = location)) +
      geom_line() +
      labs(x = "Word rank", y = "Term frequencey (tf)",
           title = "Zipf's Law for Donald Trump's Rallies Data",
           color = "Location") +
      scale_x_log10() +
      scale_y_log10(labels = comma) 
    return(plot)
  }
    
  else if (plot_type==2){
    
    lin_reg = coef(lm(log(rank) ~ log(tf), data = law_data))
    
    #ADDING THE LINEAR REGRESSION
    plot <- law_data %>% 
      ggplot(aes(x = rank, y = tf, color = location)) +
      geom_line() +
      labs(x = "Word rank", y = "Term frequencey (tf)", 
           title = "Zipf's Law for Donald Trump's Rallies Data",
           color = "Location") +
      scale_x_log10() +
      scale_y_log10(labels = comma) +
      geom_abline(intercept = lin_reg[1], slope = round(lin_reg[2], digits = 0))
    
    rm(lin_reg)
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
    
    
    require(scales)
    
    # PLOT THE THEORETICAL ZIPF'S LAW AGAINS THE WORD FREQUENCY
    plot <- ggplot(theor_data_zipfs, aes(x = rank, y = n)) + 
      geom_line(aes(color = "observed")) +
      theme_bw() + 
      geom_line(aes(y = zipfs_freq, color = "theoretical")) +
      labs(x = "Word's Rank", y = "Frequency", title = "Zipf's law visualization") +
      scale_colour_manual(name = "Word count", values=c("theoretical" = "red", "observed" = "black")) +
      # theme(legend.position = "top") +
      # xlim(0, NA) +
      # ylim(0, NA)+
      scale_x_log10() +
      scale_y_log10(labels=comma) +
      geom_hline(yintercept = 0, linetype="dashed", color = "red", size=1) 
      
    
    return(plot)
  }
  
}

zipfs_law(trump_data, 3)
