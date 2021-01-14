#' Trump Speech Data
#'
#' A dataset consisting of 10 speeches made by Donald Trump throughout September 2020.
#'
#' @format A data.frame with 10 rows and 3 columns:
#' \describe{
#'  \item{speech}{Speech transcript}
#'  \item{location}{Location of speech}
#'  \item{date}{Date of speech}
#' }
#'
#' @examples
#' plot_tf_idf(trump_speeches, 10)
#' word_frequency(trump_speeches, c("america", "biden", "China"))
#' zipfs_law(trump_speeches, 1)
#' 
#' @source University of Plymouth, MATH513 Assignment 2020/21
#'
"trump_speeches"
