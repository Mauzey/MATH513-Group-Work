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
#' # plot the top 10 most frequent words, according to their tf-idf index
#' plot_tf_idf(trump_speeches, 10)
#'
#' @source ???
#'
"trump_speeches"
