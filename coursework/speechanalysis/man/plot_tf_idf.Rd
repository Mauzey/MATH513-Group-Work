% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_tf_idf.R
\name{plot_tf_idf}
\alias{plot_tf_idf}
\title{Most Frequent Words per Speech, by tf-idf Index}
\usage{
plot_tf_idf(df, n_words = 10)
}
\arguments{
\item{df}{A dataframe with the following columns: 'speech' <chr>, 'location' <chr>, 'date' <date>}

\item{n_words}{<int> The number of 'top' values to plot}
}
\value{
<ggplot> A plot of the most frequent words used
}
\description{
Returns a plot of the most frequent words used (in descending order) during a collection of speeches. The plot is faceted by
the location of the speech, and uses the tf-idf index to determine word frequencies.
}
\examples{
plot_tf_idf(trump_speeches, 10)

}
\author{
10570155 \email{10570155}
}
