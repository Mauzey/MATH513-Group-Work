% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zipfs_law.R
\name{zipfs_law}
\alias{zipfs_law}
\title{Graph of word frequencies against linear regression and against the theoretical Zipf's Law.}
\usage{
zipfs_law(df, plot_type)
}
\arguments{
\item{df}{A dataframe with the following columns: 'speech' <chr>, 'location' <chr>, 'date' <date>}

\item{plot_type}{A number 1, 2, or 3 which indicates of the type of plot to be displayed.
1 stands for 'frequency plot', 2 for 'frequency + linear',
3 for 'theoretical zipf's law and practical frequencies'}
}
\value{
<ggplot> Graph of word frequencies against linear regression and against the theoretical Zipf's Law.
}
\description{
This function provides a graph using ggplt2 of a data set containing speeches.
}
\examples{
zipfs_law(trump_speeches, 1)

}
\author{
10696253 \email{10696253}
}
