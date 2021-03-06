% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pull_configuration.R
\name{attr_to_df}
\alias{attr_to_df}
\title{Make data frame for category and attributes}
\usage{
attr_to_df(category, attr_ls, metro_config)
}
\arguments{
\item{category}{character, one of "detector", "r_node", or "corridor"}

\item{attr_ls}{list of attributes corresponding to the given category}

\item{metro_config}{sensor configuration}
}
\value{
a data frame of clean attributes for the given category
}
\description{
Make data frame for category and attributes
}
\keyword{internal}
