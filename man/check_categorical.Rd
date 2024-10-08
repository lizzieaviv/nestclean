% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{check_categorical}
\alias{check_categorical}
\title{Summarize Categorical Variables}
\usage{
check_categorical(df, ...)
}
\arguments{
\item{df}{A data frame containing the categorical variables to be summarized.}

\item{...}{One or more unquoted expressions separated by commas, indicating variables to summarize (e.g., column names, column ranges, or selection helpers like contains()).}
}
\value{
A data frame in wide format with the frequency counts for each value of the specified variables.
}
\description{
This function calculates the frequency count for each unique value of the specified categorical variables in a data frame. The results include counts of missing values and are returned in a wide format.
}
