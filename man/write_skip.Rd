% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{write_skip}
\alias{write_skip}
\title{Write Skip}
\usage{
write_skip(file, text, db = NULL, skip_file = NULL, context = NULL,
  test = NULL, overwrite = FALSE)
}
\arguments{
\item{file}{The file that skips should be written to}

\item{text}{The text used to note the skip}

\item{db}{optional The db that should be skipped}

\item{skip_file}{optional The file that should be skipped}

\item{context}{optional The context that should be skipped}

\item{test}{optional The test or verb that should be skipped}

\item{overwrite}{optional Whether to overwrite the file in question (defaults to false)}
}
\value{
The input filename
}
\description{
A utility to make writing skips a bit easier
and more reproducible
}
