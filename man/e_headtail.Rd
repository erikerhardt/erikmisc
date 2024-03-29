% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/e_headtail.R
\name{e_headtail}
\alias{e_headtail}
\title{Print head and tail of a data.frame}
\usage{
e_headtail(x, n = 3L, which_col = NULL, ...)
}
\arguments{
\item{x}{data.frame or vector}

\item{n}{number of top rows and bottom rows to print, it prints 2n rows}

\item{which_col}{selected columns, if desired}

\item{...}{passed to head() and tail() functions}
}
\value{
dat_ht     the head and tail of the data.frame
}
\description{
Print head and tail of a data.frame
}
\examples{
e_headtail(dat_mtcars_e)
}
