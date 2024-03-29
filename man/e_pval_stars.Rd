% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/e_pval_stars.R
\name{e_pval_stars}
\alias{e_pval_stars}
\title{Return star symbols for ranges between a set of p-values, or any set of numbers}
\usage{
e_pval_stars(
  p_values = NULL,
  cutpoints = c(0, 1e-04, 0.001, 0.01, 0.05, 0.1, 1),
  symbols = c("****", "***", "**", "*", "-", " ")
)
}
\arguments{
\item{p_values}{list of p-values}

\item{cutpoints}{list of p-value cutpoints, should include 0 and 1 at extremes}

\item{symbols}{list of symbols with length one fewer than cutpoints to indicate which two cutpoints the p-value was between}
}
\value{
a list of symbols of the same length as p_values
}
\description{
Default text for caption: "p-value stars:  (blank): not sig.;   -: p > 0.10;   *: p <= 0.05;   **: p <= 0.01;   ***: p <= 0.001;   ****: p <= 0.0001"
}
\examples{
e_pval_stars(c(0.049, 0.050, 0.051, NA))
}
