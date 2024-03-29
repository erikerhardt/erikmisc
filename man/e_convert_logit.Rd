% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/e_convert_oddsratio-probability_ALL.R
\name{e_convert_logit}
\alias{e_convert_logit}
\title{Logit: Convert from Probability to log(Odds Ratio)}
\usage{
e_convert_logit(p = NULL, sw_percent = FALSE)
}
\arguments{
\item{p}{list of probabilities}

\item{sw_percent}{T/F input is percent}
}
\value{
z         list of log(odds ratios) logit-scale values
}
\description{
See \code{car::logit} for a better implementation.
}
\examples{
p <- seq(0, 1, by = 0.1)
e_convert_logit(p)
}
