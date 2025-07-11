% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/e_plot_model_diagnostics_EACH.R
\name{e_plot_model_diagnostics_gvlma}
\alias{e_plot_model_diagnostics_gvlma}
\title{Model diagnostics, gvlma}
\usage{
e_plot_model_diagnostics_gvlma(fit = NULL, dat = NULL)
}
\arguments{
\item{fit}{fit object}

\item{dat}{dataset data.frame or tibble}
}
\value{
out      list including text and ggplot grobs
}
\description{
Methods from the paper: Pena, EA and Slate, EH,
"Global Validation of Linear Model Assumptions,"
J. American Statistical Association, 101(473):341-354, 2006.
}
