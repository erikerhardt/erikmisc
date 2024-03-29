% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/e_plot_HELPERS.R
\name{e_plot_extract_legend}
\alias{e_plot_extract_legend}
\title{Extract ggplot legend as it's own plot grob object}
\usage{
e_plot_extract_legend(p = NULL)
}
\arguments{
\item{p}{ggplot grob}
}
\value{
p_legend ggplot grob of only the legend
}
\description{
Extract ggplot legend as it's own plot grob object
}
\examples{
\dontrun{
p_legend <- e_plot_extract_legend(p)
}
}
