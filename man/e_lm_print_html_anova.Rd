% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/e_lm_print_results_ALL.R
\name{e_lm_print_html_anova}
\alias{e_lm_print_html_anova}
\title{Print an lm() anova table to html}
\usage{
e_lm_print_html_anova(
  fit = fit,
  n_digits = 3,
  filename_html = "table_anova.html"
)
}
\arguments{
\item{fit}{result of lm()}

\item{n_digits}{number of digits to use for estimates in table}

\item{filename_html}{filename.html to save html}
}
\value{
html table to print within Rmd report
}
\description{
Open and copy html and paste into Excel to format tables for manuscript in MS Word.
}
\examples{
fit <-
  lm(
    mpg ~ cyl + disp + hp + drat + wt + qsec
  , data = dat_mtcars_e
  )
e_lm_print_html_anova(
    fit           = fit
  , n_digits      = 3
  , filename_html = NULL # NULL will not save html
  )
}
