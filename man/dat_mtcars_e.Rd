% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/erikmisc_DATA.R
\docType{data}
\name{dat_mtcars_e}
\alias{dat_mtcars_e}
\title{datasets::mtcars, factored and labeled}
\format{
tibble
}
\source{
\code{?datasets::mtcars}
}
\usage{
data(dat_mtcars_e)
}
\description{
Using \code{datasets::mtcars}, then applying labeled factors and labeling variables with \code{labelled::var_label} based on \code{?datasets::mtcars}
}
\examples{
data(dat_mtcars_e)
str(dat_mtcars_e)

# Generate missing values
dat_mtcars_miss_e <- dat_mtcars_e
prop_missing <- 0.10
n_missing <-
  sample.int(
    n    = prod(dim(dat_mtcars_miss_e))
  , size = round( prop_missing * prod(dim(dat_mtcars_miss_e)))
  )
ind_missing <- expand.grid(1:dim(dat_mtcars_miss_e)[1], 1:dim(dat_mtcars_miss_e)[2])[n_missing, ]
for (i_row in seq_along(n_missing)) {
  dat_mtcars_miss_e[ind_missing[i_row, 1], ind_missing[i_row, 2] ] <- NA
}
dat_mtcars_miss_e |> print()
}
\keyword{datasets}
