% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/e_health_ALL.R
\name{e_calc_bmi}
\alias{e_calc_bmi}
\title{BMI Calculation}
\usage{
e_calc_bmi(weight, height, system = c("Metric", "English")[1])
}
\arguments{
\item{weight}{in kg or lb}

\item{height}{in cm or in}

\item{system, }{either "Metric" (kg, cm) or "English" (lb, in)}
}
\value{
bmi value
}
\description{
BMI Calculation
}
\examples{
e_calc_bmi(60, 150)
e_calc_bmi(132, 59, system = "English")
e_calc_bmi(seq(40, 70, by = 5), 150)
}
