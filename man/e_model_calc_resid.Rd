% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/e_model_calc_resid.R
\name{e_model_calc_resid}
\alias{e_model_calc_resid}
\title{Calculate residual types from a regression model}
\usage{
e_model_calc_resid(
  fit = NULL,
  resid_type = c(NA, "pearson", "response", "standardized", "deviance", "stand.deviance",
    "stand.pearson", "studentized", "partial")[1]
)
}
\arguments{
\item{fit}{fit from \code{lm}, \code{glm}, \code{lme}, \code{lmerMod}, \code{lmerModLmerTest}, or \code{glmerMod}}

\item{resid_type}{type of residuals, see options below for details.

\strong{\code{type} options}

Several residual types are available based on the model.
\itemize{
\item \code{lm} residual options
\itemize{
\item \code{"pearson"}: The Pearson residuals
\item \code{"response"}: The raw residuals (Default for "lm")
\item \code{"standardized"}: The standardized raw residuals
\item \code{"studentized"}: The externally studentized residuals
\item \code{"partial"}: The partial residuals for each covariate
}
\item \code{glm} residual options
\itemize{
\item \code{"pearson"}: The Pearson residuals
\item \code{"deviance"}: The deviance residuals (Default for "glm")
\item \code{"response"}: The raw residuals
\item \code{"stand.deviance"}: The standardized deviance residuals
\item \code{"stand.pearson"}: The standardized Pearson residuals
\item \code{"studentized"}: The externally studentized residuals
\item \code{"partial"}: The partial residuals for each covariate
}
\item \code{lmer}, \code{lmerTest}, and \code{lme} residual options
\itemize{
\item \code{"pearson"}: The Pearson residuals (Default for "lmer", "lmerTest", and "lme")
\item \code{"response"}: The raw residuals
}
\item \code{glmer} residual options
\itemize{
\item \code{"pearson"}: The Pearson residuals
\item \code{"deviance"}: The deviance residuals (Default for "glmer")
\item \code{"response"}: The raw residuals
} }}
}
\value{
res    specified type of residuals from the model
}
\description{
Calculate residual types from a regression model
}
