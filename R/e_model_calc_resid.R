#' Calculate residual types from a regression model
#'
#' @param fit           fit from \code{lm}, \code{glm}, \code{lme}, \code{lmerMod}, \code{lmerModLmerTest}, or \code{glmerMod}
#' @param resid_type    type of residuals, see options below for details.
#'
#' \strong{\code{type} options}
#'
#' Several residual types are available based on the model.
#' \itemize{
#' \item \code{lm} residual options
#' \itemize{
#' \item \code{"pearson"}: The Pearson residuals
#' \item \code{"response"}: The raw residuals (Default for "lm")
#' \item \code{"standardized"}: The standardized raw residuals
#' \item \code{"studentized"}: The externally studentized residuals
#' \item \code{"partial"}: The partial residuals for each covariate
#' }
#' \item \code{glm} residual options
#' \itemize{
#' \item \code{"pearson"}: The Pearson residuals
#' \item \code{"deviance"}: The deviance residuals (Default for "glm")
#' \item \code{"response"}: The raw residuals
#' \item \code{"stand.deviance"}: The standardized deviance residuals
#' \item \code{"stand.pearson"}: The standardized Pearson residuals
#' \item \code{"studentized"}: The externally studentized residuals
#' \item \code{"partial"}: The partial residuals for each covariate
#' }
#' \item \code{lmer}, \code{lmerTest}, and \code{lme} residual options
#' \itemize{
#' \item \code{"pearson"}: The Pearson residuals (Default for "lmer", "lmerTest", and "lme")
#' \item \code{"response"}: The raw residuals
#' }
#' \item \code{glmer} residual options
#' \itemize{
#' \item \code{"pearson"}: The Pearson residuals
#' \item \code{"deviance"}: The deviance residuals (Default for "glmer")
#' \item \code{"response"}: The raw residuals
#' } }
#'
#' @return res    specified type of residuals from the model
#' @importFrom stats resid rstudent
#' @importFrom MASS stdres
#' @export
#'
e_model_calc_resid <-
  function(
    fit         = NULL
  , resid_type  = c(NA, "pearson", "response", "standardized", "deviance", "stand.deviance", "stand.pearson", "studentized", "partial")[1]
  ) {

  fit_resid <- NULL
  fit_class <- class(fit)[1]

  # lm residuals
  if(fit_class == "lm") {

    # Default: standardized residuals
    if(is.na(resid_type) | resid_type == "standardized") {
      if (any(!is.finite(MASS::stdres(fit)))) {
        warning("erikmisc::e_model_calc_resid, Leverage 1 observation(s) encountered, reporting Pearson Residuals")
        fit_resid <- stats::resid(fit, resid_type = "response") / summary(fit)$sigma
        attr(fit_resid, "resid_type") <- "standardized"
        return(fit_resid)
      } else {
      fit_resid <- MASS::stdres(fit)
      attr(fit_resid, "resid_type") <- "standardized"
      return(fit_resid)
      }
    }
    if(resid_type == "pearson") {
      fit_resid <- stats::resid(fit, resid_type = "response") / summary(fit)$sigma
      attr(fit_resid, "resid_type") <- "pearson"
      return(fit_resid)
    }
    if(resid_type == "response") {
      fit_resid <- stats::resid(fit, "response")
      attr(fit_resid, "resid_type") <- "response"
      return(fit_resid)
    }
    if(resid_type == "studentized") {
      fit_resid <- stats::rstudent(fit)
      attr(fit_resid, "resid_type") <- "studentized"
      return(fit_resid)
    }
    if(resid_type == "partial") {
      fit_resid <- stats::residuals(fit, "partial")
      attr(fit_resid, "resid_type") <- "partial"
      return(fit_resid)
    }
  }

  # glm residuals
  if (fit_class == "glm") {

    # Default: deviance residuals
    if(is.na(resid_type) | resid_type == "deviance") {
      fit_resid <- stats::resid(fit, resid_type = "deviance")
      attr(fit_resid, "resid_type") <- "deviance"
      return(fit_resid)
    }
    if (resid_type == "response") {
      fit_resid <- stats::resid(fit, resid_type = "response")
      attr(fit_resid, "resid_type") <- "response"
      return(fit_resid)
    }
    if (resid_type == "pearson") {
      fit_resid <- stats::resid(fit, resid_type = "pearson")
      attr(fit_resid, "resid_type") <- "pearson"
      return(fit_resid)
    }
    if (resid_type == "stand.deviance") {
      fit_resid <- (stats::resid(fit, resid_type = "deviance")) /
                    (sqrt(summary(fit)$dispersion*(1 - hatvalues(fit))))
      attr(fit_resid, "resid_type") <- "stand.deviance"
      return(fit_resid)
    }
    if (resid_type == "stand.pearson") {
      fit_resid <- (stats::resid(fit, resid_type = "pearson")) /
                    (sqrt(summary(fit)$dispersion*(1 - hatvalues(fit))))
      attr(fit_resid, "resid_type") <- "stand.pearson"
      return(fit_resid)
    }
    if(resid_type == "studentized") {
      fit_resid <- stats::rstudent(fit)
      attr(fit_resid, "resid_type") <- "studentized"
      return(fit_resid)
    }
    if(resid_type == "partial") {
      fit_resid <- stats::residuals(fit, "partial")
      attr(fit_resid, "resid_type") <- "partial"
      return(fit_resid)
    }
  }

  # lme residuals
  if (fit_class == "lme") {

    # Default: Pearson residuals (conditional on BLUPs)
    if(is.na(resid_type) | resid_type == "pearson") {
      fit_resid <- stats::resid(fit, resid_type = "response") / summary(fit)$sigma
      attr(fit_resid, "resid_type") <- "pearson"
      return(fit_resid)
    }
    if (resid_type == "response") {
      fit_resid <- stats::resid(fit, resid_type = "response")
      attr(fit_resid, "resid_type") <- "response"
      return(fit_resid)
    }
  }

  # lmer residuals
  if (fit_class == "lmerMod") {

    # Default: Pearson residuals (conditional on BLUPs)
    if(is.na(resid_type) | resid_type == "pearson") {
      fit_resid <- stats::resid(fit, resid_type = "response") / summary(fit)$sigma
      attr(fit_resid, "resid_type") <- "pearson"
      return(fit_resid)
    }
    if (resid_type == "response") {
      fit_resid <- stats::resid(fit, resid_type = "response")
      attr(fit_resid, "resid_type") <- "response"
      return(fit_resid)
    }
  }

  # lmerTest residuals
  if (fit_class == "lmerModLmerTest") {

    # Default: Pearson residuals (conditional on BLUPs)
    if(is.na(resid_type) | resid_type == "pearson") {
      fit_resid <- stats::resid(fit, resid_type = "response") / summary(fit)$sigma
      attr(fit_resid, "resid_type") <- "pearson"
      return(fit_resid)
    }
    if (resid_type == "response") {
      fit_resid <- stats::resid(fit, resid_type = "response")
      attr(fit_resid, "resid_type") <- "response"
      return(fit_resid)
    }
  }

  # glmer residuals
  if (fit_class == "glmerMod") {

    # Default: deviance residuals
    if(is.na(resid_type) | resid_type == "deviance") {
      fit_resid <- stats::resid(fit, resid_type = "deviance")
      attr(fit_resid, "resid_type") <- "deviance"
      return(fit_resid)
    }
    if (resid_type == "response") {
      fit_resid <- stats::resid(fit, resid_type = "response")
      attr(fit_resid, "resid_type") <- "response"
      return(fit_resid)
    }
    if (resid_type == "pearson") {
      fit_resid <- stats::resid(fit, resid_type = "pearson")
      attr(fit_resid, "resid_type") <- "pearson"
      return(fit_resid)
    }
  }

  if (is.null(fit_resid)) {
    warning("erikmisc::e_model_calc_resid, model class or resid type not supported")
    return(NULL)
  }

} # e_model_calc_resid
