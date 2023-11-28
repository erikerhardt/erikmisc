#' Return table of model selection criteria
#'
#' @description
#'
#' Calculates a large set of model selection criteria and returns them in a one-row tibble.
#'
#' AIC and BIC criteria from the stats package:
#' * `aic` Akaike's An Information Criterion
#' * `bic` Bayesian Information Criterion (AIC with k = log(nobs(object)))
#' * `caic` Conditional Akaike Information for 'lme4' and 'lme'
#' * `nobs` number of observations used to fit \code{lm_fit}
#'
#' Several criteria from the lm summary:
#' * `r2` R-squared statistic
#' * `r2adj` Adjusted R-squared statistic
#' * `f_stat` F-statistic compared to grand mean model (NA if y ~ 1 intercept-only model)
#' * `p` F-statistic numerator degrees-of-freedom (number of parameters in the model)
#' * `df` F-statistic denominator degrees-of-freedom (number of df estimating the variance)
#' * `f_stat_pval` F-statistic p-value
#' * `rse` Residual standard error.
#'
#' Several criteria are defined in \code{?modelr::`model-quality`}:
#'
#' Three summaries are immediately interpretible on the scale of the response
#'  variable:
#' * `rmse` is the root-mean-squared-error
#' * `mae` is the mean absolute error
#' * `qae` is quantiles of absolute error.
#'
#' Other summaries have varying scales and interpretations:
#' * `mape` mean absolute percentage error.
#' * `rsae` is the relative sum of absolute errors.
#' * `mse` is the mean-squared-error.
#' * `rsquare` is the variance of the predictions divided by the
#'   variance of the response.
#'
#' @param lm_fit    fitted model object
#' @param dat_fit   data used for model fit
#' @param model_id  label for each model that is fit, helps to match up with original model
#'
#' @return          a tibble of model selection criteria
#' @importFrom tibble tibble
#' @importFrom stats  AIC
#' @importFrom stats  BIC
#' @importFrom stats  nobs
#' @importFrom stats  pf
#' @importFrom modelr mse
#' @importFrom modelr rmse
#' @importFrom modelr rsquare
#' @importFrom modelr mae
#' @importFrom modelr mape
#' @importFrom modelr rsae
#' @importFrom cAIC4  cAIC
#' @export
#'
#' @examples
#' lm_form <- formula(mpg ~ cyl + carb + disp + hp + disp:hp + wt + vs + am + gear)
#' lm_fit <-
#'   stats::lm(
#'     formula = lm_form
#'   , data    = dat_mtcars_e
#'   )
#' lm_crit <-
#'   e_lm_model_criteria(
#'     lm_fit   = lm_fit
#'   , dat_fit  = dat_mtcars_e
#'   , model_id = 1
#'   )
#' lm_crit |> print(width = Inf)
#'
#' # intercept-only model
#' lm_form <- formula(mpg ~ 1)
#' lm_fit <-
#'   stats::lm(
#'     formula = lm_form
#'   , data    = dat_mtcars_e
#'   )
#' lm_crit <-
#'   e_lm_model_criteria(
#'     lm_fit   = lm_fit
#'   , dat_fit  = dat_mtcars_e
#'   , model_id = 1
#'   )
#' lm_crit |> print(width = Inf)
e_lm_model_criteria <-
  function(
    lm_fit    = NULL
  , dat_fit   = NULL
  , model_id  = NULL
  ) {

  if ( is.null(lm_fit) | is.null(dat_fit) ) {
    warning("erikmisc::e_lm_model_criteria, specify both lm_fit and dat_fit.")
    return(NULL)
  }


  if (inherits(lm_fit, "lm")) {
    out <-
      tibble::tibble(
        model_id      = model_id
      , aic           = stats::AIC      (object = lm_fit)
      , bic           = stats::BIC      (object = lm_fit)
      , nobs          = stats::nobs     (object = lm_fit)
      , r2            = summary(object = lm_fit)$r.squared
      , r2adj         = summary(object = lm_fit)$adj.r.squared
      , f_stat        = ifelse(is.null(summary(object = lm_fit)$fstatistic["value"]), NA, summary(object = lm_fit)$fstatistic["value"])  # if intercept-only model, then NULL
      , p             = ifelse(is.null(summary(object = lm_fit)$fstatistic["numdf"]), summary(object = lm_fit)$df[1], summary(object = lm_fit)$fstatistic["numdf"])  # if intercept-only model, then NULL
      , df            = ifelse(is.null(summary(object = lm_fit)$fstatistic["dendf"]), summary(object = lm_fit)$df[2], summary(object = lm_fit)$fstatistic["dendf"])  # if intercept-only model, then NULL
      , f_stat_pval   = stats::pf(q = f_stat, df1 = p, df2 = df, lower.tail = FALSE)
      , rse           = summary(object = lm_fit)$sigma
      , mse           = modelr::mse     (model = lm_fit, data = dat_fit)
      , rmse          = modelr::rmse    (model = lm_fit, data = dat_fit)
      , rsquare       = modelr::rsquare (model = lm_fit, data = dat_fit)
      , mae           = modelr::mae     (model = lm_fit, data = dat_fit)
      #, qae           = modelr::qae     (model = lm_fit, data = dat_fit)
      , mape          = modelr::mape    (model = lm_fit, data = dat_fit)
      , rsae          = modelr::rsae    (model = lm_fit, data = dat_fit)
      )
  }

  if (inherits(lm_fit, "lmerMod")) {
    out <-
      tibble::tibble(
        model_id      = model_id
      , aic           = stats::AIC      (object = lm_fit)
      , bic           = stats::BIC      (object = lm_fit)
      , caic          = cAIC4::cAIC     (object = lm_fit)$caic
      , nobs          = stats::nobs     (object = lm_fit)
      # , r2            = summary(object = lm_fit)$r.squared
      # , r2adj         = summary(object = lm_fit)$adj.r.squared
      # , f_stat        = summary(object = lm_fit)$fstatistic["value"]
      # , f_stat_numdf  = summary(object = lm_fit)$fstatistic["numdf"]
      # , f_stat_dendf  = summary(object = lm_fit)$fstatistic["dendf"]
      # , f_stat_pval   = stats::pf(q = f_stat, df1 = f_stat_numdf, df2 = f_stat_dendf, lower.tail = FALSE)
      , p             = summary(object = lm_fit)$devcomp$dims["p"]
      , df            = summary(object = lm_fit)$devcomp$dims["nmp"]
      , rse           = summary(object = lm_fit)$sigma
      , mse           = modelr::mse     (model = lm_fit, data = dat_fit)
      , rmse          = modelr::rmse    (model = lm_fit, data = dat_fit)
      , rsquare       = modelr::rsquare (model = lm_fit, data = dat_fit)
      , mae           = modelr::mae     (model = lm_fit, data = dat_fit)
      #, qae           = modelr::qae     (model = lm_fit, data = dat_fit)
      , mape          = modelr::mape    (model = lm_fit, data = dat_fit)
      , rsae          = modelr::rsae    (model = lm_fit, data = dat_fit)
      )
  }

  return(out)

} # e_lm_model_criteria

