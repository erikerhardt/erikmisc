#' Logistic Regression power analysis (determine sample size)
#'
#' This function computes the sample size required for a logistic regression
#' with a single continuous covariate of interest x
#' and one or more optional control variables  z1-zk, Pr(y=1 | x, z1,...zk),
#'
#' It is based on Phil Ender's powerlog.ado (version 1.0) for Stata,
#' which is itself based on POWERLOG.SAS (version 1.1) for SAS by Michael Friendly.
#' \url{https://www.datavis.ca/books/vcd/powerlog.html}
#'
#' @param p1        is the probability that the outcome equals 1 when the predictor x is at its mean.
#' @param p2        is the probability that the outcome equals 1 when the predictor x is one standard deviation above its mean. Both \code{p1} and \code{p2} must be strictly greater than 0 and strictly less than 1. The best way to obtain these numbers is through a pilot study that closely mimics your research design or from previous literature.
#' @param r2        is the squared multiple correlation between the predictor variable x and all other variables in the model z1-zk (default = 0).  Use r2=0 for a one-predictor model with only x.
#' @param sw_alpha  is the significance level for the test. It is the probability of rejecting the null hypothesis when the null hypothesis is true (default = 0.05).
#' @param sw_power  is the probability of detecting a "true" effect when it exists, defined as 1 minus the probability of incorrectly accepting the null hypothesis (default = 0.80).
#' @param sw_print_details T/F, prints an explanation (default = TRUE)
#' @param sw_glm_type   glm model type, currently only \code{"logistic"}.
#'
#' @return out      a list containing the inputs and the result \code{n}, the sample size required for the logit model
#' @importFrom stats qnorm
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#'
#' out_glm_power <-
#'   e_glm_power(
#'     p1                = 0.2
#'   , p2                = 0.5
#'   , r2                = 0
#'   , sw_alpha          = 0.05
#'   , sw_power          = 0.8
#'   , sw_print_details  = c(TRUE, FALSE)[1]
#'   , sw_glm_type       = c("logistic")[1]
#'   )
#' out_glm_power |> print()
#'
e_glm_power <-
  function(
    p1                = 0.5
  , p2                = 0.5
  , r2                = 0
  , sw_alpha          = 0.05
  , sw_power          = 0.8
  , sw_print_details  = c(TRUE, FALSE)[1]
  , sw_glm_type       = c("logistic")[1]
  ) {
  # https://stats.stackexchange.com/questions/601826/power-analysis-for-probit-model-in-r/601941#601941
  pd      <- p2 - p1
  l1      <- p1 / (1 - p1)
  l2      <- p2 / (1 - p2)
  theta   <- l2 / l1
  or      <- theta
  lambda  <- log(theta)
  lambda2 <- lambda^2
  za      <- stats::qnorm(1 - sw_alpha)

  cat("\nLogistic regression power analysis\n")
  cat("One-tailed test: alpha =", sw_alpha, ", power =", sw_power, ", p1 =", p1, ", p2 =", p2, ", r2 =", r2, ", odds ratio =", or, "\n\n")
  cat("n\n")
  delta <- (1 + (1 + lambda2) * exp(5 * lambda2 / 4)) / (1 + exp(-1 * lambda2 / 4))
  zb <- stats::qnorm(sw_power)
  N <- ((za + zb * exp(-1 * lambda2 / 4))^2 * (1 + 2 * p1 * delta)) / (p1 * lambda2)
  N <- ceiling(N / (1 - r2))  # original was round()
  cat(formatC(N, format="d", big.mark=","), "\n")

  if (sw_print_details) {
    cat("\nExplanation of terms:\n")
    cat("\np1 is the probability that the outcome equals 1 when the predictor x is at the mean")
    cat("\np2 is the probability that the outcome equals 1 when the predictor x is one standard deviation above its mean")
    if (r2 != 0) {
      cat("\nr2 is the squared multiple correlation between the predictor variable and all other variables in the model")
    }
    cat("\npd is the difference in probabilities")
    cat("\nor is the corresponding odds ratio")
    cat("\nn is the sample size required for the logit model")
  }

  out <-
    tibble::tibble(
      p1    = p1
    , p2    = p2
    , r2    = r2
    , pd    = pd
    , or    = or
    , alpha = sw_alpha
    , power = sw_power
    , n     = N
    )

  return(out)

} # e_glm_power
