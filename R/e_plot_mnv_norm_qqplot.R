#' Graphical Assessment (QQ-plot) for assessing Multivariate Normality
#'
#' @param x    data.frame or matrix of numeric columns
#' @param name label for title
#'
#' @return \code{invisible(NULL)}
#' @importFrom graphics abline
#' @importFrom stats mahalanobis
#' @importFrom stats ppoints
#' @importFrom stats qchisq
#' @importFrom stats cov
#' @importFrom stats qqplot
#' @export
#'
#' @examples
#' dat_mtcars_e |>
#'   dplyr::filter(cyl == "four") |>
#'   dplyr::select(mpg, hp, wt) |>
#'   e_plot_mnv_norm_qqplot(name = "cyl = 4")
#' dat_mtcars_e |>
#'   dplyr::filter(cyl == "six") |>
#'   dplyr::select(mpg, hp, wt) |>
#'   e_plot_mnv_norm_qqplot(name = "cyl = 6")
#' dat_mtcars_e |>
#'   dplyr::filter(cyl == "eight") |>
#'   dplyr::select(mpg, hp, wt) |>
#'   e_plot_mnv_norm_qqplot(name = "cyl = 8")
e_plot_mnv_norm_qqplot <-
  function(
    x
  , name = ""
  ) {

  x      <- as.matrix(x)    # n x p numeric matrix
  center <- colMeans(x)     # centroid
  n      <- nrow(x)
  p      <- ncol(x)
  cov    <- stats::cov(x)
  d      <- stats::mahalanobis(x, center, cov) # distances
  stats::qqplot(
      x    = stats::qchisq(stats::ppoints(n), df = p)
    , y    = d
    , main = paste0("QQ Plot MV Normality: ", name)
    , ylab = "Mahalanobis D2 distance"
    , xlab = "Chi-squared quantiles"
  )
  graphics::abline(a = 0, b = 1, col = "red")

  invisible(NULL)
} # e_plot_mnv_norm_qqplot

