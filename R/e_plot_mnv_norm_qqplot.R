#' Graphical Assessment (QQ-plot) for assessing Multivariate Normality
#'
#' @param x    data.frame or matrix of numeric columns
#' @param name label for title
#'
#' @return \code{invisible(NULL)}
#' @export
#'
#' @examples
#' datasets::mtcars %>%
#'   dplyr::filter(cyl == 4) %>%
#'   dplyr::select(mpg, hp, wt) %>%
#'   e_plot_mnv_norm_qqplot(name = "cyl = 4")
#' datasets::mtcars %>%
#'   dplyr::filter(cyl == 6) %>%
#'   dplyr::select(mpg, hp, wt) %>%
#'   e_plot_mnv_norm_qqplot(name = "cyl = 6")
#' datasets::mtcars %>%
#'   dplyr::filter(cyl == 8) %>%
#'   dplyr::select(mpg, hp, wt) %>%
#'   e_plot_mnv_norm_qqplot(name = "cyl = 8")
e_plot_mnv_norm_qqplot <-
  function(
    x
  , name = ""
  ) {

  x <- as.matrix(x)         # n x p numeric matrix
  center <- colMeans(x)     # centroid
  n <- nrow(x)
  p <- ncol(x)
  cov <- cov(x)
  d <- mahalanobis(x, center, cov) # distances
  qqplot(
      qchisq(ppoints(n), df = p)
    , d
    , main = paste("QQ Plot MV Normality:", name)
    , ylab = "Mahalanobis D2 distance"
    , xlab = "Chi-squared quantiles"
  )
  abline(a = 0, b = 1, col = "red")

  invisible(NULL)
} # e_plot_mnv_norm_qqplot

