#' Limit the range of values from a distribution function
#'
#' @param fun function, such as distribution function like `dnorm`
#' @param min min value to include in range
#' @param max max value to include in range
#' @param ... arguments passed to fun
#'
#' @return filtered range of the function
#' @export
#'
#' @examples
#'
#' f_dnorm_limit <- e_plot_function_limit_range(dnorm, 0, 2)
#' f_dnorm_limit(-2:4)
#'
#' library(ggplot2)
#' p <- ggplot(data.frame(x = c(-3, 3)), aes(x = x))
#' p <- p + stat_function(fun = dnorm)
#' p <- p + stat_function(fun = e_plot_function_limit_range(dnorm, 0, 2)
#'                      , geom = "area", fill = "blue", alpha = 0.2)
#' p
e_plot_function_limit_range <-
  function(
    fun = dnorm
  , min = -Inf
  , max = +Inf
  , ...
  ) {
  ## From https://r-graphics.org/recipe-miscgraph-function-shade

  function(x) {
    y <- fun(x, ...)
    y[x < min  |  x > max] <- NA
    return(y)
  }
} # e_plot_function_limit_range
