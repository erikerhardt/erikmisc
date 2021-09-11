#' Plot the result of a t-test by shading the p-value area under the t-distribution
#'
#' @param t_summary   the returned object from stats::t.test()
#' @param sw_graphics choice of ggplot or base graphics
#'
#' @return if base graphics, NULL invisibly; if ggplot, the plot grob
#' @export
#'
#' @examples
#' t_summary <- t.test(datasets::mtcars$mpg, mu = 20, data = datasets::mtcars)
#' e_plot_ttest_pval(t_summary)
#' t_summary <- t.test(mpg ~ am, mu = 0, data = datasets::mtcars)
#' e_plot_ttest_pval(t_summary)
e_plot_ttest_pval <-
  function(
    t_summary
  , sw_graphics = c("ggplot", "base")[2]
  ) {

  lim_extreme <- max(4, abs(t_summary$statistic) + 0.5)
  lim_lower   <- -lim_extreme
  lim_upper   <-  lim_extreme
  x_curve     <- seq(lim_lower, lim_upper, length=200)
  y_curve     <- dt(x_curve, df = t_summary$parameter)
  text_title  <- t_summary$method
  text_subtitle  <- e_ttest_alt_hyp_text(t_summary)
  text_y      <- paste("t-dist( df =", signif(t_summary$parameter, 3), ")")
  text_x      <- paste("t-stat =", signif(t_summary$statistic, 5)
                     , ", Shaded area is p-value =", signif(t_summary$p.value, 5))

  if (sw_graphics == c("ggplot", "base")[1]) {
    warning("e_t_dist_pval: ggplot not available, reverting to base graphics")
    sw_graphics = c("ggplot", "base")[2]
  }

  if (sw_graphics == c("ggplot", "base")[2]) {
    par(mfrow=c(1,1))
    plot(x_curve, y_curve, type = "n"
        , ylab = text_y
        , xlab = text_x
        , main = text_title
        , sub  = text_subtitle
        )
    if ((t_summary$alternative == "less")
        | (t_summary$alternative == "two.sided")) {
      x_pval_l <- seq(lim_lower, -abs(t_summary$statistic), length=200)
      y_pval_l <- dt(x_pval_l, df = t_summary$parameter)
      polygon(c(lim_lower, x_pval_l, -abs(t_summary$statistic))
            , c(0, y_pval_l, 0), col="gray")
    }
    if ((t_summary$alternative == "greater")
        | (t_summary$alternative == "two.sided")) {
      x_pval_u <- seq(abs(t_summary$statistic), lim_upper, length=200)
      y_pval_u <- dt(x_pval_u, df = t_summary$parameter)
      polygon(c(abs(t_summary$statistic), x_pval_u, lim_upper)
            , c(0, y_pval_u, 0), col="gray")
    }
    points(x_curve, y_curve, type = "l", lwd = 2, col = "black")

    invisible(NULL)
  }

} # e_plot_ttest_pval


#' Return text of alternative hypothesis via modified code from stats::print.htest
#'
#` Credit: <https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/htest.R>
#'
#' @param x      stats::t.test object
#' @param digits number of digits to return, from \code{getOption("digits")}
#' @param ...    additional parameters
#'
#' @return out text describing alternative hypothesis
#' @export
#'
e_ttest_alt_hyp_text <-
  function(
    x
  , digits = getOption("digits")
  , ...
  )
  {
  # cat("\n")
  # cat(strwrap(x$method, prefix = prefix), sep = "\n")
  # cat("\n")
  # cat("data:  ", x$data.name, "\n", sep = "")
  out <- character()
  # if(!is.null(x$statistic))
  #   out <- c(out, paste(names(x$statistic), "=",
  #       format(signif(x$statistic, max(1L, digits - 2L)))))
  # if(!is.null(x$parameter))
  #   out <- c(out, paste(names(x$parameter), "=",
  #       format(signif(x$parameter, max(1L, digits - 2L)))))
  # if(!is.null(x$p.value)) {
  #   fp <- format.pval(x$p.value, digits = max(1L, digits - 3L))
  #   out <- c(out, paste("p-value",
  #       if(substr(fp, 1L, 1L) == "<") fp else paste("=",fp)))
  # }
  # cat(strwrap(paste(out, collapse = ", ")), sep = "\n")
  if(!is.null(x$alternative)) {
    out <-
      paste0(
        out
      , "Alternative hypothesis: "
      )
    if(!is.null(x$null.value)) {
      if(length(x$null.value) == 1L) {
        alt.char <-
            switch(
              x$alternative
            , two.sided  = "not equal to"
            , less       = "less than"
            , greater    = "greater than"
            )
        out <-
          paste0(
            out
          , "true ", names(x$null.value), " is ", alt.char, " ",
            x$null.value
          )
      } else {
        out <-
          paste0(
            out
          , x$alternative, "\nnull values:"
          , round(x$null.value, digits = digits)
          )
      }
    } else {
        out <-
          paste0(
            out
          , x$alternative
          )
    }
  }
  #   if(!is.null(x$conf.int)) {
  # cat(format(100 * attr(x$conf.int, "conf.level")),
  #     " percent confidence interval:\n", " ",
  #     paste(format(c(x$conf.int[1L], x$conf.int[2L])), collapse = " "),
  #           "\n", sep = "")
  #   }
  #   if(!is.null(x$estimate)) {
  # cat("sample estimates:\n")
  # print(x$estimate, digits=digits, ...)
  #   }
  #   cat("\n")

  return(out)
} # e_ttest_alt_hyp_text
