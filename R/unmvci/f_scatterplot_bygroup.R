#' Scatterplot by group
#'
#' @param dat_sub data subset
#' @param y_var_plot variables to plot by group \code{dx_current}
#' @param y_var_name axis label
#' @param x_var_plot variables to plot by group \code{dx_current}
#' @param x_var_name axis label
#' @param my_smooth is "loess" smoother or "lm1" for horizontal y ~ 1 lines
#' @param my_span is stat_smooth span, 0.75 is default, 0.9 is smoother, 0.5 is more wiggly
#'
#' @return
#' @import ggplot2
#' @export
#'
#' @examples
f_scatterplot_bygroup <- function(dat_sub, y_var_plot, y_var_name = NULL, x_var_plot, x_var_name = NULL, my_smooth = c("loess", "lm1"), my_span = 0.75) {

  if(is.null(x_var_name)) {
    x_var_name <- x_var_plot
  }
  if(is.null(y_var_name)) {
    y_var_name <- y_var_plot
  }

  dat_plot <-
    dat_sub %>%
    select(dx_current, y_var_plot, x_var_plot)
  colnames(dat_plot)[2] <- "y_var"
  colnames(dat_plot)[3] <- "x_var"

  print(paste0(y_var_plot, " ~ ", x_var_plot, " =================================================="))

  dat_analy <- subset(dat_plot, is.finite(y_var) & is.finite(x_var) & !is.na(dx_current))

  p <- ggplot(dat_analy, aes(x = x_var, y = y_var, colour = dx_current, shape = dx_current, fill = dx_current))  # , linetype = dx_current
  p <- p + theme_bw()
  if(my_smooth == "loess") {
    p <- p + stat_smooth(se = TRUE, linetype = 0, alpha = 0.1, span = my_span) #method = "lm")  # bands
    p <- p + geom_point(alpha = 0.75)
    p <- p + stat_smooth(se = FALSE, alpha = 0.75, size = 1.5, span = my_span) #method = "lm")  # line
  }
  if(my_smooth == "lm1") {
    p <- p + stat_smooth(method = "lm", formula = y ~ 1, se = TRUE, linetype = 0, alpha = 0.1) #method = "lm")  # bands
    p <- p + geom_point(alpha = 0.75)
    p <- p + stat_smooth(method = "lm", formula = y ~ 1, se = FALSE, alpha = 0.75, size = 1.5) #method = "lm")  # line
  }

  p <- p + labs(  y = y_var_name
                , x = x_var_name
                , colour    = "Diagnosis"
                , shape     = "Diagnosis"
                , linetype  = "Diagnosis"
                , fill      = "Diagnosis"
                )
  p <- p + theme(legend.position = "bottom")
  #p <- p + coord_cartesian(ylim = c(NA, max(dat_analy$y_var)))
  return(p)
}
