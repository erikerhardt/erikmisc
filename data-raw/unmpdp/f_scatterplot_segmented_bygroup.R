#' Segmented scatterplot by group
#'
#' @param dat_sub data subset
#' @param y_var_plot variables to plot by group \code{dx_current}
#' @param y_var_name axis label
#' @param x_var_plot variables to plot by group \code{dx_current}
#' @param x_var_name axis label
#' @param sig_level for whether to use segmented regression lines. If p-value < sig_level, then use segmented regression lines, otherwise use single lm line.
#'
#' @return
#' @import ggplot2
#' @importFrom segmented segmented davies.test
#' @export
#'
#' @examples
f_scatterplot_segmented_bygroup <- function(dat_sub, y_var_plot, y_var_name = NULL, x_var_plot, x_var_name = NULL, sig_level = 0.10) {

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

  x_lim <- range(dat_plot$x_var, na.rm = TRUE)

  print(paste0(y_var_plot, " ~ ", x_var_plot, " =================================================="))

  dat_analy <- subset(dat_plot, is.finite(y_var) & is.finite(x_var) & !is.na(dx_current))

  # store predictions
  list_fit <- NULL

  for (i_diag in seq_along(unique(dat_sub$dx_current))) {
    ## i_diag = 1
    ## i_diag = 3
    dat_analy_1 <-
      dat_analy %>%
      filter(dx_current == !!unique(dat_sub$dx_current)[i_diag])

    if(nrow(dat_analy_1) < 2) {
      next
    }

    lm_fit <- lm(y_var ~ x_var, dat = dat_analy_1)

    # segmented() gives error when breakpoint is on the boundary
    seg_fit <- NULL
    try(
      seg_fit <- segmented::segmented(lm_fit, seg.Z = ~ x_var, psi = 70) #, control = seg.control(stop.if.error = FALSE))
    )
    if(is.null(seg_fit)) {
      seg_fit <- lm_fit
    } else {
      # # if different slopes is not significant, revert to lm()
      # if (segmented::davies.test(seg_fit, seg.Z = ~ x_var)$p.value >= sig_level) {
      #   seg_fit <- lm_fit
      # }
    }
    # reset lm_fit
    lm_fit <- NULL

    list_fit[[i_diag]] <-
      tibble(
        dx_current = unique(dat_sub$dx_current)[i_diag]
      , x_var = dat_analy_1$x_var
      , y_var = fitted(seg_fit)
      )


    # plot(y_var ~ x_var, dat = dat_analy_1)
    # plot(seg_fit, add = TRUE)

    # break points:
    #segmented.mod$psi[,"Est."]

  }

  df_fit <- bind_rows(list_fit)

  # testing plot
  #ggplot(df_fit, aes(x = x_var, y = y_var, colour = dx_current)) + geom_line()



  p <- ggplot(dat_analy, aes(x = x_var, y = y_var, colour = dx_current, shape = dx_current, fill = dx_current))  # , linetype = dx_current
  p <- p + theme_bw()
  #p <- p + stat_smooth(linetype = 0, alpha = 0.1) #method = "lm")  # bands
  p <- p + geom_point(alpha = 0.75)
  #p <- p + stat_smooth(se = FALSE  , alpha = 0.75 , size = 1.5) #method = "lm")  # line
  p <- p + geom_line(data = df_fit, aes(x = x_var, y = y_var, colour = dx_current), size = 1.5)

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
