



## Individual diagnostic functions

#' Model diagnostics, gvlma
#'
#'  Methods from the paper: Pena, EA and Slate, EH,
#'  "Global Validation of Linear Model Assumptions,"
#'   J. American Statistical Association, 101(473):341-354, 2006.
#'
#' @param fit     fit object
#' @param dat     dataset data.frame or tibble
#'
#' @return out      list including text and ggplot grobs
#' @import gvlma
#' @import ggplot2
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom patchwork wrap_plots
#'
e_plot_model_diagnostics_gvlma <-
  function(
    fit                 = NULL
  , dat                 = NULL
  ) {

  # set wide witdh so each printed line is correctly on a single line
  op <- options()  # op is a named list
  options(width = 1e3)

  out <- list()

  # gvlma
  #  Methods from the paper: Pena, EA and Slate, EH,
  #  "Global Validation of Linear Model Assumptions,"
  # J. American Statistical Association, 101(473):341-354, 2006.
  out[[ "gvlma_object" ]] <-
    gvlma::gvlma(
      x           = fit
    , data        = dat
    , alphalevel  = 0.05
    )

  out[[ "gvlma_tests_overall_print" ]] <-
    capture.output(
      out[[ "gvlma_object" ]] |>
      gvlma::display.gvlmatests()
    , type = c("output", "message")[1]
    , split = FALSE
    )

  out[[ "gvlma_deletion_object" ]] <-
    out[[ "gvlma_object" ]] |>
    gvlma::deletion.gvlma()

  out[[ "gvlma_tests_deletion_print" ]] <-
    capture.output(
      summary(
        out[[ "gvlma_deletion_object" ]]
      , allstats = TRUE
      )
    , type = c("output", "message")[1]
    , split = FALSE
    )


  out[[ "gvlma_plots_indy" ]] <- list()

  # code from plot.gvlmaDel.R
  gvlmaDelobj <- out[[ "gvlma_deletion_object" ]]
  pointlabels <- rownames(gvlmaDelobj)
  timeseq <- attr(gvlmaDelobj, "timeseq")
  # plot deleted statistics and p-values versus time sequence
  for (w in seq(1, 10, by = 2)) {
    statname <- names(gvlmaDelobj)[w]
    statnm <-
      switch(statname,
      "DeltaGlobalStat" =
      expression(paste("Global: Deleted ", {G[4]}^2, " statistic")), #  (% change)
      "DeltaStat1" =
      expression(paste("Skewness: Deleted ", {S[1]}^2, " statistic")), #  (% change)
      "DeltaStat2" =
      expression(paste("Kurtosis: Deleted ", {S[2]}^2, " statistic")), #  (% change)
      "DeltaStat3" =
      expression(paste("Link function: Deleted ", {S[3]}^2, " statistic")), #  (% change)
      "DeltaStat4" =
      expression(paste("Heteroscedasticity: Deleted ", {S[4]}^2, " statistic")) #  (% change)
      )
    pvalname <- names(gvlmaDelobj[w+1])
    pvalnm <-
      switch(pvalname,
      "GStatpvalue" =
      expression(paste("Global: Deleted ", {G[4]}^2, " p-value")),
      "Stat1pvalue" =
      expression(paste("Skewness: Deleted ", {S[1]}^2, " p-value")),
      "Stat2pvalue" =
      expression(paste("Kurtosis: Deleted ", {S[2]}^2, " p-value")),
      "Stat3pvalue" =
      expression(paste("Link function: Deleted ", {S[3]}^2, " p-value")),
      "Stat4pvalue" =
      expression(paste("Heteroscedasticity: Deleted ", {S[4]}^2, " p-value")),
      )

    label_name <-
      switch(pvalname,
        "GStatpvalue" = "Global"
      , "Stat1pvalue" = "Skewness"
      , "Stat2pvalue" = "Kurtosis"
      , "Stat3pvalue" = "Link function"
      , "Stat4pvalue" = "Heteroscedasticity"
      )
    label_stat <- c("Statistic", "p-value")


    ## base graphics version
    # out[[ "gvlma_plots_indy" ]][[ paste(label_name[ (w+1)/2 ], label_stat[ 1 ], sep = "__") ]] <-
    #   patchwork::wrap_elements(
    #     full =
    #     ~plot(timeseq, gvlmaDelobj[,w], xlab = "Time sequence",
    #        ylab = statnm)
    #   )

    ## ggplot version
    dat_plot_temp <-
      tibble::tibble(
        x = timeseq
      , y = gvlmaDelobj[,w]
      , p = gvlmaDelobj[,w+1]
      ) |>
      dplyr::mutate(
        sig = ifelse(p < 0.05, 1, 0) |> factor(levels = c(0, 1))
      )
    p <- ggplot(dat_plot_temp, aes(x = x, y = y))
    p <- p + theme_bw()
    p <- p + geom_hline(yintercept = 0, colour = "black", linetype = c("none", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")[2], linewidth = 0.3, alpha = 0.5)
    p <- p + geom_point(aes(color = sig))
    p <- p + scale_color_manual(values = c("0" = "black", "1" = "red"))
    p <- p + labs(
                    title     = statnm
                  , x         = "Time sequence"
                  , y         = "Stat (% change)"
                  )
    p <- p + guides(color = "none")
    out[[ "gvlma_plots_indy" ]][[ paste(label_name, label_stat[1], sep = "__") ]] <-
      p



    ## base graphics version
    # out[[ "gvlma_plots_indy" ]][[ paste(label_name[ (w+1)/2 ], label_stat[ 2 ], sep = "__") ]] <-
    #   patchwork::wrap_elements(
    #     full =
    #     ~plot(timeseq, gvlmaDelobj[, w+1], xlab = "Time sequence",
    #        ylab = pvalnm, ylim = c(0,1))
    #   )

    ## ggplot version
    p <- ggplot(dat_plot_temp, aes(x = x, y = p))
    p <- p + theme_bw()
    p <- p + geom_hline(yintercept = 0.00, colour = "black", linetype = c("none", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")[2], linewidth = 0.3, alpha = 0.5)
    p <- p + geom_hline(yintercept = 0.05, colour = "black", linetype = c("none", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")[3], linewidth = 0.3, alpha = 0.5)
    p <- p + geom_point(aes(color = sig))
    p <- p + scale_color_manual(values = c("0" = "black", "1" = "red"))
    p <- p + scale_y_continuous(limits = c(0, 1))
    p <- p + labs(
                    title     = pvalnm
                  , x         = "Time sequence"
                  , y         = "p-value"
                  )
    p <- p + guides(color = "none")
    out[[ "gvlma_plots_indy" ]][[ paste(label_name, label_stat[2], sep = "__") ]] <-
      p

  } # for w


  out[[ "gvlma_plots_grid" ]] <-
    patchwork::wrap_plots(
      out[[ "gvlma_plots_indy" ]]
    , ncol        = 2
    , nrow        = NULL
    , byrow       = c(TRUE, FALSE)[1]
    , widths      = NULL
    , heights     = NULL
    , guides      = c("collect", "keep", "auto")[1]
    , tag_level   = c("keep", "new")[1]
    , design      = NULL
    , axes        = NULL
    , axis_titles = c("keep", "collect", "collect_x", "collect_y")[1]
    ) +
    patchwork::plot_annotation(
      title       = "GVLMA: Global Validation of Linear Model Assumptions"
    #, subtitle    = text_formula_sel
    , tag_levels  = "A"
    )


  # restore original options
  options(op)

  return(out)

} # e_plot_model_diagnostics_gvlma


#' Model diagnostics, car::inverseResponsePlot
#'
#'
#' @param fit     fit object
#'
#' @return out      list including text and ggplot grobs
#' @import car
#' @importFrom patchwork wrap_elements wrap_plots plot_annotation
#' @importFrom ggplot2 theme
#' @importFrom grDevices pdf dev.off
#'
e_plot_model_diagnostics_car__inverseResponsePlot <-
  function(
    fit                 = NULL
  ) {

  out <- list()

  xy_var_names_list <- e_model_extract_var_names(formula(fit$terms))
  y_var_name  <- xy_var_names_list$y_var_name

  grDevices::pdf(NULL) # begin capture and kill plots
  out[[ "car__inverseResponsePlot_table" ]] <-
    car::inverseResponsePlot(
      model   = fit
    , lambda  = c(-1, 0, 1, 2)
    , robust  = FALSE
    , xlab    = NULL
    , id      = FALSE #list(method=list(method="x", n=4, cex=1, col=car::carPalette()[1], location="lr"))  # TRUE  #FALSE
    ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      y_var = y_var_name
    ) |>
    dplyr::relocate(
      y_var
    )

  grDevices::dev.off() # end   capture and kill plots

  p_list <-
    patchwork::wrap_elements(
      full =
      ~
      car::inverseResponsePlot(
        model   = fit
      , lambda  = c(-1, 0, 1, 2)
      , robust  = FALSE
      , xlab    = NULL
      , id      = FALSE #list(method=list(method="x", n=4, cex=1, col=carPalette()[1], location="lr"))  # TRUE  #FALSE
      )
    )

  out[[ "car__inverseResponsePlot_plot" ]] <-
    patchwork::wrap_plots(
      p_list
    , ncol        = NULL
    , nrow        = NULL
    , byrow       = c(TRUE, FALSE)[1]
    , widths      = NULL
    , heights     = NULL
    , guides      = c("collect", "keep", "auto")[1]
    , tag_level   = c("keep", "new")[1]
    , design      = NULL
    , axes        = NULL
    , axis_titles = c("keep", "collect", "collect_x", "collect_y")[1]
    ) +
    patchwork::plot_annotation(
      title       = paste0("Inverse Response Plot to Transform the Response y")
    #, subtitle    = text_formula_sel
    , caption     = paste0(
                      "Observations with missing values have been removed."
                    )
    #, tag_levels  = "A"
    , theme = ggplot2::theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1, Caption align left
    )

  return(out)

} # e_plot_model_diagnostics_car__inverseResponsePlot


#' Model diagnostics, car::invTranPlot
#'
#'
#' @param fit     fit object
#' @param dat     dataset data.frame or tibble
#'
#' @return out      list including text and ggplot grobs
#' @import car
#' @importFrom dplyr select mutate
#' @importFrom tidyselect one_of where
#' @importFrom tidyr drop_na
#' @importFrom cowplot as_grob
#' @importFrom patchwork wrap_elements wrap_plots plot_annotation
#' @importFrom ggplot2 theme
#'
e_plot_model_diagnostics_car__invTranPlot <-
  function(
    fit                 = NULL
  , dat                 = NULL
  ) {

  out <- list()

  xy_var_names_list <- e_model_extract_var_names(formula(fit$terms))
  y_var_name  <- xy_var_names_list$y_var_name
  x_var_names <- xy_var_names_list$x_var_names

  # remove factor variables
  dat <-
    dat |>
    dplyr::select(
      tidyselect::one_of(y_var_name)
    , tidyselect::one_of(x_var_names)
    ) |>
    tidyr::drop_na() |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.factor   ), ~NULL)) |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~NULL))

  x_var_names <-
    x_var_names[x_var_names %in% names(dat)]


  ## Tables and plots of y vs each x
  t_list <- list()
  p_list <- list()
  for (i_covar in seq_along(x_var_names)) {
    ## i_covar = 1

    t_list[[ i_covar ]] <-
      car::invTranEstimate(
        x           = dat[[ x_var_names[i_covar] ]]
      , y           = dat[[ y_var_name ]]
      , family      = c("bcPower", "yjPower")[1]
      , confidence  = 0.95
      , robust      = FALSE
      ) |>
      tibble::as_tibble() |>
      dplyr::mutate(
        x_var = x_var_names[i_covar]
      ) |>
      dplyr::relocate(
        x_var
      )

  } # i_covar


  p_list <-
    cowplot::as_grob(
      ~
      {
        par(mfrow = c(ceiling(length(x_var_names) / 2), 2))  # Set up a k x 2 grid
        lapply(
          x_var_names
        , \(x)
          car::invTranPlot(
            x           = dat[[ x ]]
          , y           = dat[[ y_var_name ]]
          , lambda      = c(-1, 0, 1, 2)
          , robust      = FALSE
          , family      = c("bcPower", "yjPower")[1]
          , xlab        = labelled::var_label(dat[[ x ]])
          , ylab        = labelled::var_label(dat[[ y_var_name ]])
          , main        = paste0("X power transform: ", y_var_name, " ~ ", x)
          )
        )
      }
    )

  p_arranged <-
    patchwork::wrap_plots(
      p_list
    , ncol        = NULL
    , nrow        = NULL
    , byrow       = c(TRUE, FALSE)[1]
    , widths      = NULL
    , heights     = NULL
    , guides      = c("collect", "keep", "auto")[1]
    , tag_level   = c("keep", "new")[1]
    , design      = NULL
    , axes        = NULL
    , axis_titles = c("keep", "collect", "collect_x", "collect_y")[1]
    ) +
    patchwork::plot_annotation(
      title       = paste0("Predictor transforms for each x, y ~ x")
    , caption     = paste0(
                      "Observations with missing values have been removed."
                    )
    , theme = ggplot2::theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1, Caption align left
    )


  out[[ "car__invTranPlot_table" ]] <-
    t_list |>
    dplyr::bind_rows()

  out[[ "car__invTranPlot_plot" ]] <-
    p_arranged

  return(out)

} # e_plot_model_diagnostics_car__invTranPlot



#' Model diagnostics, resid vs x vars, car::residualPlots
#'
#'
#' @param fit                fit object
#' @param resid_type         Specifies the type of residual to be plotted.  Any of \code{c("working", "response", "deviance", "pearson", "partial", "rstudent", "rstandard")} may be specified.  The default \code{resid_type = "pearson"} is usually appropriate, since it is equal to ordinary residuals observed minus fit with ols, and correctly weighted residuals with wls or for a glm.  The last two options use the \code{\link{rstudent}} and \code{\link{rstandard}} functions and use studentized or standardized residuals.
#'
#' @return out      list including text and ggplot grobs
#' @import car
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom cowplot as_grob
#' @importFrom patchwork wrap_plots
#'
e_plot_model_diagnostics_car__residualPlots <-
  function(
    fit                 = NULL
  , resid_type          = c("working", "response", "deviance", "pearson", "partial", "rstudent", "rstandard")[6]
  ) {

  out <- list()

  xy_var_names_list <- e_model_extract_var_names(formula(fit$terms))
  #y_var_name  <- xy_var_names_list$y_var_name
  x_var_names <- xy_var_names_list$x_var_names

  # test table
  capture.output(
    out[[ "car__residualPlots_table" ]] <-
      car::residualPlots(
        model   = fit
      , terms   = ~ .
      , layout  = c(ceiling(length(x_var_names) / 3), 3)
      , ask     = FALSE
      , main    = paste0("Residual plots, type = ", resid_type)
      , fitted  = FALSE
      , AsIs    = TRUE
      , plot    = FALSE
      , tests   = TRUE
      , type    = resid_type
      ) |>
      tibble::as_tibble(
        rownames = "Var"
      ) |>
      dplyr::mutate(
        sig = `Pr(>|Test stat|)` |> e_pval_stars()
      )
  , type = c("output", "message")[1]
  , split = FALSE
  )

  # plots
  p_list <-
    cowplot::as_grob(
      ~
      {
      car::residualPlots(
        model   = fit
      , terms   = ~ .
      , layout  = c(ceiling(length(x_var_names) / 3), 3)
      , ask     = FALSE
      , main    = paste0("Residual plots, type = ", resid_type)
      , fitted  = FALSE
      , AsIs    = TRUE
      , plot    = TRUE
      , tests   = FALSE
      , type    = resid_type
      )
      }
    )

  p_arranged <-
    patchwork::wrap_plots(
      p_list
    , ncol        = NULL
    , nrow        = NULL
    , byrow       = c(TRUE, FALSE)[1]
    , widths      = NULL
    , heights     = NULL
    , guides      = c("collect", "keep", "auto")[1]
    , tag_level   = c("keep", "new")[1]
    , design      = NULL
    , axes        = NULL
    , axis_titles = c("keep", "collect", "collect_x", "collect_y")[1]
    )
    # +
    #patchwork::plot_annotation(
    #  title       = paste0("Predictor transforms for each x, y ~ x")
    #, caption     = paste0(
    #                  "Observations with missing values have been removed."
    #                )
    #, theme = ggplot2::theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1, Caption align left
    #)


  out[[ "car__residualPlots_plot" ]] <-
    p_arranged

  return(out)

} # e_plot_model_diagnostics_car__residualPlots


#' Model diagnostics, dfbetas, car::dfbetasPlots
#'
#'
#' @param fit                fit object
#'
#' @return out      list including text and ggplot grobs
#' @import car
#' @importFrom cowplot as_grob
#' @importFrom patchwork wrap_plots
#'
e_plot_model_diagnostics_car__dfbetasPlots <-
  function(
    fit                 = NULL
  ) {

  out <- list()

  # plots
  p_list <-
    cowplot::as_grob(
      ~
      {
      car::dfbetaPlots(
        model       = fit
      , terms       = ~ .
      , intercept   = FALSE
      # , layout=NULL
      , ask         = FALSE
      # , main
      # , xlab
      # , ylab
      # , labels=rownames(dfbeta)
      , id.method   = "y"
      , id.n        = 4  #if(id.method[1]=="identify") Inf else 0
      # , id.cex=1
      , id.col=car::carPalette()[1]
      , id.location = c("lr", "ab", "avoid")[3]
      , col         = car::carPalette()[1]
      , grid        = TRUE
      )
      }
    )

  p_arranged <-
    patchwork::wrap_plots(
      p_list
    , ncol        = NULL
    , nrow        = NULL
    , byrow       = c(TRUE, FALSE)[1]
    , widths      = NULL
    , heights     = NULL
    , guides      = c("collect", "keep", "auto")[1]
    , tag_level   = c("keep", "new")[1]
    , design      = NULL
    , axes        = NULL
    , axis_titles = c("keep", "collect", "collect_x", "collect_y")[1]
    )
    # +
    #patchwork::plot_annotation(
    #  title       = paste0("Predictor transforms for each x, y ~ x")
    #, caption     = paste0(
    #                  "Observations with missing values have been removed."
    #                )
    #, theme = ggplot2::theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1, Caption align left
    #)


  out[[ "car__dfbetasPlots_plot" ]] <-
    p_arranged

  return(out)

} # e_plot_model_diagnostics_car__dfbetasPlots




#' Model diagnostics, Bonferroni outlier test, car::outlierTest
#'
#'
#' @param fit                fit object
#'
#' @return out      list including text and ggplot grobs
#' @import car
#'
e_plot_model_diagnostics_car__outlierTest <-
  function(
    fit                 = NULL
  ) {

  # set wide witdh so each printed line is correctly on a single line
  op <- options()  # op is a named list
  options(width = 1e3)

  out <- list()

  # test table
  out[[ "car__outlierTest_table" ]] <-
    capture.output(
      car::outlierTest(
        model   = fit
      , cutoff  = 0.05
      , n.max   = 10
      , order   = TRUE
      #, labels  = names(rstudent)
      )
    , type = c("output", "message")[1]
    , split = FALSE
    )

  # restore original options
  options(op)

  return(out)

} # e_plot_model_diagnostics_car__outlierTest



#' Model diagnostics, Influence index plot, car::infIndexPlot
#'
#'
#' @param fit                fit object
#'
#' @return out      list including text and ggplot grobs
#' @import car
#' @importFrom cowplot as_grob
#' @importFrom patchwork wrap_plots
#'
e_plot_model_diagnostics_car__infIndexPlot <-
  function(
    fit                 = NULL
  ) {

  out <- list()

  # plots
  p_list <-
    cowplot::as_grob(
      ~
      {
      car::infIndexPlot(
        model       = fit
      , vars        = c("Cook", "Studentized", "Bonf", "hat")
      , id          = list(method="y", n=4, cex=1, col=car::carPalette()[1], location=c("lr", "ab", "avoid")[3]) # TRUE
      , grid        = TRUE
      , main        = "Influence Index Plot"
      )
      }
    )

  p_arranged <-
    patchwork::wrap_plots(
      p_list
    , ncol        = NULL
    , nrow        = NULL
    , byrow       = c(TRUE, FALSE)[1]
    , widths      = NULL
    , heights     = NULL
    , guides      = c("collect", "keep", "auto")[1]
    , tag_level   = c("keep", "new")[1]
    , design      = NULL
    , axes        = NULL
    , axis_titles = c("keep", "collect", "collect_x", "collect_y")[1]
    )
    # +
    #patchwork::plot_annotation(
    #  title       = paste0("Predictor transforms for each x, y ~ x")
    #, caption     = paste0(
    #                  "Observations with missing values have been removed."
    #                )
    #, theme = ggplot2::theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1, Caption align left
    #)


  out[[ "car__infIndexPlot_plot" ]] <-
    p_arranged

  return(out)

} # e_plot_model_diagnostics_car__infIndexPlot



#' Model diagnostics, Quantile-Comparison Plot, car::qqPlot
#'
#'
#' @param fit     fit object
#' @param dat     dataset data.frame or tibble
#'
#' @return out      list including text and ggplot grobs
#' @import car
#' @importFrom cowplot as_grob
#' @importFrom patchwork wrap_elements wrap_plots plot_annotation
#' @importFrom ggplot2 theme
#'
e_plot_model_diagnostics_car__qqPlot <-
  function(
    fit                 = NULL
  , dat                 = NULL
  ) {

  out <- list()

  # plots
  capture.output(
  p_list <-
    cowplot::as_grob(
      ~
      {
      car::qqPlot(
        x             = fit
      , data          = dat
      #, xlab          = paste(distribution, "Quantiles")
      #, ylab          = paste("Studentized Residuals(", deparse(substitute(x)), ")", sep                   = "")
      #, main          = "QQ-plot"
      #, distribution  = c("t", "norm")[1]
      , line          = c("robust", "quartiles", "none")[1]
      #, las           = par("las")
      , simulate      = TRUE
      , envelope      = 0.95 #TRUE
      , reps          = 1e3
      #, col           = car::carPalette()[1]
      #, col.lines     = car::carPalette()[2]
      #, lwd           = 2
      #, pch           = 1
      #, cex           = par("cex")
      , id            = list(method="y", n=4, cex=1, col=car::carPalette()[1], location=c("lr", "ab", "avoid")[3]) #TRUE
      , grid          = TRUE
      )
      }
    )
  , type = c("output", "message")[1]
  , split = FALSE
  )

  p_arranged <-
    patchwork::wrap_plots(
      p_list
    , ncol        = NULL
    , nrow        = NULL
    , byrow       = c(TRUE, FALSE)[1]
    , widths      = NULL
    , heights     = NULL
    , guides      = c("collect", "keep", "auto")[1]
    , tag_level   = c("keep", "new")[1]
    , design      = NULL
    , axes        = NULL
    , axis_titles = c("keep", "collect", "collect_x", "collect_y")[1]
    ) +
    patchwork::plot_annotation(
      title       = paste0("QQ-plot, Quantile-Comparison Plot")
    , caption     = paste0(
                      "Observations with missing values have been removed."
                    )
    , theme = ggplot2::theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1, Caption align left
    )


  out[[ "car__qqPlot_plot" ]] <-
    p_arranged

  return(out)

} # e_plot_model_diagnostics_car__qqPlot




#' Model diagnostics, qqplot, qqplotr:: stat_* functions
#'
#'
#' @param fit_resid   list of residuals from \code{e_model_calc_resid()}
#'
#' @return out      list including ggplot grobs, one normal, one detrended
#' @import ggplot2
#' @import qqplotr
#' @importFrom tibble as_tibble tribble
#' @importFrom dplyr rename pull
#' @importFrom patchwork wrap_elements wrap_plots plot_annotation
#' @importFrom ggplot2 theme
#'
e_plot_model_diagnostics_qqplotr <-
  function(
    fit_resid           = fit_resid
  , fit                 = NULL
  , band_conf           = 0.95
  ) {

  out <- list()

  resid_type <- attr(fit_resid, "resid_type")
  param_model <- list()
  # 7/4/2025 not sure if all of these are normal, but good enough for now
  if (is.null(fit) | resid_type %in% c(NA, "pearson", "response", "deviance", "stand.deviance", "stand.pearson", "partial")) {
    param_model[[ "dist"   ]] <- "norm"
    param_model[[ "dparam" ]] <- list()
    param_model[[ "label"  ]] <- "normal"
  }
  if (!is.null(fit)) {
    if (resid_type %in% c("standardized")) {
      param_model[[ "dist"   ]] <- "t"
      param_model[[ "dparam" ]] <- list(df = as.numeric(summary(fit)$fstatistic["dendf"] - 1)) # n - p - 2
      param_model[[ "label"  ]] <- paste0("t(n - p - 2 = ", param_model[[ "dparam" ]], ")")
    }
    if (resid_type %in% c("studentized")) {
      param_model[[ "dist"   ]] <- "t"
      param_model[[ "dparam" ]] <- list(df = as.numeric(summary(fit)$fstatistic["dendf"])) # n - p - 1
      param_model[[ "label"  ]] <- paste0("t(n - p - 1 = ", param_model[[ "dparam" ]], ")")
    }
  }


  dat_resid <-
    fit_resid |>
    tibble::as_tibble(
      rownames = "Obs"
    ) |>
    dplyr::rename(
      resid = value
    )

  p1 <- ggplot(data = dat_resid, mapping = aes(sample = resid))
  p1 <- p1 + theme_bw()
  #p1 <- p1 + geom_qq_band(bandType = "ks", mapping = aes(fill = "KS"), alpha = 0.25)
  #p1 <- p1 + geom_qq_band(bandType = "ts", mapping = aes(fill = "TS"), alpha = 0.25)
  #p1 <- p1 + geom_qq_band(bandType = "pointwise", mapping = aes(fill = "Normal"), alpha = 0.25)
  #p1 <- p1 + geom_qq_band(bandType = "boot", mapping = aes(fill = "Bootstrap"), alpha = 0.25)
  p1 <- p1 + stat_qq_band (distribution = param_model[[ "dist"   ]], dparams = param_model[[ "dparam" ]], detrend = c(FALSE, TRUE)[1]
              , conf = band_conf, bandType = c("pointwise", "boot", "ks", "ts", "ell")[1]
              , fill = "gray75", color = "gray25", alpha = 0.5)
  p1 <- p1 + stat_qq_line (distribution = param_model[[ "dist"   ]], dparams = param_model[[ "dparam" ]], detrend = c(FALSE, TRUE)[1])
  p1 <- p1 + stat_qq_point(distribution = param_model[[ "dist"   ]], dparams = param_model[[ "dparam" ]], detrend = c(FALSE, TRUE)[1])
  p1 <- p1 + labs(
              title = paste0("QQ-plot, pointwise distributional ", 100*band_conf, "%")
            , x     = paste0("Theoretical Quantiles\n", param_model[[ "label"  ]], " distribution")
            , y     = paste0("Sample Quantiles\n(residual type = ", resid_type, ")")
            )
  #p1 <- p1 + scale_fill_discrete("Bandtype")
  #p1


  p2 <- ggplot(data = dat_resid, mapping = aes(sample = resid))
  p2 <- p2 + theme_bw()
  p2 <- p2 + stat_qq_band (distribution = param_model[[ "dist"   ]], dparams = param_model[[ "dparam" ]], detrend = c(FALSE, TRUE)[2]
              , conf = band_conf, bandType = c("pointwise", "boot", "ks", "ts", "ell")[1]
              , fill = "gray75", color = "gray25", alpha = 0.5)
  p2 <- p2 + stat_qq_line (distribution = param_model[[ "dist"   ]], dparams = param_model[[ "dparam" ]], detrend = c(FALSE, TRUE)[2])
  p2 <- p2 + stat_qq_point(distribution = param_model[[ "dist"   ]], dparams = param_model[[ "dparam" ]], detrend = c(FALSE, TRUE)[2])
  p2 <- p2 + labs(
              title = paste0("QQ-plot, pointwise distributional ", 100*band_conf, "%", ", detrended")
            , x     = paste0("Theoretical Quantiles\n", param_model[[ "label"  ]], " distribution")
            , y     = paste0("Sample Quantiles\n(residual type = ", resid_type, ")")
            )
  #p2

  p3 <- ggplot(data = dat_resid, mapping = aes(sample = resid))
  p3 <- p3 + theme_bw()
  p3 <- p3 + stat_pp_band (distribution = param_model[[ "dist"   ]], dparams = param_model[[ "dparam" ]], detrend = c(FALSE, TRUE)[1]
              , conf = band_conf, B = 5e3, bandType = c("pointwise", "boot", "ks", "ts", "ell")[2]
              , fill = "gray75", color = "gray25", alpha = 0.5)
  p3 <- p3 + stat_pp_line (distribution = param_model[[ "dist"   ]], dparams = param_model[[ "dparam" ]], detrend = c(FALSE, TRUE)[1])
  p3 <- p3 + stat_pp_point(distribution = param_model[[ "dist"   ]], dparams = param_model[[ "dparam" ]], detrend = c(FALSE, TRUE)[1])
  p3 <- p3 + labs(
              title = paste0("PP-plot, pointwise bootstrap ", 100*band_conf, "%")
            , x     = paste0("Probability Points\n", param_model[[ "label"  ]], " distribution")
            , y     = paste0("Cumulative Probability\n(residual type = ", resid_type, ")")
            )
  #p3


  p4 <- ggplot(data = dat_resid, mapping = aes(sample = resid))
  p4 <- p4 + theme_bw()
  p4 <- p4 + stat_pp_band (distribution = param_model[[ "dist"   ]], dparams = param_model[[ "dparam" ]], detrend = c(FALSE, TRUE)[2]
              , conf = band_conf, B = 5e3, bandType = c("pointwise", "boot", "ks", "ts", "ell")[2]
              , fill = "gray75", color = "gray25", alpha = 0.5)
  p4 <- p4 + stat_pp_line (distribution = param_model[[ "dist"   ]], dparams = param_model[[ "dparam" ]], detrend = c(FALSE, TRUE)[2])
  p4 <- p4 + stat_pp_point(distribution = param_model[[ "dist"   ]], dparams = param_model[[ "dparam" ]], detrend = c(FALSE, TRUE)[2])
  p4 <- p4 + labs(
              title = paste0("PP-plot, pointwise bootstrap ", 100*band_conf, "%", ", detrended")
            , x     = paste0("Probability Points\n", param_model[[ "label"  ]], " distribution")
            , y     = paste0("Cumulative Probability\n(residual type = ", resid_type, ")")
            )
  #p4


  out[[ "qqplotr_qqplot_diagonal_plot"  ]] <- p1
  out[[ "qqplotr_qqplot_detrended_plot" ]] <- p2
  out[[ "qqplotr_ppplot_diagonal_plot"  ]] <- p3
  out[[ "qqplotr_ppplot_detrended_plot" ]] <- p4


  # normality test
  out[[ "normality_test_table" ]] <- e_distr_test(fit_resid)

  text_caption <-
    paste0(
      "Normality tests:  "
    , out[[ "normality_test_table" ]] |>
      dplyr::pull(text) |>
      paste(collapse = ";  ")
    )

  # diagonal and detrended grids with normality test in caption
  p_arranged_1 <-
    patchwork::wrap_plots(
      list(p1, p3)
    , ncol        = 1
    , nrow        = NULL
    , byrow       = c(TRUE, FALSE)[1]
    , widths      = NULL
    , heights     = NULL
    , guides      = c("collect", "keep", "auto")[1]
    , tag_level   = c("keep", "new")[1]
    , design      = NULL
    , axes        = NULL
    , axis_titles = c("keep", "collect", "collect_x", "collect_y")[1]
    ) +
    patchwork::plot_annotation(
    #  title       = text_formula
    #, subtitle    = text_formula_sel
    , caption     = text_caption
    , tag_levels  = "A"
    , theme = theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1, Caption align left
    )

  p_arranged_2 <-
    patchwork::wrap_plots(
      list(p2, p4)
    , ncol        = 1
    , nrow        = NULL
    , byrow       = c(TRUE, FALSE)[1]
    , widths      = NULL
    , heights     = NULL
    , guides      = c("collect", "keep", "auto")[1]
    , tag_level   = c("keep", "new")[1]
    , design      = NULL
    , axes        = NULL
    , axis_titles = c("keep", "collect", "collect_x", "collect_y")[1]
    ) +
    patchwork::plot_annotation(
    #  title       = text_formula
    #, subtitle    = text_formula_sel
    , caption     = text_caption
    , tag_levels  = "A"
    , theme = theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1, Caption align left
    )

  out[[ "qqplotr_grid_diagonal_plot"  ]] <- p_arranged_1
  out[[ "qqplotr_grid_detrended_plot" ]] <- p_arranged_2

  return(out)

} # e_plot_model_diagnostics_qqplotr




#' Model diagnostics, car::boxCox and car::symbox
#'
#'
#' @param fit     fit object
#'
#' @return out      list including text and ggplot grobs
#' @import car
#' @importFrom patchwork wrap_elements wrap_plots plot_annotation
#' @importFrom ggplot2 theme
#' @importFrom rlang expr
#' @importFrom dplyr mutate
#' @importFrom tibble as_tibble
#'
e_plot_model_diagnostics_car__boxCox <-
  function(
    fit                 = NULL
  ) {

  out <- list()

  # Box-Cox results and tests
  fit_boxcox <-
    fit |>
    car::powerTransform() |>
    summary()

  # reformat into tables
  fit_boxcox$result <-
    fit_boxcox$result |>
    tibble::as_tibble()
  fit_boxcox$tests <-
    fit_boxcox$tests |>
    tibble::as_tibble(rownames = "label") |>
    dplyr::mutate(
      pval = pval |> as.numeric()
    , sig  = pval |> e_pval_stars()
    , text = paste0(label, " (p = ", sprintf("%04.4f", pval), ") ", sig)
    )



  text_caption <-
    paste0(
      #fit_boxcox$label
      "Box-Cox Power Transformation to Normality"
    , "\n"
    , "  lambda:  "
    , "Est Power = "
    , sprintf("%03.2f", fit_boxcox$result[[ "Est Power"    ]])
    #, fit_boxcox$result[[ "Rounded Pwr"  ]]
    , ", "
    , "Wald 95% CI ("
    , sprintf("%03.2f", fit_boxcox$result[[ "Wald Lwr Bnd" ]])
    , ", "
    , sprintf("%03.2f", fit_boxcox$result[[ "Wald Upr Bnd" ]])
    , ")."
    , "\n"
    , "Likelihood Ratio Tests for lambda = 0 (log) and 1 (none):"
    , "\n"
    , paste0(
        "  "
      , fit_boxcox$tests$text
      , collapse = "\n"
      )
    )

  p1 <-
    patchwork::wrap_elements(
      full =
      ~
      {
      car::boxCox(
          fit
        , main = expression(paste("Box-Cox (", y^lambda, "), 95% CI"))  #  to maximize normality of residuals
        #, xlab = expression(lambda) #paste(lambda, " of 1 is none (", y^1, "); 0 is ", log(y), " of any base"))
        )
      abline(v = 1, col = "orange", lty = 2, lwd = 2)
      }
    )

  p2 <-
    patchwork::wrap_elements(
      full =
      ~
      {
      car::symbox(
        fit
      , powers  = seq(-2, +2, by = 0.5)
      , main    = expression(paste("Boxplots after ", y^lambda))
      )
      #abline(v = "1", col = "orange", lty = 2, lwd = 2)  # doesn't work on boxplot
      }
    )


  out[[ "car__boxCox_table" ]] <-
    fit_boxcox

  out[[ "car__boxCox_plot" ]] <-
    patchwork::wrap_plots(
      list(p1, p2)
    , ncol        = NULL
    , nrow        = 1
    , byrow       = c(TRUE, FALSE)[1]
    , widths      = c(3, 3)
    , heights     = NULL
    , guides      = c("collect", "keep", "auto")[1]
    , tag_level   = c("keep", "new")[1]
    , design      = NULL
    , axes        = NULL
    , axis_titles = c("keep", "collect", "collect_x", "collect_y")[1]
    ) +
    patchwork::plot_annotation(
      title       = expression(paste("Box-Cox transformation (", y^lambda, ") to maximize normality of residuals"))  #
    #, subtitle    = text_formula_sel
    , caption     = text_caption
                    # rlang::expr(paste(
                    #   "Optimal ", lambda, " = ", !!fit_boxcox_lambda, ".  "
                    # , lambda, " = 1 is no transformation (", y^1, "), denoted by orange line.  "
                    # , lambda, " = 0 is defined as ", log(y), " of any positive base."
                    # ))
                    #expression(paste(
                    #  "Optimal ", lambda, " = ", fit_boxcox_lambda, ".  "
                    #, lambda, " of 1 is none (", y^1
                    #, "); 0 is ", log(y), " of any base"
                    #))
    , tag_levels  = "A"
    , theme = ggplot2::theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1, Caption align left
    )

  return(out)

} # e_plot_model_diagnostics_car__boxCox



#' Model diagnostics, car::spreadLevelPlot and car::ncvTest
#'
#'
#' @param fit     fit object
#'
#' @return out      list including text and ggplot grobs
#' @import car
#' @importFrom patchwork wrap_elements wrap_plots plot_annotation
#' @importFrom ggplot2 theme
#'
e_plot_model_diagnostics_car__spreadLevelPlot <-
  function(
    fit                 = NULL
  ) {

  out <- list()

  # Breusch-Pagan (Score) test for non-constant error variance
  fit_ncvTest <-
    fit |>
    car::ncvTest()

  text_caption <-
    paste0(
      #fit_ncvTest$label
      "Breusch-Pagan (Score) test for non-constant error variance"
    , "\n"
    , "X2 = "   , sprintf("%03.2f", fit_ncvTest$ChiSquare)
    , ", df = " , sprintf("%d"    , fit_ncvTest$Df)
    , " (p = "  , sprintf("%04.4f", fit_ncvTest$p), ") "
    , fit_ncvTest$p |> e_pval_stars()
    )

  p <-
    patchwork::wrap_elements(
      full =
      ~
      {
      car::spreadLevelPlot(
        x           = fit
      , robust.line = TRUE
      , xlab        = "Fitted Values (log-scale)"
      , ylab        = "Absolute Studentized Residuals (log-scale)"
      , las         = par("las")
      , main        = "Spread-Level Plot" #paste0("Breusch-Pagan (Score) test for \nnon-constant error variance") #NULL #paste("Spread-Level Plot for\n", deparse(substitute(x)))
      , pch         = 1
      , col         = car::carPalette()[1]
      , col.lines   = car::carPalette()[2:3]
      , lwd         = 2
      , grid        = TRUE
      , id          = list(method=list("x", "y"), n=2, cex=1, col=car::carPalette()[1], location="lr")
      , smooth      = TRUE
      )
      }
    )

  out[[ "car__spreadLevelPlot_table" ]] <-
    text_caption

  out[[ "car__spreadLevelPlot_plot" ]] <-
    patchwork::wrap_plots(
      p
    , ncol        = NULL
    , nrow        = NULL
    , byrow       = c(TRUE, FALSE)[1]
    , widths      = NULL
    , heights     = NULL
    , guides      = c("collect", "keep", "auto")[1]
    , tag_level   = c("keep", "new")[1]
    , design      = NULL
    , axes        = NULL
    , axis_titles = c("keep", "collect", "collect_x", "collect_y")[1]
    ) +
    patchwork::plot_annotation(
    #  title       = "Breusch-Pagan (Score) test"
    #, subtitle    = "for non-constant error variance"
    , caption     = text_caption
    #, tag_levels  = "A"
    , theme = ggplot2::theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1, Caption align left
    )

  return(out)

} # e_plot_model_diagnostics_car__spreadLevelPlot


#' Model diagnostics, car::vif
#'
#'
#' @param fit     fit object
#'
#' @return out      list including text grobs
#' @import car
#' @importFrom patchwork wrap_elements wrap_plots plot_annotation
#' @importFrom ggplot2 theme
#' @importFrom rlang expr
#' @importFrom dplyr mutate
#' @importFrom tibble as_tibble
#' @importFrom labelled var_label
#'
e_plot_model_diagnostics_car__vif <-
  function(
    fit                 = NULL
  ) {

  out <- list()

  fit_class <- class(fit)[1]

  if (fit_class == "lm") {
    fit_vif <-
      fit |>
      car::vif(
        type = c("terms", "predictor")[2]
      ) |>
      tibble::as_tibble(
        rownames = "Var"
      )

    labelled::var_label(fit_vif$GVIF) <- "GVIFs computed for predictors, see ?car::vif"

  } # lm
  if (fit_class == "glm") {
    fit_vif <-
      fit |>
      car::vif() |>
      tibble::as_tibble(
        rownames = "Var"
      )

  } # glm

  # label magnitude stars
  fit_vif <-
    fit_vif |>
    dplyr::mutate(
      Worry =
        `GVIF^(1/(2*Df))` |>
        e_pval_stars(
          cutpoints = rev(c(1e99, 100, 50, 20, 10, 5, 0))
        , symbols = rev(c("****", "***", "**", "*", "-", " "))
        )
    )

  labelled::var_label(fit_vif$Worry) <- "Symbols based on being greater than arbitrary thresholds; stars start at `GVIF^(1/(2*Df))` > 10"


  out[[ "car__vif_table" ]] <-
    fit_vif

  return(out)

} # e_plot_model_diagnostics_car__vif




#' Model diagnostics, Added-variable plots, car::avPlots
#'
#'
#' @param fit                fit object
#'
#' @return out      list including text and ggplot grobs
#' @import car
#' @importFrom cowplot as_grob
#' @importFrom patchwork wrap_plots plot_annotation
#' @importFrom ggplot2 theme
#'
e_plot_model_diagnostics_car__avPlots <-
  function(
    fit                 = NULL
  , sw_avplot_main_only = c(TRUE, FALSE)[1]
  ) {

  out <- list()

  fit_class <- class(fit)[1]

  xy_var_names_list <- e_model_extract_var_names(formula(fit$terms))
  #y_var_name  <- xy_var_names_list$y_var_name
  x_var_names <- xy_var_names_list$x_var_names

  if (sw_avplot_main_only) {
    form_terms <-
      paste0(
        " ~ "
      , paste(
          x_var_names
        , collapse = " + "
        )
      ) |>
      as.formula()

    this_title <-
      "Added-Variable Plots, only Main effects"
  } else {
    form_terms <-
      " ~. " |>
      as.formula()

    this_title <-
      "Added-Variable Plots"
  }


  # plots
  if (fit_class == "lm") {
    p_list <-
      cowplot::as_grob(
        ~
        {
        car::avPlots(
          model           = fit
        , terms           = form_terms
        , id              = TRUE
        , col             = car::carPalette()[1]
        , col.lines       = car::carPalette()[2]
        , pch             = 1
        , lwd             = 2
        , cex             = par("cex")
        , pt.wts          = FALSE
        , main            = this_title
        , grid            = TRUE
        , ellipse         = list(levels=c(0.50), robust=TRUE)
        , marginal.scale  = TRUE
        #, type            = c("Wang", "Weisberg")[2]  # glm
        )
        }
      )
  } # lm

  if (fit_class == "glm") {
    p_list <-
      cowplot::as_grob(
        ~
        {
        car::avPlots(
          model           = fit
        , terms           = form_terms
        , id              = TRUE
        , col             = car::carPalette()[1]
        , col.lines       = car::carPalette()[2]
        , pch             = 1
        , lwd             = 2
        , cex             = par("cex")
        , pt.wts          = FALSE
        , main            = this_title
        , grid            = TRUE
        , ellipse         = list(levels = c(0.50), robust = TRUE)
        , marginal.scale  = TRUE
        , type            = c("Wang", "Weisberg")[2]  # glm  [1] seems very bad!
        )
        }
      )
  } # glm

  p_arranged <-
    patchwork::wrap_plots(
      p_list
    , ncol        = NULL
    , nrow        = NULL
    , byrow       = c(TRUE, FALSE)[1]
    , widths      = NULL
    , heights     = NULL
    , guides      = c("collect", "keep", "auto")[1]
    , tag_level   = c("keep", "new")[1]
    , design      = NULL
    , axes        = NULL
    , axis_titles = c("keep", "collect", "collect_x", "collect_y")[1]
    ) +
    patchwork::plot_annotation(
    #  title       = paste0("Predictor transforms for each x, y ~ x")
      caption     = paste0(
                      "Observations with missing values have been removed."
                    , "\nEllipse is robust 50% bivariate-normal probability-contour."
                    , "\nX-axis on marginal scale; if X and 'others' are highly correlated,"
                    , "\n  the points will be concentrated on the horizontal middle of the plot."
                    , ifelse(fit_class == "glm", "\nGLM AV method of Cook and Weisberg (1999).", "")
                    )
    , theme = ggplot2::theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1, Caption align left
    )


  out[[ "car__avPlots_plot" ]] <-
    p_arranged

  return(out)

} # e_plot_model_diagnostics_car__avPlots


#' Model diagnostics, Marginal and Conditional Plots, car::mcPlots
#'
#'
#' @param fit                fit object
#'
#' @return out      list including text and ggplot grobs
#' @import car
#' @importFrom cowplot as_grob
#' @importFrom patchwork wrap_plots plot_annotation
#' @importFrom ggplot2 theme
#'
e_plot_model_diagnostics_car__mcPlots <-
  function(
    fit                 = NULL
  , sw_avplot_main_only = c(TRUE, FALSE)[1]
  ) {

  out <- list()

  fit_class <- class(fit)[1]

  xy_var_names_list <- e_model_extract_var_names(formula(fit$terms))
  #y_var_name  <- xy_var_names_list$y_var_name
  x_var_names <- xy_var_names_list$x_var_names

  if (sw_avplot_main_only) {
    form_terms <-
      paste0(
        " ~ "
      , paste(
          x_var_names
        , collapse = " + "
        )
      ) |>
      as.formula()

    this_title <-
      "Marginal and Conditional Plots, only Main effects"
  } else {
    form_terms <-
      " ~. " |>
      as.formula()

    this_title <-
      "Marginal and Conditional Plots"
  }


  # plots
  p_list <-
    cowplot::as_grob(
      ~
      {
      car::mcPlots(
        model           = fit
      , terms           = form_terms
      , id              = TRUE
      , col.marginal    = car::carPalette()[2]
      , col.conditional = car::carPalette()[3]
      , col.arrows      = "gray"
      , pch             = c(16, 16) # c(16, 1)
      , cex             = par("cex")
      , pt.wts          = FALSE
      , lwd             = 2
      , grid            = TRUE
      , ellipse         = list(levels = c(0.5), robust = TRUE)
      , overlaid        = TRUE
      #, new             = TRUE
      , title           = FALSE #TRUE
      )
      }
    )


  p_arranged <-
    patchwork::wrap_plots(
      p_list
    , ncol        = NULL
    , nrow        = NULL
    , byrow       = c(TRUE, FALSE)[1]
    , widths      = NULL
    , heights     = NULL
    , guides      = c("collect", "keep", "auto")[1]
    , tag_level   = c("keep", "new")[1]
    , design      = NULL
    , axes        = NULL
    , axis_titles = c("keep", "collect", "collect_x", "collect_y")[1]
    ) +
    patchwork::plot_annotation(
      title       = paste0("Marginal/Conditional plots for each x | others")
    , caption     = paste0(
                      "Observations with missing values have been removed."
                    , "\nEllipse is robust 50% bivariate-normal probability-contour."
                    , "\nOverlays two graphs:"
                    , "\n  (Blue) Marginal plot of Y vs X with both variables centered, ignoring all other regressors."
                    , "\n  (Magenta) Conditional plot of Y vs X after adjusting for all other regressors (x as added-variable)."
                    , "\n  Note that conditioning removes variation in both the regressor and the response."
                    , "\nThe plot is primarily intended as a pedagogical tool for understanding coefficients in first-order models."
                    )
    , theme = ggplot2::theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1, Caption align left
    )


  out[[ "car__mcPlots_plot" ]] <-
    p_arranged

  return(out)

} # e_plot_model_diagnostics_car__mcPlots



#' Model diagnostics, Marginal Model Plots, car::marginalModelPlots
#'
#'
#' @param fit                fit object
#'
#' @return out      list including text and ggplot grobs
#' @import car
#' @importFrom cowplot as_grob
#' @importFrom patchwork wrap_plots plot_annotation
#' @importFrom ggplot2 theme
#'
e_plot_model_diagnostics_car__marginalModelPlots <-
  function(
    fit                 = NULL
  ) {

  out <- list()

  xy_var_names_list <- e_model_extract_var_names(formula(fit$terms))
  y_var_name  <- xy_var_names_list$y_var_name
  x_var_names <- xy_var_names_list$x_var_names

  # plots
  p_list <-
    cowplot::as_grob(
      ~
      {
      car::marginalModelPlots(
        model           = fit
      , terms           = form_terms
      , fitted          = TRUE
      , layout          = NULL
      , ask             = FALSE
      , main            = ""
      #, groups
      , key             = TRUE
      , sd              = FALSE    # too salient
      , ylab            = y_var_name
      , smooth          = list(smoother = car::loessLine, span = 2/3)  # list(smoother = car::gamLine, k = 3)   #gamLine may be too smooth
      #, pch
      #, groups          = NULL
      , col.line        = carPalette()[c(2, 8)]
      , col             = carPalette()[1]
      , id              = FALSE
      , grid            = TRUE
      )
      }
    )


  p_arranged <-
    patchwork::wrap_plots(
      p_list
    , ncol        = NULL
    , nrow        = NULL
    , byrow       = c(TRUE, FALSE)[1]
    , widths      = NULL
    , heights     = NULL
    , guides      = c("collect", "keep", "auto")[1]
    , tag_level   = c("keep", "new")[1]
    , design      = NULL
    , axes        = NULL
    , axis_titles = c("keep", "collect", "collect_x", "collect_y")[1]
    ) +
    patchwork::plot_annotation(
      title       = paste0("Marginal Model plots for each numeric x and fitted values")
    , caption     = paste0(
                      "Observations with missing values have been removed."
                    , "\nInteractions and/or factors skipped."
                    , "\nSmoothed with Loess line with span = 2/3."
                    )
    , theme = ggplot2::theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1, Caption align left
    )


  out[[ "car__marginalModelPlots_plot" ]] <-
    p_arranged

  return(out)

} # e_plot_model_diagnostics_car__marginalModelPlots



#' Model diagnostics, Component+Residual (Partial Residual) Plots, car::crPlots
#'
#'
#' @param fit                fit object
#'
#' @return out      list including text and ggplot grobs
#' @import car
#' @importFrom cowplot as_grob
#' @importFrom patchwork wrap_plots plot_annotation
#' @importFrom ggplot2 theme
#'
e_plot_model_diagnostics_car__crPlots <-
  function(
    fit                 = NULL
  ) {

  out <- list()

  fit_class <- class(fit)[1]

  xy_var_names_list <- e_model_extract_var_names(formula(fit$terms))
  y_var_name                <- xy_var_names_list$y_var_name
  y_var_name_glm            <- xy_var_names_list$y_var_name_glm
  x_var_names               <- xy_var_names_list$x_var_names
  x_var_names_interactions  <- xy_var_names_list$x_var_names_interactions

  this_fit <-
    fit

  this_title <-
    "Component+Residual (Partial Residual) Plots"

  if (length(x_var_names_interactions)) {
    #warning("erikmisc::e_plot_model_diagnostics, Component+Residual (Partial Residual) Plots only for main effects; refitting with only main effects only for this plot (original model unaffected).")

    this_title <-
      "Component+Residual (Partial Residual) Plots, main-effects only"

    if (fit_class == "lm") {

      form_terms <-
        paste0(
          y_var_name
        , " ~ "
        , paste(
            x_var_names
          , collapse = " + "
          )
        ) |>
        as.formula()

      this_fit <- lm(formula = form_terms, data = dat)

    } # lm
    if (fit_class == "glm") {

      form_terms <-
        paste0(
          y_var_name_glm
        , " ~ "
        , paste(
            x_var_names
          , collapse = " + "
          )
        ) |>
        as.formula()

      this_fit <- glm(formula = form_terms, data = dat, family = binomial(link = "logit"))

    } # glm

  } # interactions


  # plots
  p_list <-
    cowplot::as_grob(
      ~
      {
      car::crPlots(
        model           = this_fit
      #, terms           = form_terms
      , id              = FALSE
      , order           = 1
      , line            = TRUE
      , smooth          = list(smoother = car::loessLine, span = 2/3)  # list(smoother = car::gamLine, k = 3)   #gamLine may be too smooth
      , col             = car::carPalette()[1]
      , col.lines       = car::carPalette()[-1]
      #, xlab
      , ylab            = "Partial residuals"
      , pch             = 1
      , lwd             = 2
      , grid            = TRUE
      , main            = this_title
      )
      }
    )


  p_arranged <-
    patchwork::wrap_plots(
      p_list
    , ncol        = NULL
    , nrow        = NULL
    , byrow       = c(TRUE, FALSE)[1]
    , widths      = NULL
    , heights     = NULL
    , guides      = c("collect", "keep", "auto")[1]
    , tag_level   = c("keep", "new")[1]
    , design      = NULL
    , axes        = NULL
    , axis_titles = c("keep", "collect", "collect_x", "collect_y")[1]
    ) +
    patchwork::plot_annotation(
    #  title       = paste0("Component+Residual (Partial Residual) Plots")
      caption     = paste0(
                      "Observations with missing values have been removed."
                    , "\nThe fit is represented by a broken blue line and a smooth of the partial residuals by a solid magenta line."
                    , "\nParallel boxplots of the partial residuals are drawn for the levels of a factor."
                    )
    , theme = ggplot2::theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1, Caption align left
    )


  out[[ "car__crPlots_plot" ]] <-
    p_arranged

  return(out)

} # e_plot_model_diagnostics_car__crPlots




