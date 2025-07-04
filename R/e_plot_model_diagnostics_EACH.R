



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
#' @importFrom patchwork wrap_plots
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
#' @importFrom patchwork wrap_plots
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
      , id.col=carPalette()[1]
      , id.location = c("lr", "ab", "avoid")[3]
      , col         = carPalette()[1]
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
      , id          = list(method="y", n=4, cex=1, col=carPalette()[1], location=c("lr", "ab", "avoid")[3]) # TRUE
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
#' @importFrom patchwork wrap_plots
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
      #, col           = carPalette()[1]
      #, col.lines     = carPalette()[2]
      #, lwd           = 2
      #, pch           = 1
      #, cex           = par("cex")
      , id            = list(method="y", n=4, cex=1, col=carPalette()[1], location=c("lr", "ab", "avoid")[3]) #TRUE
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
#' @importFrom tibble as_tibble
#' @importFrom dplyr rename
#' @importFrom cowplot as_grob
#' @importFrom patchwork wrap_plots
#'
e_plot_model_diagnostics_qqplotr <-
  function(
    fit_resid           = fit_resid
  , band_conf           = 0.95
  ) {

  out <- list()

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
  p1 <- p1 + stat_qq_band (distribution = "norm", detrend = c(FALSE, TRUE)[1]
              , band_conf = 0.95, bandType = c("pointwise", "boot", "ks", "ts", "ell")[1]
              , fill = "gray75", color = "gray25", alpha = 0.5)
  p1 <- p1 + stat_qq_line (distribution = "norm", detrend = c(FALSE, TRUE)[1])
  p1 <- p1 + stat_qq_point(distribution = "norm", detrend = c(FALSE, TRUE)[1])
  p1 <- p1 + labs(
              title = paste0("QQ-plot, pointwise normal ", 100*band_conf, "%")
            , x     = "Theoretical Quantiles\n(normal distribution)"
            , y     = paste0("Sample Quantiles\n(residual type = ", attr(fit_resid, "resid_type"), ")")
            )
  #p1 <- p1 + scale_fill_discrete("Bandtype")
  #p1


  p2 <- ggplot(data = dat_resid, mapping = aes(sample = resid))
  p2 <- p2 + theme_bw()
  p2 <- p2 + stat_qq_band (distribution = "norm", detrend = c(FALSE, TRUE)[2]
              , band_conf = 0.95, bandType = c("pointwise", "boot", "ks", "ts", "ell")[1]
              , fill = "gray75", color = "gray25", alpha = 0.5)
  p2 <- p2 + stat_qq_line (distribution = "norm", detrend = c(FALSE, TRUE)[2])
  p2 <- p2 + stat_qq_point(distribution = "norm", detrend = c(FALSE, TRUE)[2])
  p2 <- p2 + labs(
              title = paste0("QQ-plot, pointwise normal ", 100*band_conf, "%", ", detrended")
            , x     = "Theoretical Quantiles\n(normal distribution)"
            , y     = paste0("Sample Quantiles\n(residual type = ", attr(fit_resid, "resid_type"), ")")
            )
  #p2

  p3 <- ggplot(data = dat_resid, mapping = aes(sample = resid))
  p3 <- p3 + theme_bw()
  p3 <- p3 + stat_pp_band (distribution = "norm", detrend = c(FALSE, TRUE)[1]
              , band_conf = 0.95, B = 5e3, bandType = c("pointwise", "boot", "ks", "ts", "ell")[2]
              , fill = "gray75", color = "gray25", alpha = 0.5)
  p3 <- p3 + stat_pp_line (distribution = "norm", detrend = c(FALSE, TRUE)[1])
  p3 <- p3 + stat_pp_point(distribution = "norm", detrend = c(FALSE, TRUE)[1])
  p3 <- p3 + labs(
              title = paste0("PP-plot, pointwise bootstrap ", 100*band_conf, "%")
            , x     = "Probability Points\n(normal distribution)"
            , y     = paste0("Cumulative Probability\n(residual type = ", attr(fit_resid, "resid_type"), ")")
            )
  p3


  p4 <- ggplot(data = dat_resid, mapping = aes(sample = resid))
  p4 <- p4 + theme_bw()
  p4 <- p4 + stat_pp_band (distribution = "norm", detrend = c(FALSE, TRUE)[2]
              , band_conf = 0.95, B = 5e3, bandType = c("pointwise", "boot", "ks", "ts", "ell")[2]
              , fill = "gray75", color = "gray25", alpha = 0.5)
  p4 <- p4 + stat_pp_line (distribution = "norm", detrend = c(FALSE, TRUE)[2])
  p4 <- p4 + stat_pp_point(distribution = "norm", detrend = c(FALSE, TRUE)[2])
  p4 <- p4 + labs(
              title = paste0("PP-plot, pointwise bootstrap ", 100*band_conf, "%", ", detrended")
            , x     = "Probability Points\n(normal distribution)"
            , y     = paste0("Cumulative Probability\n(residual type = ", attr(fit_resid, "resid_type"), ")")
            )
  p4



  out[[ "qqplotr_qqplot_normal_plot" ]] <-
    p1
  out[[ "qqplotr_qqplot_detrended_plot" ]] <-
    p2
  out[[ "qqplotr_ppplot_normal_plot" ]] <-
    p3
  out[[ "qqplotr_ppplot_detrended_plot" ]] <-
    p4

  return(out)

} # e_plot_model_diagnostics_qqplotr








