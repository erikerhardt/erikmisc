#' From model formula, extract y and x variable lists
#'
#' @param form    formula of form y ~ x
#'
#' @return out      list with lists of y variable and x variables
#' @importFrom stringr str_detect
#' @importFrom stringr str_split
#' @export
#'
e_model_extract_var_names <-
  function(
    form = NULL
  ) {
  # decompose formula into each covariate
  # identify response and main effect terms from the formula
  form_terms <-
    form |>
    terms() |>
    attr("variables") |>
    as.character()
  # remove "list" artifact element from as.character() and the random effect
  ind_form_terms <-
    stringr::str_detect(string = form_terms, pattern = "list", negate = TRUE)
  form_terms <-
    form_terms[ind_form_terms]
  ind_form_terms_covar <-
    stringr::str_detect(string = form_terms, pattern = stringr::fixed(form_terms[1]), negate = TRUE)

  y_var_name <-
    form_terms[1]
  x_var_names <-
    form_terms[ind_form_terms_covar]

  # for glm, extract the "y" from "cbind(y, 1-y)"
  y_var_name_components <-
    stringr::str_split(
      string    = y_var_name
    , pattern   = "\\(|,"
    ) |>
    unlist()
  if (y_var_name_components[1] == "cbind") {
    y_var_name <-
      y_var_name_components[2]
  }

  out <-
    list(
      y_var_name  = y_var_name
    , x_var_names = x_var_names
    )

  return(out)

} # e_model_extract_var_names


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
    , id      = list(method=list(method="x", n=4, cex=1, col=car::carPalette()[1], location="lr"))  # TRUE  #FALSE
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
      , id      = list(method=list(method="x", n=4, cex=1, col=carPalette()[1], location="lr"))  # TRUE  #FALSE
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
    , theme = theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1, Caption align left
    )

  return(out)

} # e_plot_model_diagnostics_car__inverseResponsePlot


#' Model diagnostics, car::invTranPlot
#'
#'
#' @param fit     fit object
#'
#' @return out      list including text and ggplot grobs
#' @import car
#' @importFrom patchwork wrap_plots
#' @importFrom grDevices pdf dev.off
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


  #plot_record <- grDevices::recordPlot()
  #plot.new() # Clear the plot window
  #par(mfrow = c(ceiling(length(x_var_names) / 2), 2))  # Set up a k x 2 grid

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

    # p_list[[ i_covar ]] <-
    #   patchwork::wrap_elements(
    #     full =
    #     ~
    #     car::invTranPlot(
    #       x           = dat[[ x_var_names[i_covar] ]]
    #     , y           = dat[[ y_var_name ]]
    #     , lambda      = c(-1, 0, 1, 2)
    #     , robust      = FALSE
    #     , family      = c("bcPower", "yjPower")[1]
    #     , xlab        = labelled::var_label(dat[[ x_var_names[i_covar] ]])
    #     , ylab        = labelled::var_label(dat[[ y_var_name ]])
    #     )
    #     # |>
    #     #force()
    #   ) +
    #   #patchwork::plot_annotation(
    #   labs(
    #     title       = paste0("X power transform: ", y_var_name, " ~ ", x_var_names[i_covar])
    #   )

    # p_list[[ i_covar ]] <-
    #   patchwork::wrap_elements(
    #     full =
    #     ~
    #     car::invTranPlot(
    #       x           = dat[[ x_var_names[i_covar] ]]
    #     , y           = dat[[ y_var_name ]]
    #     , lambda      = c(-1, 0, 1, 2)
    #     , robust      = FALSE
    #     , family      = c("bcPower", "yjPower")[1]
    #     , xlab        = labelled::var_label(dat[[ x_var_names[i_covar] ]])
    #     , ylab        = labelled::var_label(dat[[ y_var_name ]])
    #     )
    #     # |>
    #     #force()
    #   ) +
    #   labs(
    #     title       = paste0("X power transform: ", y_var_name, " ~ ", x_var_names[i_covar])
    #   )

    # plot into recordPlot
    car::invTranPlot(
      x           = dat[[ x_var_names[i_covar] ]]
    , y           = dat[[ y_var_name ]]
    , lambda      = c(-1, 0, 1, 2)
    , robust      = FALSE
    , family      = c("bcPower", "yjPower")[1]
    , xlab        = labelled::var_label(dat[[ x_var_names[i_covar] ]])
    , ylab        = labelled::var_label(dat[[ y_var_name ]])
    , main        = paste0("X power transform: ", y_var_name, " ~ ", x_var_names[i_covar])
    )


    # p_list[[ i_covar ]] <-
    #   patchwork::wrap_elements(
    #     full =
    #     ~
    #     car::invTranPlot(
    #       x           = eval(sym(x_var_names[i_covar]), dat)
    #     , y           = eval(sym(y_var_name          ), dat)
    #     , lambda      = c(-1, 0, 1, 2)
    #     , robust      = FALSE
    #     , family      = c("bcPower", "yjPower")[1]
    #     #, xlab        = labelled::var_label(dat[[ x_var_names[i_covar] ]])
    #     #, ylab        = labelled::var_label(dat[[ y_var_name ]])
    #     )
    #   ) +
    #   labs(
    #     title       = paste0("X power transform: ", y_var_name, " ~ ", x_var_names[i_covar])
    #   )







    # p_list[[ i_covar ]] <-
    #   cowplot::as_grob(
    #     ~
    #     car::invTranPlot(
    #       x           = eval(rlang::sym(x_var_names[i_covar]), dat)
    #     , y           = eval(rlang::sym(y_var_name), dat)
    #     , lambda      = c(-1, 0, 1, 2)
    #     , robust      = FALSE
    #     , family      = c("bcPower", "yjPower")[1]
    #     #, xlab        = labelled::var_label(dat[[ x_var_names[i_covar] ]])
    #     #, ylab        = labelled::var_label(dat[[ y_var_name ]])
    #     )
    #   ) +
    #   #patchwork::plot_annotation(
    #   labs(
    #     title       = paste0("X power transform: ", y_var_name, " ~ ", x_var_names[i_covar])
    #   )


    #par(mfrow = c(2, 2))  # Set up a 2x2 grid


  } # i_covar

  #dev.off() # Close the graphics device

  #plot_record
  #grDevices::replayPlot(plot_record)

  # want to capture multiple base graphics in a single grob?
  # use par(), lapply(), cowplot::as_grob(), then patchwork::wrap_plots()


  p_list <-
    cowplot::as_grob(
      ~
      {
        par(mfrow = c(ceiling(length(x_var_names) / 2), 2))  # Set up a k x 2 grid
        lapply(
          x_var_names
        , \(x)
          #plot(dat[[x]], main=x, xlab='', ylab='', ylim=range(dat)))
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
    #, subtitle    = text_formula_sel
    , caption     = paste0(
                      "Observations with missing values have been removed."
                    )
    #, tag_levels  = "A"
    , theme = theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1, Caption align left
    )

  ## p_arranged <-
  ##   cowplot::plot_grid(
  ##       plotlist         = p_list
  ##     , align            = c("none", "h", "v", "hv")[4]
  ##     , axis             = c("none", "l", "r", "t", "b", "lr", "tb", "tblr")[1]
  ##     , nrow             = NULL
  ##     , ncol             = 2
  ##     , rel_widths       = 1
  ##     , rel_heights      = 1
  ##     , labels           = "AUTO" # "auto", c("A", "B")
  ##     , label_size       = 14
  ##     , label_fontfamily = NULL
  ##     , label_fontface   = "bold"
  ##     , label_colour     = NULL
  ##     , label_x          = 0
  ##     , label_y          = 1
  ##     , hjust            = -0.5
  ##     , vjust            = 1.5
  ##     , scale            = 1
  ##     , greedy           = TRUE
  ##     , byrow            = TRUE
  ##   )
  ##
  ##
  ## p_arranged


  out[[ "car__invTranPlot_table" ]] <-
    t_list |>
    dplyr::bind_rows()

  out[[ "car__invTranPlot_plot" ]] <-
    p_arranged

  return(out)

} # e_plot_model_diagnostics_car__invTranPlot



