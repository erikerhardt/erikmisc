#' Plot and report model diagnostics for a lm or glm object.
#'
#' Many are created from package \code{ggResidpanel}, working with models of
#' types \code{lm}, \code{glm}, \code{lme}, \code{lmer}, \code{glmer},
#' and \code{lmerTest}.
#'
#' The following options can be chosen for the \code{sw_plot_set} argument.
#' \itemize{
#' \item "all": This creates a panel of all plot types included in the package
#' that are available for the model type input into \code{resid_panel}. (See note
#' below.)
#' \item "default": This creates a panel with a residual plot, a normal quantile plot
#' of the residuals, an index plot of the residuals, and a histogram of the residuals.
#' \item "R": This creates a panel with a residual plot, a normal
#' quantile plot of the residuals, a location-scale plot, and a residuals versus leverage
#' plot. This was modeled after the plots shown in R if the \code{plot()} base function
#' is applied to an \code{lm} model. This option can only be used with an \code{lm} or
#' \code{glm} model.
#' \item "SAS": This creates a panel with a residual plot, a normal quantile plot of
#' the residuals, a histogram of the residuals, and a boxplot of the residuals.
#' This was modeled after the residualpanel option in proc mixed from SAS version 9.4.
#' \item A vector of individual plots can also be specified.
#' For example, one can specify \code{plots = c("boxplot", "hist")} or
#' \code{plots = "qq"}. The individual plot options are as follows.
#' \itemize{
#' \item \code{"boxplot"}: A boxplot of residuals
#' \item \code{"cookd"}: A plot of Cook's D values versus observation numbers
#' \item \code{"hist"}: A histogram of residuals
#' \item \code{"index"}: A plot of residuals versus observation numbers
#' \item \code{"ls"}: A location scale plot of the residuals
#' \item \code{"qq"}: A normal quantile plot of residuals
#' \item \code{"lev"}: A plot of standardized residuals versus leverage values
#' \item \code{"resid"}: A plot of residuals versus predicted values
#' \item \code{"yvp":}: A plot of observed response values versus predicted values
#' } }
#'
#'
#' @param fit                       model object
#' @param dat                       data used for model fit
#' @param resid_type                Specifies the type of residual to be plotted.  Any of \code{c("working", "response", "deviance", "pearson", "partial", "rstudent", "rstandard")} may be specified.  The default \code{resid_type = "pearson"} is usually appropriate, since it is equal to ordinary residuals observed minus fit with ols, and correctly weighted residuals with wls or for a glm.  The last two options use the \code{\link{rstudent}} and \code{\link{rstandard}} functions and use studentized or standardized residuals.
#' @param sw_interp                 T/F to provide interpretation guidance in the plot caption or with a table.
#' @param sw_write_output           T/F for whether to save plots and text to a path
#' @param sw_write_output_path      path to save results
#' @param sw_write_output_prefix    filename prefix for results
#' @param sw_write_output_plot_fmt  plot filename extension that determs the \code{ggsave} \code{device} argument
#' ## @param sw_plot_set        NULL to accept other plot options, or "simple" to exclude boxcox, constant var, collinearity, order of data, and added-variable plots. "simpleAV" to add back in the added-variable plots.  "all" includes all possible plots in this function.
#' ## @param rp_type            option passed to \code{ggResidpanel::resid_panel}
#' ## @param rp_bins            option passed to \code{ggResidpanel::resid_panel}
#' ## @param rp_smoother        option passed to \code{ggResidpanel::resid_panel}
#' ## @param rp_qqline          option passed to \code{ggResidpanel::resid_panel}
#' ## @param rp_qqbands         option passed to \code{ggResidpanel::resid_panel}
#' ## @param rp_scale           option passed to \code{ggResidpanel::resid_panel}
#' ## @param rp_theme           option passed to \code{ggResidpanel::resid_panel}
#' ## @param rp_axis.text.size  option passed to \code{ggResidpanel::resid_panel}
#' ## @param rp_title.text.size option passed to \code{ggResidpanel::resid_panel}
#' ## @param rp_title.opt       option passed to \code{ggResidpanel::resid_panel}
#' ## @param rp_alpha           option passed to \code{ggResidpanel::resid_panel}
#' ## @param ...                options passed to other function.
#' ## @param rc_mfrow           number of rows and columns for the graphic plot, default is c(1, 3); use "NA" for a single plot with 3 columns
#' ## @param which_plot         default plot numbers for lm()
#' ## @param n_outliers         number to identify in plots from lm() and qqPlot()
#' ## @param sw_qqplot          T/F for whether to show the QQ-plot
#' ## @param sw_boxcox          T/F for whether to show Box-Cox transformation
#' ## @param sw_constant_var    T/F for whether to assess constant variance
#' ## @param sw_collinearity    T/F for whether to assess multicollinearity between predictor variables
#' ## @param sw_order_of_data   T/F for whether to show residuals by order of data
#' ## @param sw_addedvar        T/F for whether to show added-variables plot
#'
#' @return out_diagn        list of tables and plots
#' @import dplyr
#' @importFrom grDevices pdf dev.off
#' @importFrom labelled get_variable_labels
#' @export
#'
#' @examples
#' ## lm example
#' dat <-
#'    erikmisc::dat_mtcars_e
#' form_model_lm <-
#'   mpg ~ cyl + disp + hp + wt + vs + am + disp:hp + hp:vs
#' fit_lm <- lm(formula = form_model_lm, data = dat)
#' out_diagn <-
#'   e_plot_model_diagnostics(
#'     fit = fit_lm
#'   , dat = dat
#'   , resid_type = "studentized"
#'   , sw_interp                 = c(TRUE, FALSE)[1]
#'   , sw_write_output           = c(TRUE, FALSE)[1]
#'   , sw_write_output_path      = "out/XXX"
#'   , sw_write_output_prefix    = "prefix_lm_"
#'   , sw_write_output_plot_fmt  = c("png", "pdf", "jpeg", "eps", "ps", "tex", "tiff", "bmp", "svg", "wmf")[1]
#'   )
#'
#' ## glm example
#' dat <-
#'   erikmisc::dat_mtcars_e |>
#'   dplyr::mutate(
#'     am_01 =
#'       dplyr::case_when(
#'         am == "manual"    ~ 0
#'       , am == "automatic" ~ 1
#'       )
#'   )
#' labelled::var_label(dat[["am_01"]]) <- labelled::var_label(dat[["am"]])
#' form_model_glm <-
#'   cbind(am_01, 1 - am_01) ~ cyl + disp + hp + wt + vs + hp:vs
#' fit_glm <- glm(formula = form_model_glm, data = dat, family = binomial(link = "logit"))
#' out_diagn <-
#'   e_plot_model_diagnostics(
#'     fit = fit_glm
#'   , dat = dat
#'   , resid_type = NA
#'   , sw_interp                 = c(TRUE, FALSE)[1]
#'   , sw_write_output           = c(TRUE, FALSE)[1]
#'   , sw_write_output_path      = "out/XXX"
#'   , sw_write_output_prefix    = "prefix_glm_"
#'   , sw_write_output_plot_fmt  = c("png", "pdf", "jpeg", "eps", "ps", "tex", "tiff", "bmp", "svg", "wmf")[1]
#'   )
#'
e_plot_model_diagnostics <-
  function(
    fit                       = NULL
  , dat                       = NULL
  , resid_type                = c(NA, "pearson", "response", "standardized", "deviance", "stand.deviance", "stand.pearson", "studentized", "partial")[8]
  , sw_interp                 = c(TRUE, FALSE)[2]
  , sw_write_output           = c(TRUE, FALSE)[2]
  , sw_write_output_path      = "."
  , sw_write_output_prefix    = "prefix_"
  , sw_write_output_plot_fmt  = c("png", "pdf", "jpeg", "eps", "ps", "tex", "tiff", "bmp", "svg", "wmf")[1]

  # , sw_plot_set         = c("simple", "simpleAV", "all", "boxplot", "cookd", "hist", "index", "ls", "qq", "lev", "resid", "yvp")[3]
  # , rp_type             = c(NA, "pearson", "response", "standardized", "deviance", "stand.deviance", "stand.pearson")[1]
  # , rp_bins             = 30
  # , rp_smoother         = c(TRUE, FALSE)[1]
  # , rp_qqline           = c(TRUE, FALSE)[1]
  # , rp_qqbands          = c(TRUE, FALSE)[1]
  # , rp_scale            = 1
  # , rp_theme            = c("bw", "classic", "grey")[1]
  # , rp_axis.text.size   = 10
  # , rp_title.text.size  = 12
  # , rp_title.opt        = c(TRUE, FALSE)[1]
  # , rc_mfrow         = c(1, 3)
  # , which_plot       = c(4, 6, 1)
  # , n_outliers       = 3
  # , sw_qqplot        = TRUE
  # , sw_boxcox        = TRUE
  # , sw_constant_var  = TRUE
  # , sw_collinearity  = TRUE
  # , sw_order_of_data = TRUE
  # , sw_addedvar      = TRUE
  # , ...
  ) {
  #### DEBUG
  ## library(tidyverse)
  ## library(erikmisc)
  ## source("R/e_plot_model_diagnostics_EACH.R")
  ## ## lm example
  ## dat <-
  ##    erikmisc::dat_mtcars_e
  ## form_model_lm <-
  ##   mpg ~ cyl + disp + hp + wt + vs + am + disp:hp + hp:vs
  ## fit_lm <- lm(formula = form_model_lm, data = dat)
  ##   fit = fit_lm
  ##   dat = dat
  ##   resid_type = "studentized"
  ##   sw_interp  = c(TRUE, FALSE)[2]
  ##   sw_write_output          = c(TRUE, FALSE)[1]
  ##   sw_write_output_path     = "C:/Users/erike/Desktop/TEMP/out/XXX"
  ##   sw_write_output_prefix   = "TEST_lm_"
  ##   sw_write_output_plot_fmt = c("png", "pdf", "jpeg", "eps", "ps", "tex", "tiff", "bmp", "svg", "wmf")[1]
  ## ## glm example
  ## dat <-
  ##   erikmisc::dat_mtcars_e |>
  ##   dplyr::mutate(
  ##     am_01 =
  ##       dplyr::case_when(
  ##         am == "manual"    ~ 0
  ##       , am == "automatic" ~ 1
  ##       )
  ##   )
  ## labelled::var_label(dat[["am_01"]]) <- labelled::var_label(dat[["am"]])
  ## form_model_glm <-
  ##   cbind(am_01, 1 - am_01) ~ cyl + disp + hp + wt + vs + hp:vs
  ## fit_glm <- glm(formula = form_model_glm, data = dat, family = binomial(link = "logit"))
  ##   fit = fit_glm
  ##   dat = dat
  ##   resid_type = NA
  ##   sw_interp                 = c(TRUE, FALSE)[1]
  ##   sw_write_output           = c(TRUE, FALSE)[1]
  ##   sw_write_output_path      = "C:/Users/erike/Desktop/TEMP/out/XXX"
  ##   sw_write_output_prefix    = "TEST_glm_"
  ##   sw_write_output_plot_fmt  = c("png", "pdf", "jpeg", "eps", "ps", "tex", "tiff", "bmp", "svg", "wmf")[1]


  ## XXX
  # Index residuals and other observations by an ID
  # out_diagn[[ "gvlma" ]][[ "gvlma_plots_grid" ]]
  #   hollow gray50 points for no issue, solid red for issues
  #   table, can add sig stars to p-values


  # write many separate functions and call them from the master function

  # Capture base plots as ggplot objects
    # https://wilkelab.org/cowplot/reference/as_grob.html,
      # p <- ~plot(x, y); cowplot::grid.draw(cowplot::as_grob(p))

  #
    # https://goodekat.github.io/ggResidpanel/
    # https://goodekat.github.io/ggResidpanel/articles/introduction.html
      # includes regression trees at bottom
    # https://goodekat.github.io/ggResidpanel-tutorial/tutorial.html
  # https://github.com/graysonwhite/gglm
    # resid vs fitted
    # normal qq
    # scale-location
    # resid vs leverage
  # glm https://steffilazerte.ca/posts/dharma/
  # https://github.com/svVale/rdiagnostats


  ## Useful list of diags: http://www.statmethods.net/stats/rdiagnostics.html
  # https://www.geeksforgeeks.org/linear-regression-assumptions-and-diagnostics-using-r/
  # https://olsrr.rsquaredacademy.com/articles/intro
  # maybe not good: https://github.com/JWiley/JWileymisc/blob/main/R/diagnostics.R
    # maybe good for distribution tests
  # car https://rpubs.com/DragonflyStats/Linear-Model-Diagnostics-Prestige

  sw_kable_format <- c("latex", "html", "pipe", "simple", "rst", "jira", "org")[4]

  if (sw_write_output) {
    # create folder if it doesn't already exist
    dir.create(sw_write_output_path, recursive = TRUE, showWarnings = FALSE)

    fn_output <-
      file.path(
            sw_write_output_path
          , paste0(
              sw_write_output_prefix
            , "diagn_text.txt"
            )
          )

    output_write <-
      c(
        fn_output
      , "\n\n"
      )
    readr::write_lines(x = output_write, file = fn_output, append = FALSE)

  } # sw_write_output


  out_diagn <- list()

  #fit <- fit_lm
  #fit <- fit_glm

  fit_class <- class(fit)[1]
  if (fit_class == "lm") {
    print("e_plot_model_diagnostics: lm model")
  } # lm
  if (fit_class == "glm") {
    print("e_plot_model_diagnostics: glm model")
  } # glm

  xy_var_names_list <- e_model_extract_var_names(formula(fit$terms), dat)


  # Calculate residuals for later
  fit_resid <-
    e_model_calc_resid(
      fit         = fit
    , resid_type  = resid_type
    )

  # Convert name of residuals to car's type
  resid_type <- attr(fit_resid, "resid_type") # may have changed if requested was not available
  resid_type_car <-
    dplyr::case_when(
      is.na(resid_type)              ~ "pearson"
    , resid_type == "response"       ~ "response"
    , resid_type == "deviance"       ~ "deviance"
    , resid_type == "pearson"        ~ "pearson"
    , resid_type == "partial"        ~ "partial"
    , resid_type == "studentized"    ~ "rstudent"
    , resid_type == "standardized"   ~ "rstandard"
    , resid_type == "stand.deviance" ~    "deviance"
    , resid_type == "stand.pearson"  ~    "pearson"
    #, .default                       ~ "pearson"
    )

  # Calculate Cook's D for later
  fit_cooksD <-
    stats::cooks.distance(
      model = fit
    )
  # Calculate Influence (hat values) for later
  fit_leverage <-
    stats::hatvalues(
      model = fit
    )




  # Residual histogram
  if (fit_class == "lm") {

    out_diagn[[ "Resid_histogram" ]] <-
      e_plot_model_diagnostics_Resid_histogram(
          fit_resid           = fit_resid
        , sw_interp           = sw_interp
        )

    if (sw_interp) {
      out_diagn[[ "Resid_histogram" ]][[ "Resid_histogram_interp" ]] |> cli::cat_line(col = "#2980B9")
    }
    out_diagn[[ "Resid_histogram" ]][[ "Resid_histogram_plot" ]] |> print()

  } # lm
  if (fit_class == "glm") {
    out_diagn[[ "Resid_histogram" ]] <-
      NULL
  } # glm

  # write output
  if (sw_write_output & !is.null(out_diagn[[ "Resid_histogram" ]])) {
    # create folder if it doesn't already exist
    dir.create(sw_write_output_path, recursive = TRUE, showWarnings = FALSE)

    fn_name <-
      paste0(
        sw_write_output_prefix
      #, "_"
      , "Resid_histogram_plot."
      , sw_write_output_plot_fmt
      )
    ggplot2::ggsave(
        file.path(
          sw_write_output_path
        , fn_name
        )
      , plot   = out_diagn[[ "Resid_histogram" ]][[ "Resid_histogram_plot" ]]
      , width  = 5
      , height = 5
      , units  = "in"
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      #, useDingbats = FALSE
      )

    output_write <-
      c(
        "\n\n"
      , "====="
      , "\nResid_histogram:  "
      , fn_name
      , "\n"
      , out_diagn[[ "Resid_histogram" ]][[ "Resid_histogram_interp" ]]
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)

  } # sw_write_output



  # # qqplot, Quantile-Comparison Plot
  # if (fit_class == "lm") {
  #
  #   # base graphics version
  #   out_diagn[[ "car__qqPlot" ]] <-
  #     e_plot_model_diagnostics_car__qqPlot(
  #       fit                 = fit
  #     , dat                 = dat
  #     )
  #
  #   out_diagn[[ "car__qqPlot" ]][[ "car__qqPlot_plot"  ]] |> print()
  #
  # } # lm
  # if (fit_class == "glm") {
  #   out_diagn[[ "car__qqPlot" ]] <-
  #     NULL
  # } # glm



  ## qqplot
  if (fit_class == "lm") {

    # ggplot version
    out_diagn[[ "qqplotr" ]] <-
      e_plot_model_diagnostics_qqplotr(
        fit_resid           = fit_resid
      , fit                 = fit
      , sw_interp           = sw_interp
      )

    #out_diagn[[ "qqplotr" ]][[ "qqplotr_qqplot_diagonal_plot"  ]] |> print()
    #out_diagn[[ "qqplotr" ]][[ "qqplotr_qqplot_detrended_plot" ]] |> print()
    #out_diagn[[ "qqplotr" ]][[ "qqplotr_ppplot_diagonal_plot"  ]] |> print()
    #out_diagn[[ "qqplotr" ]][[ "qqplotr_ppplot_detrended_plot" ]] |> print()

    #out_diagn[[ "qqplotr" ]][[ "qqplotr_grid_diagonal_plot"    ]] |> print()
    out_diagn[[ "qqplotr" ]][[ "qqplotr_grid_detrended_plot"   ]] |> print()

    out_diagn[[ "qqplotr" ]][[ "normality_test_table" ]]          |> print()

  } # lm
  if (fit_class == "glm") {
    out_diagn[[ "qqplotr" ]] <-
      NULL
  } # glm

  # write output
  if (sw_write_output & !is.null(out_diagn[[ "qqplotr" ]])) {
    # create folder if it doesn't already exist
    dir.create(sw_write_output_path, recursive = TRUE, showWarnings = FALSE)

    fn_name <-
      paste0(
        sw_write_output_prefix
      #, "_"
      , "qqplotr_detrended_plot."
      , sw_write_output_plot_fmt
      )
    ggplot2::ggsave(
        file.path(
          sw_write_output_path
        , fn_name
        )
      , plot   = out_diagn[[ "qqplotr" ]][[ "qqplotr_grid_detrended_plot"   ]]
      , width  = 8
      , height = 8
      , units  = "in"
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      #, useDingbats = FALSE
      )

    output_write <-
      c(
        "\n\n"
      , "====="
      , "\nqqplotr:  "
      , fn_name
      , "\n"
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
    output_write <-
      out_diagn[[ "qqplotr" ]][[ "normality_test_table" ]] |>
      dplyr::mutate(
        dplyr::across(tidyselect::where(is.numeric), ~signif(.x, digits = 3))
      ) |>
      knitr::kable(format = sw_kable_format) |>
      as.character()
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
  } # sw_write_output


  # Influence plots
  if (fit_class %in% c("lm", "glm")) {

    out_diagn[[ "CooksD_Leverage_Resid" ]] <-
      e_plot_model_diagnostics_CooksD_Leverage_Resid(
          fit                 = fit
        , fit_resid           = fit_resid
        , fit_cooksD          = fit_cooksD
        , fit_leverage        = fit_leverage
        , sw_interp           = sw_interp
      )

    #out_diagn[[ "CooksD_Leverage_Resid" ]][[ "CooksD_Index_plot"                   ]] |> print()
    #out_diagn[[ "CooksD_Leverage_Resid" ]][[ "CooksD_Leverage_plot"                ]] |> print()
    #out_diagn[[ "CooksD_Leverage_Resid" ]][[ "Resid_Leverage_CooksD_plot"          ]] |> print()
    #out_diagn[[ "CooksD_Leverage_Resid" ]][[ "Resid_CooksD_Leverage_plot"          ]] |> print()
    out_diagn[[ "CooksD_Leverage_Resid" ]][[ "CooksD_Leverage_Resid_arranged_plot" ]] |> print()

  } # lm or glm

  # write output
  if (sw_write_output & !is.null(out_diagn[[ "CooksD_Leverage_Resid" ]])) {
    # create folder if it doesn't already exist
    dir.create(sw_write_output_path, recursive = TRUE, showWarnings = FALSE)

    fn_name <-
      paste0(
        sw_write_output_prefix
      #, "_"
      , "CooksD_Leverage_Resid_plot."
      , sw_write_output_plot_fmt
      )
    ggplot2::ggsave(
        file.path(
          sw_write_output_path
        , fn_name
        )
      , plot   = out_diagn[[ "CooksD_Leverage_Resid" ]][[ "CooksD_Leverage_Resid_arranged_plot" ]]
      , width  = 10
      , height = 10
      , units  = "in"
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      #, useDingbats = FALSE
      )

    output_write <-
      c(
        "\n\n"
      , "====="
      , "\nCooksD_Leverage_Resid:  "
      , fn_name
      , "\n"
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
  } # sw_write_output



  # Influence index plots
  if (fit_class %in% c("lm", "glm")) {

    # base graphics version
    out_diagn[[ "car__influenceIndexPlot" ]] <-
      e_plot_model_diagnostics_car__influenceIndexPlot(
        fit                 = fit
      , sw_interp           = sw_interp
      )

    out_diagn[[ "car__influenceIndexPlot" ]][[ "car__influenceIndexPlot_plot"  ]] |> print()

  } # lm or glm

  # write output
  if (sw_write_output & !is.null(out_diagn[[ "car__influenceIndexPlot" ]])) {
    # create folder if it doesn't already exist
    dir.create(sw_write_output_path, recursive = TRUE, showWarnings = FALSE)

    fn_name <-
      paste0(
        sw_write_output_prefix
      #, "_"
      , "car__influenceIndexPlot_plot."
      , sw_write_output_plot_fmt
      )
    ggplot2::ggsave(
        file.path(
          sw_write_output_path
        , fn_name
        )
      , plot   = out_diagn[[ "car__influenceIndexPlot" ]][[ "car__influenceIndexPlot_plot"  ]]
      , width  = 10
      , height = 10
      , units  = "in"
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      #, useDingbats = FALSE
      )

    output_write <-
      c(
        "\n\n"
      , "====="
      , "\ncar__influenceIndexPlot:  "
      , fn_name
      , "\n"
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
  } # sw_write_output




  ## outlier test
  if (fit_class %in% c("lm", "glm")) {
    out_diagn[[ "car__outlierTest" ]] <-
      e_plot_model_diagnostics_car__outlierTest(
        fit                 = fit
      , sw_interp           = sw_interp
      )

    out_diagn[[ "car__outlierTest" ]][[ "car__outlierTest_print" ]] |> print()
    out_diagn[[ "car__outlierTest" ]][[ "car__outlierTest_table" ]] |> print()

  } # lm or glm

  # write output
  if (sw_write_output & !is.null(out_diagn[[ "car__outlierTest" ]])) {
    # create folder if it doesn't already exist
    dir.create(sw_write_output_path, recursive = TRUE, showWarnings = FALSE)

    output_write <-
      c(
        "\n\n"
      , "====="
      , "\ncar__outlierTest:  "
      #, fn_name
      , "\n"
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
    output_write <-
      out_diagn[[ "car__outlierTest" ]][[ "car__outlierTest_print" ]]
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
    output_write <-
      c(
        "\n\n"
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
    output_write <-
      out_diagn[[ "car__outlierTest" ]][[ "car__outlierTest_table" ]] |>
      dplyr::mutate(
        dplyr::across(tidyselect::where(is.numeric), ~signif(.x, digits = 3))
      ) |>
      knitr::kable(format = sw_kable_format) |>
      as.character()
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
  } # sw_write_output


  ## Autocorrelated Errors test
  if (fit_class %in% c("lm", "glm")) {
    out_diagn[[ "car__durbinWatsonTest" ]] <-
      e_plot_model_diagnostics_car__durbinWatsonTest(
        fit                 = fit
      , sw_interp           = sw_interp
      )

    out_diagn[[ "car__durbinWatsonTest" ]][[ "car__durbinWatsonTest_table" ]] |> print()

  } # lm or glm

  # write output
  if (sw_write_output & !is.null(out_diagn[[ "car__durbinWatsonTest" ]])) {
    # create folder if it doesn't already exist
    dir.create(sw_write_output_path, recursive = TRUE, showWarnings = FALSE)

    output_write <-
      c(
        "\n\n"
      , "====="
      , "\ncar__durbinWatsonTest:  "
      #, fn_name
      , "\n"
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
    output_write <-
      out_diagn[[ "car__durbinWatsonTest" ]][[ "car__durbinWatsonTest_table" ]]
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
  } # sw_write_output


  ## Homoscedacticity, Equal variance
  if (fit_class == "lm") {

    # base graphics version
    out_diagn[[ "car__spreadLevelPlot" ]] <-
      e_plot_model_diagnostics_car__spreadLevelPlot(
        fit                 = fit
      , sw_interp           = sw_interp
      )

    out_diagn[[ "car__spreadLevelPlot" ]][[ "car__spreadLevelPlot_plot"  ]] |> print()
    out_diagn[[ "car__spreadLevelPlot" ]][[ "car__spreadLevelPlot_table" ]] |> print()

  } # lm
  if (fit_class == "glm") {
    out_diagn[[ "car__spreadLevelPlot" ]] <-
      NULL
  } # glm

  # write output
  if (sw_write_output & !is.null(out_diagn[[ "car__spreadLevelPlot" ]])) {
    # create folder if it doesn't already exist
    dir.create(sw_write_output_path, recursive = TRUE, showWarnings = FALSE)

    fn_name <-
      paste0(
        sw_write_output_prefix
      #, "_"
      , "car__spreadLevelPlot_plot."
      , sw_write_output_plot_fmt
      )
    ggplot2::ggsave(
        file.path(
          sw_write_output_path
        , fn_name
        )
      , plot   = out_diagn[[ "car__spreadLevelPlot" ]][[ "car__spreadLevelPlot_plot"  ]]
      , width  = 6
      , height = 6
      , units  = "in"
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      #, useDingbats = FALSE
      )

    output_write <-
      c(
        "\n\n"
      , "====="
      , "\ncar__spreadLevelPlot:  "
      , fn_name
      , "\n"
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
    output_write <-
      out_diagn[[ "car__spreadLevelPlot" ]][[ "car__spreadLevelPlot_table" ]]
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)

  } # sw_write_output


  ## resid vs y plots
  if (fit_class %in% c("lm", "glm")) {
    out_diagn[[ "car__residualPlots_y" ]] <-
      e_plot_model_diagnostics_car__residualPlots_y(
        fit                 = fit
      , resid_type          = resid_type_car
      , sw_interp           = sw_interp
      )

    out_diagn[[ "car__residualPlots_y" ]][[ "car__residualPlots_y_table" ]] |> print()
    out_diagn[[ "car__residualPlots_y" ]][[ "car__residualPlots_y_plot"  ]] |> print()

  } # lm or glm

  # write output
  if (sw_write_output & !is.null(out_diagn[[ "car__residualPlots_y" ]])) {
    # create folder if it doesn't already exist
    dir.create(sw_write_output_path, recursive = TRUE, showWarnings = FALSE)

    fn_name <-
      paste0(
        sw_write_output_prefix
      #, "_"
      , "car__residualPlots_y_plot."
      , sw_write_output_plot_fmt
      )
    ggplot2::ggsave(
        file.path(
          sw_write_output_path
        , fn_name
        )
      , plot   = out_diagn[[ "car__residualPlots_y" ]][[ "car__residualPlots_y_plot"  ]]
      , width  = 6
      , height = 6
      , units  = "in"
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      #, useDingbats = FALSE
      )

    output_write <-
      c(
        "\n\n"
      , "====="
      , "\ncar__residualPlots_y:  "
      , fn_name
      , "\n"
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
    if(!is.null(out_diagn[[ "car__residualPlots_y" ]][[ "car__residualPlots_y_table" ]])) {
      output_write <-
        out_diagn[[ "car__residualPlots_y" ]][[ "car__residualPlots_y_table" ]] |>
        dplyr::mutate(
          dplyr::across(tidyselect::where(is.numeric), ~signif(.x, digits = 3))
        ) |>
        knitr::kable(format = sw_kable_format) |>
        as.character()
      readr::write_lines(x = output_write, file = fn_output, append = TRUE)
    }

  } # sw_write_output





  ## resid vs x plots
  if (fit_class %in% c("lm", "glm")) {
    out_diagn[[ "car__residualPlots_x" ]] <-
      e_plot_model_diagnostics_car__residualPlots_x(
        fit                 = fit
      , resid_type          = resid_type_car
      , sw_interp           = sw_interp
      )

    out_diagn[[ "car__residualPlots_x" ]][[ "car__residualPlots_x_table" ]] |> print()
    out_diagn[[ "car__residualPlots_x" ]][[ "car__residualPlots_x_plot"  ]] |> print()

  } # lm or glm

  # write output
  if (sw_write_output & !is.null(out_diagn[[ "car__residualPlots_x" ]])) {
    # create folder if it doesn't already exist
    dir.create(sw_write_output_path, recursive = TRUE, showWarnings = FALSE)

    fn_name <-
      paste0(
        sw_write_output_prefix
      #, "_"
      , "car__residualPlots_x_plot."
      , sw_write_output_plot_fmt
      )
    ggplot2::ggsave(
        file.path(
          sw_write_output_path
        , fn_name
        )
      , plot   = out_diagn[[ "car__residualPlots_x" ]][[ "car__residualPlots_x_plot"  ]]
      , width  = 8
      , height = 1 + 3 * ceiling(length(xy_var_names_list$x_var_names) / 3)
      , units  = "in"
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      #, useDingbats = FALSE
      )

    output_write <-
      c(
        "\n\n"
      , "====="
      , "\ncar__residualPlots_x:  "
      , fn_name
      , "\n"
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
    output_write <-
      out_diagn[[ "car__residualPlots_x" ]][[ "car__residualPlots_x_table" ]] |>
      dplyr::mutate(
        dplyr::across(tidyselect::where(is.numeric), ~signif(.x, digits = 3))
      ) |>
      knitr::kable(format = sw_kable_format) |>
      as.character()
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)

  } # sw_write_output




  # dfbetas plots
  if (fit_class == "lm") {

    # base graphics version
    out_diagn[[ "car__dfbetasPlots" ]] <-
      e_plot_model_diagnostics_car__dfbetasPlots(
        fit                 = fit
      , sw_interp           = sw_interp
      )

    out_diagn[[ "car__dfbetasPlots" ]][[ "car__dfbetasPlots_plot"  ]] |> print()

  } # lm
  if (fit_class == "glm") {
    out_diagn[[ "car__dfbetasPlots" ]] <-
      NULL
  } # glm

  # write output
  if (sw_write_output & !is.null(out_diagn[[ "car__dfbetasPlots" ]])) {
    # create folder if it doesn't already exist
    dir.create(sw_write_output_path, recursive = TRUE, showWarnings = FALSE)

    fn_name <-
      paste0(
        sw_write_output_prefix
      #, "_"
      , "car__dfbetasPlots_plot."
      , sw_write_output_plot_fmt
      )
    ggplot2::ggsave(
        file.path(
          sw_write_output_path
        , fn_name
        )
      , plot   = out_diagn[[ "car__dfbetasPlots" ]][[ "car__dfbetasPlots_plot"  ]]
      , width  = 8
      , height = 1 + 3 * ceiling((length(xy_var_names_list$x_var_names) + length(xy_var_names_list$x_var_names_interactions)) / 3)
      , units  = "in"
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      #, useDingbats = FALSE
      )

    output_write <-
      c(
        "\n\n"
      , "====="
      , "\ncar__dfbetasPlots:  "
      , fn_name
      , "\n"
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)

  } # sw_write_output




  ## gvlma, "Global Validation of Linear Model Assumptions"
  if (fit_class == "lm") {
    out_diagn[[ "gvlma" ]] <-
      e_plot_model_diagnostics_gvlma(
        fit                 = fit
      , dat                 = dat
      , sw_interp           = sw_interp
      )

    if (sw_interp) {
      out_diagn[[ "gvlma" ]][[ "gvlma_interp" ]] |> cli::cat_line(col = "#2980B9")
    }
    out_diagn[[ "gvlma" ]][[ "gvlma_tests_overall_print" ]]  |> cat(sep="\n") |> print()
    out_diagn[[ "gvlma" ]][[ "gvlma_tests_deletion_print" ]] |> cat(sep="\n") |> print()
    out_diagn[[ "gvlma" ]][[ "gvlma_plots_grid" ]]           |> print()

  } # lm
  if (fit_class == "glm") {
    out_diagn[[ "gvlma" ]] <-
      NULL
  } # glm

  # write output
  if (sw_write_output & !is.null(out_diagn[[ "gvlma" ]])) {
    # create folder if it doesn't already exist
    dir.create(sw_write_output_path, recursive = TRUE, showWarnings = FALSE)

    fn_name <-
      paste0(
        sw_write_output_prefix
      #, "_"
      , "gvlma_plot."
      , sw_write_output_plot_fmt
      )
    ggplot2::ggsave(
        file.path(
          sw_write_output_path
        , fn_name
        )
      , plot   = out_diagn[[ "gvlma" ]][[ "gvlma_plots_grid" ]]
      , width  =  9
      , height = 10
      , units  = "in"
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      #, useDingbats = FALSE
      )

    output_write <-
      c(
        "\n\n"
      , "====="
      , "\ngvlma:  "
      , fn_name
      , "\n"
      , out_diagn[[ "gvlma" ]][[ "gvlma_interp" ]]
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
    output_write <-
      out_diagn[[ "gvlma" ]][[ "gvlma_tests_overall_print" ]]
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
    output_write <-
      c(
        "\n\n"
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
    output_write <-
      out_diagn[[ "gvlma" ]][[ "gvlma_tests_deletion_print" ]]
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)

  } # sw_write_output



  ### transform y
  ## Box-Cox transformation of y
  if (fit_class == "lm") {

    # base graphics version
    out_diagn[[ "car__boxCox" ]] <-
      e_plot_model_diagnostics_car__boxCox(
        fit                 = fit
      , sw_interp           = sw_interp
      )

    out_diagn[[ "car__boxCox" ]][[ "car__boxCox_plot"       ]] |> print()
    out_diagn[[ "car__boxCox" ]][[ "car__boxCox_table"      ]][[ "result" ]] |> print()
    out_diagn[[ "car__boxCox" ]][[ "car__boxCox_table_list" ]] |> print()

  } # lm
  if (fit_class == "glm") {
    out_diagn[[ "car__boxCox" ]] <-
      NULL
  } # glm

  # write output
  if (sw_write_output & !is.null(out_diagn[[ "car__boxCox" ]])) {
    # create folder if it doesn't already exist
    dir.create(sw_write_output_path, recursive = TRUE, showWarnings = FALSE)

    fn_name <-
      paste0(
        sw_write_output_prefix
      #, "_"
      , "car__boxCox_plot."
      , sw_write_output_plot_fmt
      )
    ggplot2::ggsave(
        file.path(
          sw_write_output_path
        , fn_name
        )
      , plot   = out_diagn[[ "car__boxCox" ]][[ "car__boxCox_plot"  ]]
      , width  = 8
      , height = 6
      , units  = "in"
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      #, useDingbats = FALSE
      )

    output_write <-
      c(
        "\n\n"
      , "====="
      , "\ncar__boxCox:  "
      , fn_name
      , "\n"
      #, out_diagn[[ "" ]][[ "_interp" ]]
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
    output_write <-
      out_diagn[[ "car__boxCox" ]][[ "car__boxCox_table" ]][[ "result" ]] |>
      dplyr::mutate(
        dplyr::across(tidyselect::where(is.numeric), ~signif(.x, digits = 3))
      ) |>
      knitr::kable(format = sw_kable_format) |>
      as.character()
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
    output_write <-
      c(
        "\n\n"
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
    output_write <-
      out_diagn[[ "car__boxCox" ]][[ "car__boxCox_table_list" ]] |>
      dplyr::mutate(
        dplyr::across(tidyselect::where(is.numeric), ~signif(.x, digits = 3))
      ) |>
      knitr::kable(format = sw_kable_format) |>
      as.character()
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)

  } # sw_write_output


  ## Inverse response plot
  if (fit_class == "lm") {

    # base graphics version
    out_diagn[[ "car__inverseResponsePlot" ]] <-
      e_plot_model_diagnostics_car__inverseResponsePlot(
        fit                 = fit
      , sw_interp           = sw_interp
      )

    out_diagn[[ "car__inverseResponsePlot" ]][[ "car__inverseResponsePlot_table" ]] |> print()
    out_diagn[[ "car__inverseResponsePlot" ]][[ "car__inverseResponsePlot_plot"  ]] |> print()

  } # lm
  if (fit_class == "glm") {
    out_diagn[[ "car__inverseResponsePlot" ]] <-
      NULL
  } # glm

  # write output
  if (sw_write_output & !is.null(out_diagn[[ "car__inverseResponsePlot" ]])) {
    # create folder if it doesn't already exist
    dir.create(sw_write_output_path, recursive = TRUE, showWarnings = FALSE)

    fn_name <-
      paste0(
        sw_write_output_prefix
      #, "_"
      , "car__inverseResponsePlot_plot."
      , sw_write_output_plot_fmt
      )
    ggplot2::ggsave(
        file.path(
          sw_write_output_path
        , fn_name
        )
      , plot   = out_diagn[[ "car__inverseResponsePlot" ]][[ "car__inverseResponsePlot_plot"  ]]
      , width  = 6
      , height = 6
      , units  = "in"
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      #, useDingbats = FALSE
      )

    output_write <-
      c(
        "\n\n"
      , "====="
      , "\ncar__inverseResponsePlot:  "
      , fn_name
      , "\n"
      #, out_diagn[[ "" ]][[ "_interp" ]]
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
    output_write <-
      out_diagn[[ "car__inverseResponsePlot" ]][[ "car__inverseResponsePlot_table" ]] |>
      dplyr::mutate(
        dplyr::across(tidyselect::where(is.numeric), ~signif(.x, digits = 3))
      ) |>
      knitr::kable(format = sw_kable_format) |>
      as.character()
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
  } # sw_write_output


  ### transform x
  ## Marginal Model Plots
  if (fit_class %in% c("lm", "glm")) {
    out_diagn[[ "car__marginalModelPlots" ]] <-
      e_plot_model_diagnostics_car__marginalModelPlots(
        fit                 = fit
      , sw_interp           = sw_interp
      )

    out_diagn[[ "car__marginalModelPlots" ]][[ "car__marginalModelPlots_plot"  ]] |> print()

  } # lm or glm

  # write output
  if (sw_write_output & !is.null(out_diagn[[ "car__marginalModelPlots" ]])) {
    # create folder if it doesn't already exist
    dir.create(sw_write_output_path, recursive = TRUE, showWarnings = FALSE)

    fn_name <-
      paste0(
        sw_write_output_prefix
      #, "_"
      , "car__marginalModelPlots_plot."
      , sw_write_output_plot_fmt
      )
    ggplot2::ggsave(
        file.path(
          sw_write_output_path
        , fn_name
        )
      , plot   = out_diagn[[ "car__marginalModelPlots" ]][[ "car__marginalModelPlots_plot"  ]]
      , width  = 10   # length(xy_var_names_list$x_var_names__numeric) + 1
      , height = 10   # length(xy_var_names_list$x_var_names__numeric) + 1
      , units  = "in"
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      #, useDingbats = FALSE
      )

    output_write <-
      c(
        "\n\n"
      , "====="
      , "\ncar__marginalModelPlots:  "
      , fn_name
      , "\n"
      #, out_diagn[[ "" ]][[ "_interp" ]]
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
  } # sw_write_output



  ## Inverse response plot
  if (fit_class == "lm") {

    # base graphics version
    out_diagn[[ "car__invTranPlot" ]] <-
      e_plot_model_diagnostics_car__invTranPlot(
        fit                 = fit
      , dat                 = dat
      , sw_interp           = sw_interp
      )

    out_diagn[[ "car__invTranPlot" ]][[ "car__invTranPlot_table" ]] |> print()
    out_diagn[[ "car__invTranPlot" ]][[ "car__invTranPlot_plot"  ]] |> print()

  } # lm
  if (fit_class == "glm") {
    out_diagn[[ "car__invTranPlot" ]] <-
      NULL
  } # glm

  # write output
  if (sw_write_output & !is.null(out_diagn[[ "car__invTranPlot" ]])) {
    # create folder if it doesn't already exist
    dir.create(sw_write_output_path, recursive = TRUE, showWarnings = FALSE)

    fn_name <-
      paste0(
        sw_write_output_prefix
      #, "_"
      , "car__invTranPlot_plot."
      , sw_write_output_plot_fmt
      )
    ggplot2::ggsave(
        file.path(
          sw_write_output_path
        , fn_name
        )
      , plot   = out_diagn[[ "car__invTranPlot" ]][[ "car__invTranPlot_plot"  ]]
      , width  = 8
      , height = 1 + 4 * ceiling(length(xy_var_names_list$x_var_names__numeric) / 2)
      , units  = "in"
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      #, useDingbats = FALSE
      )

    output_write <-
      c(
        "\n\n"
      , "====="
      , "\ncar__invTranPlot:  "
      , fn_name
      , "\n"
      #, out_diagn[[ "" ]][[ "_interp" ]]
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
    output_write <-
      out_diagn[[ "car__invTranPlot" ]][[ "car__invTranPlot_table" ]] |>
      dplyr::mutate(
        dplyr::across(tidyselect::where(is.numeric), ~signif(.x, digits = 3))
      ) |>
      knitr::kable(format = sw_kable_format) |>
      as.character()
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
  } # sw_write_output


  ## VIF, GVIF
  if (fit_class %in% c("lm", "glm")) {
    out_diagn[[ "car__vif" ]] <-
      e_plot_model_diagnostics_car__vif(
        fit                 = fit
      , sw_interp           = sw_interp
      )

    out_diagn[[ "car__vif" ]][[ "car__vif_table" ]] |> print()

    out_diagn[[ "car__vif" ]][[ "car__vif_table" ]] |>
      labelled::get_variable_labels() |> unlist() |> print()

  } # lm or glm

  # write output
  if (sw_write_output & !is.null(out_diagn[[ "car__vif" ]])) {
    # create folder if it doesn't already exist
    dir.create(sw_write_output_path, recursive = TRUE, showWarnings = FALSE)

    output_write <-
      c(
        "\n\n"
      , "====="
      , "\ncar__vif:  "
      , fn_name
      , "\n"
      #, out_diagn[[ "" ]][[ "_interp" ]]
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
    output_write <-
      out_diagn[[ "car__vif" ]][[ "car__vif_table" ]] |>
      dplyr::mutate(
        dplyr::across(tidyselect::where(is.numeric), ~signif(.x, digits = 3))
      ) |>
      knitr::kable(format = sw_kable_format) |>
      as.character()
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
    output_write <-
      c(
        "\n\n"
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
    output_write <-
      c(
        out_diagn[[ "car__vif" ]][[ "car__vif_table" ]] |>
          labelled::get_variable_labels() |> unlist()
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
  } # sw_write_output


  ## Added-Variable Plots
  if (fit_class %in% c("lm", "glm")) {
    out_diagn[[ "car__avPlots" ]] <-
      e_plot_model_diagnostics_car__avPlots(
        fit                 = fit
      , sw_avplot_main_only = c(TRUE, FALSE)[1]
      , sw_interp           = sw_interp
      )

    out_diagn[[ "car__avPlots" ]][[ "car__avPlots_plot"  ]] |> print()

  } # lm or glm

  # write output
  if (sw_write_output & !is.null(out_diagn[[ "car__avPlots" ]])) {
    # create folder if it doesn't already exist
    dir.create(sw_write_output_path, recursive = TRUE, showWarnings = FALSE)

    fn_name <-
      paste0(
        sw_write_output_prefix
      #, "_"
      , "car__avPlots_plot."
      , sw_write_output_plot_fmt
      )
    ggplot2::ggsave(
        file.path(
          sw_write_output_path
        , fn_name
        )
      , plot   = out_diagn[[ "car__avPlots" ]][[ "car__avPlots_plot"  ]]
      , width  = 8
      , height = 1 + 2.5 * ceiling((length(xy_var_names_list$x_var_names__numeric) +
                                  length(unlist(xy_var_names_list$x_var_names__factor_levels)) -
                                  length(xy_var_names_list$x_var_names__factor)  # subtract baseline levels
                                ) / 3)
      , units  = "in"
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      #, useDingbats = FALSE
      )

    output_write <-
      c(
        "\n\n"
      , "====="
      , "\ncar__avPlots:  "
      , fn_name
      , "\n"
      #, out_diagn[[ "" ]][[ "_interp" ]]
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
  } # sw_write_output



  ## Marginal and Conditional Plots
  if (fit_class %in% c("lm", "glm")) {
    out_diagn[[ "car__mcPlots" ]] <-
      e_plot_model_diagnostics_car__mcPlots(
        fit                 = fit
      , sw_avplot_main_only = c(TRUE, FALSE)[1]
      , sw_interp           = sw_interp
      )

    out_diagn[[ "car__mcPlots" ]][[ "car__mcPlots_plot"  ]] |> print()

  } # lm or glm

  # write output
  if (sw_write_output & !is.null(out_diagn[[ "car__mcPlots" ]])) {
    # create folder if it doesn't already exist
    dir.create(sw_write_output_path, recursive = TRUE, showWarnings = FALSE)

    fn_name <-
      paste0(
        sw_write_output_prefix
      #, "_"
      , "car__mcPlots_plot."
      , sw_write_output_plot_fmt
      )
    ggplot2::ggsave(
        file.path(
          sw_write_output_path
        , fn_name
        )
      , plot   = out_diagn[[ "car__mcPlots" ]][[ "car__mcPlots_plot"  ]]
      , width  = 8
      , height = 1 + 2.5 * ceiling((length(xy_var_names_list$x_var_names__numeric) +
                                  length(unlist(xy_var_names_list$x_var_names__factor_levels)) -
                                  length(xy_var_names_list$x_var_names__factor)  # subtract baseline levels
                                ) / 3)
      , units  = "in"
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      #, useDingbats = FALSE
      )

    output_write <-
      c(
        "\n\n"
      , "====="
      , "\ncar__mcPlots:  "
      , fn_name
      , "\n"
      #, out_diagn[[ "" ]][[ "_interp" ]]
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
  } # sw_write_output



  ### Partial residual plots
  ## Component+Residual (Partial Residual) Plots
  if (fit_class %in% c("lm", "glm")) {
    out_diagn[[ "car__crPlots" ]] <-
      e_plot_model_diagnostics_car__crPlots(
        fit                 = fit
      , sw_interp           = sw_interp
      )

    out_diagn[[ "car__crPlots" ]][[ "car__crPlots_plot"  ]] |> print()

  } # lm or glm

  # write output
  if (sw_write_output & !is.null(out_diagn[[ "car__crPlots" ]])) {
    # create folder if it doesn't already exist
    dir.create(sw_write_output_path, recursive = TRUE, showWarnings = FALSE)

    fn_name <-
      paste0(
        sw_write_output_prefix
      #, "_"
      , "car__crPlots_plot."
      , sw_write_output_plot_fmt
      )
    ggplot2::ggsave(
        file.path(
          sw_write_output_path
        , fn_name
        )
      , plot   = out_diagn[[ "car__crPlots" ]][[ "car__crPlots_plot"  ]]
      , width  = 8
      , height = 1 + 4 * ceiling(length(xy_var_names_list$x_var_names__numeric) / 2)
      , units  = "in"
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      #, useDingbats = FALSE
      )

    output_write <-
      c(
        "\n\n"
      , "====="
      , "\ncar__crPlots:  "
      , fn_name
      , "\n"
      #, out_diagn[[ "" ]][[ "_interp" ]]
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
  } # sw_write_output



  ## Ceres (Generalized Partial Residual) Plots
  if (fit_class %in% c("lm", "glm")) {
    out_diagn[[ "car__ceresPlots" ]] <-
      e_plot_model_diagnostics_car__ceresPlots(
        fit                 = fit
      , sw_interp           = sw_interp
      )

    out_diagn[[ "car__ceresPlots" ]][[ "car__ceresPlots_plot"  ]] |> print()

  } # lm or glm

  # write output
  if (sw_write_output & !is.null(out_diagn[[ "car__ceresPlots" ]])) {
    # create folder if it doesn't already exist
    dir.create(sw_write_output_path, recursive = TRUE, showWarnings = FALSE)

    fn_name <-
      paste0(
        sw_write_output_prefix
      #, "_"
      , "car__ceresPlots_plot."
      , sw_write_output_plot_fmt
      )
    ggplot2::ggsave(
        file.path(
          sw_write_output_path
        , fn_name
        )
      , plot   = out_diagn[[ "car__ceresPlots" ]][[ "car__ceresPlots_plot"  ]]
      , width  = 8
      , height = 1 + 4 * ceiling(length(xy_var_names_list$x_var_names__numeric) / 2)
      , units  = "in"
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      #, useDingbats = FALSE
      )

    output_write <-
      c(
        "\n\n"
      , "====="
      , "\ncar__ceresPlots:  "
      , fn_name
      , "\n"
      #, out_diagn[[ "" ]][[ "_interp" ]]
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
  } # sw_write_output



  # glm, DHARMa residual diagnostics
  if (fit_class == "lm") {

    out_diagn[[ "DHARMa_Resid" ]] <-
      NULL
  } # lm
  if (fit_class == "glm") {

    out_diagn[[ "DHARMa_Resid" ]] <-
      e_plot_model_diagnostics_DHARMa_Resid(
        fit                 = fit
      , dat                 = dat
      , sw_interp           = sw_interp
      )

    out_diagn[[ "DHARMa_Resid" ]][[ "DHARMa__AllTests_table" ]] |> print()
    #out_diagn[[ "DHARMa_Resid" ]][[ "DHARMa__AllTests_text"  ]] |> print()
    out_diagn[[ "DHARMa_Resid" ]][[ "DHARMa_Resid_plot"      ]] |> print()

  } # glm

  # write output
  if (sw_write_output & !is.null(out_diagn[[ "DHARMa_Resid" ]])) {
    # create folder if it doesn't already exist
    dir.create(sw_write_output_path, recursive = TRUE, showWarnings = FALSE)

    fn_name <-
      paste0(
        sw_write_output_prefix
      #, "_"
      , "DHARMa_Resid_plot."
      , sw_write_output_plot_fmt
      )
    ggplot2::ggsave(
        file.path(
          sw_write_output_path
        , fn_name
        )
      , plot   = out_diagn[[ "DHARMa_Resid" ]][[ "DHARMa_Resid_plot"      ]]
      , width  = 9
      , height = 7
      , units  = "in"
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      #, useDingbats = FALSE
      )

    output_write <-
      c(
        "\n\n"
      , "====="
      , "\nDHARMa_Resid:  "
      , fn_name
      , "\n"
      #, out_diagn[[ "" ]][[ "_interp" ]]
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
    output_write <-
      out_diagn[[ "DHARMa_Resid" ]][[ "DHARMa__AllTests_table" ]] |>
      dplyr::mutate(
        dplyr::across(tidyselect::where(is.numeric), ~signif(.x, digits = 3))
      ) |>
      knitr::kable(format = sw_kable_format) |>
      as.character()
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)
  } # sw_write_output








  if (fit_class == "lm") {

    ## base graphics are not working
    ## will need to run car and base plots to ggplot format
    #
    # # patchwork model selection plot design
    # #   appears: after model selection if no x variables
    # #            at end to summarize ROC with model selection
    # plot_design <-
    #   #"ADCC
    #   # BECC
    #   # BFFF
    #   # GFFF
    #   # HFFF"
    #   "A#CC
    #    B#CC
    #    B###
    #    ####
    #    ####"
    #
    #
    # p_list <-
    #   list(
    #     cowplot::plot_grid(out_diagn[[ "Resid_histogram"          ]][[ "Resid_histogram_plot" ]]                ) # 1x1
    #   , cowplot::plot_grid(out_diagn[[ "qqplotr"                  ]][[ "qqplotr_grid_detrended_plot" ]]         ) # 2x1
    #   , cowplot::plot_grid(out_diagn[[ "CooksD_Leverage_Resid"    ]][[ "CooksD_Leverage_Resid_arranged_plot" ]] ) # 2x2
    #     #out_diagn[[ "car__influenceIndexPlot"  ]]
    #     #out_diagn[[ "car__outlierTest"         ]]
    #     #out_diagn[[ "car__durbinWatsonTest"    ]]
    #   , cowplot::plot_grid(out_diagn[[ "car__spreadLevelPlot"     ]][[ "car__spreadLevelPlot_plot" ]]           ) # 1x1
    #   , cowplot::plot_grid(out_diagn[[ "car__residualPlots_y"     ]][[ "car__residualPlots_y_plot" ]]           ) # 1x1
    #   , cowplot::plot_grid(out_diagn[[ "car__residualPlots_x"     ]][[ "car__residualPlots_x_plot" ]]           ) # 3x3
    #     #out_diagn[[ "car__dfbetasPlots"        ]]
    #     #out_diagn[[ "gvlma"                    ]]
    #   , cowplot::plot_grid(out_diagn[[ "car__boxCox"              ]][[ "car__boxCox_plot" ]]                    ) # 1x2
    #   , cowplot::plot_grid(out_diagn[[ "car__inverseResponsePlot" ]][[ "car__inverseResponsePlot_plot" ]]       ) # 1x1
    #     #out_diagn[[ "car__marginalModelPlots"  ]]
    #     #out_diagn[[ "car__invTranPlot"         ]]
    #     #out_diagn[[ "car__vif"                 ]]
    #     #out_diagn[[ "car__avPlots"             ]]
    #     #out_diagn[[ "car__mcPlots"             ]]
    #     #out_diagn[[ "car__crPlots"             ]]
    #     #out_diagn[[ "car__ceresPlots"          ]]
    #   )
    #
    #
    #
    #
    # out_diagn[[ "p_arranged" ]] <-
    #   cowplot::plot_grid(out_diagn[[ "Resid_histogram"          ]][[ "Resid_histogram_plot" ]]                ) + # 1x1
    #   cowplot::plot_grid(out_diagn[[ "qqplotr"                  ]][[ "qqplotr_grid_detrended_plot" ]]         ) + # 2x1
    #   cowplot::plot_grid(out_diagn[[ "CooksD_Leverage_Resid"    ]][[ "CooksD_Leverage_Resid_arranged_plot" ]] ) + # 2x2
    #   #cowplot::plot_grid(out_diagn[[ "car__spreadLevelPlot"     ]][[ "car__spreadLevelPlot_plot" ]]           ) + # 1x1
    #   #cowplot::plot_grid(out_diagn[[ "car__residualPlots_y"     ]][[ "car__residualPlots_y_plot" ]]           ) + # 1x1
    #   #cowplot::plot_grid(out_diagn[[ "car__residualPlots_x"     ]][[ "car__residualPlots_x_plot" ]]           ) + # 3x3
    #   #cowplot::plot_grid(out_diagn[[ "car__boxCox"              ]][[ "car__boxCox_plot" ]]                    ) + # 1x2
    #   #cowplot::plot_grid(out_diagn[[ "car__inverseResponsePlot" ]][[ "car__inverseResponsePlot_plot" ]]       ) + # 1x1
    #   patchwork::plot_layout(
    #     design = plot_design
    #   )
    #   # , ncol        = NULL
    #   # , nrow        = NULL
    #   # , byrow       = c(TRUE, FALSE)[1]
    #   # , widths      = NULL
    #   # , heights     = NULL
    #   #, guides      = c("collect", "keep", "auto")[2]
    #   #, tag_level   = c("keep", "new")[1]
    #   # , design      = NULL
    #   # , axes        = NULL
    #   #, axis_titles = c("keep", "collect", "collect_x", "collect_y")[1]
    #   #)
    #   # +
    #   # patchwork::wrap_plots(
    #   #   p_list
    #   # , ncol        = NULL
    #   # , nrow        = NULL
    #   # , byrow       = c(TRUE, FALSE)[1]
    #   # , widths      = NULL
    #   # , heights     = NULL
    #   # , guides      = c("collect", "keep", "auto")[1]
    #   # , tag_level   = c("keep", "new")[1]
    #   # , design      = NULL
    #   # , axes        = NULL
    #   # , axis_titles = c("keep", "collect", "collect_x", "collect_y")[1]
    #   # ) +
    #   # patchwork::plot_layout(design = plot_design) +
    #   #patchwork::plot_annotation(
    #   #  title       = paste0("lm diagnostic plots")
    #   #, subtitle    = text_formula_sel
    #   #, caption     = paste0(
    #   #                  "Caption down here."
    #   #                )
    #   #, tag_levels  = "A"
    #   #, theme = ggplot2::theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1, Caption align left
    #   #)

  } # lm
  if (fit_class == "glm") {

  } # glm

  if (fit_class %in% c("lm", "glm")) {

  } # lm or glm







  ## ## ============================================================ XX
  ## if ("all" %in% sw_plot_set) {
  ##   sw_plot_set <-
  ##     c(
  ##       "boxplot"
  ##     , "cookd"
  ##     , "hist"
  ##     , "index"
  ##     , "ls"
  ##     , "qq"
  ##     , "lev"
  ##     , "resid"
  ##     , "yvp"
  ##     )
  ## }
  ##
  ##
  ##
  ## # Create a boxplot of the residuals if selected in plots otherwise set as NULL
  ## if ("boxplot" %in% sw_plot_set) {
  ##   p_boxplot <-
  ##     ggResidpanel::resid_panel(
  ##       plots = "boxplot"
  ##     , type = rp_type
  ##     , model = fit
  ##     , theme = rp_theme
  ##     , axis.text.size = rp_axis.text.size
  ##     , title.text.size = rp_title.text.size
  ##     , title.opt = rp_title.opt
  ##     )
  ##   #p_boxplot <- p_boxplot + geom_point(alpha = 0.5)
  ##
  ## }
  ##
  ## ###  XXX
  ## "boxplot"
  ## "cookd"
  ## "hist"
  ## "index"
  ## "ls"
  ## "qq"
  ## "lev"
  ## "resid"
  ## "yvp"
  ##
  ##
  ##
  ## # sw_plot_set version.  NA accepts argument switch settings.
  ## if(!is.na(sw_plot_set)) {
  ##   if(sw_plot_set == "all") {
  ##     which_plot = c(4, 6, 1, 2, 3, 5)
  ##   }
  ##   if(sw_plot_set == "simple") {
  ##     #sw_boxcox        <- FALSE
  ##     sw_constant_var  <- FALSE
  ##     sw_collinearity  <- FALSE
  ##     sw_order_of_data <- FALSE
  ##     sw_addedvar      <- FALSE
  ##   }
  ##   if(sw_plot_set == "simpleAV") {
  ##     #sw_boxcox        <- FALSE
  ##     sw_constant_var  <- FALSE
  ##     sw_collinearity  <- FALSE
  ##     sw_order_of_data <- FALSE
  ##   }
  ## }
  ##
  ## # variable names
  ## var_names <- names(fit$model)[-1]
  ## # display settings
  ## if (is.na(rc_mfrow[1])) {
  ##   n_plots <-
  ##     sw_qqplot           +
  ##     length(which_plot)  +
  ##     length(var_names)   +
  ##     sw_boxcox           +
  ##     sw_constant_var     +
  ##     sw_collinearity     +
  ##     sw_order_of_data    +
  ##     sw_addedvar
  ##   rc_mfrow <- c(ceiling(n_plots / 3), 3)
  ##   #rc_mfrow <- c(1, 3)
  ## }
  ## op <- par(no.readonly = TRUE) # the whole list of settable par
  ## par(mfrow = rc_mfrow)
  ##
  ##
  ## # histogram of residuals
  ## hist(
  ##   fit$residuals
  ## , breaks = 20
  ## , freq = FALSE
  ## , main = "Residuals"
  ## , sub = "black = residuals, red = normal"
  ## , xlab = "Residuals"
  ## )
  ## # density curve
  ## lines(
  ##   density(fit$residuals)
  ## , lwd   = 2
  ## , col   = "black"
  ## #, add   = TRUE
  ## )
  ## # normal distribution
  ## f_norm_curve <- function(x) {dnorm(x, mean = 0, sd = sd(fit$residuals))}
  ## graphics::curve(
  ##   expr  = f_norm_curve
  ## , from  = min(fit$residuals) - 0.10 * diff(range(fit$residuals))
  ## , to    = max(fit$residuals) + 0.10 * diff(range(fit$residuals))
  ## , lwd   = 2
  ## , col   = "red"
  ## , add   = TRUE
  ## )
  ##
  ## # Normal quantile plot (QQ-plot)
  ## #library(car)
  ## if(sw_qqplot) {
  ##   car::qqPlot(as.numeric(fit$residuals), las = 1, id = list(n = n_outliers), main = "QQ Plot", ylab = "Residuals")
  ## }
  ##
  ## #library(nortest)
  ## nortest::ad.test(fit$residuals)
  ##
  ##
  ## # Box-Cox transformation suggestion
  ## # only if all values are positive
  ## if(sw_boxcox) {
  ##   if(min(fit$model[,1] > 0)){
  ##     #library(car)  # car::boxCox relies on family="bcPower", but "bcPower" is a function in the car package
  ##     bcPower <- car::bcPower   # load the required function into the environment
  ##     try(  # this may not work if the model function isn't in the environment, or if other objects are not available, too
  ##       car::boxCox(
  ##         fit
  ##       , lambda = seq(-3, 3, length = 101)
  ##       , main = "Box-Cox power transformation"
  ##       , xlab = "lambda\nlambda of 1 is none (y^1); 0 is log(y) of any base"
  ##       )
  ##     )
  ##     rm(bcPower)               # remove it
  ##     abline(v = 1  , col = "orange", lty = 3, lwd = 2)  # horizontal line at zero
  ##   }
  ## }
  ##
  ##
  ## # default: Fitted, Cook's distance (with cutoff), and Leverage (with cutoffs)
  ## for(i_plot in which_plot) {
  ##   par(ask = FALSE)  # do not ask for next plot
  ##   plot(fit, which = i_plot, id.n = n_outliers)
  ##   if (i_plot == 4) {
  ##     Di_large <- 4 / (dim(fit$model)[1] - dim(fit$model)[2] - 1)
  ##     abline(h = Di_large, col = "blue", lty = 3)  # horizontal line
  ##   }
  ##   if (i_plot == 6) {
  ##     lev_large <- c(2, 3) * dim(fit$model)[2] / dim(fit$model)[1]
  ##     abline(h = Di_large    , col = "blue", lty = 3)  # horizontal line
  ##     abline(v = lev_large[1], col = "blue", lty = 3)  # vertical line
  ##     abline(v = lev_large[2], col = "blue", lty = 2)  # vertical line
  ##   }
  ## }
  ##
  ## # residuals plotted vs each main effect
  ## if (length(var_names)) {
  ##   for(i_plot in 1:length(var_names)) {
  ##     m_lab <- paste("Residuals vs.", var_names[i_plot])
  ##     if(inherits(fit$model[,var_names[i_plot]], "character")) {
  ##       message(paste0("e_plot_lm_diagnostics: ", var_names[i_plot], " is character and may need to be a factor."))
  ##     }
  ##     plot(x = fit$model[,var_names[i_plot]], y = fit$residuals, main = m_lab, ylab = "Residuals", xlab = var_names[i_plot])
  ##     abline(h = 0, col = "gray75", lty = 3)  # horizontal line at zero
  ##
  ##     if(inherits(fit$model[,var_names[i_plot]], c("numeric", "integer"))) {
  ##       # use simple smoother if less than 4 observations, otherwise use splines
  ##       if (length(unique(fit$model[,var_names[i_plot]])) < 4) {
  ##         # Loess
  ##         #lines(predict(loess(fit$residuals ~ fit$model[,var_names[i_plot]], enp.target=1)), col="red", lwd=1)
  ##         # Friedman's SuperSmoother
  ##         graphics::lines(
  ##           supsmu(
  ##             x = fit$model[,var_names[i_plot]]
  ##           , y = fit$residuals
  ##           )
  ##         , col = "red"
  ##         , lwd = 1
  ##         )
  ##       } else {
  ##         # if the IQR is 0, we need to increase the tol to be strictly positive
  ##         ss_tol <- 1e-6 * IQR(fit$model[,var_names[i_plot]]) # default
  ##         if (ss_tol == 0) { ss_tol <- 1e-6 * diff(range(fit$model[,var_names[i_plot]])) }
  ##         if (ss_tol == 0) { ss_tol <- 0.1 }
  ##         graphics::lines(
  ##           stats::smooth.spline(
  ##             fit$residuals ~ fit$model[,var_names[i_plot]]
  ##           , spar = 0.8
  ##           , tol = ss_tol
  ##           )
  ##         , col = "red"
  ##         , lwd = 1
  ##         , lty = 1
  ##         )
  ##       }
  ##
  ##     }
  ##   }
  ## }
  ##
  ## # residuals vs order of data
  ## if(sw_order_of_data) {
  ##   # order of data (not always interesting)
  ##   plot(fit$residuals, main = "Residuals vs Order of data", ylab = "Residuals")
  ##   abline(h = 0, col = "gray75", lty = 3)  # horizontal line at zero
  ## }
  ##
  ## # Evaluate homoscedasticity
  ## if (length(var_names)) {
  ##   if(sw_constant_var) {
  ##     #library(car)
  ##     # non-constant error variance test
  ##     try(
  ##       print(car::ncvTest(fit))
  ##     )
  ##     # plot studentized residuals vs. fitted values
  ##     try(
  ##       car::spreadLevelPlot(fit, sub = "(Homoscedasticity)")
  ##     )
  ##   }
  ## }
  ##
  ## # Evaluate Collinearity
  ## if (length(var_names)) {
  ##   if(sw_collinearity) {
  ##     if (length(var_names) >= 2) {
  ##       #library(car)
  ##       vif_val <- car::vif(fit) # variance inflation factors
  ##       graphics::dotchart(vif_val, main = "Collinearity", xlab = "Variance Inflation Factor (VIF)", sub = "Not as useful with interactions")
  ##       abline(v = 0  , col = "gray75", lty = 3)  # horizontal line at zero
  ##       abline(v = 2^2, col = "blue"  , lty = 2)  # vertical line
  ##       abline(v = 10 , col = "blue"  , lty = 3)  # vertical line
  ##       warning("Note: Collinearity plot unreliable for predictors that also have interactions in the model.")
  ##     } else {
  ##       warning("Collinearity plot only available with at least two predictor (x) variables.")
  ##     }
  ##   }
  ## }
  ##
  ## # Evaluate Partial regression residual plot (added-variables plot)
  ## if (length(var_names)) {
  ##   if(sw_addedvar) {
  ##     #library(car)
  ##     car::avPlots(fit, id = list(n = n_outliers), layout = NA, ask = FALSE)
  ##   }
  ## }
  ##
  ##
  ## par(op) # reset plotting options

  #invisible(NULL)

  return(out_diagn)

} # e_plot_model_diagnostics
