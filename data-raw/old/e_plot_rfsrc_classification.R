#' Plots Random Forests classification workflow using \code{randomForestSRC}
#'
#' @param out_e_rf        output from \code{e_rfsrc_classification()}, a list with many RF objects, summaries, and plots
#' @param path            path to write plot files
#' @param prefix          prefix for plot files
#' @param image_format    png, pdf, or other formats supported by ggplot2::ggsave()
#' @param y_level         NULL for all, otherwise specify one level (by name), one from \code{level(dat_rf_class[, 1])}
#'
#' @return NULL, invisibly
#' @import dplyr
#' @import ggplot2
#' @import cowplot
#' @export
#'
#' @examples
#' ## Same example from e_rfsrc_classification()
#' dat_rf_class <-
#'   erikmisc::dat_mtcars_e |>
#'   dplyr::select(
#'     -model
#'   ) |>
#'   dplyr::select(
#'     cyl
#'   , tidyselect::everything()
#'   )
#'
#' out_e_rf <-
#'   e_rfsrc_classification(
#'     dat_rf_class    = dat_rf_class
#'   , rf_y_var        = NULL
#'   , rf_x_var        = NULL
#'   , sw_rfsrc_ntree  = 200
#'   , sw_alpha        = 0.05
#'   )
#'
#' # Plots
#' e_plot_rfsrc_classification(
#'     out_e_rf     = out_e_rf
#'   , path         = NULL
#'   , prefix       = NULL
#'   , image_format = c("png", "pdf", "tiff")[1]
#'   , y_level      = NULL
#'   )
#'
e_plot_rfsrc_classification <-
    function(
    out_e_rf     = out_e_rf
  , path         = NULL
  , prefix       = NULL
  , image_format = c("png", "pdf", "tiff")[1]
  , y_level      = NULL
  ) {

  ## path         = "C:/Users/erike/Desktop/temp"
  ## prefix       = "This_Plot"
  ## image_format = c("png", "pdf", "tiff")[1]
  ## y_level      = NULL

  if(is.null(path)) {
    path <- ""
  }
  if(is.null(prefix)) {
    prefix <- ""
  } else {
    prefix <- paste0(prefix, "__")
  }
  if(is.null(image_format)) {
    image_format <- "png"
  }

  y_var_levels <-
    out_e_rf[[ "o_class" ]]$yvar |>
    levels()


  ## Overall summaries

  # Summary of Full and reduced model fits with ROC curves
  ggplot2::ggsave(
      filename =
        file.path(
          path
        , paste0(
            prefix
          , "plot_rf_train_all_summary"
          , "."
          , image_format
          )
        )
    , plot   =
        out_e_rf[[ "plot_rf_train_all_summary" ]]
    , width  = 16
    , height = 16
    ## png, jpeg
    , dpi    = 300
    , bg     = "white"
    ## pdf
    , units  = "in"
    #, useDingbats = FALSE  # https://stackoverflow.com/questions/9992275/ggplot2-pdf-import-in-adobe-illustrator-missing-font-adobepistd
    )


  # Reduced model: ROC object from e_plot_roc()
  ggplot2::ggsave(
      filename =
        file.path(
          path
        , paste0(
            prefix
          , "plot_o_class_sel_ROC_ALL"
          , "."
          , image_format
          )
        )
    , plot   =
        out_e_rf[[ "plot_o_class_sel_ROC" ]]$plot_roc |>
        cowplot::plot_grid(plotlist = _, nrow = 1)
    , width  = 5 * length(out_e_rf[[ "plot_o_class_sel_ROC" ]]$plot_roc)
    , height = 5
    ## png, jpeg
    , dpi    = 300
    , bg     = "white"
    ## pdf
    , units  = "in"
    #, useDingbats = FALSE  # https://stackoverflow.com/questions/9992275/ggplot2-pdf-import-in-adobe-illustrator-missing-font-adobepistd
    )

  for (i_level in seq_along(y_var_levels)) {
    ## i_level = 1

    n_level <- y_var_levels[i_level]

    ggplot2::ggsave(
        filename =
          file.path(
            path
          , paste0(
              prefix
            , "plot_o_class_sel_ROC_", n_level
            , "."
            , image_format
            )
          )
      , plot   =
          out_e_rf[[ "plot_o_class_sel_ROC" ]]$plot_roc[[ n_level ]]
      , width  = 5
      , height = 5
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      , units  = "in"
      #, useDingbats = FALSE  # https://stackoverflow.com/questions/9992275/ggplot2-pdf-import-in-adobe-illustrator-missing-font-adobepistd
      )
  } # i_level


  # Reduced model: Partial effects plots
    # for each level of the response variable
  if (is.null(y_level)) {
    for (i_level in seq_along(y_var_levels)) {
      ## i_level = 1

      n_level <- y_var_levels[i_level]

      ggplot2::ggsave(
          filename =
            file.path(
              path
            , paste0(
                prefix
              , "plot_o_class_sel_partial_effects_", n_level
              , "."
              , image_format
              )
            )
        , plot   =
            out_e_rf[[ "plot_o_class_sel_partial_effects" ]][[ i_level ]] |>
            cowplot::plot_grid()
        , width  = 16
        , height = 4 * ceiling(length(out_e_rf[[ "rf_x_var_sel" ]]) / 4)
        ## png, jpeg
        , dpi    = 300
        , bg     = "white"
        ## pdf
        , units  = "in"
        #, useDingbats = FALSE  # https://stackoverflow.com/questions/9992275/ggplot2-pdf-import-in-adobe-illustrator-missing-font-adobepistd
        )
    } # i_level
  } else {

    ## i_level = 1

    i_level <- which(y_var_levels %in% y_level)

    if (length(i_level) == 1) {
      n_level <- y_var_levels[i_level]

      ggplot2::ggsave(
          filename =
            file.path(
              path
            , paste0(
                prefix
              , "plot_o_class_sel_partial_effects_", n_level
              , "."
              , image_format
              )
            )
        , plot   =
            out_e_rf[[ "plot_o_class_sel_partial_effects" ]][[ i_level ]] |>
            cowplot::plot_grid()
        , width  = 16
        , height = 4 * ceiling(length(out_e_rf[[ "rf_x_var_sel" ]]) / 4)
        ## png, jpeg
        , dpi    = 300
        , bg     = "white"
        ## pdf
        , units  = "in"
        #, useDingbats = FALSE  # https://stackoverflow.com/questions/9992275/ggplot2-pdf-import-in-adobe-illustrator-missing-font-adobepistd
        )
    } else {
      warning(paste0("erikmisc::e_plot_rfsrc_classification, y_level argument (", y_level, ") does not match a level of y: ", paste(names(out_e_rf[[ "plot_o_class_sel_ROC" ]]$plot_roc), collapse = ", ")))
    }

  } # is.null(y_level)


  # Reduced model: Marginal effects plots
    # for each level of the response variable
  if (is.null(y_level)) {
    for (i_level in seq_along(y_var_levels)) {
      ## i_level = 1

      n_level <- y_var_levels[i_level]

      ggplot2::ggsave(
          filename =
            file.path(
              path
            , paste0(
                prefix
              , "plot_o_class_sel_marginal_effects_", n_level
              , "."
              , image_format
              )
            )
        , plot   =
            out_e_rf[[ "plot_o_class_sel_marginal_effects" ]][[ i_level ]] |>
            cowplot::plot_grid()
        , width  = 16
        , height = 4 * ceiling(length(out_e_rf[[ "rf_x_var_sel" ]]) / 4)
        ## png, jpeg
        , dpi    = 300
        , bg     = "white"
        ## pdf
        , units  = "in"
        #, useDingbats = FALSE  # https://stackoverflow.com/questions/9992275/ggplot2-pdf-import-in-adobe-illustrator-missing-font-adobepistd
        )
    } # i_level
  } else {

    ## i_level = 1

    i_level <- which(y_var_levels %in% y_level)

    if (length(i_level) == 1) {
      n_level <- y_var_levels[i_level]

      ggplot2::ggsave(
          filename =
            file.path(
              path
            , paste0(
                prefix
              , "plot_o_class_sel_marginal_effects_", n_level
              , "."
              , image_format
              )
            )
        , plot   =
            out_e_rf[[ "plot_o_class_sel_marginal_effects" ]][[ i_level ]] |>
            cowplot::plot_grid()
        , width  = 16
        , height = 4 * ceiling(length(out_e_rf[[ "rf_x_var_sel" ]]) / 4)
        ## png, jpeg
        , dpi    = 300
        , bg     = "white"
        ## pdf
        , units  = "in"
        #, useDingbats = FALSE  # https://stackoverflow.com/questions/9992275/ggplot2-pdf-import-in-adobe-illustrator-missing-font-adobepistd
        )
    } else {
      warning(paste0("erikmisc::e_plot_rfsrc_classification, y_level argument (", y_level, ") does not match a level of y: ", paste(names(out_e_rf[[ "plot_o_class_sel_ROC" ]]$plot_roc), collapse = ", ")))
    }

  } # is.null(y_level)


  ## Full model summaries
  # y response variable
  out_e_rf[[ "rf_y_var" ]]
  # Full model: x predictor variables
  out_e_rf[[ "rf_x_var" ]]
  # Full model: formula
  out_e_rf[[ "rf_formula" ]]
  # Full model: rfsrc classification object
  out_e_rf[[ "o_class" ]]
  # Full model: convergence
  out_e_rf[[ "plot_o_class" ]] |>
    cowplot::plot_grid()
  # Full model: variable importance (VIMP) table
  out_e_rf[[ "o_class_importance" ]]
  # Full model: variable importance (VIMP) plot
  out_e_rf[[ "plot_o_class_importance" ]]
  # Full model: ROC AUC (area under the curve)
  out_e_rf[[ "o_class_AUC" ]]
  # Full model: subsample iterates for VIMP and model selection
  out_e_rf[[ "o_class_subsample" ]]
  # Full model: subsample VIMP model selection
  out_e_rf[[ "o_class_subsample_extract_subsample" ]]
  # Full model: subsample double bootstrap VIMP model selection
  out_e_rf[[ "o_class_subsample_extract_bootsample" ]]
  # Full model: variable importance (VIMP) plot boxplots
  out_e_rf[[ "plot_o_class_subsample" ]] |>
    cowplot::plot_grid()

  ## Reduced model summaries
  # Reduced model: x predictor variables
  out_e_rf[[ "rf_x_var_sel" ]]
  # Reduced model: formula
  out_e_rf[[ "rf_formula_sel" ]]
  # Reduced model: rfsrc classification object
  out_e_rf[[ "o_class_sel" ]]
  # Reduced model: convergence
  out_e_rf[[ "plot_o_class_sel" ]] |>
    cowplot::plot_grid()
  # Reduced model: variable importance (VIMP) table
  out_e_rf[[ "o_class_sel_importance" ]]
  # Reduced model: variable importance (VIMP) plot
  out_e_rf[[ "plot_o_class_sel_importance" ]]
  # Reduced model: ROC AUC (area under the curve)
  out_e_rf[[ "o_class_sel_AUC" ]]
  # Reduced model: subsample iterates for VIMP and model selection
  out_e_rf[[ "o_class_sel_subsample" ]]
  # Reduced model: subsample VIMP model selection
  out_e_rf[[ "o_class_sel_subsample_extract_subsample" ]]
  # Reduced model: subsample double bootstrap VIMP model selection
  out_e_rf[[ "o_class_sel_subsample_extract_bootsample" ]]
  # Reduced model: variable importance (VIMP) plot boxplots
  out_e_rf[[ "plot_o_class_sel_subsample" ]] |>
    cowplot::plot_grid()

  # add oob predictions, match with ID number (if possible)
    # to original function (e_rfsrc_classification.R), option to include an ID
    # then exclude that column from the modeling
    # but join the ID back with the predictions.


  invisible(NULL)

} # e_plot_rfsrc_classification
