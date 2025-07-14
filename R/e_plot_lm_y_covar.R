#' Plot the response vs each predictor in a model formula
#'
#' @param form  lm formula for a model
#' @param dat   data to use
#' @param sw_facet_rows   in \code{ggplot::facet_grid}, number of rows
#' @param sw_facet_cols   in \code{ggplot::facet_grid}, number of columns; \code{NULL} to use as many as necessary
#' @param sw_version      "simple" for a single row of plots, "detail" for a grid of scatterplots with additional details
#'
#' @return    p ggplot facet scatterplot
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect one_of
#' @import dplyr
#' @import ggplot2
#' @export
#'
#' @examples
#' # Set specific model with some interactions
#' form_model <-
#'   mpg ~ cyl + disp + hp + wt + vs + am + cyl:vs + disp:hp + hp:vs
#'
#' p <-
#'   e_plot_lm_y_covar(
#'     form = form_model
#'   , dat  = erikmisc::dat_mtcars_e
#'   , sw_version    = c("simple", "detail")[1]
#'   )
#' p |> print()
#'
#' p <-
#'   e_plot_lm_y_covar(
#'     form = form_model
#'   , dat  = erikmisc::dat_mtcars_e
#'   , sw_version    = c("simple", "detail")[2]
#'   )
#' p |> print()
e_plot_lm_y_covar <-
  function(
    form          = form
  , dat           = dat
  , sw_facet_rows = 1
  , sw_facet_cols = NULL
  , sw_version    = c("simple", "detail")[2]
  ) {

  ## plot all covariates vs response

  # decompose formula into each covariate
  xy_var_names_list <- e_model_extract_var_names(form)
  #xy_var_names_list
  y_var_name                <- xy_var_names_list$y_var_name
  #y_var_name_glm            <- xy_var_names_list$y_var_name_glm
  x_var_names               <- xy_var_names_list$x_var_names
  #x_var_names_interactions  <- xy_var_names_list$x_var_names_interactions

  dat_plot <-
  dat |>
  dplyr::select(
    tidyselect::one_of(y_var_name)
  , tidyselect::one_of(x_var_names)
  ) |>
  tidyr::drop_na()

  # simple
  if (sw_version    == c("simple", "detail")[1]) {
    dat_plot <-
      dat_plot |>
      # convert factor to numeric for plotting
      e_data_convert_factor_to_numeric()

    # reshape long
    dat_plot <-
      dat_plot |>
      tidyr::pivot_longer(
        cols = tidyselect::all_of(x_var_names)
      )

    if (is.null(sw_facet_cols)) {
      sw_facet_cols <- length(x_var_names)
    }

    #library(ggplot2)
    p <- ggplot(dat_plot, aes(x = .data[[ "value"]], y = .data[[ y_var_name ]]))
    p <- p + geom_point(alpha = 1/4, position = position_jitter(w = 0.02, h = 0.02))
    p <- p + geom_smooth(method = "gam", formula = y ~ x, linewidth = 1, se = TRUE, span = 1, alpha = 0.25)
    p <- p + theme_bw()
    p <- p + facet_wrap( ~ name, drop = TRUE, scales = "free_x", nrow = sw_facet_rows, ncol = sw_facet_cols)
    p <- p + labs(  title = y_var_name  #labelled::var_label(dat[[ form_terms[1] ]]) |> as.character()
                  , subtitle = "Scatterplots with covariates"
                  , y = labelled::var_label(dat[[ y_var_name ]]) |> as.character()
                  )
    return(p)
  } # simple

  # detail
  if (sw_version    == c("simple", "detail")[2]) {

    ## Plot scatterplots of y vs each x
    p_list_scatterplot <- list()
    for (i_covar in seq_along(x_var_names)) {
      ## i_covar = 5
      p_list_scatterplot[[ i_covar ]] <-
        e_plot_scatterplot(
          dat_plot                  = dat_plot
        , var_x                     = x_var_names[i_covar]
        , var_y                     = y_var_name
        , var_color                 = NULL
        , var_facet                 = NULL
        , text_title                = NULL
        , sw_print                  = FALSE
        , smooth_all                = c("none", "loess", "lm", "glm", "gam")[2]
        , sw_smooth_all_se          = c(TRUE, FALSE)[1]
        , smooth_by_var_color       = c("none", "loess", "lm", "glm", "gam")[1]
        , sw_smooth_by_var_color_se = c(TRUE, FALSE)[2]
        , sw_corr_in_subtitle       = c(TRUE, FALSE)[1]
        , sw_var_x_name_str_wrap_width  = 40
        , sw_var_y_name_str_wrap_width  = 40
        )
     } # i_covar

    p_arranged <-
      patchwork::wrap_plots(
        p_list_scatterplot
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
        title       = paste0("Bivariate plots of ", y_var_name, " with each predictor")
      #, subtitle    = text_formula_sel
      , caption     = paste0(
                        "Observations with missing values have been removed."
                      )
      , tag_levels  = "A"
      , theme = theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1, Caption align left
      )

    return(p_arranged)
  } # detail

} # e_plot_lm_y_covar
