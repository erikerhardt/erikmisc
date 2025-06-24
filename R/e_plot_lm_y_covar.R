#' Plot the response vs each predictor in a model formula
#'
#' @param form  lm formula for a model
#' @param dat   data to use
#' @param sw_facet_rows   in \code{ggplot::facet_grid}, number of rows
#' @param sw_facet_cols   in \code{ggplot::facet_grid}, number of columns; \code{NULL} to use as many as necessary
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
#'   )
#' p |> print()
e_plot_lm_y_covar <-
  function(
    form          = form
  , dat           = dat
  , sw_facet_rows = 1
  , sw_facet_cols = NULL
  ) {

  ## plot all covariates vs response

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


  dat_plot <-
    dat |>
    dplyr::select(
      tidyselect::one_of(y_var_name)
    , tidyselect::one_of(x_var_names)
    ) |>
    # convert factor to numeric for plotting
    e_data_convert_factor_to_numeric()

  # reshape long
  dat_plot <-
    dat_plot |>
    tidyr::pivot_longer(
      cols = tidyselect::all_of(x_var_names)
    )

  if (is.null(sw_facet_cols)) {
    sw_facet_cols <- length(form_terms)
  }

  #library(ggplot2)
  p <- ggplot(dat_plot, aes(x = .data[[ "value"]], y = .data[[ y_var_name ]]))
  p <- p + geom_point(alpha = 1/4, position = position_jitter(w = 0.02, h = 0.02))
  p <- p + geom_smooth(method = "gam", formula = y ~ x, size = 1, se = TRUE, span = 1, alpha = 0.25)
  p <- p + theme_bw()
  p <- p + facet_wrap( ~ name, drop = TRUE, scales = "free_x", nrow = sw_facet_rows, ncol = sw_facet_cols)
  p <- p + labs(  title = y_var_name  #labelled::var_label(dat[[ form_terms[1] ]]) |> as.character()
                , subtitle = "Scatterplots with covariates"
                , y = labelled::var_label(dat[[ y_var_name ]]) |> as.character()
                )
  return(p)
} # e_plot_lm_y_covar
