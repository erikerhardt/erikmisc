#' Plot the response vs each predictor in a model formula
#'
#' @param form  lm formula for a model
#' @param dat   data to use
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
    form = form
  , dat  = dat
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

  # pull out those covariates into a dataset (along with response, time, and program)

  dat_plot <-
    dat |>
    dplyr::select(
      tidyselect::one_of(form_terms[1])
    #, Time_y
    , tidyselect::one_of(form_terms)
    ) |>
    # convert factor to numeric for plotting
    e_data_convert_factor_to_numeric()

  ind_form_terms_covar <-
    stringr::str_detect(string = form_terms, pattern = form_terms[1]       , negate = TRUE)

  # reshape long
  dat_plot <-
    dat_plot |>
    tidyr::pivot_longer(
      cols = form_terms[ind_form_terms_covar]
    )

  library(ggplot2)
  p <- ggplot(dat_plot, aes_string(x = "value", y = form_terms[1])) #, colour = "surv_prog.factor", shape = "surv_prog.factor", group = "surv_prog.factor"))
  p <- p + geom_point(alpha = 1/4, position = position_jitter(w = 0.2, h = 0.2))
  p <- p + geom_smooth(method = "gam", formula = y ~ x, size = 1, se = TRUE, span = 1, alpha = 0.25)
  p <- p + theme_bw()
  p <- p + facet_grid(. ~ name, drop = TRUE, scales = "free_x")
  p <- p + labs(  title = form_terms[1]  #labelled::var_label(dat[[ form_terms[1] ]]) |> as.character()
                , subtitle = "Scatterplots with covariates"
                , y = form_terms[1]  #labelled::var_label(dat[[ form_terms[1] ]]) |> as.character()
                )
  return(p)
} # e_plot_lm_y_covar
