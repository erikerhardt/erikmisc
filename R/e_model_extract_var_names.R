#' From model formula, extract y and x variable (and x interactions) lists
#'
#' @param form    formula of form y ~ x
#'
#' @return out      list with lists of y variable and x variables
#' @importFrom stringr str_detect str_split
#' @export
#'

#' @examples
#' ## From formula
#' # Set specific model with some interactions
#' form_model <-
#'   mpg ~ cyl + disp + hp + wt + vs + am + cyl:vs + disp:hp + hp:vs
#'
#' xy_var_names_list <- e_model_extract_var_names(form_model)
#' xy_var_names_list
#' y_var_name                <- xy_var_names_list$y_var_name
#' y_var_name_glm            <- xy_var_names_list$y_var_name_glm
#' x_var_names               <- xy_var_names_list$x_var_names
#' x_var_names_interactions  <- xy_var_names_list$x_var_names_interactions
#'
#' ## From model fit, lm example
#' dat_sel <-
#'    erikmisc::dat_mtcars_e
#' form_model <-
#'   mpg ~ cyl + disp + hp + wt + vs + am + cyl:vs + disp:hp + hp:vs
#'
#' fit <- lm(form_model, data = dat_sel)
#'
#' xy_var_names_list <- e_model_extract_var_names(formula(fit$terms))
#' xy_var_names_list
#' y_var_name                <- xy_var_names_list$y_var_name
#' y_var_name_glm            <- xy_var_names_list$y_var_name_glm
#' x_var_names               <- xy_var_names_list$x_var_names
#' x_var_names_interactions  <- xy_var_names_list$x_var_names_interactions
#'
#' ## From model fit, glm example
#' dat_sel <-
#'   erikmisc::dat_mtcars_e |>
#'   dplyr::mutate(
#'     am_01 =
#'       dplyr::case_when(
#'         am == "manual"    ~ 0
#'       , am == "automatic" ~ 1
#'       )
#'   )
#' labelled::var_label(dat_sel[["am_01"]]) <- labelled::var_label(dat_sel[["am"]])
#' form_model <-
#'   cbind(am_01, 1 - am_01) ~ cyl + disp + hp + wt + vs + hp:vs
#'
#' fit <- glm(form_model, data = dat_sel, family  = binomial(link = logit))
#'
#' xy_var_names_list <- e_model_extract_var_names(formula(fit$terms))
#' xy_var_names_list
#' y_var_name                <- xy_var_names_list$y_var_name
#' y_var_name_glm            <- xy_var_names_list$y_var_name_glm
#' x_var_names               <- xy_var_names_list$x_var_names
#' x_var_names_interactions  <- xy_var_names_list$x_var_names_interactions
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

  # interactions
  x_var_names_interactions <-
    form |>
    terms() |>
    attr("term.labels") |>
    as.character() |>
    stringr::str_subset(
      pattern = stringr::fixed(":")
    )


  # for glm, extract the "y" from "cbind(y, 1-y)"
  y_var_name_components <-
    stringr::str_split(
      string    = y_var_name
    , pattern   = "\\(|,"
    ) |>
    unlist()
  if (y_var_name_components[1] == "cbind") {
    y_var_name_glm <-
      y_var_name
    y_var_name <-
      y_var_name_components[2]
  } else {
    y_var_name_glm <-
      NULL
  }

  out <-
    list(
      y_var_name                = y_var_name
    , y_var_name_glm            = y_var_name_glm
    , x_var_names               = x_var_names
    , x_var_names_interactions  = x_var_names_interactions
    )

  return(out)

} # e_model_extract_var_names

