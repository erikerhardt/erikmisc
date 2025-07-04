#' From model formula, extract y and x variable lists
#'
#' @param form    formula of form y ~ x
#'
#' @return out      list with lists of y variable and x variables
#' @importFrom stringr str_detect str_split
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

