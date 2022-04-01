#' All model subsets
#'
#' Return all possible model subsets based on specified model scopes.
#'
#' @param var_formula   NOT YET IMPLEMENTED
#' @param x_var_names   a character list of variable names or list of character lists.
#'                        If a list of multiple formulas or character lists is provided,
#'                        then each is subject to the \code{max_scope} independently,
#'                        or each can have it's own \code{max_scope}.
#' @param y_var_name    a character string
#' @param max_scope     one of or a list (corresponding to \code{x_var_names}) of
#'                        "always" to always keep this full list (no binary subsets),
#'                        "FO" for first-order (main) effects,
#'                        "TWI" for two-way interactions,
#'                        "PQ" for pure quadratic terms (squares of the "FO"), and
#'                        "SO" for all terms in "FO", "TWI", and "PQ".
#' @param sw_return     return a list of "formula", or a "table" of x-variable combinations
#'
#' @return              a list of functions
#'
#' @import dplyr
#' @importFrom utils combn
#' @importFrom tibble tibble
#' @importFrom tidyselect ends_with
#' @importFrom stringr str_subset
#' @importFrom stringr str_replace
#' @importFrom stringr fixed
#'
#' @export
#'
#' @examples
#' # Two-way interaction model, return formulas
#' e_model_all_subsets_formula(
#'     var_formula = NULL
#'   , x_var_names = c("a", "b", "c")
#'   , y_var_name  = "z"
#'   , max_scope   = "TWI"
#'   , sw_return   = c("formula", "table")[1]
#'   )
#' # Two-way interaction model, return table of x-variable combinations
#' e_model_all_subsets_formula(
#'     var_formula = NULL
#'   , x_var_names = c("a", "b", "c")
#'   , y_var_name  = "z"
#'   , max_scope   = "TWI"
#'   , sw_return   = c("formula", "table")[2]
#'   )
#'
#' # Two variables always in the model with a second-order model of two others
#' e_model_all_subsets_formula(
#'     var_formula = NULL
#'   , x_var_names = list(c("a", "b"), c("c", "d"))
#'   , y_var_name  = "z"
#'   , max_scope   = list("always", "SO")
#'   , sw_return   = c("formula", "table")[2]
#'   )
#' \dontrun{
#' # Large example showing all options
#' e_model_all_subsets_formula(
#'     var_formula = NULL
#'   , x_var_names = list(letters[1:2], letters[4:5], letters[7:8], letters[10:11], letters[13:14])
#'   , y_var_name  = "z"
#'   , max_scope   = list("always", "FO", "TWI", "PQ", "SO")
#'   , sw_return   = c("formula", "table")[2]
#'   )
#' }
#'
e_model_all_subsets_formula <-
  function(
    var_formula = NULL
  , x_var_names = NULL
  , y_var_name  = NULL
  , max_scope   = c("always", "FO", "TWI", "PQ", "SO")[3]
  , sw_return   = c("formula", "table")[1]
  ) {
  ## var_formula  = NULL
  ## x_var_names  = list(letters[1:2], letters[4:5], letters[7:8], letters[10:11], letters[13:14])
  ## y_var_name   = "z"
  ## max_scope    = list("always", "FO", "TWI", "PQ", "SO")


  # if formula, convert to character list
  if ( !is.null(var_formula) ) {

    warning("erikmisc::e_model_all_subsets_formula, var_formula not yet implemented.")
    return(NULL)

    #if ( class(var_formula) == "formula" ) {
    #var_list <- attr(terms(var_list), "term.labels")
  }



  if ( class(x_var_names) == "character" ) {
    x_var_names <- list(x_var_names)
  }

  # at this point, it should be in a list
  if ( !(class(x_var_names) == "list") ) {
    stop("erikmisc::e_model_all_subsets_formula, x_var_names did not become a list.")
  }

  # length of lists
  n_var_lists <-
    x_var_names %>%
    length()

  # make max_scope a list as long as the x_var_names
  if ( class(max_scope) == "character" ) {
    temp_max_scope <- max_scope
    max_scope <- list()
    for (i_list in seq_len(n_var_lists)) {
      max_scope[[i_list]] <- temp_max_scope
    }
  }

  if (!(length(max_scope) == n_var_lists)) {
    stop("erikmisc::e_model_all_subsets_formula, max_scope and x_var_names list lengths differ.")
  }


  # location to store completed models
  sub_models <- list()

  for (i_list in seq_len(n_var_lists)) {
    ## i_list = 1
    sub_models[[i_list]] <- list()

    this_x_var_names <- x_var_names[[i_list]]

    mod_lists <- list()
    mod_lists[["always"]] <- list()
    mod_lists[["FO"    ]] <- list()
    mod_lists[["TWI"   ]] <- list()
    mod_lists[["PQ"    ]] <- list()

    # if always, then no binary selection, so keep these variables and move to the next list
    if (max_scope[[i_list]] == "always") {
      sub_models[[i_list]] <-
        matrix(
          data     = this_x_var_names
        , nrow     = 1
        , dimnames = list(NULL, this_x_var_names)
        ) %>%
        #tibble::as_tibble()
        as.data.frame()

      # go to next list
      next
    }

    # FO
    FO_binary <-
      this_x_var_names %>%
      e_model_binary_complete()

    # FO lists, this indexes the TWI and SO lists
    for (i_FO in seq_len(nrow(FO_binary))) {
      mod_lists[["FO" ]][[i_FO]] <- FO_binary[i_FO, ]
      mod_lists[["TWI"]][[i_FO]] <- tibble::tibble(TWI_NONE__ = "")
      mod_lists[["PQ" ]][[i_FO]] <- tibble::tibble(PQ_NONE__  = "")
    } # i_FO

    for (i_FO in seq_len(nrow(FO_binary))) {
      ## i_FO = 8

      # PQ
      if (max_scope[[i_list]] %in% c("PQ", "SO")) {
        ind_PQ <- which(as.character(mod_lists[["FO" ]][[i_FO]]) %in% names(mod_lists[["FO" ]][[i_FO]]))
        # if at least one variable
        if (length(ind_PQ) >= 1) {
          PQ_binary <-
            #paste0(names(mod_lists[["FO" ]][[i_FO]])[ind_PQ], "_2") %>%
            paste0("I(", names(mod_lists[["FO" ]][[i_FO]])[ind_PQ], "^2)") %>%
            e_model_binary_complete()
          mod_lists[["PQ" ]][[i_FO]] <- PQ_binary
        }
      } # PQ

      # TWI
      if (max_scope[[i_list]] %in% c("TWI", "SO")) {
        ind_TWI <- which(as.character(mod_lists[["FO" ]][[i_FO]]) %in% names(mod_lists[["FO" ]][[i_FO]]))
        # if at least two variables
        if (length(ind_TWI) >= 2) {
          TWI_binary_pairs <-
            combn(
              x = names(mod_lists[["FO" ]][[i_FO]])[ind_TWI]
            , m = 2
            , simplify = FALSE
            )

          for (i_TWI_pairs in seq_len(length(TWI_binary_pairs))) {
            ## i_TWI_pairs = 1
            TWI_binary_pairs[[i_TWI_pairs]] <-
              paste0(TWI_binary_pairs[[i_TWI_pairs]], collapse = ":")
          }

          TWI_binary_pairs <-
            TWI_binary_pairs %>%
            unlist()

          TWI_binary <-
            TWI_binary_pairs %>%
            e_model_binary_complete()
          mod_lists[["TWI"]][[i_FO]] <- TWI_binary
        }
      } # TWI

      sub_models[[i_list]][[i_FO]] <-
        e_expand_grid_df(
          mod_lists[["FO" ]][[i_FO]]
        , mod_lists[["TWI"]][[i_FO]]
        , mod_lists[["PQ" ]][[i_FO]]
        )

    } # i_FO

    sub_models[[i_list]] <-
      sub_models[[i_list]] %>%
      dplyr::bind_rows()

  } # i_list


  ## Combine across x_var_names lists
  sub_models_combine <-
    sub_models[[1]] %>%
    dplyr::select(
      -tidyselect::ends_with("NONE__")
    )

  if (n_var_lists > 1) {
    for (i_list in 2:n_var_lists) {
      ## i_list = 2

      sub_models_combine <-
        e_expand_grid_df(
          sub_models_combine
        , sub_models[[i_list]] %>%
            dplyr::select(
              -tidyselect::ends_with("NONE__")
            )
        )

    } # i_list
  } # if n_var_lists


  # turn in to formulas
  sub_models_combine_formulas <- list()

  for (i_row in seq_len(nrow(sub_models_combine))) {
    ## i_row = 20
    this_x_var_names <-
      sub_models_combine[i_row, ] %>%
      as.character() %>%
      na.omit() %>%
      stringr::str_subset(
        pattern = "."  # non-blank
      )

    if (length(this_x_var_names) == 0) {
      this_x_var_names <- 1  # intercept-only
    }

    sub_models_combine_formulas[[ i_row ]] <-
      paste0(
        y_var_name
      , " ~ "
      , paste0(
          this_x_var_names
        , collapse = " + "
        )
      ) %>%
      formula()
  } # i_row

  if (sw_return == "formula") {
    return(sub_models_combine_formulas)
  }
  if (sw_return == "table") {
    return(sub_models_combine)
  }

} # e_model_all_subsets_formula

