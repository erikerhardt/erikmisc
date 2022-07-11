#' All model subsets
#'
#' Return all possible model subsets based on specified model scopes.
#'
#' @param x_var_names   a character list of variable names or list of character lists.
#'                        If a list of multiple formulas or character lists is provided,
#'                        then each is subject to the \code{max_scope} independently,
#'                        or each can have it's own \code{max_scope}.
#' @param y_var_name    a character string
#' @param var_formula   NOT YET IMPLEMENTED
#' @param max_scope     one of or a list (corresponding to \code{x_var_names}) of
#'                        "always" to always keep this full list (no binary subsets),
#'                        "FO" for first-order (main) effects,
#'                        "TWI" for two-way interactions,
#'                        "PQ" for pure quadratic terms (squares of the "FO"),
#'                        "SO" for all terms in "FO", "TWI", and "PQ", and
#'                        "one_TWI" for two-way interactions with only the first in the list.
#'                        The options that start with "only_" must be used with
#'                        the same \code{x_var_names} variables as a subset in an "always" list.
#'                        For example, \code{c("a", "b")} specified as both "always" and "only_TWI"
#'                        will always keep the main effects performing selection only on the TWIs.
#' @param sw_return     return a list of "formula", or a "table" of x-variable combinations, or a list of "both"
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
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#' # Two-way interaction model, return formulas
#' e_model_all_subsets_formula(
#'     x_var_names = c("a", "b", "c")
#'   , y_var_name  = "z"
#'   , var_formula = NULL
#'   , max_scope   = "TWI"
#'   , sw_return   = c("both", "formula", "table")[2]
#'   )
#' # Two-way interaction model, return table of x-variable combinations
#' e_model_all_subsets_formula(
#'     x_var_names = c("a", "b", "c")
#'   , y_var_name  = "z"
#'   , var_formula = NULL
#'   , max_scope   = "TWI"
#'   , sw_return   = c("both", "formula", "table")[3]
#'   )
#'
#' # Two variables always in the model with a second-order model of two others
#' e_model_all_subsets_formula(
#'     x_var_names = list(c("a", "b"), c("c", "d"))
#'   , y_var_name  = "z"
#'   , var_formula = NULL
#'   , max_scope   = list("always", "SO")
#'   , sw_return   = c("both", "formula", "table")[3]
#'   )
#' # One variable in two-way interactions with the other variables
#' e_model_all_subsets_formula(
#'     x_var_names = list(c("a", "b", "c", "d"))
#'   , y_var_name  = "z"
#'   , var_formula = NULL
#'   , max_scope   = list("one_TWI")
#'   , sw_return   = c("both", "formula", "table")[3]
#'   )
#' # Four variables always in the model (a, b, c, d)
#' #   with selection only on the two second-order variables (c, d),
#' #   with TWI on the two variables (e, f)
#' e_model_all_subsets_formula(
#'     x_var_names = list(c("a", "b", "c", "d"), c("c", "d"), c("e", "f"))
#'   , y_var_name  = "z"
#'   , var_formula = NULL
#'   , max_scope   = list("always", "only_SO", "TWI")
#'   , sw_return   = c("both", "formula", "table")[3]
#'   )
#' \dontrun{
#' # Large example showing all options
#' e_model_all_subsets_formula(
#'     x_var_names = list(letters[1:2], letters[4:5], letters[7:8], letters[10:11], letters[13:14])
#'   , y_var_name  = "z"
#'   , var_formula = NULL
#'   , max_scope   = list("always", "FO", "TWI", "PQ", "SO")
#'   , sw_return   = c("both", "formula", "table")[3]
#'   )
#' }
#'
e_model_all_subsets_formula <-
  function(
    x_var_names = NULL
  , y_var_name  = NULL
  , var_formula = NULL
  , max_scope   = c("always", "FO", "TWI", "PQ", "SO", "one_TWI", "only_TWI", "only_PQ", "only_SO", "only_one_TWI")[3]
  , sw_return   = c("both", "formula", "table")[1]
  ) {
  ## var_formula  = NULL
  ## x_var_names  = list(letters[1:2], letters[4:5], letters[7:8], letters[10:11], letters[13:14], letters[15:18])
  ## y_var_name   = "z"
  ## max_scope    = list("always", "FO", "TWI", "PQ", "SO", "one_TWI")
  ##
  ## var_formula  = NULL
  ## x_var_names  = list(letters[1:2], letters[15:18])
  ## y_var_name   = "z"
  ## max_scope    = list("always", "one_TWI")
  ##
  ## var_formula  = NULL
  ## x_var_names  = list(letters[1:2], letters[3:5], letters[1:2])
  ## y_var_name   = "z"
  ## max_scope    = list("always", "FO", "only_SO")


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
    mod_lists[["always"      ]] <- list()
    mod_lists[["FO"          ]] <- list()
    mod_lists[["TWI"         ]] <- list()
    mod_lists[["PQ"          ]] <- list()
    mod_lists[["one_TWI"     ]] <- list()
    mod_lists[["only_TWI"    ]] <- list()
    mod_lists[["only_PQ"     ]] <- list()
    mod_lists[["only_SO"     ]] <- list()
    mod_lists[["only_one_TWI"]] <- list()

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

    # if only_*, this is used with "always", so only keep last "complete" row
    if (max_scope[[i_list]] %in% c("only_TWI", "only_PQ", "only_SO", "only_one_TWI")) {
      FO_binary <-
        FO_binary %>%
        dplyr::filter(
          dplyr::row_number() == dplyr::n()
        )
    }


    # FO lists, this indexes the TWI, SO, and one_TWI , and only_* lists
    for (i_FO in seq_len(nrow(FO_binary))) {
      mod_lists[["FO"          ]][[i_FO]] <- FO_binary[i_FO, ]
      mod_lists[["TWI"         ]][[i_FO]] <- tibble::tibble(TWI_NONE__      = "")
      mod_lists[["PQ"          ]][[i_FO]] <- tibble::tibble(PQ_NONE__       = "")
      mod_lists[["one_TWI"     ]][[i_FO]] <- tibble::tibble(one_TWI_NONE__  = "")
      mod_lists[["only_TWI"    ]][[i_FO]] <- tibble::tibble(only_TWI_NONE__      = "")
      mod_lists[["only_PQ"     ]][[i_FO]] <- tibble::tibble(only_PQ_NONE__       = "")
      mod_lists[["only_SO"     ]][[i_FO]] <- tibble::tibble(only_SO_NONE__       = "")
      mod_lists[["only_one_TWI"]][[i_FO]] <- tibble::tibble(only_one_TWI_NONE__  = "")
    } # i_FO

    for (i_FO in seq_len(nrow(FO_binary))) {
      ## i_FO = 1

      # PQ
      if (max_scope[[i_list]] %in% c("PQ", "SO", "only_PQ", "only_SO")) {
        ind_PQ <- which(as.character(mod_lists[["FO" ]][[i_FO]]) %in% names(mod_lists[["FO" ]][[i_FO]]))
        # if at least one variable
        if (length(ind_PQ) >= 1) {
          PQ_binary <-
            #paste0(names(mod_lists[["FO" ]][[i_FO]])[ind_PQ], "_2") %>%
            paste0("I(", names(mod_lists[["FO" ]][[i_FO]])[ind_PQ], "^2)") %>%
            e_model_binary_complete()

          if (max_scope[[i_list]] %in% c("PQ", "SO")) {
            mod_lists[["PQ" ]][[i_FO]] <- PQ_binary
          }
          if (max_scope[[i_list]] %in% c("only_PQ", "only_SO")) {
            mod_lists[["only_PQ" ]][[i_FO]] <- PQ_binary
          }
        } # if
      } # PQ

      # TWI
      if (max_scope[[i_list]] %in% c("TWI", "SO", "only_TWI", "only_SO")) {
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

          if (max_scope[[i_list]] %in% c("TWI", "SO")) {
            mod_lists[["TWI"]][[i_FO]] <- TWI_binary
          }
          if (max_scope[[i_list]] %in% c("only_TWI", "only_SO")) {
            mod_lists[["only_TWI"]][[i_FO]] <- TWI_binary
          }

        } # if
      } # TWI

      # one_TWI
      if (max_scope[[i_list]] %in% c("one_TWI", "only_one_TWI")) {
        # if the list doesn't include the first variable, then skip
        if (!any(as.character(mod_lists[["FO" ]][[i_FO]]) %in% names(mod_lists[["FO" ]][[i_FO]])[1])) {
          mod_lists[["one_TWI"]][[i_FO]] <- NULL
          next
        } # if

        ind_one_TWI <- which(as.character(mod_lists[["FO" ]][[i_FO]]) %in% names(mod_lists[["FO" ]][[i_FO]]))
        # if at least two variables
        if (length(ind_one_TWI) >= 2) {
          one_TWI_binary_pairs <-
            paste0(
              names(mod_lists[["FO" ]][[i_FO]])[ind_one_TWI[ 1]]
            , ":"
            , names(mod_lists[["FO" ]][[i_FO]])[ind_one_TWI[-1]]
            )

          one_TWI_binary <-
            one_TWI_binary_pairs %>%
            e_model_binary_complete()

          if (max_scope[[i_list]] %in% c("one_TWI")) {
            mod_lists[["one_TWI"]][[i_FO]] <- one_TWI_binary
          }
          if (max_scope[[i_list]] %in% c("only_one_TWI")) {
            mod_lists[["one_TWI"]][[i_FO]] <- one_TWI_binary
          }
        } # if
      } # one_TWI


      if (max_scope[[i_list]] %in% c("FO", "TWI", "PQ", "SO", "one_TWI")) {
        sub_models[[i_list]][[i_FO]] <-
          e_expand_grid_df(
            mod_lists[["FO"          ]][[i_FO]]
          , mod_lists[["TWI"         ]][[i_FO]]
          , mod_lists[["PQ"          ]][[i_FO]]
          , mod_lists[["one_TWI"     ]][[i_FO]]
          )
      }
      if (max_scope[[i_list]] %in% c("only_TWI", "only_PQ",  "only_SO", "only_one_TWI")) {
        sub_models[[i_list]][[i_FO]] <-
          e_expand_grid_df(
          #  mod_lists[["FO"          ]][[i_FO]]
            mod_lists[["only_TWI"    ]][[i_FO]]
          , mod_lists[["only_PQ"     ]][[i_FO]]
          , mod_lists[["only_one_TWI"]][[i_FO]]
          )
      }

    } # i_FO

    sub_models[[i_list]] <-
      sub_models[[i_list]] %>%
      dplyr::bind_rows()

  } # i_list


  # "only_*" to be used with the variables in "always"
  # Need to combine in order: "always", "only_*", then others
  max_scope_list <-
    max_scope %>%
    unlist()
  max_scope_order <- NULL
  for (n_max in c("always", "only_TWI", "only_PQ", "only_SO", "only_one_TWI", "FO", "TWI", "PQ", "SO", "one_TWI")) {
    max_scope_order <-
      c(
        max_scope_order
      , which(max_scope_list == n_max)
      )
  }

  ## Combine across x_var_names lists
  # 1
  sub_models_combine <-
    sub_models[[ max_scope_order[1] ]] %>%
    dplyr::select(
      -tidyselect::ends_with("NONE__")
    )

  # 2:n
  if (n_var_lists > 1) {
    for (i_list in 2:n_var_lists) {
      ## i_list = 2

      sub_models_combine <-
        e_expand_grid_df(
          sub_models_combine
        , sub_models[[ max_scope_order[i_list] ]] %>%
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

  if (sw_return == "both") {
    out <-
      list(
        table    = sub_models_combine
      , formulas = sub_models_combine_formulas
      )
    return(out)
  }
} # e_model_all_subsets_formula

