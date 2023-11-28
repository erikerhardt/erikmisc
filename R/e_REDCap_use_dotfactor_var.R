#' REDCap data, use ".factor" var with original variable label
#'
#' For the factor (categorical) variables,
#' the non-".factor" variable name is a numeric class with the levels (and a variable label)
#' while the ".factor" variable name is a factor class with the labels in the appropriate order (and no variable label).
#' This function copies the variable label from the non-".factor" variable name to the ".factor" variable,
#' then copies the ".factor" variable to the non-".factor" variable name.
#' This makes the non-".factor" variable name have all the best attributes.
#' Finally, drop all the ".factor" variables from the data.
#'
#' @param dat               data.frame file to process
#' @param var_names         non-".factor" variable names to update
#' @param sw_all_dotfactor  T/F to process all ".factor" variables
#'
#' @return dat        with updated non-".factor" variables
#'
#' @import labelled
#' @importFrom stringr str_subset
#' @importFrom stringr str_replace
#' @export
#'
#' @examples
#' \dontrun{
#' # dat_temp <-
#' #   e_REDCap_use_dotfactor_var(
#' #     var_names = c("labs_bun_hl", "labs_cr_hl", "labs_elec_na_hl")
#' #   , dat       = dat_vci
#' #   )
#' }

# For factor variables, copy label from original variable then copy back to non-".factor" variable name
e_REDCap_use_dotfactor_var <-
  function(
    dat
  , var_names         = NULL
  , sw_all_dotfactor  = TRUE
  ) {
  ## var_names <- c("labs_bun_hl", "labs_cr_hl", "labs_elec_na_hl", "fl_cyto_plasma_il_2" , "labs_elec_k_hl_XXX")
  ## var_names <- c("labs_bun_hl", "labs_cr_hl", "labs_elec_na_hl")
  ## dat <- dat_vci

  # process all ".factor" variables
  if (sw_all_dotfactor) {
    var_names <-
      dat |>
      names() |>
      stringr::str_subset(
        pattern     = stringr::fixed(".factor")
      ) |>
      stringr::str_replace(
        pattern     = stringr::fixed(".factor")
      , replacement = ""
      )
  }


  #library(labelled)
  # create ".factor" variable names from the original list
  var_names.f <- paste0(var_names, ".factor")

  # check whether all var_names and var_names.f are in the dataset,
  #   if not, return the list of variables that are not in the dataset
  #   and stop with an error.
  var_names_all <-
    c(
      var_names
    , var_names.f
    )

  var_names_all_in <-
    (var_names_all %in% names(dat))

  if (!all(var_names_all_in)) {
    message("erikmisc::e_REDCap_use_dotfactor_var, not all var_names or var_names.factor in dat")
    message("  These varibles not in dat:")
    for (n_var in var_names_all[!var_names_all_in]) {
      message(paste0("    ", n_var))
    }

    stop("erikmisc::e_REDCap_use_dotfactor_var")
  }

  for (i_var in seq_along(var_names)) {
    # write var label from var_names to var_names.f variable
    labelled::var_label(dat[, var_names.f[i_var]]) <-
      labelled::var_label(dat[, var_names[i_var]]) |> as.character()
    # copy var_names.f to var_names
    dat[, var_names[i_var]] <- dat[, var_names.f[i_var]]

    # Finally, drop all the ".factor" variables from the data.
    dat[, var_names.f[i_var]] <- NULL
  }

  return(dat)
} # e_REDCap_use_dotfactor_var


