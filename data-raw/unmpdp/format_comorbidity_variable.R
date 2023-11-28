#' Take single column of comorbidities, combine common diseases, create separate columns with indicators.  Treats >1 in a category as 1 to simplify summary tables.
#'
#' @param dat_pdp is the PDP data
#' @param sw_print_unique will print the list of unique comorbidities
#'
#' @return dat_pdp
#' @import stringr
#' @import dplyr
#' @export
#'
#' @examples
#' dat_pdp <- format_comorbidity_variable(dat_pdp, sw_print_unique = TRUE)
format_comorbidity_variable <-
  function(
    dat_pdp
  , var_comorbid_conditions = NULL
  , code_other_below_freq = 8
  , sw_print_unique = FALSE
  ) {

  if(is.null(var_comorbid_conditions)) {
    warning("No comorbid condition variables provided, returning")
    return(dat_pdp)
  }

  # function for table of comorbid conditions
  f_table_comorbid <- function(dat_pdp, var_comorbid_conditions) {
    # vector of comorbid conditions
    comorbid_conditions <-
      dat_pdp |>
      select(
        one_of(var_comorbid_conditions)
      ) |>
      unlist()
    comorbid_conditions <-
      comorbid_conditions[!(comorbid_conditions == "")] |>
      sort()

    tab_comorbid_conditions <-
      comorbid_conditions |>
      table() |>
      sort(decreasing = TRUE) |>
      as.data.frame()
    print(paste0("Comorbid conditions: ", nrow(tab_comorbid_conditions)," ----"))
    print(tab_comorbid_conditions)
    print(paste0("Coding to 'other' comorbid conditions with frequencies less than: ", code_other_below_freq))

    return(tab_comorbid_conditions)
  }


  # make all comorbid conditions lowercase
  for(n_var in var_comorbid_conditions) {
    ## n_var = var_comorbid_conditions[1]
    dat_pdp[[n_var]] <- dat_pdp[[n_var]] |> tolower()
  }

  # make table
  tab_comorbid_conditions <- f_table_comorbid(dat_pdp, var_comorbid_conditions)


  # comorbid conditions to code as "other"
  comorbid_conditions_other <-
    tab_comorbid_conditions |>
    filter(Freq < code_other_below_freq) |>
    select(1) |>
    unlist() |>
    as.character()

  for(n_var in var_comorbid_conditions) {
    ## n_var = var_comorbid_conditions[1]
    for(n_cond in comorbid_conditions_other) {
      ## n_cond = comorbid_conditions_other[1]
      dat_pdp[(dat_pdp[,n_var] == n_cond), n_var] <- "other"
      # Replace spaces and other punctuation with underscores
      dat_pdp[[n_var]] <-
        dat_pdp[[n_var]] |>
        str_replace_all("[[:punct:]]", " ") |>
        str_replace_all(" ", "_")
    }
  }

  # make table
  tab_comorbid_conditions <- f_table_comorbid(dat_pdp, var_comorbid_conditions)


  # create seperate columns for each comorbidity
  new_var_comorbid_conditions <-
    tab_comorbid_conditions |>
    select(1) |>
    unlist() |>
    as.character()

  for(n_new_var in new_var_comorbid_conditions) {
    ## n_new_var = new_var_comorbid_conditions[1]
    dat_pdp[, paste0("comorbid_", n_new_var)] <-
      (dat_pdp[, var_comorbid_conditions] == n_new_var) |> rowSums()
  }

  # Let "1" mean "at least 1"
  ind_GE1 <- (dat_pdp[,paste0("comorbid_", new_var_comorbid_conditions)] > 1)
  dat_pdp[,paste0("comorbid_", new_var_comorbid_conditions)][ind_GE1] <- 1

  #dat_pdp[,paste0("comorbid_", new_var_comorbid_conditions)]

  return(dat_pdp)
}
