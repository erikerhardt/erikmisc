#' Splits Spanish and English labels from all variable labels and factor labels, then you can choose which to use
#'
#' @param labs list of labels to split
#' @param sw_variable_factor "variable" or "factor" type to process
#' @param language which language to return
#' @param debug return "ERROR" when there are issues, for testing
#'
#' @return dat updated data.frame
#' @import stringi
#' @export
f_split_Spanish_English <-
  function(
    labs
  , sw_variable_factor  = c("variable", "factor")[1]
  , language            = c("English", "Spanish")[1]
  , debug               = FALSE  # returning only bad parsing
  ) {
  #### fix factor labels where some only have English
  ## labs = levels(data[[i_var]]); sw_variable_factor = "factor"; language = "English"


  # Spanish is 1, English is 2
  if (language == "Spanish") { lang_ind <- 1 }
  if (language == "English") { lang_ind <- 2 }

  # 8/20/2019 updated from "  " to " / "
  # break at two spaces: " / "
  if (sw_variable_factor == "variable") {
    ## labs = Var[["label"]][["var_demo_pat"]][3]
    ## labs = Var[["label"]][["var_demo_pat"]][6]
    ## labs = Var[["label"]][["var_demo_pat"]][9]
    ## labs = Var[["label"]][["var_depr_ss"]][6]

    delim <- " / "

    labs_out <-
      stringr::str_split(
        labs
      , pattern = delim
      #, n = 2
      , simplify = TRUE
      ) |>
      stringr::str_trim()

    if(debug) {
      if (length(labs_out) > 2) {
        #print(labs)
        return("ERROR")
      }
    }

    # only one language, return that one
    if (length(labs_out) == 1) {
      return(labs_out)
    }
    return(labs_out[lang_ind])
  }

  # break at space-slash-space: " / "
  if (sw_variable_factor == "factor") {
    ## labs = levels(dat_pdp$pdi_eng_prof)

    delim <- " / "

    labs_out <-
      stringr::str_split(
        labs
      , pattern = delim
      #, n = 2
      , simplify = TRUE
      )

    # only one language, return that one
    if (ncol(labs_out) == 1) {
      return(labs_out)
    }

    # if all levels have a language value, then return the Language column you want
    if (sum(labs_out[,c(1,2)] == "") == 0) {
      return(stringr::str_trim(labs_out[,lang_ind]))
    } else {
    # If some options have only on language and others have both,
    #   then copy the existing value to both
      for (i_row in 1:nrow(labs_out)) {
        ## i_row = 1
        if (labs_out[i_row, 2] == "") {
          labs_out[i_row, 2] <- labs_out[i_row, 1]
        }

      }
      return(stringr::str_trim(labs_out[,lang_ind]))
    }

  }

}
