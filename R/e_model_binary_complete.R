#' All variable subsets
#'
#' Used in \code{e_model_all_subsets_formula()} to complete all subsets of variables.
#'
#' Return data.frame of all binary subsets of a list of variable names
#'
#' @param var_names a character list of variable names
#'
#' @return          a data.frame of all binary subsets of variables
#'
#' @importFrom stringr str_replace
#' @importFrom stringr fixed
#'
#' @export
#'
#' @examples
#'
#' e_model_binary_complete(
#'    var_names = c("a", "b", "c")
#'  )
#'
e_model_binary_complete <-
  function(
    var_names
  ) {
  # var_names = letters[1:4]

  var_binary <-
    expand.grid(
      replicate(
        n         = length(var_names)
      , expr      = 0:1 |> as.character()
      , simplify  = FALSE
      )
    , stringsAsFactors = FALSE
    )

  attr(var_binary, "out.attrs") <- NULL

  colnames(var_binary) <- var_names

  for (i_col in seq_len(ncol(var_binary))) {
    ## i_col = 1
    var_binary[[ i_col ]] <-
      stringr::str_replace(
        string      = var_binary[[ i_col ]]
      , pattern     = stringr::fixed("0")
      , replacement = "" #NA
      )

    var_binary[[ i_col ]] <-
      stringr::str_replace(
        string      = var_binary[[ i_col ]]
      , pattern     = stringr::fixed("1")
      , replacement = var_names[i_col]
      )

  }

  return(var_binary)

} # e_model_binary_complete

