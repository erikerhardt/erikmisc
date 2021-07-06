#' Replace Hmisc \code{label()} with labelled \code{var_label()} in the R formatting file
#'
#' @param dat_to_var_label is the R file that needs \code{label()} replaced with \code{var_label()}
#'
#' @return file with \code{var_label()} replaced
#' @import stringi
#' @export
f_replace_var_label <- function(dat_to_var_label = NULL) {

  dat_to_var_label <-
    stringi::stri_replace_all_fixed(
      dat_to_var_label
    , "label("
    , "var_label("
    )

  # remove extra \
  dat_to_var_label <-
    stringi::stri_replace_all_fixed(dat_to_var_label, "\\", "")

  return(dat_to_var_label)
}
