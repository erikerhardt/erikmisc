#' Removes Spanish labels from all variable labels and factor labels
#'
#' @param dat data.frame to modify
#'
#' @return dat updated data.frame
#' @import stringi
#' @export
f_remove_spanish_labels <- function(dat = NULL) {


  #str(dat$pci_part_type.factor)

  lab_vars <- labelled::var_label(dat, unlist = TRUE)

  lab_vars[25]

  dat_to_var_label <-
    stringi::stri_replace_all_fixed(
      dat_to_var_label
    , "label("
    , "var_label("
    )

  # remove extra \
  dat_to_var_label <-
    stringi::stri_replace_all_fixed(dat_to_var_label, "\\", "")

  return(dat)
}
