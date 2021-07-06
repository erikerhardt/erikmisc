#' Additional variable formatting beyond the REDCap provided formats
#'
#' @param dat_vci is the VCI data
#'
#' @return dat_vci
#' @import editrules
#' @export
#'
#' @examples
#' dat_vci <- additional_variable_formatting(dat_vci)
clean_vci <- function(dat_vci) {

  #library(editrules)



  # bmi heights and weights have some letters in it, for example "68.5 kg", extract numbers
  dat_vci$bmi_height_1 <- extract_numbers_from_string(dat_vci$bmi_height_1)
  dat_vci$bmi_height_2 <- extract_numbers_from_string(dat_vci$bmi_height_2)
  dat_vci$bmi_weight_1 <- extract_numbers_from_string(dat_vci$bmi_weight_1)
  dat_vci$bmi_weight_2 <- extract_numbers_from_string(dat_vci$bmi_weight_2)

  # meters to centimeters
  dat_vci$bmi_height_1 <- ifelse(dat_vci$bmi_height_1 < 3, dat_vci$bmi_height_1 * 100, dat_vci$bmi_height_1)
  dat_vci$bmi_height_2 <- ifelse(dat_vci$bmi_height_2 < 3, dat_vci$bmi_height_2 * 100, dat_vci$bmi_height_2)


  return(dat_vci)
}
