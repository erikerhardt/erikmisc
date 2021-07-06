#' Take single column of comorbidities, combine common diseases, create separate columns with indicators.
#'
#' @param dat_vci is the VCI data
#' @param sw_print_unique will print the list of unique comorbidities
#'
#' @return dat_vci
#' @import stringr
#' @import dplyr
#' @export
#'
#' @examples
#' dat_vci <- format_comorbidity_variable(dat_vci, sw_print_unique = TRUE)
format_comorbidity_variable <- function(dat_vci, sw_print_unique = FALSE) {

  # determine which comorbidity string replacements to perform below
  #library(stringr)
  # list of unique comorbidities
  comorb_full_list <-
    dat_vci$pdi_which_disease %>%
    tolower() %>%
    str_split(",") %>%
    unlist() %>%
    lapply(str_trim) %>%
    unlist() %>%
    unique() %>%
    sort()

  # remove blank
  comorb_full_list <- comorb_full_list[!(comorb_full_list == "")]

  # replacement list using comorb_full_list
    # Note that I need to turn "high cholesterol" to "cholesterolX" before going the other way otherwise I get "high high cholesterol"
    # similar for thyroidX
  comorb_list_from_to <-
    read.table(text = "
    From,To
    XXX,ZZZ
    heartburn,acid reflux
    heart conditions,heart condition
    high cholesterol,XXX
    cholesterol,XXX
    XXX,high cholesterol
    hypothyroidism,XXX
    hyperthyroidism,XXX
    low thyroid,XXX
    thyroid problems,XXX
    thyroid,XXX
    XXX,thyroid condition
    leg nerve damage,XXX
    neuropathy,XXX
    sciatic nerve,XXX
    XXX,nerve damage and/or neuropathy
    breast cancer,cancer
    chronic kidney disease,kidney disease
    chronic scoliosis,scoliosis
    rheumatoid arthritis,arthritis
    astro arthritis,arthritis
    carpal tunnel,carpal tunnel syndrome
    high blood pressure,hypertension",
    sep = ",", header = TRUE, stringsAsFactors = FALSE)
  # remove extra spaces
  comorb_list_from_to$From <- str_trim(comorb_list_from_to$From)
  comorb_list_from_to$To   <- str_trim(comorb_list_from_to$To  )

  # perform replacement in unique file
  for (i.replace in 1:nrow(comorb_list_from_to)) {
    comorb_full_list <- stringr::str_replace_all(comorb_full_list, comorb_list_from_to$From[i.replace], comorb_list_from_to$To[i.replace])
  }

  # perform replacement in
  # list of unique comorbidities
  comorb_full_list <-
    comorb_full_list %>%
    unique() %>%
    sort()

  if (sw_print_unique) {
    print("Comorbidity list ----------")
    print(comorb_full_list)
  }

  # ----------------------------------------
  # perform replacement in data
  TEMP_pdi_which_disease <-
    dat_vci$pdi_which_disease %>%
    tolower()

  for (i.replace in 1:nrow(comorb_list_from_to)) {
    TEMP_pdi_which_disease <- stringr::str_replace_all(TEMP_pdi_which_disease, comorb_list_from_to$From[i.replace], comorb_list_from_to$To[i.replace])
  }

  # create new comorbid variables, cbind them to dat_vci
  #comorbid <- list()
  for (n_comorb in comorb_full_list) {
    var_name <- paste0("comorbid_", stringr::str_replace_all(n_comorb, " ", "_"))
    #comorbid[[var_name]] <- as.numeric(grepl(n_comorb, TEMP_pdi_which_disease))
    dat_vci[, var_name] <- grepl(n_comorb, TEMP_pdi_which_disease)
  }
  #comorbid <- dplyr::as_tibble(comorbid)
  #dat_vci <- dplyr::bind_cols(dat_vci, comorbid)

  return(dat_vci)
}
