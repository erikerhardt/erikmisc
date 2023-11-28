#' Additional variable formatting beyond the REDCap provided formats
#'
#' @param dat_vci is the VCI data
#'
#' @return dat_vci
#' @importFrom lubridate ymd
#' @export
#'
#' @examples
#' dat_vci <- additional_variable_formatting(dat_vci)
additional_variable_formatting_vci <- function(dat_vci) {

  #nrow(dat_vci)

  ## Email requesting updating these in the database
  # https://mail.google.com/mail/u/0/?tab=cm#search/JPrestopnik%40salud.unm.edu+data/FMfcgxmZVXzLMzfDCShzGfXlQZKDChQX

  # ----------------------------------------
  ## Rename and Relabel variables

  # pigf to plgf
  ind_this <-
    stringr::str_detect(tolower(names(dat_vci)), "pigf") |>
    which()
  colnames(dat_vci)[ind_this] <-
    colnames(dat_vci)[ind_this] |>
    stringr::str_replace(
      stringr::fixed("pigf")
    , stringr::fixed("plgf")
    )
  for (i_this in ind_this) {
    labelled::var_label(dat_vci[, i_this]) <-
      labelled::var_label(dat_vci[, i_this]) |>
      stringr::str_replace(
        stringr::fixed("PIGF")
      , stringr::fixed("PlGF")
      )
  }

  # ifn_y to ifn_g
  ind_this <-
    stringr::str_detect(tolower(names(dat_vci)), "ifn_y") |>
    which()
  colnames(dat_vci)[ind_this] <-
    colnames(dat_vci)[ind_this] |>
    stringr::str_replace(
      stringr::fixed("ifn_y")
    , stringr::fixed("ifn_g")
    )
  for (i_this in ind_this) {
    labelled::var_label(dat_vci[, i_this]) <-
      labelled::var_label(dat_vci[, i_this]) |>
      stringr::str_replace(
        stringr::fixed("IFN-y")
      , stringr::fixed("IFN-g")
      )
  }

  # VEGF (no suffix) add "-A"
  ind_this <-
    stringr::str_detect(tolower(names(dat_vci)), "vegf$") |>
    which()
  colnames(dat_vci)[ind_this] <-
    colnames(dat_vci)[ind_this] |>
    stringr::str_replace(
      "vegf"
    , stringr::fixed("vegf_a")
    )
  for (i_this in ind_this) {
    labelled::var_label(dat_vci[, i_this]) <-
      labelled::var_label(dat_vci[, i_this]) |>
      stringr::str_replace(
        "VEGF$"
      , stringr::fixed("VEGF-A")
      )
  }

  # csf_flt add "_1"
  ind_this <-
    stringr::str_detect(tolower(names(dat_vci)), "fl_angio_csf_flt") |>
    which()
  colnames(dat_vci)[ind_this] <-
    colnames(dat_vci)[ind_this] |>
    stringr::str_replace(
      stringr::fixed("fl_angio_csf_flt")
    , stringr::fixed("fl_angio_csf_flt_1")
    )

  # ----------------------------------------
  ## Only Relabel variables

  for (i_var in seq_along(names(dat_vci))) {

    # plamsa to plasma
    labelled::var_label(dat_vci[[i_var]]) <-
      labelled::var_label(dat_vci[[i_var]]) |>
      stringr::str_replace(
        stringr::fixed("Plamsa")
      , stringr::fixed("Plasma")
      )


    # Move MSD after CSF or Plasma
    labelled::var_label(dat_vci[[i_var]]) <-
      labelled::var_label(dat_vci[[i_var]]) |>
      stringr::str_replace(
        stringr::fixed("MSD Plasma")
      , stringr::fixed("Plasma MSD")
      )
    labelled::var_label(dat_vci[[i_var]]) <-
      labelled::var_label(dat_vci[[i_var]]) |>
      stringr::str_replace(
        stringr::fixed("MSD CSF")
      , stringr::fixed("CSF MSD")
      )

    # Add CSF
    labelled::var_label(dat_vci[[i_var]]) <-
      labelled::var_label(dat_vci[[i_var]]) |>
      stringr::str_replace(
        stringr::fixed("Albumin Index")
      , stringr::fixed("CSF Albumin Index")
      )
    labelled::var_label(dat_vci[[i_var]]) <-
      labelled::var_label(dat_vci[[i_var]]) |>
      stringr::str_replace(
        stringr::fixed("A Beta")
      , stringr::fixed("CSF A Beta")
      )
    labelled::var_label(dat_vci[[i_var]]) <-
      labelled::var_label(dat_vci[[i_var]]) |>
      stringr::str_replace(
        stringr::fixed("P-Tau")
      , stringr::fixed("CSF P-Tau")
      )


  }


  # ----------------------------------------
  ## Format dates
  #library(lubridate)

  dat_vci$dx_current_date           <- lubridate::ymd(dat_vci$dx_current_date        )
  dat_vci$demo_consent_date         <- lubridate::ymd(dat_vci$demo_consent_date      )
  #dat_vci$demo_dob                  <- lubridate::ymd(dat_vci$demo_dob               )
  dat_vci$demo_bl_visit_date        <- lubridate::ymd(dat_vci$demo_bl_visit_date     )
  dat_vci$date_of_visit             <- lubridate::ymd(dat_vci$date_of_visit          )
  dat_vci$neuro_exam_date           <- lubridate::ymd(dat_vci$neuro_exam_date        )
  dat_vci$npsy_exam_date            <- lubridate::ymd(dat_vci$npsy_exam_date         )
  dat_vci$labs_bun_date             <- lubridate::ymd(dat_vci$labs_bun_date          )
  dat_vci$labs_electrolytes_date    <- lubridate::ymd(dat_vci$labs_electrolytes_date )
  dat_vci$labs_tsh_date             <- lubridate::ymd(dat_vci$labs_tsh_date          )
  dat_vci$labs_lipid_panel_date     <- lubridate::ymd(dat_vci$labs_lipid_panel_date  )
  dat_vci$labs_b12_date             <- lubridate::ymd(dat_vci$labs_b12_date          )
  dat_vci$labs_a1c_date             <- lubridate::ymd(dat_vci$labs_a1c_date          )
  dat_vci$fl_lp_date                <- lubridate::ymd(dat_vci$fl_lp_date             )
  dat_vci$mri_date                  <- lubridate::ymd(dat_vci$mri_date               )


  return(dat_vci)
}
