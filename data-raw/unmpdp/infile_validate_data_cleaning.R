#' Creating data cleaning files for use in the validate fuctions.
#'
#' @param subpath is the location of the data dataclean files.  Erik will specify as \code{NULL} to update data in the R package.
#'
#' @return
#' @export
#'
#' @examples
#' infile_validate_data_cleaning(subpath = NULL) # for Erik to update package data
#' infile_validate_data_cleaning(subpath = "dataclean")            # for others to format data in current path
infile_validate_data_cleaning <- function(subpath = "dataclean") {

# for Erik
if (is.null(subpath)) {
  subpath = "../data-raw/dataclean"
}

# create directory if it doesn't already exist
ifelse(!dir.exists(subpath), dir.create(subpath), FALSE)

### This is the master data cleaning file.
# By running this file, separate files are created for
#   each appropriate combination of
#   instrument, patient/social support, and time point.


## Return to do dates and times

## Conditionals
# If A, then ask about B
# !is.na(A) & (not A | B)


# PDP data cleaning rules

i_name_group <- "01-ParticipantContactInfo"
## Patients and Social support at 00
for (i_ps in c("p", "s")) { # "p", "s" Patient / Social support
  for (i_time in c("00")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
study_id                                                     %in% 1:10000       # study_id
!is.na(redcap_event_name)                                                       # redcap_event_name
pci_prog_contact                                             %in% 1:2           # pci_prog_contact
!is.na(pci_part_id)                                                             # pci_part_id
pci_data_col_id                                              %in% 1:7           # pci_data_col_id
!is.na(pci_date)                                                                # pci_date
!is.na(pci_time_start)                                                          # pci_time_start
pci_locale                                                   %in% 1:5           # pci_locale
!is.na(pci_locale) & (pci_locale != 5 | !is.na(pci_locale_other))               # pci_locale_other
pci_part_type                                                %in% 1:2           # pci_part_type
!is.na(pci_f_name_contact)                                                      # pci_f_name_contact
!is.na(pci_l_name_contact)                                                      # pci_l_name_contact
!is.na(pci_street_adr)                                                          # pci_street_adr
!is.na(pci_zip_code)                                                            # pci_zip_code
!is.na(pci_email)                                                               # pci_email
!is.na(pci_phone_prefer)                                                        # pci_phone_prefer
!is.na(pci_phone_alt)                                                           # pci_phone_alt
!is.na(pci_alt_contact)                                                         # pci_alt_contact
!is.na(pci_part_type) & (pci_part_type != 1 | !is.na(pci_doc_contact))          # pci_doc_contact
!is.na(pci_comments)                                                            # pci_comments
informacin_del_contacto_participant_contact_info_complete    %in% 1:2           # informacin_del_contacto_participant_contact_info_complete
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}
#!is.na(redcap_survey_identifier)                                                # redcap_survey_identifier
#!is.na(informacin_del_contacto_participant_contact_info_timestamp)              # informacin_del_contacto_participant_contact_info_timestamp
#!is.na(informacin_demogrfica_participant_demographic_info_timestamp)            # informacin_demogrfica_participant_demographic_info_timestamp


i_name_group <- "02a-ParticipantDemographicInfo"
## Patients and Social support at 00
for (i_ps in c("p", "s")) { # "p", "s" Patient / Social support
  for (i_time in c("00")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
pdi_data_col_id_demog                                            %in% 1:7                                                         # pdi_data_col_id_demog
pdi_part_type                                                    %in% 1:2                                                         # pdi_part_type
!is.na(pdi_part_type) & (pdi_part_type != 1 | pdi_how_heard___1  %in% 0:1 )                                                       # pdi_how_heard___1
!is.na(pdi_part_type) & (pdi_part_type != 1 | pdi_how_heard___2  %in% 0:1 )                                                       # pdi_how_heard___2
!is.na(pdi_part_type) & (pdi_part_type != 1 | pdi_how_heard___3  %in% 0:1 )                                                       # pdi_how_heard___3
!is.na(pdi_part_type) & (pdi_part_type != 1 | pdi_how_heard___4  %in% 0:1 )                                                       # pdi_how_heard___4
!is.na(pdi_part_type) & (pdi_part_type != 1 | pdi_how_heard___5  %in% 0:1 )                                                       # pdi_how_heard___5
!is.na(pdi_part_type) & (pdi_part_type != 1 | pdi_how_heard___6  %in% 0:1 )                                                       # pdi_how_heard___6
!is.na(pdi_part_type) & (pdi_part_type != 1 | pdi_how_heard___7  %in% 0:1 )                                                       # pdi_how_heard___7
!is.na(pdi_part_type) & (pdi_part_type != 1 | pdi_how_heard___8  %in% 0:1 )                                                       # pdi_how_heard___8
!is.na(pdi_how_heard___8 != 1) & (pdi_how_heard___8 != 1 | !is.na(pdi_heard_other))                                               # pdi_heard_other
!is.na(pdi_dob)                                                                                                                   # pdi_dob, XXX ignore Jan 1st XXX
pdi_sex                                                            %in% 1:3                                                       # pdi_sex
!is.na(pdi_part_type) & (pdi_part_type != 1 | pdi_marital_stat     %in% 1:6 )                                                     # pdi_marital_stat
!is.na(pdi_part_type) & (pdi_part_type != 1 | pdi_household_number %in% c(1:12, 97, 98, 99) )                                     # pdi_household_number
!is.na(pdi_part_type) & (pdi_part_type != 1 | !(pdi_household_number %in% 2:12) | !is.na(pdi_household_info) )                                  # pdi_household_info
pdi_educ                                                           %in% c(1:11, 97, 98, 99)                                       # pdi_educ
pdi_language___1                                                   %in% 0:1                                                       # pdi_language___1
pdi_language___2                                                   %in% 0:1                                                       # pdi_language___2
pdi_language___3                                                   %in% 0:1                                                       # pdi_language___3
!is.na(pdi_language___3) & (pdi_language___3 != 1 | !is.na(pdi_what_other_language_s) )                                           # pdi_what_other_language_s
!is.na(pdi_language___1) & !is.na(pdi_language___2) & ((pdi_language___1 != 1 | pdi_language___2 != 1) | pdi_lang_pref %in% 1:2 ) # pdi_lang_pref
!is.na(pdi_language___1) & !is.na(pdi_language___3) & ((pdi_language___1 != 1 | pdi_language___3 != 1) | pdi_eng_prof %in% 1:5 )  # pdi_eng_prof
pdi_born_us                                                        %in% c(1:2, 97, 98, 99)                                        # pdi_born_us
!is.na(pdi_born_us) & (pdi_born_us != 2 | !is.na(pdi_born_us_no) )                                                                # pdi_born_us_no
pdi_diagnosis                                                      %in% c(1:3, 97, 98, 99)                                        # pdi_diagnosis
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}


i_name_group <- "02b-ParticipantDemographicInfo"
#FIXED-MAY NEED TO FIX OTHERS (!is.na(pdi_meds_as_prescribed) | !(pdi_part_type == 1)) & (!(pdi_part_type == 1) | !(pdi_meds_as_prescribed == 2) | !is.na(pdi_not_as_prescribed_why))                                                                                                # pdi_not_as_prescribed_why

## Patients and Social support at 00
for (i_ps in c("p", "s")) { # "p", "s" Patient / Social support
  for (i_time in c("00")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
!is.na(pdi_part_type) & !is.na(pdi_diagnosis) & !is.na(pci_prog_contact) & (!(pdi_part_type == 1 & (pdi_diagnosis %in% c(1,2)) & pci_prog_contact == 1) | pdi_diag_one_hope %in% c(1:2, 97, 98, 99)) # pdi_diag_one_hope
!is.na(pdi_part_type) & !is.na(pdi_diagnosis) & (!(pdi_part_type == 1 & pdi_diagnosis %in% c(1,2)) | !is.na(pdi_diag_when))                                                                          # pdi_diag_when
(!is.na(pdi_part_type) | !(pdi_part_type == 1)) & (!(pdi_part_type == 1) | pdi_diag_other %in% c(1:2, 97, 98, 99))                                                                                                             # pdi_diag_other
(!is.na(pdi_diag_other) | !(pdi_part_type == 1)) & (!(pdi_part_type == 1) | !(pdi_diag_other == 1) | !is.na(pdi_which_disease))                                                                                                                        # pdi_which_disease
(!is.na(pdi_part_type) | !(pdi_part_type == 1)) & (!(pdi_part_type == 1) | pdi_meds_1 %in% c(1:2, 97, 98, 99))                                                                                                                 # pdi_meds_1
(!is.na(pdi_meds_1) | !(pdi_part_type == 1)) & (!(pdi_part_type == 1) | !(pdi_meds_1 == 1) | !is.na(pdi_meds_2))                                                                                                                                       # pdi_meds_2
!is.na(pdi_meds)                                                                                                                                                                                     # pdi_meds: photo, ignore
(!is.na(pdi_meds_1) | !(pdi_part_type == 1)) & (!(pdi_part_type == 1) | !(pdi_meds_1 == 1) | pdi_meds_as_prescribed %in% c(1:2, 97, 98, 99))                                                                                                           # pdi_meds_as_prescribed
(!is.na(pdi_meds_as_prescribed) & !is.na(pdi_part_type) & !is.na(pdi_meds_1)) & (!(pdi_meds_1 == 1 | pdi_part_type == 1 | pdi_meds_as_prescribed == 2) | !is.na(pdi_not_as_prescribed_why))     # pdi_not_as_prescribed_why
(!is.na(pdi_part_type) | !(pdi_part_type == 1)) & (!(pdi_part_type == 1) | pdi_supps %in% c(1:2, 97, 98, 99))                                                                                                                  # pdi_supps
(!is.na(pdi_supps) | !(pdi_part_type == 1)) & (!(pdi_part_type == 1) | !(pdi_supps == 1) | !is.na(pdi_supps_info))                                                                                                                                     # pdi_supps_info
(!is.na(pdi_part_type) | !(pdi_part_type == 1)) & (!(pdi_part_type == 1) | pdi_family_diag %in% c(1:2, 97, 98, 99))                                                                                                            # pdi_family_diag
(!is.na(pdi_family_diag) | !(pdi_part_type == 1)) & (!(pdi_part_type == 1) | !(pdi_family_diag == 1) | !is.na(pdi_family_which))                                                                                                                       # pdi_family_which
(!is.na(pdi_part_type) | !(pdi_part_type == 1)) & (!(pdi_part_type == 1) | pdi_smoke %in% c(1:2, 97, 98, 99))                                                                                                                  # pdi_smoke
(!is.na(pdi_smoke) | !(pdi_part_type == 1)) & (!(pdi_part_type == 1) | !(pdi_smoke == 1) | !is.na(pdi_smoke_years))                                                                                                                                    # pdi_smoke_years
(!is.na(pdi_part_type) | !(pdi_part_type == 1)) & (!(pdi_part_type == 1) | pdi_smoke_prev %in% c(1:2, 97, 98, 99))                                                                                                             # pdi_smoke_prev
(!is.na(pdi_smoke_prev) | !(pdi_part_type == 1)) & (!(pdi_part_type == 1) | !(pdi_smoke_prev == 1) | !is.na(pdi_smoke_prev_years))                                                                                                                     # pdi_smoke_prev_years
(!is.na(pdi_part_type) | !(pdi_part_type == 1)) & (!(pdi_part_type == 1) | pdi_smoke_house %in% c(1:2, 97, 98, 99))                                                                                                            # pdi_smoke_house
pdi_diabetes_prog                                            %in% c(1:2, 97, 98, 99)                                                                                                                 # pdi_diabetes_prog
!is.na(pdi_diabetes_prog) & (!(pdi_diabetes_prog == 1) | !is.na(pdi_diabetes_prog_which))                                                                                                            # pdi_diabetes_prog_which
!is.na(pdi_diabetes_prog) & (!(pdi_diabetes_prog == 1) | !is.na(pdi_diabetes_prog_when))                                                                                                             # pdi_diabetes_prog_when
!is.na(pdi_part_type) & (!(pdi_part_type == 1) | pdi_pat_rel_soc %in% 1:6)                                                                                                                           # pdi_pat_rel_soc
!is.na(pdi_part_type) & (!(pdi_part_type == 1 & !is.na(pdi_pat_rel_soc) & pdi_pat_rel_soc == 6) | !is.na(pdi_pat_rel_soc_other))                                                                     # pdi_pat_rel_soc_other
!is.na(pdi_part_type) & (!(pdi_part_type == 1) | pdi_pat_involve       %in% 1:4)                                                                                                                     # pdi_pat_involve
!is.na(pdi_part_type) & (!(pdi_part_type == 1) | pdi_pat_accomp        %in% 1:4)                                                                                                                     # pdi_pat_accomp
!is.na(pdi_part_type) & (!(pdi_part_type == 1) | pdi_pat_know          %in% 1:4)                                                                                                                     # pdi_pat_know
!is.na(pdi_part_type) & (!(pdi_part_type == 1) | pdi_pat_interact      %in% 1:4)                                                                                                                     # pdi_pat_interact
!is.na(pdi_part_type) & (!(pdi_part_type == 1) | pdi_pat_interact_type %in% 1:4)                                                                                                                     # pdi_pat_interact_type
!is.na(pdi_part_type) & (!(pdi_part_type == 1) | pdi_pat_live_with     %in% c(1:2, 97, 98, 99))                                                                                                      # pdi_pat_live_with
!is.na(pdi_part_type) & (!(pdi_part_type == 2) | pdi_soc_rel_pat %in% 1:6)                                                                                                                           # pdi_soc_rel_pat
!is.na(pdi_part_type) & (!(pdi_part_type == 2 & !is.na(pdi_soc_rel_pat) & pdi_soc_rel_pat == 6) | !is.na(pdi_soc_rel_pat_other))                                                                     # pdi_soc_rel_pat_other
!is.na(pdi_part_type) & (!(pdi_part_type == 2) | pdi_soc_involve       %in% 1:4)                                                                                                                     # pdi_soc_involve
!is.na(pdi_part_type) & (!(pdi_part_type == 2) | pdi_soc_accomp        %in% 1:4)                                                                                                                     # pdi_soc_accomp
!is.na(pdi_part_type) & (!(pdi_part_type == 2) | pdi_soc_know          %in% 1:4)                                                                                                                     # pdi_soc_know
!is.na(pdi_part_type) & (!(pdi_part_type == 2) | pdi_soc_interact      %in% 1:4)                                                                                                                     # pdi_soc_interact
!is.na(pdi_part_type) & (!(pdi_part_type == 2) | pdi_soc_interact_type %in% 1:4)                                                                                                                     # pdi_soc_interact_type
!is.na(pdi_part_type) & (!(pdi_part_type == 2) | pdi_soc_live_with     %in% c(1:2, 97, 98, 99))                                                                                                      # pdi_soc_live_with
!is.na(pdi_comments)                                                                                                                                                                                 # pdi_comments
informacin_demogrfica_participant_demographic_info_complete  %in% 0:2                                                                                                                                # informacin_demogrfica_participant_demographic_info_complete
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}




### Continue here with "text" fields and conditionals
### Continue here with "text" fields and conditionals
### Continue here with "text" fields and conditionals
### Continue here with "text" fields and conditionals

# i_name_group <- "03a-OralSurvey"
# ## Patients and Social support at 03, 06, 12
# for (i_ps in c("p", "s")) { # "p", "s" Patient / Social support
#   for (i_time in c("03", "06", "12")) { # "00", "03", "06", "12" month of data collection
# writeLines(text = "
# encuesta_oral_oral_survey_timestamp                          %in% 0:2           # encuesta_oral_oral_survey_timestamp
# surv_baseline_yn                                             %in% 1:2           # surv_baseline_yn
# surv_fu_which                                                %in% 1:4           # surv_fu_which
# surv_fu_part_id                                              %in% 1:4           # surv_fu_part_id
# surv_fu_data_col_id                                          %in% 1:4           # surv_fu_data_col_id
# surv_fu_date                                                 %in% 1:4           # surv_fu_date
# surv_fu_time_start                                           %in% 1:4           # surv_fu_time_start
# surv_fu_locale                                               %in% 1:4           # surv_fu_locale
# surv_fu_locale_other                                         %in% 1:4           # surv_fu_locale_other
# ", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
#   }
# }



i_name_group <- "03b-Activ"
    # don't check since it's usually empty as an optional textbox.
    # surv_prog == 2 | ((!is.na(surv_prog) & !is.na(surv_baseline_yn)) & (!(surv_baseline_yn == 2) | surv_one_hope_activ_other                                    %in% 1:2))  # surv_one_hope_activ_other
## Patients and Social support at 03, 06, 12
for (i_ps in c("p", "s")) { # "p", "s" Patient / Social support
  for (i_time in c("03", "06", "12")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
surv_prog                                                    %in% 1:2           # surv_prog
surv_part_type                                               %in% 1:2           # surv_part_type
surv_prog == 2 | ((!is.na(surv_prog) & !is.na(surv_baseline_yn)) & (!(surv_baseline_yn == 2) | surv_one_hope_activ_1                                        %in% 1:7))  # surv_one_hope_activ_1
surv_prog == 2 | ((!is.na(surv_prog) & !is.na(surv_baseline_yn)) & (!(surv_baseline_yn == 2) | surv_one_hope_activ_2                                        %in% 1:7))  # surv_one_hope_activ_2
surv_prog == 2 | ((!is.na(surv_prog) & !is.na(surv_baseline_yn)) & (!(surv_baseline_yn == 2) | surv_one_hope_activ_3                                        %in% 1:7))  # surv_one_hope_activ_3
surv_prog == 2 | ((!is.na(surv_prog) & !is.na(surv_baseline_yn)) & (!(surv_baseline_yn == 2) | surv_one_hope_activ_4                                        %in% 1:7))  # surv_one_hope_activ_4
surv_prog == 2 | ((!is.na(surv_prog) & !is.na(surv_baseline_yn)) & (!(surv_baseline_yn == 2) | surv_one_hope_activ_5                                        %in% 1:7))  # surv_one_hope_activ_5
surv_prog == 2 | ((!is.na(surv_prog) & !is.na(surv_baseline_yn)) & (!(surv_baseline_yn == 2) | surv_one_hope_activ_6                                        %in% 1:7))  # surv_one_hope_activ_6
surv_prog == 2 | ((!is.na(surv_prog) & !is.na(surv_baseline_yn)) & (!(surv_baseline_yn == 2) | surv_one_hope_activ_7                                        %in% 1:7))  # surv_one_hope_activ_7
surv_prog == 2 | ((!is.na(surv_prog) & !is.na(surv_baseline_yn)) & (!(surv_baseline_yn == 2) | surv_one_hope_activ_8                                        %in% 1:7))  # surv_one_hope_activ_8
surv_prog == 2 | ((!is.na(surv_prog) & !is.na(surv_baseline_yn)) & (!(surv_baseline_yn == 2) | surv_one_hope_activ_9                                        %in% 1:7))  # surv_one_hope_activ_9
surv_prog == 2 | ((!is.na(surv_prog) & !is.na(surv_baseline_yn)) & (!(surv_baseline_yn == 2) | surv_one_hope_activ_12                                       %in% 1:7))  # surv_one_hope_activ_12
surv_prog == 1 | ((!is.na(surv_prog) & !is.na(surv_baseline_yn)) & (!(surv_baseline_yn == 2) | surv_cde_activ_1                                             %in% 1:2))  # surv_cde_activ_1
surv_prog == 1 | ((!is.na(surv_prog) & !is.na(surv_cde_activ_1 ) | surv_prog == 2) & (!(surv_cde_activ_1  == 1) |    surv_cde_activ_1a                      %in% 1:2))  # surv_cde_activ_1a
surv_prog == 1 | ((!is.na(surv_prog) & !is.na(surv_cde_activ_1a )) & (!(surv_prog == 2 & surv_cde_activ_1a  == 1) |  !is.na(surv_cde_activ_1a1              )))         # surv_cde_activ_1a1
surv_prog == 1 | ((!is.na(surv_prog) & !is.na(surv_cde_activ_1a1)) & (!(surv_prog == 2 & surv_cde_activ_1a1 >= 1) |  surv_cde_activ_1a3                     %in% 0:1))  # surv_cde_activ_1a3
surv_prog == 1 | ((!is.na(surv_prog) & !is.na(surv_cde_activ_1a3)) & (!(surv_prog == 2 & surv_cde_activ_1a3 == 1) |  !is.na(surv_cde_activ_1a4              )))         # surv_cde_activ_1a4
surv_prog == 1 | ((!is.na(surv_prog) & !is.na(surv_cde_activ_1a )) & (!(surv_prog == 2 & surv_cde_activ_1a  == 2) |  !is.na(surv_cde_activ_1a2              )))         # surv_cde_activ_1a2
surv_prog == 1 | ((!is.na(surv_prog) & !is.na(surv_cde_activ_1 )) & (!(surv_prog == 2 & surv_cde_activ_1  == 1) |    !is.na(surv_unm_activities             )))         # surv_unm_activities
surv_prog == 1 | ((!is.na(surv_prog) & !is.na(surv_baseline_yn)) & (!(surv_prog == 2 & surv_baseline_yn == 2) |      surv_cde_activ_2                       %in% 1:2))  # surv_cde_activ_2
surv_prog == 1 | ((!is.na(surv_prog) & !is.na(surv_cde_activ_2 )) & (!(surv_prog == 2 & surv_cde_activ_2  == 1) |    surv_cde_activ_2a                      %in% 1:2))  # surv_cde_activ_2a
surv_prog == 1 | ((!is.na(surv_prog) & !is.na(surv_cde_activ_2 )) & (!(surv_prog == 2 & surv_cde_activ_2  == 1) |    !is.na(surv_unm_activities_2           )))         # surv_unm_activities_2
surv_prog == 1 | ((!is.na(surv_prog) & !is.na(surv_baseline_yn)) & (!(surv_prog == 2 & surv_baseline_yn == 2) |      surv_cde_activ_3                       %in% 1:2))  # surv_cde_activ_3
surv_prog == 1 | ((!is.na(surv_prog) & !is.na(surv_cde_activ_3 )) & (!(surv_prog == 2 & surv_cde_activ_3  == 1) |    !is.na(surv_cde_activ_3a               )))         # surv_cde_activ_3a
surv_prog == 1 | ((!is.na(surv_prog) & !is.na(surv_cde_activ_3 )) & (!(surv_prog == 2 & surv_cde_activ_3  == 1) |    !is.na(surv_unm_activities_3           )))         # surv_unm_activities_3
surv_prog == 1 | ((!is.na(surv_prog) & !is.na(surv_baseline_yn)) & (!(surv_prog == 2 & surv_baseline_yn == 2) |      surv_cde_activ_4                       %in% 1:2))  # surv_cde_activ_4
surv_prog == 1 | ((!is.na(surv_prog) & !is.na(surv_baseline_yn)) & (!(surv_prog == 2 & surv_baseline_yn == 2) |      surv_cde_activ_5                       %in% 1:2))  # surv_cde_activ_5
surv_prog == 1 | ((!is.na(surv_prog) & !is.na(surv_cde_activ_5 )) & (!(surv_prog == 2 & surv_cde_activ_5  == 1) |    surv_cde_activ_5a                      %in% 1:2))  # surv_cde_activ_5a
surv_prog == 1 | ((!is.na(surv_prog) & !is.na(surv_baseline_yn)) & (!(surv_prog == 2 & surv_baseline_yn == 2) |      surv_cde_activ_6                       %in% 1:2))  # surv_cde_activ_6
surv_prog == 1 | ((!is.na(surv_prog) & !is.na(surv_cde_activ_6 )) & (!(surv_prog == 2 & surv_cde_activ_6  == 1) |    surv_cde_activ_6a                      %in% 1:2))  # surv_cde_activ_6a
surv_prog == 1 | ((!is.na(surv_prog) & !is.na(surv_cde_activ_6a )) & (!(surv_prog == 2 & surv_cde_activ_6a  == 1) |  !is.na(surv_cde_activ_6a1              )))         # surv_cde_activ_6a1
surv_prog == 1 | ((!is.na(surv_prog) & !is.na(surv_cde_activ_6a )) & (!(surv_prog == 2 & surv_cde_activ_6a  == 2) |  !is.na(surv_cde_activ_6a2              )))         # surv_cde_activ_6a2
surv_prog == 1 | ((!is.na(surv_prog) & !is.na(surv_baseline_yn)) & (!(surv_prog == 2 & surv_baseline_yn == 2) |      surv_cde_activ_7                       %in% 1:2))  # surv_cde_activ_7
surv_prog == 1 | ((!is.na(surv_prog) & !is.na(surv_cde_activ_7 )) & (!(surv_prog == 2 & surv_cde_activ_7  == 1) |    !is.na(surv_cde_activ_7a               )))         # surv_cde_activ_7a
surv_prog == 1 | ((!is.na(surv_prog) & !is.na(surv_baseline_yn)) & (!(surv_prog == 2 & surv_baseline_yn == 2) |      !is.na(surv_cde_activ_other            )))         # surv_cde_activ_other
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}


## notes fields below
# either no changes (first is 1, rest is 0s) OR (notes not blank AND at least one is a 1

i_name_group <- "03c-Changes"
## Patients and Social support at 03, 06, 12
#((surv_demo_changes___1 == 1) & (surv_demo_changes___2 == 0) & (surv_demo_changes___3 == 0) & (surv_demo_changes___4 == 0) & (surv_demo_changes___5 == 0) & (surv_demo_changes___6 == 0)) | (((surv_demo_changes___1 == 0) & ((surv_demo_changes___2 == 1) | (surv_demo_changes___3 == 1) | (surv_demo_changes___4 == 1) | (surv_demo_changes___5 == 1) | (surv_demo_changes___6 == 1))) & !is.na(surv_change) ) # surv_change
#((surv_health_changes___1 == 1) & (surv_health_changes___2 == 0) & (surv_health_changes___3 == 0) & (surv_health_changes___4 == 0) & (surv_health_changes___5 == 0) & (surv_health_changes___6 == 0)) | (((surv_health_changes___1 == 0) & ((surv_health_changes___2 == 1) | (surv_health_changes___3 == 1) | (surv_health_changes___4 == 1) | (surv_health_changes___5 == 1) | (surv_health_changes___6 == 1))) & !is.na(surv_change_health) )  # surv_change_health
for (i_ps in c("p")) { # "p", "s" Patient / Social support
  for (i_time in c("03", "06", "12")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
(!is.na(surv_baseline_yn) & !is.na(surv_part_type)) & (!(surv_baseline_yn == 2 | surv_part_type == 1) | surv_demo_changes___1                                        %in% 0:1)     # surv_demo_changes___1
(!is.na(surv_baseline_yn) & !is.na(surv_part_type)) & (!(surv_baseline_yn == 2 | surv_part_type == 1) | surv_demo_changes___2                                        %in% 0:1)     # surv_demo_changes___2
(!is.na(surv_baseline_yn) & !is.na(surv_part_type)) & (!(surv_baseline_yn == 2 | surv_part_type == 1) | surv_demo_changes___3                                        %in% 0:1)     # surv_demo_changes___3
(!is.na(surv_baseline_yn) & !is.na(surv_part_type)) & (!(surv_baseline_yn == 2 | surv_part_type == 1) | surv_demo_changes___4                                        %in% 0:1)     # surv_demo_changes___4
(!is.na(surv_baseline_yn) & !is.na(surv_part_type)) & (!(surv_baseline_yn == 2 | surv_part_type == 1) | surv_demo_changes___5                                        %in% 0:1)     # surv_demo_changes___5
(!is.na(surv_baseline_yn) & !is.na(surv_part_type)) & (!(surv_baseline_yn == 2 | surv_part_type == 1) | surv_demo_changes___6                                        %in% 0:1)     # surv_demo_changes___6
(!((surv_demo_changes___2 == 1) | (surv_demo_changes___3 == 1) | (surv_demo_changes___4 == 1) | (surv_demo_changes___5 == 1) | (surv_demo_changes___6 == 1))) | !is.na(surv_change)   # surv_change
(!is.na(surv_baseline_yn) & !is.na(surv_part_type)) & (!(surv_baseline_yn == 2 | surv_part_type == 1) | surv_health_changes___1                                      %in% 0:1)     # surv_health_changes___1
(!is.na(surv_baseline_yn) & !is.na(surv_part_type)) & (!(surv_baseline_yn == 2 | surv_part_type == 1) | surv_health_changes___2                                      %in% 0:1)     # surv_health_changes___2
(!is.na(surv_baseline_yn) & !is.na(surv_part_type)) & (!(surv_baseline_yn == 2 | surv_part_type == 1) | surv_health_changes___3                                      %in% 0:1)     # surv_health_changes___3
(!is.na(surv_baseline_yn) & !is.na(surv_part_type)) & (!(surv_baseline_yn == 2 | surv_part_type == 1) | surv_health_changes___4                                      %in% 0:1)     # surv_health_changes___4
(!is.na(surv_baseline_yn) & !is.na(surv_part_type)) & (!(surv_baseline_yn == 2 | surv_part_type == 1) | surv_health_changes___5                                      %in% 0:1)     # surv_health_changes___5
(!is.na(surv_baseline_yn) & !is.na(surv_part_type)) & (!(surv_baseline_yn == 2 | surv_part_type == 1) | surv_health_changes___6                                      %in% 0:1)     # surv_health_changes___6
(!((surv_health_changes___2 == 1) | (surv_health_changes___3 == 1) | (surv_health_changes___4 == 1) | (surv_health_changes___5 == 1) | (surv_health_changes___6 == 1))) | !is.na(surv_change_health)   # surv_change_health
(!is.na(surv_baseline_yn) & !is.na(surv_part_type)) & (!(surv_baseline_yn == 2 | surv_part_type == 1) | surv_meds_change                                             %in% 1:2)     # surv_meds_change
(!(surv_meds_change == 1) | !is.na(surv_meds_change_descr))                                                                                                                        # surv_meds_change_descr
(!is.na(surv_baseline_yn) & !is.na(surv_part_type)) & (!(surv_baseline_yn == 2 | surv_part_type == 2) | surv_demo_changes_soc___1                                    %in% 0:1)     # surv_demo_changes_soc___1
(!is.na(surv_baseline_yn) & !is.na(surv_part_type)) & (!(surv_baseline_yn == 2 | surv_part_type == 2) | surv_demo_changes_soc___2                                    %in% 0:1)     # surv_demo_changes_soc___2
(!is.na(surv_baseline_yn) & !is.na(surv_part_type)) & (!(surv_baseline_yn == 2 | surv_part_type == 2) | surv_demo_changes_soc___3                                    %in% 0:1)     # surv_demo_changes_soc___3
(!(surv_demo_changes_soc___2 == 1) | (surv_demo_changes_soc___3 == 1)) | !is.na(surv_change_soc)                                                                                    # surv_change_soc
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}



## moved these two lines into 05a and 05b
# i_name_group <- "04-BRFSS"
# ## Patients and Social support at 00, 03, 06, 12
# for (i_ps in c("p", "s")) { # "p", "s" Patient / Social support
#   for (i_time in c("00", "03", "06", "12")) { # "00", "03", "06", "12" month of data collection
# writeLines(text = "
# ", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
#   }
# }


i_name_group <- "05a-CAHPSCC"
## Patients at 03, 06, 12
#cahpscc_pat_24            %in% 1:4                                                                                        # cahpscc_pat_24
for (i_ps in c("p")) { # "p", "s" Patient / Social support
  for (i_time in c("03", "06", "12")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
(!is.na(surv_part_type)) & (!(surv_part_type == 1) |            brfss_pat_1                                                  %in% 1:5)           # brfss_pat_1
cahpscc_pat_01            %in% 1:4                                                                                        # cahpscc_pat_01
!is.na(cahpscc_pat_01) & (cahpscc_pat_01 == 1 | cahpscc_pat_01_lang       %in% c(1:3, 98)                               ) # cahpscc_pat_01_lang
!is.na(cahpscc_pat_01) & (cahpscc_pat_01 == 1 | cahpscc_pat_01_lang %in% c(1, 2, 98) | !is.na(cahpscc_pat_01_lang_other)) # cahpscc_pat_01_lang_other
cahpscc_pat_02            %in% 1:4                                                                                        # cahpscc_pat_02
cahpscc_pat_03            %in% 1:4                                                                                        # cahpscc_pat_03
cahpscc_pat_04            %in% 1:4                                                                                        # cahpscc_pat_04
cahpscc_pat_05            %in% 1:4                                                                                        # cahpscc_pat_05
cahpscc_pat_06            %in% 1:4                                                                                        # cahpscc_pat_06
cahpscc_pat_07            %in% 1:4                                                                                        # cahpscc_pat_07
cahpscc_pat_08            %in% 1:4                                                                                        # cahpscc_pat_08
cahpscc_pat_09            %in% 1:2                                                                                        # cahpscc_pat_09
cahpscc_pat_10            %in% 1:2                                                                                        # cahpscc_pat_10
cahpscc_pat_11            %in% 1:2                                                                                        # cahpscc_pat_11
cahpscc_pat_12            %in% 1:2                                                                                        # cahpscc_pat_12
cahpscc_pat_13            %in% 1:2                                                                                        # cahpscc_pat_13
cahpscc_pat_14            %in% 1:4                                                                                        # cahpscc_pat_14
cahpscc_pat_15            %in% 1:4                                                                                        # cahpscc_pat_15
cahpscc_pat_16            %in% 1:3                                                                                        # cahpscc_pat_16
cahpscc_pat_17            %in% 1:3                                                                                        # cahpscc_pat_17
cahpscc_pat_18            %in% 1:3                                                                                        # cahpscc_pat_18
cahpscc_pat_19            %in% 1:3                                                                                        # cahpscc_pat_19
cahpscc_pat_20            %in% 1:3                                                                                        # cahpscc_pat_20
cahpscc_pat_21            %in% 0:10                                                                                       # cahpscc_pat_21
cahpscc_pat_22            %in% 1:2                                                                                        # cahpscc_pat_22
!is.na(cahpscc_pat_22) & (!(cahpscc_pat_22 == 2) | !is.na(cahpscc_pat_22a)           )                                       # cahpscc_pat_22a
!is.na(cahpscc_pat_22) & (!(cahpscc_pat_22 == 2) | cahpscc_pat_23            %in% 1:4)                                       # cahpscc_pat_23
!is.na(cahpscc_pat_23) & (!(cahpscc_pat_23 %in% 2:4) | cahpscc_pat_24            %in% 1:4)                                       # cahpscc_pat_24
cahpscc_pat_25            %in% 1:2                                                                                        # cahpscc_pat_25
!is.na(cahpscc_pat_25) & (!(cahpscc_pat_25 == 1) | cahpscc_pat_26            %in% 1:2 )                                      # cahpscc_pat_26
!is.na(cahpscc_pat_25) & (!(cahpscc_pat_25 == 1) | cahpscc_pat_27            %in% 1:4 )                                      # cahpscc_pat_27
!is.na(cahpscc_pat_25) & (!(cahpscc_pat_25 == 1 | cahpscc_pat_27 != 1) | cahpscc_pat_28            %in% 1:4 )                                      # cahpscc_pat_28
!is.na(cahpscc_pat_25) & (!(cahpscc_pat_25 == 1 | cahpscc_pat_27 != 1) | cahpscc_pat_29            %in% 1:4 )                                      # cahpscc_pat_29
!is.na(cahpscc_pat_25) & (!(cahpscc_pat_25 == 1 | cahpscc_pat_27 != 1) | cahpscc_pat_30            %in% 0:10)                                      # cahpscc_pat_30
!is.na(cahpscc_pat_25) & (!(cahpscc_pat_25 == 1 | cahpscc_pat_27 != 1) | cahpscc_pat_31            %in% 1:2 )                                      # cahpscc_pat_31
!is.na(cahpscc_pat_25) & (!(cahpscc_pat_25 == 1 | cahpscc_pat_27 != 1 | cahpscc_pat_31 == 1) | cahpscc_pat_32            %in% 1:2 )                                      # cahpscc_pat_32
cahpscc_pat_33            %in% 1:4                                                                                        # cahpscc_pat_33
!is.na(cahpscc_pat_33) & (!(cahpscc_pat_33 == 1) | cahpscc_pat_34            %in% 1:2)                                       # cahpscc_pat_34
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}


i_name_group <- "05b-CAHPSCC"
## Social support at 03, 06, 12
for (i_ps in c("s")) { # "p", "s" Patient / Social support
  for (i_time in c("03", "06", "12")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
(!is.na(surv_part_type)) & (!(surv_part_type == 2) |            brfss_soc_1                                                  %in% 1:5)           # brfss_soc_1
(!is.na(surv_part_type)) & (!(surv_part_type == 2) | cahpscc_soc_01            %in% 1:5 )                                                 # cahpscc_soc_01
(!is.na(surv_part_type)) & (!(surv_part_type == 2) | cahpscc_soc_02            %in% 1:3 )                                                 # cahpscc_soc_02
(!is.na(surv_part_type) & !is.na(cahpscc_soc_02)) & (!((surv_part_type == 2) & (cahpscc_soc_02 == 1)) | cahpscc_soc_02a           %in% 1:4) # cahpscc_soc_02a
(!is.na(surv_part_type) & !is.na(cahpscc_soc_02)) & (!((surv_part_type == 2) & (cahpscc_soc_02 == 4)) | !is.na(cahpscc_soc_02a_other) )     # cahpscc_soc_02a_other
(!is.na(surv_part_type)) & (!(surv_part_type == 2) | cahpscc_soc_03            %in% 1:3 )                                                 # cahpscc_soc_03
(!is.na(surv_part_type) & !is.na(cahpscc_soc_03)) & (!((surv_part_type == 2) & (cahpscc_soc_03 == 1)) | !is.na(cahpscc_soc_03b)  )        # cahpscc_soc_03b
(!is.na(surv_part_type)) & (!(surv_part_type == 2) | cahpscc_soc_04            %in% 1:3 )                                                 # cahpscc_soc_04
(!is.na(surv_part_type)) & (!(surv_part_type == 2) | cahpscc_soc_05            %in% 1:3 )                                                 # cahpscc_soc_05
(!is.na(surv_part_type)) & (!(surv_part_type == 2) | cahpscc_soc_06            %in% 1:5 )                                                 # cahpscc_soc_06
(!is.na(surv_part_type)) & (!(surv_part_type == 2) | cahpscc_soc_07            %in% 1:3 )                                                 # cahpscc_soc_07
(!is.na(surv_part_type) & !is.na(cahpscc_soc_07)) & (!((surv_part_type == 2) & (cahpscc_soc_07 == 1)) | cahpscc_soc_07a           %in% 1:5) # cahpscc_soc_07a
(!is.na(surv_part_type)) & (!(surv_part_type == 2) | cahpscc_soc_08            %in% 1:3 )                                                 # cahpscc_soc_08
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}


i_name_group <- "06-DKQ"
## Patients and Social support at 00, 03, 06, 12
for (i_ps in c("p", "s")) { # "p", "s" Patient / Social support
  for (i_time in c("00", "03", "06", "12")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
dkq_1                     %in% 1:3                                              # dkq_1
dkq_2                     %in% 1:3                                              # dkq_2
dkq_3                     %in% 1:3                                              # dkq_3
dkq_4                     %in% 1:3                                              # dkq_4
dkq_5                     %in% 1:3                                              # dkq_5
dkq_6                     %in% 1:3                                              # dkq_6
dkq_7                     %in% 1:3                                              # dkq_7
dkq_8                     %in% 1:3                                              # dkq_8
dkq_9                     %in% 1:3                                              # dkq_9
dkq_10                    %in% 1:3                                              # dkq_10
dkq_11                    %in% 1:3                                              # dkq_11
dkq_12                    %in% 1:3                                              # dkq_12
dkq_13                    %in% 1:3                                              # dkq_13
dkq_14                    %in% 1:3                                              # dkq_14
dkq_15                    %in% 1:3                                              # dkq_15
dkq_16                    %in% 1:3                                              # dkq_16
dkq_17                    %in% 1:3                                              # dkq_17
dkq_18                    %in% 1:3                                              # dkq_18
dkq_19                    %in% 1:3                                              # dkq_19
dkq_20                    %in% 1:3                                              # dkq_20
dkq_21                    %in% 1:3                                              # dkq_21
dkq_22                    %in% 1:3                                              # dkq_22
dkq_23                    %in% 1:3                                              # dkq_23
dkq_24                    %in% 1:3                                              # dkq_24
dkq_25                    %in% 1:3                                              # dkq_25
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}


i_name_group <- "07a-PAM"
## Patients at 00, 03, 06, 12
for (i_ps in c("p")) { # "p", "s" Patient / Social support
  for (i_time in c("00", "03", "06", "12")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
pam_pat_1                 %in% 1:5                                              # pam_pat_1
pam_pat_2                 %in% 1:5                                              # pam_pat_2
pam_pat_3                 %in% 1:5                                              # pam_pat_3
pam_pat_4                 %in% 1:5                                              # pam_pat_4
pam_pat_5                 %in% 1:5                                              # pam_pat_5
pam_pat_6                 %in% 1:5                                              # pam_pat_6
pam_pat_7                 %in% 1:5                                              # pam_pat_7
pam_pat_8                 %in% 1:5                                              # pam_pat_8
pam_pat_9                 %in% 1:5                                              # pam_pat_9
pam_pat_10                %in% 1:5                                              # pam_pat_10
pam_pat_11                %in% 1:5                                              # pam_pat_11
pam_pat_12                %in% 1:5                                              # pam_pat_12
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}


i_name_group <- "07b-PAM"
## Social support at 00, 03, 06, 12
for (i_ps in c("s")) { # "p", "s" Patient / Social support
  for (i_time in c("00", "03", "06", "12")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
pam_soc_1                 %in% 1:5                                              # pam_soc_1
pam_soc_2                 %in% 1:5                                              # pam_soc_2
pam_soc_3                 %in% 1:5                                              # pam_soc_3
pam_soc_4                 %in% 1:5                                              # pam_soc_4
pam_soc_5                 %in% 1:5                                              # pam_soc_5
pam_soc_12                %in% 1:5                                              # pam_soc_12
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}


i_name_group <- "08a-PHQ9"
## Patients at 00, 03, 06, 12
for (i_ps in c("p")) { # "p", "s" Patient / Social support
  for (i_time in c("00", "03", "06", "12")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
phq9_pat_1               %in% 0:3                                               # phq9_pat_1
phq9_pat_2               %in% 0:3                                               # phq9_pat_2
phq9_pat_3               %in% 0:3                                               # phq9_pat_3
phq9_pat_4               %in% 0:3                                               # phq9_pat_4
phq9_pat_5               %in% 0:3                                               # phq9_pat_5
phq9_pat_6               %in% 0:3                                               # phq9_pat_6
phq9_pat_7               %in% 0:3                                               # phq9_pat_7
phq9_pat_8               %in% 0:3                                               # phq9_pat_8
phq9_pat_9               %in% 0:3                                               # phq9_pat_9
phq9_pat_total_score     %in% 0:27                                              # phq9_pat_total_score
(!is.na(surv_part_type) & !is.na(phq9_pat_total_score)) & (surv_part_type != 1 | phq9_pat_total_score == 0 | phq9_pat_how_difficult   %in% 1:4)                                               # phq9_pat_how_difficult
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}


i_name_group <- "08b-PHQ9"
## Social support at 00, 03, 06, 12
for (i_ps in c("s")) { # "p", "s" Patient / Social support
  for (i_time in c("00", "03", "06", "12")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
phq9_soc_1               %in% 0:3                                               # phq9_soc_1
phq9_soc_2               %in% 0:3                                               # phq9_soc_2
phq9_soc_3               %in% 0:3                                               # phq9_soc_3
phq9_soc_4               %in% 0:3                                               # phq9_soc_4
phq9_soc_5               %in% 0:3                                               # phq9_soc_5
phq9_soc_6               %in% 0:3                                               # phq9_soc_6
phq9_soc_7               %in% 0:3                                               # phq9_soc_7
phq9_soc_8               %in% 0:3                                               # phq9_soc_8
phq9_soc_9               %in% 0:3                                               # phq9_soc_9
phq9_soc_total_score     %in% 0:27                                              # phq9_soc_total_score
(!is.na(phq9_soc_total_score)) & (!(phq9_soc_total_score > 0) | phq9_soc_how_difficult   %in% 1:4  )  # phq9_soc_how_difficult
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}


i_name_group <- "09a-Stress"
## Patients at 00, 06
for (i_ps in c("p")) { # "p", "s" Patient / Social support
  for (i_time in c("06")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
stress_1                 %in% 0:3                                               # stress_1
stress_2                 %in% 1:2                                               # stress_2
!is.na(stress_3)                                                                # stress_3
stress_4                 %in% 1:2                                               # stress_4
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}


i_name_group <- "09b-Hair"
## Patients at 00, 06
for (i_ps in c("p")) { # "p", "s" Patient / Social support
  for (i_time in c("06")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
hair_pat_1               %in% 1:5                                               # hair_pat_1
hair_pat_2               %in% 1:5                                               # hair_pat_2
hair_pat_3               %in% 1:2                                               # hair_pat_3
!is.na(hair_pat_3a)                                                             # hair_pat_3a
hair_pat_4               %in% 1:2                                               # hair_pat_4
!is.na(hair_pat_4a)                                                             # hair_pat_4a
!is.na(hair_pat_5)                                                              # hair_pat_5
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}


i_name_group <- "09c-Hair"
## Social support at 00, 06
for (i_ps in c("s")) { # "p", "s" Patient / Social support
  for (i_time in c("06")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
hair_soc_1               %in% 1:5                                               # hair_soc_1
hair_soc_2               %in% 1:5                                               # hair_soc_2
hair_soc_3               %in% 1:2                                               # hair_soc_3
!is.na(hair_soc_3a)                                                             # hair_soc_3a
hair_soc_4               %in% 1:2                                               # hair_soc_4
!is.na(hair_soc_4a)                                                             # hair_soc_4a
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}


## i_name_group <- "01-ParticipantContactInfo"
## for (i_ps in c("p", "s")) { # "p", "s" Patient / Social support
##   for (i_time in c("00", "03", "06", "12")) { # "00", "03", "06", "12" month of data collection
## writeLines(text = "
##
##
## ## Patients and Social support at 00, 03, 06, 12
## # surv_comments
## encuesta_oral_oral_survey_complete     %in% 0:2
## ", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
##   }
## }


i_name_group <- "10a-BMI"
#indice_de_masa_corporal_bmi_timestamp
## Patients at 00, 03, 06, 12
# bmi_comments
for (i_ps in c("p")) { # "p", "s" Patient / Social support
  for (i_time in c("00", "03", "06", "12")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
bmi_part_type  %in% 1:2   # bmi_part_type
!is.na(bmi_part_type) & (bmi_part_type != 1 | !is.na(bmi_height_1))    # bmi_height_1
!is.na(bmi_part_type) & (bmi_part_type != 1 | !is.na(bmi_weight_1))    # bmi_weight_1
!is.na(bmi_part_type) & (bmi_part_type != 1 | !is.na(bmi_height_2))    # bmi_height_2
!is.na(bmi_part_type) & (bmi_part_type != 1 | !is.na(bmi_weight_2))    # bmi_weight_2
indice_de_masa_corporal_bmi_complete  %in%  0:2                        # indice_de_masa_corporal_bmi_complete
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}


i_name_group <- "10b-A1c-a"
# Patients at 00
#a1c_baseline_no_reason               # a1c_baseline_no_reason
for (i_ps in c("p")) { # "p", "s" Patient / Social support
  for (i_time in c("00")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
a1c_baseline                         # a1c_baseline
a1c_baseline_cde                     # a1c_baseline_cde
a1c_baseline_oh                      # a1c_baseline_oh
a1c_unm_date                         # a1c_unm_date
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}


i_name_group <- "10b-A1c-a"
# Patients at 00
#a1c_unm_date                         # a1c_unm_date
#(is.na(a1c_baseline) & is.na(a1c_baseline_cde) & is.na(a1c_baseline_oh)) & !is.na(a1c_baseline_no_reason)  # a1c_baseline_no_reason
for (i_ps in c("p")) { # "p", "s" Patient / Social support
  for (i_time in c("00")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
!is.na(a1c_baseline)                         # a1c_baseline
!is.na(a1c_baseline_cde)                     # a1c_baseline_cde
!is.na(a1c_baseline_oh)                      # a1c_baseline_oh
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}


i_name_group <- "10b-A1c-b"
# Patients at 03
#!is.na(a1c_3_mo)                             # a1c_3_mo
for (i_ps in c("p")) { # "p", "s" Patient / Social support
  for (i_time in c("03")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
!(a1c_3_mo == \"\")                       # a1c_3_mo
(a1c_3_mo == \"\") & !(a1c_3_mo_no_reason == \"\")          # a1c_3_mo_no_reason
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}


i_name_group <- "10b-A1c-c"
# Patients at 06
for (i_ps in c("p")) { # "p", "s" Patient / Social support
  for (i_time in c("06")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
!(a1c_6_mo == \"\")                       # a1c_6_mo
(a1c_6_mo == \"\") & !(a1c_6_mo_no_reason == \"\")          # a1c_6_mo_no_reason
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}


i_name_group <- "10b-A1c-d"
# Patients at 06
for (i_ps in c("p")) { # "p", "s" Patient / Social support
  for (i_time in c("12")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
!(a1c_12_mo == \"\")                       # a1c_12_mo
(a1c_12_mo == \"\") & !(a1c_12_mo_no_reason == \"\")          # a1c_12_mo_no_reason
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}


i_name_group <- "10b-A1c-e"
# Patients at 00, 03, 06, 12
#a1c_notes                           # #a1c_notes
for (i_ps in c("p")) { # "p", "s" Patient / Social support
  for (i_time in c("00", "03", "06", "12")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
a1c_complete   %in% 0:2                      # a1c_complete
a1c_complete   %in% 0:2                      # a1c_complete_DUP
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}


i_name_group <- "10c-Hair-a"
# Patients at 00
for (i_ps in c("p")) { # "p", "s" Patient / Social support
  for (i_time in c("00")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
!is.na(hair_baseline)                              # hair_baseline
!is.na(hair_baseline)                              # hair_baseline_DUP
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}


i_name_group <- "10c-Hair-b"
# Patients at 06                           # # Patients at 06
for (i_ps in c("p")) { # "p", "s" Patient / Social support
  for (i_time in c("06")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
!is.na(hair_6_mo)                                  # hair_6_mo
!is.na(hair_6_mo)                                  # hair_6_mo_DUP
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}


i_name_group <- "10c-Hair-c"
# Patients at 00, 06                       # # Patients at 00, 06
for (i_ps in c("p")) { # "p", "s" Patient / Social support
  for (i_time in c("00", "06")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
hair_cortisol_complete  %in%               0:2 # hair_cortisol_complete
hair_cortisol_complete  %in%               0:2 # hair_cortisol_complete_DUP
(!is.na(final_part_type) & !is.na(final_hair_collect_yn)) & (!(final_part_type == 1 & final_hair_collect_yn == 2) | final_hair_collect_no_why                  %in% 1:3)             # final_hair_collect_no_why
(!is.na(final_part_type) & !is.na(final_hair_collect_yn) & !is.na(final_hair_collect_no_why)) & (!(final_part_type == 1 & final_hair_collect_yn == 2 & final_hair_collect_no_why == 3) | !(final_hair_collect_no_other == \"\"))                             # final_hair_collect_no_other
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}



## Conditionals
# If A, then ask about B
# !is.na(A) & (not A | B)


i_name_group <- "11-Final-a"
# Patients at 00, 03, 06, 12
#preguntas_finales_final_questions_timestamp             # preguntas_finales_final_questions_timestamp
for (i_ps in c("p")) { # "p", "s" Patient / Social support
  for (i_time in c("00", "03", "06", "12")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
final_part_type                            %in% 1:2             # final_part_type
final_data_collect_time                    %in% 1:4             # final_data_collect_time
(!is.na(final_part_type) & !is.na(final_data_collect_time)) & (!(final_part_type == 1 & final_data_collect_time == 3) | final_hair_collect_yn  %in% 1:2)             # final_hair_collect_yn
(!is.na(final_part_type)) & (!(final_part_type == 1) | final_blood_collect                        %in% 1:2)             # final_blood_collect
(!is.na(final_part_type) & !is.na(final_blood_collect) & !is.na(final_data_collect_time)) & (!(final_part_type == 1 & final_blood_collect == 2 & final_data_collect_time == 1) | final_blood_collect_no_why                 %in% 1:4)             # final_blood_collect_no_why
(!is.na(final_part_type) & !is.na(final_blood_collect) & !is.na(final_data_collect_time)) & (!(final_part_type == 1 & final_blood_collect == 2 & final_data_collect_time != 1) | final_blood_collect_no_why_fu              %in% 1:3)             # final_blood_collect_no_why_fu
(!is.na(final_part_type) & !is.na(final_blood_collect) & !is.na(final_blood_collect_no_why) & !is.na(final_blood_collect_no_why_fu)) & (!(final_part_type == 1 & final_blood_collect == 2 & final_blood_collect_no_why == 4 & final_blood_collect_no_why_fu == 3) | !(final_blood_collect_no_other == \"\"))              # final_blood_collect_no_other
(!is.na(final_part_type)) & (!(final_part_type == 1) | final_bmi_collect                          %in% 0:2)             # final_bmi_collect
(!is.na(final_part_type) & !is.na(final_bmi_collect)) & (!(final_part_type == 1 & final_bmi_collect %in% 0:1) | final_bmi_collect_no_why                   %in% 1:3)             # final_bmi_collect_no_why
(!is.na(final_part_type) & !is.na(final_bmi_collect) & !is.na(final_bmi_collect_no_why)) & (!(final_part_type == 1 & final_bmi_collect %in% 0:1 & final_bmi_collect_no_why == 3) | !(final_bmi_collect_no_other == \"\"))   # final_bmi_collect_no_other
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}


i_name_group <- "11-Final-b"
# Patients and Social support at 00, 03, 06, 12
#preguntas_finales_final_questions_timestamp             # preguntas_finales_final_questions_timestamp
for (i_ps in c("p", "s")) { # "p", "s" Patient / Social support
  for (i_time in c("00", "03", "06", "12")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
final_appt_language                        %in% 1:4             # final_appt_language
(!is.na(final_appt_language)) & (!(final_appt_language == 4) | !(final_appt_language_other == \"\"))  # final_appt_language_other
final_merch_card                           %in% 1:2             # final_merch_card
(!is.na(final_merch_card)) & (!(final_merch_card == 1) | final_merch_card_receipt %in% 1:2)  # final_merch_card_receipt
(!is.na(final_merch_card)) & (!(final_merch_card == 1) | !is.na(final_merch_card_no))  # final_merch_card_no
(!is.na(final_data_collect_time)) & (!(final_data_collect_time == 1) | !is.na(final_next_dca_dtd))      # final_next_dca_dtd
(!is.na(final_data_collect_time)) & (!(final_data_collect_time == 2) | !is.na(final_next_dca_dtd_6mo))  # final_next_dca_dtd_6mo
(!is.na(final_data_collect_time)) & (!(final_data_collect_time == 3) | !is.na(final_next_dca_dtd2))     # final_next_dca_dtd2
(!is.na(final_data_collect_time)) & (!(final_data_collect_time %in% 1:3) | !is.na(final_dca_next_time))     # final_dca_next_time
!is.na(final_time_end)  # final_time_end
!is.na(final_comments)  # final_comments
preguntas_finales_final_questions_complete %in% 0:2             # preguntas_finales_final_questions_complete
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}




i_name_group <- "12-Qual"
## Patients and Social support at 12
for (i_ps in c("p", "s")) { # "p", "s" Patient / Social support
  for (i_time in c("12")) { # "00", "03", "06", "12" month of data collection
writeLines(text = "
qual_data_collection          %in% 1:4            # qual_data_collection
!is.na(qual_research_staff)                       # qual_research_staff
qual_focus_grp                %in% 1:3            # qual_focus_grp
(!is.na(qual_focus_grp)) & (!(qual_focus_grp == 3) | !is.na(qual_no_contact))  # qual_no_contact
!is.na(qual_focus_grp_date)                       # qual_focus_grp_date
qual_interview                %in% 1:3            # qual_interview
(!is.na(qual_interview)) & (!(qual_interview == 3) | !is.na(qual_no_contact_2))                         # qual_no_contact_2
!is.na(qual_interview_date)                       # qual_interview_date
!is.na(qual_transcript)                           # qual_transcript
!is.na(qual_notes)                                # qual_notes
qualitative_participation_complete  %in% 0:2      # qualitative_participation_complete
", con = paste0(subpath, "/", "unmpdp_data_cleaning_T", i_time, "_item-", i_name_group, "_P", i_ps, ".txt"))
  }
}



#redcap_event_name.factor
#pci_prog_contact.factor
#pci_data_col_id.factor
#pci_locale.factor
#pci_part_type.factor
#informacin_del_contacto_participant_contact_info_complete.factor
#pdi_data_col_id_demog.factor
#pdi_part_type.factor
#pdi_how_heard___1.factor
#pdi_how_heard___2.factor
#pdi_how_heard___3.factor
#pdi_how_heard___4.factor
#pdi_how_heard___5.factor
#pdi_how_heard___6.factor
#pdi_how_heard___7.factor
#pdi_how_heard___8.factor
#pdi_sex.factor
#pdi_marital_stat.factor
#pdi_household_number.factor
#pdi_educ.factor
#pdi_language___1.factor
#pdi_language___2.factor
#pdi_language___3.factor
#pdi_lang_pref.factor
#pdi_eng_prof.factor
#pdi_born_us.factor
#pdi_diagnosis.factor
#pdi_diag_one_hope.factor
#pdi_diag_other.factor
#pdi_meds_1.factor
#pdi_meds_as_prescribed.factor
#pdi_supps.factor
#pdi_family_diag.factor
#pdi_smoke.factor
#pdi_smoke_prev.factor
#pdi_smoke_house.factor
#pdi_diabetes_prog.factor
#pdi_pat_rel_soc.factor
#pdi_pat_involve.factor
#pdi_pat_accomp.factor
#pdi_pat_know.factor
#pdi_pat_interact.factor
#pdi_pat_interact_type.factor
#pdi_pat_live_with.factor
#pdi_soc_rel_pat.factor
#pdi_soc_involve.factor
#pdi_soc_accomp.factor
#pdi_soc_know.factor
#pdi_soc_interact.factor
#pdi_soc_interact_type.factor
#pdi_soc_live_with.factor
#informacin_demogrfica_participant_demographic_info_complete.factor
#surv_baseline_yn.factor
#surv_fu_which.factor
#surv_fu_data_col_id.factor
#surv_fu_locale.factor
#surv_prog.factor
#surv_part_type.factor
#surv_one_hope_activ_1.factor
#surv_one_hope_activ_2.factor
#surv_one_hope_activ_3.factor
#surv_one_hope_activ_4.factor
#surv_one_hope_activ_5.factor
#surv_one_hope_activ_6.factor
#surv_one_hope_activ_7.factor
#surv_one_hope_activ_8.factor
#surv_one_hope_activ_9.factor
#surv_one_hope_activ_12.factor
#surv_cde_activ_1.factor
#surv_cde_activ_1a.factor
#surv_cde_activ_2.factor
#surv_cde_activ_2a.factor
#surv_cde_activ_3.factor
#surv_cde_activ_4.factor
#surv_cde_activ_5.factor
#surv_cde_activ_5a.factor
#surv_cde_activ_6.factor
#surv_cde_activ_6a.factor
#surv_cde_activ_7.factor
#surv_demo_changes___1.factor
#surv_demo_changes___2.factor
#surv_demo_changes___3.factor
#surv_demo_changes___4.factor
#surv_demo_changes___5.factor
#surv_demo_changes___6.factor
#surv_health_changes___1.factor
#surv_health_changes___2.factor
#surv_health_changes___3.factor
#surv_health_changes___4.factor
#surv_health_changes___5.factor
#surv_health_changes___6.factor
#surv_meds_change.factor
#surv_demo_changes_soc___1.factor
#surv_demo_changes_soc___2.factor
#surv_demo_changes_soc___3.factor
#brfss_pat_1.factor
#brfss_soc_1.factor
#cahpscc_pat_01.factor
#cahpscc_pat_01_lang.factor
#cahpscc_pat_01_lang_other.factor
#cahpscc_pat_02.factor
#cahpscc_pat_03.factor
#cahpscc_pat_04.factor
#cahpscc_pat_05.factor
#cahpscc_pat_06.factor
#cahpscc_pat_07.factor
#cahpscc_pat_08.factor
#cahpscc_pat_09.factor
#cahpscc_pat_10.factor
#cahpscc_pat_11.factor
#cahpscc_pat_12.factor
#cahpscc_pat_13.factor
#cahpscc_pat_14.factor
#cahpscc_pat_15.factor
#cahpscc_pat_16.factor
#cahpscc_pat_17.factor
#cahpscc_pat_18.factor
#cahpscc_pat_19.factor
#cahpscc_pat_20.factor
#cahpscc_pat_21.factor
#cahpscc_pat_22.factor
#cahpscc_pat_23.factor
#cahpscc_pat_24.factor
#cahpscc_pat_25.factor
#cahpscc_pat_26.factor
#cahpscc_pat_27.factor
#cahpscc_pat_28.factor
#cahpscc_pat_29.factor
#cahpscc_pat_30.factor
#cahpscc_pat_31.factor
#cahpscc_pat_32.factor
#cahpscc_pat_33.factor
#cahpscc_pat_34.factor
#cahpscc_soc_01.factor
#cahpscc_soc_02.factor
#cahpscc_soc_02a.factor
#cahpscc_soc_03.factor
#cahpscc_soc_04.factor
#cahpscc_soc_05.factor
#cahpscc_soc_06.factor
#cahpscc_soc_07.factor
#cahpscc_soc_07a.factor
#cahpscc_soc_08.factor
#dkq_1.factor
#dkq_2.factor
#dkq_3.factor
#dkq_4.factor
#dkq_5.factor
#dkq_6.factor
#dkq_7.factor
#dkq_8.factor
#dkq_9.factor
#dkq_10.factor
#dkq_11.factor
#dkq_12.factor
#dkq_13.factor
#dkq_14.factor
#dkq_15.factor
#dkq_16.factor
#dkq_17.factor
#dkq_18.factor
#dkq_19.factor
#dkq_20.factor
#dkq_21.factor
#dkq_22.factor
#dkq_23.factor
#dkq_24.factor
#dkq_25.factor
#pam_pat_1.factor
#pam_pat_2.factor
#pam_pat_3.factor
#pam_pat_4.factor
#pam_pat_5.factor
#pam_pat_6.factor
#pam_pat_7.factor
#pam_pat_8.factor
#pam_pat_9.factor
#pam_pat_10.factor
#pam_pat_11.factor
#pam_pat_12.factor
#pam_soc_1.factor
#pam_soc_2.factor
#pam_soc_3.factor
#pam_soc_4.factor
#pam_soc_5.factor
#pam_soc_12.factor
#phq9_pat_1.factor
#phq9_pat_2.factor
#phq9_pat_3.factor
#phq9_pat_4.factor
#phq9_pat_5.factor
#phq9_pat_6.factor
#phq9_pat_7.factor
#phq9_pat_8.factor
#phq9_pat_9.factor
#phq9_pat_how_difficult.factor
#phq9_soc_1.factor
#phq9_soc_2.factor
#phq9_soc_3.factor
#phq9_soc_4.factor
#phq9_soc_5.factor
#phq9_soc_6.factor
#phq9_soc_7.factor
#phq9_soc_8.factor
#phq9_soc_9.factor
#phq9_soc_how_difficult.factor
#stress_1.factor
#stress_2.factor
#stress_4.factor
#hair_pat_1.factor
#hair_pat_2.factor
#hair_pat_3.factor
#hair_pat_4.factor
#hair_soc_1.factor
#hair_soc_2.factor
#hair_soc_3.factor
#hair_soc_4.factor
#encuesta_oral_oral_survey_complete.factor
#bmi_part_type.factor
#indice_de_masa_corporal_bmi_complete.factor
#a1c_complete.factor
#hair_cortisol_complete.factor
#final_part_type.factor
#final_data_collect_time.factor
#final_hair_collect_yn.factor
#final_hair_collect_no_why.factor
#final_blood_collect.factor
#final_blood_collect_no_why.factor
#final_bmi_collect.factor
#final_bmi_collect_no_why.factor
#final_appt_language.factor
#final_merch_card.factor
#final_merch_card_receipt.factor
#final_next_dca.factor
#final_dca_which.factor
#preguntas_finales_final_questions_complete.factor
#qual_data_collection.factor
#qual_focus_grp.factor
#qual_interview.factor
#qualitative_participation_complete.factor
#pci_date_appt
#pci_part_id_num
#pci_part_id_ps
#pci_part_id_ps.factor
#age
#bmi_height_cm
#bmi_weight_kg
#bmi
#bmi_factor
#comorbid_acid_reflux
#comorbid_anemia
#comorbid_arthritis
#comorbid_breathing_problems
#comorbid_cancer
#comorbid_carpal_tunnel_syndrome
#comorbid_fibromyalgia
#comorbid_heart_condition
#comorbid_high_cholesterol
#comorbid_hypertension
#comorbid_kidney_disease
#comorbid_lupus_and_fibromyalgia
#comorbid_nerve_damage_and/or_neuropathy
#comorbid_scoliosis
#comorbid_tendonitis
#comorbid_thyroid_condition
#comorbid_ulcer
#comorbid_vertigo

}
