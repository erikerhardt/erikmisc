#' Produces a consort diagram based on PreScreen, Eligibility, and PDP data files
#'
#' @param dat_PreScr
#' @param dat_Elig
#' @param dat_pdp
#' @param sw_drop_dups drop duplicate names between PreScr, Elig, and PDP data before counting?
#' @param sw_plot plot the Sankey diagram?
#' @param sw_short_labels use abbreviations for labels in plot?
#' @param report_date prints the date for the report, if specified; otherwise, prints the current date/time on the figure.
#'
#' @return Returns \code{n_patients}, the table of patient fates
#' @import dplyr
#' @import stringr
#' @export
#'
#' @examples
#' consort_diagram()
consort_diagram <- function(dat_PreScr = dat_PreScr, dat_Elig = dat_Elig, dat_pdp = dat_pdp, sw_drop_dups = FALSE, sw_plot = TRUE, sw_short_labels = TRUE, report_date = NULL) {

  fn_consort_details <- "consort_details.txt"
  cat("Consort Diagram details\n\n\n", file = fn_consort_details, append = FALSE)
  cat("Subject IDs for each subset of consort counting\n\n", file = fn_consort_details, append = TRUE)

  # print number of people and then output the data
  f_count_and_IDs <- function(dat, fn_consort_details) {
    cat(paste(nrow(dat), "people\n"), file = fn_consort_details, append = TRUE)
    capture.output(dat, file = fn_consort_details, append = TRUE)
    cat("\n\n\n", file = fn_consort_details, append = TRUE)
  }

  # order of nodes is from bottom up for each x position
  n_patients <- read.table(text = "
  ID                  x labels             n
  LeftMargin       0.99 LeftMargin         0
  RecTotal            1 Recruited          NA
  RecInterested       2 Interested         NA
  RecNotInterested    2 Not_Interested     NA
  Eligible            3 Eligible           NA
  EligInProcess       3 In_Process_Elig    NA
  EligNotParticpate   3 Do_Not_Participate NA
  EligNot             3 Not_Eligible       NA
  Baseline            4 Baseline           NA
  BaselineInProcess   4 In_Process_BL      NA
  Followup03          5 Follow_up_03       NA
  Followup03InProcess 5 In_Process_03      NA
  Followup06          6 Follow_up_06       NA
  Followup06InProcess 6 In_Process_06      NA
  Followup12          7 Follow_up_12       NA
  Followup12InProcess 7 In_Process_12      NA
  ", header = TRUE, stringsAsFactors = FALSE)

  # order of nodes is from bottom up for each x position
  n_socialsupport <- read.table(text = "
  ID                  x labels             n
  LeftMargin       0.99 LeftMargin         0
  RecTotal            1 Recruited          NA
  RecInterested       2 Interested         NA
  RecNotInterested    2 Not_Interested     NA
  Eligible            3 Eligible           NA
  EligInProcess       3 In_Process_Elig    NA
  EligNotParticpate   3 Do_Not_Participate NA
  EligNot             3 Not_Eligible       NA
  Baseline            4 Baseline           NA
  BaselineInProcess   4 In_Process_BL      NA
  Followup03          5 Follow_up_03       NA
  Followup03InProcess 5 In_Process_03      NA
  Followup06          6 Follow_up_06       NA
  Followup06InProcess 6 In_Process_06      NA
  Followup12          7 Follow_up_12       NA
  Followup12InProcess 7 In_Process_12      NA
  ", header = TRUE, stringsAsFactors = FALSE)

  if (sw_short_labels) {
    list_short_labels <- read.table(text = "
    Recruited             Rec
    Interested            Int
    Eligible              E
    Elig                  E
    In_Process_           IP
    Do_Not_Participate    DNP
    Baseline              BL
    In_Process_           IP
    Follow_up_            FU
    Not_                  N
    ", header = FALSE, stringsAsFactors = FALSE)

    for (i_rep in 1:nrow(list_short_labels)) {
      n_patients     $labels <- stringr::str_replace_all(n_patients     $labels, list_short_labels[i_rep, 1], list_short_labels[i_rep, 2])
      n_socialsupport$labels <- stringr::str_replace_all(n_socialsupport$labels, list_short_labels[i_rep, 1], list_short_labels[i_rep, 2])
    }
  }


  #library(stringr)
  n_patients     $labels <- stringr::str_replace_all(n_patients     $labels, "_", " ")
  n_socialsupport$labels <- stringr::str_replace_all(n_socialsupport$labels, "_", " ")



  ## subset data
  # PreScreening data
  dat_PreScr_sub <- subset(dat_PreScr, select = c(prescr_first_name, prescr_last_name, prescr_agree.factor))

  # Eligibility data
  dat_Elig_sub <- subset(dat_Elig, select = c(elig_f_name, elig_l_name, elig_elig_top, elig_elig_top.factor))

  # PDP data, patient
  dat_pdp_00m_sub <- subset(dat_pdp
                      , subset = ((redcap_event_name == "baseline_appt__bas_arm_1") & (pci_part_id_ps == "p"))
                      , select = c(study_id, pci_f_name_contact, pci_l_name_contact, redcap_event_name, pci_part_id_ps, pci_part_id_ps.factor))
  dat_pdp_03m_sub <- subset(dat_pdp
                      , subset = ((redcap_event_name == "3_months_appt__3_m_arm_1") & (pci_part_id_ps == "p"))
                      , select = c(study_id, pci_f_name_contact, pci_l_name_contact, redcap_event_name, pci_part_id_ps, pci_part_id_ps.factor))
  dat_pdp_06m_sub <- subset(dat_pdp
                      , subset = ((redcap_event_name == "6_months_appt_6_me_arm_1") & (pci_part_id_ps == "p"))
                      , select = c(study_id, pci_f_name_contact, pci_l_name_contact, redcap_event_name, pci_part_id_ps, pci_part_id_ps.factor))
  dat_pdp_12m_sub <- subset(dat_pdp
                      , subset = ((redcap_event_name == "12_months_appt__12_arm_1") & (pci_part_id_ps == "p"))
                      , select = c(study_id, pci_f_name_contact, pci_l_name_contact, redcap_event_name, pci_part_id_ps, pci_part_id_ps.factor))
  # PDP data, social support
  dat_pdp_00m_sub_ss <- subset(dat_pdp
                      , subset = ((redcap_event_name == "baseline_appt__bas_arm_1") & (pci_part_id_ps == "s"))
                      , select = c(study_id, pci_f_name_contact, pci_l_name_contact, redcap_event_name, pci_part_id_ps, pci_part_id_ps.factor))
  dat_pdp_03m_sub_ss <- subset(dat_pdp
                      , subset = ((redcap_event_name == "3_months_appt__3_m_arm_1") & (pci_part_id_ps == "s"))
                      , select = c(study_id, pci_f_name_contact, pci_l_name_contact, redcap_event_name, pci_part_id_ps, pci_part_id_ps.factor))
  dat_pdp_06m_sub_ss <- subset(dat_pdp
                      , subset = ((redcap_event_name == "6_months_appt_6_me_arm_1") & (pci_part_id_ps == "s"))
                      , select = c(study_id, pci_f_name_contact, pci_l_name_contact, redcap_event_name, pci_part_id_ps, pci_part_id_ps.factor))
  dat_pdp_12m_sub_ss <- subset(dat_pdp
                      , subset = ((redcap_event_name == "12_months_appt__12_arm_1") & (pci_part_id_ps == "s"))
                      , select = c(study_id, pci_f_name_contact, pci_l_name_contact, redcap_event_name, pci_part_id_ps, pci_part_id_ps.factor))

  ## rename variables
  dat_PreScr_sub  <- dplyr::rename( dat_PreScr_sub
                                  , first_name = prescr_first_name
                                  , last_name  = prescr_last_name)
  dat_Elig_sub    <- dplyr::rename( dat_Elig_sub
                                  , first_name = elig_f_name
                                  , last_name  = elig_l_name)
  dat_pdp_00m_sub     <- dplyr::rename( dat_pdp_00m_sub
                                  , first_name = pci_f_name_contact
                                  , last_name  = pci_l_name_contact)
  dat_pdp_03m_sub     <- dplyr::rename( dat_pdp_03m_sub
                                  , first_name = pci_f_name_contact
                                  , last_name  = pci_l_name_contact)
  dat_pdp_06m_sub     <- dplyr::rename( dat_pdp_06m_sub
                                  , first_name = pci_f_name_contact
                                  , last_name  = pci_l_name_contact)
  dat_pdp_12m_sub     <- dplyr::rename( dat_pdp_12m_sub
                                  , first_name = pci_f_name_contact
                                  , last_name  = pci_l_name_contact)
  dat_pdp_00m_sub_ss  <- dplyr::rename( dat_pdp_00m_sub_ss
                                  , first_name = pci_f_name_contact
                                  , last_name  = pci_l_name_contact)
  dat_pdp_03m_sub_ss  <- dplyr::rename( dat_pdp_03m_sub_ss
                                  , first_name = pci_f_name_contact
                                  , last_name  = pci_l_name_contact)
  dat_pdp_06m_sub_ss  <- dplyr::rename( dat_pdp_06m_sub_ss
                                  , first_name = pci_f_name_contact
                                  , last_name  = pci_l_name_contact)
  dat_pdp_12m_sub_ss  <- dplyr::rename( dat_pdp_12m_sub_ss
                                  , first_name = pci_f_name_contact
                                  , last_name  = pci_l_name_contact)


  # ----------------------------------------
  ## PreScr: Recruited patient numbers

  #library(dplyr)

          cat("PreScr - Patients -----------------------------\n\n", file = fn_consort_details, append = TRUE)
          f_count_and_IDs(dat_PreScr_sub, fn_consort_details)

    # RecNotInterested are those that indicate PreScr prescr_agree = "No"
    n_patients[(n_patients$ID == "RecNotInterested"), "n"] <-
      dat_PreScr_sub %>%
      subset(prescr_agree.factor == "No") %>%
      nrow()

          cat(paste(n_patients[(n_patients$ID == "RecNotInterested"), "n"], "RecNotInterested - Patients\n"), file = fn_consort_details, append = TRUE)

  # only keep PreScr Yes to continue
  dat_PreScr_sub <-
    dat_PreScr_sub %>%
    subset(subset = (prescr_agree.factor == "Yes"))

  # for name duplicates, only keep last instance
  if (sw_drop_dups == TRUE) {
    ind_dup <-
      dat_PreScr_sub %>%
      subset(select = c(first_name, last_name)) %>%
      duplicated(fromLast = TRUE)

    if(sum(ind_dup)) {
      message("Note: Patient Prescreening data have these duplicate names:")
      print(subset(dat_PreScr_sub, ind_dup, select = c(first_name, last_name)))

          cat("Note: Patient Prescreening data have these duplicate names:\n", file = fn_consort_details, append = TRUE)
          capture.output(subset(dat_PreScr_sub, ind_dup, select = c(first_name, last_name)), file = fn_consort_details, append = TRUE)
          cat("  Removing those duplicate names from count.\n", file = fn_consort_details, append = TRUE)

      message("  Removing those duplicate names from count.")
      dat_PreScr_sub <- subset(dat_PreScr_sub, !ind_dup)
    }
  }

    # RecInterested are those that indicate PreScr prescr_agree = "Yes" with unique names
    n_patients[(n_patients$ID == "RecInterested"), "n"] <-
      dat_PreScr_sub %>%
      nrow()

          cat(paste(n_patients[(n_patients$ID == "RecInterested"), "n"], "RecInterested - Patients\n"), file = fn_consort_details, append = TRUE)


    # RecTotal = RecInterested + RecNotInterested
    n_patients[(n_patients$ID == "RecTotal"), "n"] <-
      n_patients[(n_patients$ID == "RecInterested"), "n"] +
      n_patients[(n_patients$ID == "RecNotInterested"), "n"]

          cat(paste(n_patients[(n_patients$ID == "RecTotal"), "n"], "RecTotal - Patients\n"), file = fn_consort_details, append = TRUE)


  # ----------------------------------------
  ## Elig: Eligibility patient numbers

          cat("\n\n\n\n", file = fn_consort_details, append = TRUE)
          cat("Elig - Patients -------------------------------\n\n", file = fn_consort_details, append = TRUE)
          f_count_and_IDs(dat_Elig_sub, fn_consort_details)

  # for name duplicates, only keep last instance
    ind_dup <-
      dat_Elig_sub %>%
      subset(select = c(first_name, last_name)) %>%
      duplicated(fromLast = TRUE)

    if(sum(ind_dup)) {
      message("Note: Patient Eligibility data have these duplicate names:")
      print(subset(dat_Elig_sub, ind_dup, select = c(first_name, last_name)))

          cat("Note: Patient Prescreening data have these duplicate names:\n", file = fn_consort_details, append = TRUE)
          capture.output(subset(dat_Elig_sub, ind_dup, select = c(first_name, last_name)), file = fn_consort_details, append = TRUE)
          cat("  Removing those duplicate names from count.\n", file = fn_consort_details, append = TRUE)

      message("  Removing those duplicate names from count.")
      dat_Elig_sub <- subset(dat_Elig_sub, !ind_dup)
    }

    # Eligible are those that indicate Elig elig_elig_top "Yes"
    n_patients[(n_patients$ID == "Eligible"), "n"] <-
      dat_Elig_sub %>%
      subset(elig_elig_top == 1) %>%
      nrow()

          cat(paste(n_patients[(n_patients$ID == "RecInterested"), "n"], "RecInterested - Patients\n"), file = fn_consort_details, append = TRUE)
          cat(paste(n_patients[(n_patients$ID == "Eligible"), "n"], "Eligible - Patients\n"), file = fn_consort_details, append = TRUE)


    # EligNot are those that indicate Elig elig_elig_top "No"
    n_patients[(n_patients$ID == "EligNot"), "n"] <-
      dat_Elig_sub %>%
      subset(elig_elig_top == 2) %>%
      nrow()

          cat(paste(n_patients[(n_patients$ID == "EligNot"), "n"], "EligNot - Patients\n"), file = fn_consort_details, append = TRUE)

    # EligNotParticpate are those that indicate Elig elig_elig_top "Eligible but no baseline"
    n_patients[(n_patients$ID == "EligNotParticpate"), "n"] <-
      dat_Elig_sub %>%
      subset(elig_elig_top == 3) %>%
      nrow()

          cat(paste(n_patients[(n_patients$ID == "EligNotParticpate"), "n"], "EligNotParticpate - Patients\n"), file = fn_consort_details, append = TRUE)


    # EligInProcess are those that were Interested minus all the Elig statuses
    n_patients[(n_patients$ID == "EligInProcess"), "n"] <-
      n_patients[(n_patients$ID == "RecInterested"), "n"] -
        (n_patients[(n_patients$ID == "Eligible"), "n"] +
         n_patients[(n_patients$ID == "EligNot"), "n"] +
         n_patients[(n_patients$ID == "EligNotParticpate"), "n"])

          cat(paste(n_patients[(n_patients$ID == "EligInProcess"), "n"], "EligInProcess - Patients\n"), file = fn_consort_details, append = TRUE)


  # ----------------------------------------
  ## Baseline patient numbers

          cat("\n\n\n\n", file = fn_consort_details, append = TRUE)
          cat("Baseline - Patients ---------------------------\n\n", file = fn_consort_details, append = TRUE)
          f_count_and_IDs(dat_pdp_00m_sub, fn_consort_details)

    # Baseline are those that appear in the pdp data
    n_patients[(n_patients$ID == "Baseline"), "n"] <-
      dat_pdp_00m_sub %>%
      nrow()

          cat(paste(n_patients[(n_patients$ID == "Eligible"), "n"], "Eligible - Patients\n"), file = fn_consort_details, append = TRUE)
          cat(paste(n_patients[(n_patients$ID == "Baseline"), "n"], "Baseline - Patients\n"), file = fn_consort_details, append = TRUE)

    # BaselineInProcess are those that were Eligible minus all the pdp baselines
    n_patients[(n_patients$ID == "BaselineInProcess"), "n"] <-
      n_patients[(n_patients$ID == "Eligible"), "n"] -
      n_patients[(n_patients$ID == "Baseline"), "n"]

          cat(paste(n_patients[(n_patients$ID == "BaselineInProcess"), "n"], "BaselineInProcess - Patients\n"), file = fn_consort_details, append = TRUE)

  ## Baseline social support numbers

          cat("\n\n\n\n", file = fn_consort_details, append = TRUE)
          cat("Baseline - Social Support ---------------------\n\n", file = fn_consort_details, append = TRUE)
          f_count_and_IDs(dat_pdp_00m_sub_ss, fn_consort_details)

    # Baseline are those that appear in the pdp data
    n_socialsupport[(n_socialsupport$ID == "Baseline"), "n"] <-
      dat_pdp_00m_sub_ss %>%
      nrow()

          cat(paste(n_socialsupport[(n_socialsupport$ID == "Baseline"), "n"], "Baseline - Social Support\n"), file = fn_consort_details, append = TRUE)

    # Eligible are everyone who is enrolled, just as a starting point for social support
    n_socialsupport[(n_socialsupport$ID == "Eligible"), "n"] <-
      n_socialsupport[(n_socialsupport$ID == "Baseline"), "n"]

          #cat(paste(n_socialsupport[(n_socialsupport$ID == "Eligible"), "n"], "Eligible - Social Support\n"), file = fn_consort_details, append = TRUE)


              #JUNK# # dataset identifiers
              #JUNK# dat_PreScr_sub$dat_PreScr <- TRUE
              #JUNK# dat_Elig_sub$dat_Elig     <- TRUE
              #JUNK# dat_pdp_00m_sub$dat_pdp       <- TRUE
              #JUNK#
              #JUNK# # join datasets
              #JUNK# library(dplyr)
              #JUNK# dat_all <- full_join(full_join(dat_PreScr_sub, dat_Elig_sub), dat_pdp_00m_sub)
              #JUNK#
              #JUNK#
              #JUNK# subset(dat_all, dat_PreScr == TRUE)
              #JUNK#
              #JUNK# dat_all %>%
              #JUNK#   subset(dat_PreScr == TRUE) %>%
              #JUNK#   nrow()

  # ----------------------------------------
  ## Followup03 patient numbers

          cat("\n\n\n\n", file = fn_consort_details, append = TRUE)
          cat("Followup03 - Patients ---------------------------\n\n", file = fn_consort_details, append = TRUE)
          f_count_and_IDs(dat_pdp_03m_sub, fn_consort_details)

    # Followup03 are those that appear in the pdp data Followup03
    n_patients[(n_patients$ID == "Followup03"), "n"] <-
      dat_pdp_03m_sub %>%
      nrow()

          cat(paste(n_patients[(n_patients$ID == "Baseline"), "n"], "Baseline - Patients\n"), file = fn_consort_details, append = TRUE)
          cat(paste(n_patients[(n_patients$ID == "Followup03"), "n"], "Followup03 - Patients\n"), file = fn_consort_details, append = TRUE)

    # Followup03InProcess are those that were Baseline minus all the pdp Followup03
    n_patients[(n_patients$ID == "Followup03InProcess"), "n"] <-
      n_patients[(n_patients$ID == "Baseline"), "n"] -
      n_patients[(n_patients$ID == "Followup03"), "n"]

          cat(paste(n_patients[(n_patients$ID == "Followup03InProcess"), "n"], "Followup03InProcess - Patients\n"), file = fn_consort_details, append = TRUE)

  ## Followup03 social support numbers

          cat("\n\n\n\n", file = fn_consort_details, append = TRUE)
          cat("Followup03 - Social Support ---------------------\n\n", file = fn_consort_details, append = TRUE)
          f_count_and_IDs(dat_pdp_03m_sub_ss, fn_consort_details)

    # Followup03 are those that appear in the pdp data Followup03
    n_socialsupport[(n_socialsupport$ID == "Followup03"), "n"] <-
      dat_pdp_03m_sub_ss %>%
      nrow()

          cat(paste(n_socialsupport[(n_socialsupport$ID == "Baseline"), "n"], "Baseline - Social Support\n"), file = fn_consort_details, append = TRUE)
          cat(paste(n_socialsupport[(n_socialsupport$ID == "Followup03"), "n"], "Followup03 - Social Support\n"), file = fn_consort_details, append = TRUE)

    # Followup03InProcess are those that were Baseline minus all the pdp Followup03
    n_socialsupport[(n_socialsupport$ID == "Followup03InProcess"), "n"] <-
      n_socialsupport[(n_socialsupport$ID == "Baseline"), "n"] -
      n_socialsupport[(n_socialsupport$ID == "Followup03"), "n"]

          cat(paste(n_socialsupport[(n_socialsupport$ID == "Followup03InProcess"), "n"], "Followup03InProcess - Social Support\n"), file = fn_consort_details, append = TRUE)

  # ----------------------------------------
  ## Followup06 patient numbers

          cat("\n\n\n\n", file = fn_consort_details, append = TRUE)
          cat("Followup06 - Patients ---------------------------\n\n", file = fn_consort_details, append = TRUE)
          f_count_and_IDs(dat_pdp_06m_sub, fn_consort_details)

    # Followup06 are those that appear in the pdp data Followup06
    n_patients[(n_patients$ID == "Followup06"), "n"] <-
      dat_pdp_06m_sub %>%
      nrow()

          cat(paste(n_patients[(n_patients$ID == "Followup03"), "n"], "Followup03 - Patients\n"), file = fn_consort_details, append = TRUE)
          cat(paste(n_patients[(n_patients$ID == "Followup06"), "n"], "Followup06 - Patients\n"), file = fn_consort_details, append = TRUE)

    # Followup06InProcess are those that were Followup03 minus all the pdp Followup06
    n_patients[(n_patients$ID == "Followup06InProcess"), "n"] <-
      n_patients[(n_patients$ID == "Followup03"), "n"] -
      n_patients[(n_patients$ID == "Followup06"), "n"]

          cat(paste(n_patients[(n_patients$ID == "Followup06InProcess"), "n"], "Followup06InProcess - Patients\n"), file = fn_consort_details, append = TRUE)

  ## Followup06 social support numbers

          cat("\n\n\n\n", file = fn_consort_details, append = TRUE)
          cat("Followup06 - Social Support ---------------------\n\n", file = fn_consort_details, append = TRUE)
          f_count_and_IDs(dat_pdp_06m_sub_ss, fn_consort_details)

    # Followup06 are those that appear in the pdp data Followup06
    n_socialsupport[(n_socialsupport$ID == "Followup06"), "n"] <-
      dat_pdp_06m_sub_ss %>%
      nrow()

          cat(paste(n_socialsupport[(n_socialsupport$ID == "Followup03"), "n"], "Followup03 - Social Support\n"), file = fn_consort_details, append = TRUE)
          cat(paste(n_socialsupport[(n_socialsupport$ID == "Followup06"), "n"], "Followup06 - Social Support\n"), file = fn_consort_details, append = TRUE)

    # Followup06InProcess are those that were Followup03 minus all the pdp Followup06
    n_socialsupport[(n_socialsupport$ID == "Followup06InProcess"), "n"] <-
      n_socialsupport[(n_socialsupport$ID == "Followup03"), "n"] -
      n_socialsupport[(n_socialsupport$ID == "Followup06"), "n"]

          cat(paste(n_socialsupport[(n_socialsupport$ID == "Followup06InProcess"), "n"], "Followup06InProcess - Social Support\n"), file = fn_consort_details, append = TRUE)

  # ----------------------------------------
  ## Followup12 patient numbers

          cat("\n\n\n\n", file = fn_consort_details, append = TRUE)
          cat("Followup12 - Patients ---------------------------\n\n", file = fn_consort_details, append = TRUE)
          f_count_and_IDs(dat_pdp_12m_sub, fn_consort_details)

    # Followup12 are those that appear in the pdp data Followup12
    n_patients[(n_patients$ID == "Followup12"), "n"] <-
      dat_pdp_12m_sub %>%
      nrow()

          cat(paste(n_patients[(n_patients$ID == "Followup06"), "n"], "Followup06 - Patients\n"), file = fn_consort_details, append = TRUE)
          cat(paste(n_patients[(n_patients$ID == "Followup12"), "n"], "Followup12 - Patients\n"), file = fn_consort_details, append = TRUE)

    # Followup12InProcess are those that were Followup06 minus all the pdp Followup12
    n_patients[(n_patients$ID == "Followup12InProcess"), "n"] <-
      n_patients[(n_patients$ID == "Followup06"), "n"] -
      n_patients[(n_patients$ID == "Followup12"), "n"]

          cat(paste(n_patients[(n_patients$ID == "Followup12InProcess"), "n"], "Followup12InProcess - Patients\n"), file = fn_consort_details, append = TRUE)

  ## Followup12 social support numbers

          cat("\n\n\n\n", file = fn_consort_details, append = TRUE)
          cat("Followup12 - Social Support ---------------------\n\n", file = fn_consort_details, append = TRUE)
          f_count_and_IDs(dat_pdp_12m_sub_ss, fn_consort_details)

    # Followup12 are those that appear in the pdp data Followup12
    n_socialsupport[(n_socialsupport$ID == "Followup12"), "n"] <-
      dat_pdp_12m_sub_ss %>%
      nrow()

          cat(paste(n_socialsupport[(n_socialsupport$ID == "Followup06"), "n"], "Followup06 - Social Support\n"), file = fn_consort_details, append = TRUE)
          cat(paste(n_socialsupport[(n_socialsupport$ID == "Followup12"), "n"], "Followup12 - Social Support\n"), file = fn_consort_details, append = TRUE)

    # Followup12InProcess are those that were Followup06 minus all the pdp Followup12
    n_socialsupport[(n_socialsupport$ID == "Followup12InProcess"), "n"] <-
      n_socialsupport[(n_socialsupport$ID == "Followup06"), "n"] -
      n_socialsupport[(n_socialsupport$ID == "Followup12"), "n"]

          cat(paste(n_socialsupport[(n_socialsupport$ID == "Followup12InProcess"), "n"], "Followup12InProcess - Social Support\n"), file = fn_consort_details, append = TRUE)



  # ----------------------------------------
  ## Sankey diagrams
  if(sw_plot) {

      op <- par(no.readonly = TRUE) # save current plotting settings
      par(mfrow = c(2, 1), oma = rep(0, 4))

    # patients
    riverplot_pdp(n_patients     , title = "Patients", report_date = report_date)

    # social support
      rp_plot_area <- as.numeric(subset(n_socialsupport, ID == "Eligible", n) /
                                 subset(n_patients     , ID == "RecTotal", n)
                                )
    riverplot_pdp(n_socialsupport, title = "Social Support", rp_plot_area = rp_plot_area, report_date = report_date)

      par(op) # restore previous plotting settings
  }

  #n_consort <- list( n_patients      = subset(n_patients     , select = c(labels, n))
  #                 , n_socialsupport = subset(n_socialsupport, select = c(labels, n))
  #                 )
  n_consort <- data.frame(labels          = n_patients$labels
                        , n_patients      = n_patients$n
                        , n_socialsupport = n_socialsupport$n
                        )

  n_consort <- subset(n_consort, !(labels == "LeftMargin"))

  # remove label for SS Eligible line
  n_consort$n_socialsupport[n_consort$labels == "Eligible"] <- NA

  return(n_consort)
}


## # Erik 7/21/2017, mimic our data
## library(riverplot)
##
## # colors()[grep("green", colors())]
## # colors()[grep("orange", colors())]
## # colors()[grep("red", colors())]
##
## my_colors <- list(green  = "lightgreen"
##                 , orange = "orange"
##                 , red    = "palevioletred2"
##                   )
##
## # order of nodes is from bottom up for each x position
## nodes <- read.table(text = "
## ID                x labels
## RecTotal          1 Recruited=118
## RecInterested     2 Interested=93
## RecNotInterested  2 Not_Interested=25
## Eligible          3 Eligible=59
## EligInProcess     3 In_Process=5
## EligNotParticpate 3 Do_Not_Particpate=5
## EligNot           3 Not_Eligible=24
## Enrolled          4 Enrolled=54
## EnrollInProcess   4 In_Process=5
## ", header = TRUE, stringsAsFactors = FALSE)
##
## # order of edges is from bottom up for each x position
## edges <- list( RecTotal      = list(RecInterested = 93, RecNotInterested = 25)
##              , RecInterested = list(Eligible = 59, EligInProcess = 5, EligNotParticpate = 5, EligNot = 24)
##              , Eligible     = list(Enrolled = 54, EnrollInProcess = 5)
##              )
## r <- makeRiver( nodes
##                 , edges
##                 , node_styles = list(
##                     RecTotal          = list(col = my_colors$green)
##                   , RecInterested     = list(col = my_colors$green)
##                   , RecNotInterested  = list(col = my_colors$red)
##                   , Eligible          = list(col = my_colors$green)
##                   , EligInProcess     = list(col = my_colors$orange)
##                   , EligNotParticpate = list(col = my_colors$red)
##                   , EligNot           = list(col = my_colors$red)
##                   , Enrolled          = list(col = my_colors$green)
##                   , EnrollInProcess   = list(col = my_colors$orange)
##                 )
##               )
##
## plot(r
##     , main = "Patients"
##     #, mar = c(5,5,5,5)
##     , gravity = "bottom"
##     , plot_area = 1
##     , node_margin = 0.01
##     , srt = 0             # rotate labels
##     , xscale = 1.0
##     )
## legend( "topright"
##       , title = "Patients",
##       , cex = 1.2
##       , inset = 0.00
##       , legend = c("Stop", "In Process", "Continue")
##       , fill = c(my_colors$red, my_colors$orange, my_colors$green)
##       , horiz = FALSE
##       , border = "white"  # no border around colored boxes
##       #, bty = "n"         # no border around legend
##       , bg = "gray95"
##       )
## text(0, 0, "PDP 7/21/2017", pos = 4, offset = 1.75, col="black", cex = 0.8)
