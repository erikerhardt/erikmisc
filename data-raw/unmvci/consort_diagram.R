#' Produces a consort diagram based on PreScreen, Eligibility, and VCI data files
#'
#' @param dat_vci
#' @param sw_drop_dups drop duplicate names between PreScr, Elig, and VCI data before counting?
#' @param report_date prints the date for the report, if specified; otherwise, prints the current date/time on the figure.
#'
#' @return Returns \code{n_patients}, the table of patient fates
#' @import dplyr
#' @import stringr
#' @export
#'
#' @examples
#' consort_diagram()
consort_diagram <- function(dat_vci, report_date = NULL) {

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
  Baseline            1 Baseline           NA
  BaselineInProcess   1 In_Process_BL      NA
  Followup1           2 Follow_up_1        NA
  Followup1InProcess  2 In_Process_1       NA
  Followup2           3 Follow_up_2        NA
  Followup2InProcess  3 In_Process_2       NA
  Followup3           4 Follow_up_3        NA
  Followup3InProcess  4 In_Process_3       NA
  ", header = TRUE, stringsAsFactors = FALSE)

  #library(stringr)
  n_patients$labels <- stringr::str_replace_all(n_patients$labels, "_", " ")

  ## subset data
  # VCI data, patient
  dat_vci_0m_sub <- subset(dat_vci
                      , subset = ((redcap_event_name == "baseline_appt__bas_arm_1") & (pci_part_id_ps == "p"))
                      , select = c(study_id, pci_f_name_contact, pci_l_name_contact, redcap_event_name, pci_part_id_ps, pci_part_id_ps.factor))
  dat_vci_1m_sub <- subset(dat_vci
                      , subset = ((redcap_event_name == "3_months_appt__3_m_arm_1") & (pci_part_id_ps == "p"))
                      , select = c(study_id, pci_f_name_contact, pci_l_name_contact, redcap_event_name, pci_part_id_ps, pci_part_id_ps.factor))
  dat_vci_2m_sub <- subset(dat_vci
                      , subset = ((redcap_event_name == "6_months_appt_6_me_arm_1") & (pci_part_id_ps == "p"))
                      , select = c(study_id, pci_f_name_contact, pci_l_name_contact, redcap_event_name, pci_part_id_ps, pci_part_id_ps.factor))
  dat_vci_3m_sub <- subset(dat_vci
                      , subset = ((redcap_event_name == "3_months_appt__3_arm_1") & (pci_part_id_ps == "p"))
                      , select = c(study_id, pci_f_name_contact, pci_l_name_contact, redcap_event_name, pci_part_id_ps, pci_part_id_ps.factor))

  ## rename variables
  dat_vci_0m_sub     <- dplyr::rename( dat_vci_0m_sub
                                  , first_name = pci_f_name_contact
                                  , last_name  = pci_l_name_contact)
  dat_vci_1m_sub     <- dplyr::rename( dat_vci_1m_sub
                                  , first_name = pci_f_name_contact
                                  , last_name  = pci_l_name_contact)
  dat_vci_2m_sub     <- dplyr::rename( dat_vci_2m_sub
                                  , first_name = pci_f_name_contact
                                  , last_name  = pci_l_name_contact)
  dat_vci_3m_sub     <- dplyr::rename( dat_vci_3m_sub
                                  , first_name = pci_f_name_contact
                                  , last_name  = pci_l_name_contact)

  # ----------------------------------------
  ## Baseline patient numbers

          cat("\n\n\n\n", file = fn_consort_details, append = TRUE)
          cat("Baseline - Patients ---------------------------\n\n", file = fn_consort_details, append = TRUE)
          f_count_and_IDs(dat_vci_0m_sub, fn_consort_details)

    # Baseline are those that appear in the vci data
    n_patients[(n_patients$ID == "Baseline"), "n"] <-
      dat_vci_0m_sub %>%
      nrow()

          cat(paste(n_patients[(n_patients$ID == "Baseline"), "n"], "Baseline - Patients\n"), file = fn_consort_details, append = TRUE)

    # BaselineInProcess are those that were Eligible minus all the vci baselines
    n_patients[(n_patients$ID == "BaselineInProcess"), "n"] <-
      #n_patients[(n_patients$ID == "Eligible"), "n"] -
      n_patients[(n_patients$ID == "Baseline"), "n"]

          cat(paste(n_patients[(n_patients$ID == "BaselineInProcess"), "n"], "BaselineInProcess - Patients\n"), file = fn_consort_details, append = TRUE)



  # ----------------------------------------
  ## Followup1 patient numbers

          cat("\n\n\n\n", file = fn_consort_details, append = TRUE)
          cat("Followup1 - Patients ---------------------------\n\n", file = fn_consort_details, append = TRUE)
          f_count_and_IDs(dat_vci_1m_sub, fn_consort_details)

    # Followup1 are those that appear in the vci data Followup1
    n_patients[(n_patients$ID == "Followup1"), "n"] <-
      dat_vci_1m_sub %>%
      nrow()

          cat(paste(n_patients[(n_patients$ID == "Baseline"), "n"], "Baseline - Patients\n"), file = fn_consort_details, append = TRUE)
          cat(paste(n_patients[(n_patients$ID == "Followup1"), "n"], "Followup1 - Patients\n"), file = fn_consort_details, append = TRUE)

    # Followup1InProcess are those that were Baseline minus all the vci Followup1
    n_patients[(n_patients$ID == "Followup1InProcess"), "n"] <-
      n_patients[(n_patients$ID == "Baseline"), "n"] -
      n_patients[(n_patients$ID == "Followup1"), "n"]

          cat(paste(n_patients[(n_patients$ID == "Followup1InProcess"), "n"], "Followup1InProcess - Patients\n"), file = fn_consort_details, append = TRUE)

  # ----------------------------------------
  ## Followup2 patient numbers

          cat("\n\n\n\n", file = fn_consort_details, append = TRUE)
          cat("Followup2 - Patients ---------------------------\n\n", file = fn_consort_details, append = TRUE)
          f_count_and_IDs(dat_vci_2m_sub, fn_consort_details)

    # Followup2 are those that appear in the vci data Followup2
    n_patients[(n_patients$ID == "Followup2"), "n"] <-
      dat_vci_2m_sub %>%
      nrow()

          cat(paste(n_patients[(n_patients$ID == "Followup1"), "n"], "Followup1 - Patients\n"), file = fn_consort_details, append = TRUE)
          cat(paste(n_patients[(n_patients$ID == "Followup2"), "n"], "Followup2 - Patients\n"), file = fn_consort_details, append = TRUE)

    # Followup2InProcess are those that were Followup1 minus all the vci Followup2
    n_patients[(n_patients$ID == "Followup2InProcess"), "n"] <-
      n_patients[(n_patients$ID == "Followup1"), "n"] -
      n_patients[(n_patients$ID == "Followup2"), "n"]

          cat(paste(n_patients[(n_patients$ID == "Followup2InProcess"), "n"], "Followup2InProcess - Patients\n"), file = fn_consort_details, append = TRUE)

  # ----------------------------------------
  ## Followup3 patient numbers

          cat("\n\n\n\n", file = fn_consort_details, append = TRUE)
          cat("Followup3 - Patients ---------------------------\n\n", file = fn_consort_details, append = TRUE)
          f_count_and_IDs(dat_vci_3m_sub, fn_consort_details)

    # Followup3 are those that appear in the vci data Followup3
    n_patients[(n_patients$ID == "Followup3"), "n"] <-
      dat_vci_3m_sub %>%
      nrow()

          cat(paste(n_patients[(n_patients$ID == "Followup2"), "n"], "Followup2 - Patients\n"), file = fn_consort_details, append = TRUE)
          cat(paste(n_patients[(n_patients$ID == "Followup3"), "n"], "Followup3 - Patients\n"), file = fn_consort_details, append = TRUE)

    # Followup3InProcess are those that were Followup2 minus all the vci Followup3
    n_patients[(n_patients$ID == "Followup3InProcess"), "n"] <-
      n_patients[(n_patients$ID == "Followup2"), "n"] -
      n_patients[(n_patients$ID == "Followup3"), "n"]

          cat(paste(n_patients[(n_patients$ID == "Followup3InProcess"), "n"], "Followup3InProcess - Patients\n"), file = fn_consort_details, append = TRUE)



  # ----------------------------------------
  ## Sankey diagrams

    op <- par(no.readonly = TRUE) # save current plotting settings
    par(mfrow = c(1, 1), oma = rep(0, 4))

  # patients
  riverplot_vci(n_patients     , title = "Patients", report_date = report_date)

    par(op) # restore previous plotting settings

  #n_consort <- list( n_patients      = subset(n_patients     , select = c(labels, n))
  #                 , n_socialsupport = subset(n_socialsupport, select = c(labels, n))
  #                 )
  n_consort <- data.frame(labels          = n_patients$labels
                        , n_patients      = n_patients$n
                        )

  n_consort <- subset(n_consort, !(labels == "LeftMargin"))

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
## text(0, 0, "VCI 7/21/2017", pos = 4, offset = 1.75, col="black", cex = 0.8)
