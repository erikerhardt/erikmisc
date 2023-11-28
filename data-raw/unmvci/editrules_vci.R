editrules_vci <- function() {
  library(unmvci)
  library(dplyr)

  # Data subset
  dat_vci_subset <-
    dat_vci |>
    filter(redcap_event_name == "baseline_appt__bas_arm_1")

  library(editrules)

  # Edit rules
  fn_editrules <- "./data-raw/editrules_vci.txt"
  ER_edits <- editfile(fn_editrules)
  ER_edits

  # Violated edits
  ER_violated_edits <- violatedEdits(ER_edits, dat_vci_subset)

  # Report
  ER_violated_edits_print <-
    cbind(select(dat_vci_subset, ends_with("name_contact"))
        , ER_violated_edits)
  ER_violated_edits_print

}







## # ----------------------------------------
## library(unmvci)
## library(dplyr)
##
## dat_vci_baseline <-
##   dat_vci |>
##   filter(redcap_event_name == "baseline_appt__bas_arm_1")
##
## library(editrules)
##
## fn_editrules <- "./data-raw/editrules_vci.txt"
## ER_edits <- editfile(fn_editrules)
## ER_edits
## #op <- par(no.readonly = TRUE)         # save plot settings
## #par(mfrow=c(1,1), mar = c(0,0,0,0))
## #plot(ER_edits)
## #par(op)                               # restore plot settings
##
## ER_violated_edits <- violatedEdits(ER_edits, dat_vci_baseline)
## #summary(ER_violated_edits)
## #plot(ER_violated_edits)
##
## violatedEdits(ER_edits, dat_vci_baseline)
## #ER_local_errors <- localizeErrors(ER_edits, dat_vci_baseline, method = "mip")
