#' Duplicate the data and create a "Combined" surv_prog group for reports.
#'
#' @param dat_vci is the VCI data
#'
#' @return dat_vci_Combined includes an additional surv_prog group called "Combined".
#' @export
#'
#' @examples
#' dat_vci_Combined <- create_dat_vci_combined(dat_vci)
create_dat_vci_combined <- function(dat_vci) {

  # ----------------------------------------
  ## Create a "Combined" group of One Hope and UNM CDE for reports

  dat_vci_Combined                       <- dat_vci
  dat_vci_Combined$study_id              <- dat_vci_Combined$study_id              + 10000
  dat_vci_Combined$pci_part_id           <- dat_vci_Combined$pci_part_id
  dat_vci_Combined$pci_part_id_num       <- factor(as.numeric(dat_vci_Combined$pci_part_id_num) + 10000)
  dat_vci_Combined$surv_prog             <- 12
  dat_vci_Combined$surv_prog.factor      <- factor(dat_vci_Combined$surv_prog, levels = c(1, 2, 12), labels = c("One Hope", "UNM CDE", "Combined"), ordered = TRUE)
  dat_vci_Combined                       <- rbind(dat_vci, dat_vci_Combined)

  return(dat_vci_Combined)
}
