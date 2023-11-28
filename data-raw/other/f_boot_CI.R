f_boot_CI <-
  function(
    x
  , conf_level = 0.95
  ) {
  ## x = this_result_diff$AUC
  prob_CI <- c(lower = (1 - conf_level) / 2, upper = 1 - (1 - conf_level) / 2)
  ind_CI <- prob_CI * length(x)
  ind_CI[1] <- ind_CI[1] |> floor()
    if (ind_CI[1] == 0) { ind_CI[1] <- 1 }
  ind_CI[2] <- ind_CI[2] |> ceiling()

  CI_limits <- sort(x)[ind_CI]
  return(CI_limits)
}
