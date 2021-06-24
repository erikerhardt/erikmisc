#' Cleaning numeric values for nonnumeric and large/small values, similarly for dates.
#'
#'
#' @param dat_temp data gathered with selected variables
#' @param binwidth for histogram
#' @param sw_date is the value a date?
#' @param sw_plot
#'
#' @return invisible(NULL)
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @import ggplot2
#' @export
#'
#' @examples
clean_numeric_nonnum_minmax_plot <- function(dat_temp, binwidth = 7, sw_date = FALSE, sw_plot = TRUE) {

  # ## requires formatting like this for input
  # library(dplyr)
  # library(tidyr)
  # dat_temp <-
  #   dat_vci %>%
  #   select(study_id
  #          , starts_with("bmi_height")
  #          , starts_with("bmi_weight")
  #   ) %>%
  #   gather(variable, value, -study_id)


  # find non-numeric values
  if (is.character(dat_temp$value)) {
    dat_temp_NA <-
      dat_temp %>%
      filter(!(value == "")) %>%
      mutate(value_num = as.numeric(value)) %>%
      filter(is.na(value_num))

    print(paste(nrow(dat_temp_NA), "non-numeric values"))
    print(dat_temp_NA)
  }

  if (sw_date) {
    #library(lubridate)
    # convert to date and remove NAs
    dat_temp$value <- as_date(dat_temp$value)
  } else {
    # convert to numeric and remove NAs
    dat_temp$value <- as.numeric(dat_temp$value)
  }

  # remove NAs to report smallest and largest values
  dat_temp <-
    dat_temp %>%
    filter(!is.na(value))

  # Smallest values for each variable
  dat_temp %>%
    group_by(variable) %>%
    arrange(value) %>%
    slice(1:2) %>%
    print()

  # Largest values for each variable
  dat_temp %>%
    group_by(variable) %>%
    arrange(desc(value)) %>%
    slice(1:2) %>%
    print()

  if (sw_plot) {
    #library(ggplot2)
    p1 <- ggplot(dat_temp, aes(x = value))
    p1 <- p1 + geom_histogram(aes()
                              , binwidth = binwidth
                              #, colour="black", fill="white"
                              )
    p1 <- p1 + geom_rug()
    p1 <- p1 + facet_grid(variable ~ .)
    print(p1)
  }

  invisible(NULL)
}
