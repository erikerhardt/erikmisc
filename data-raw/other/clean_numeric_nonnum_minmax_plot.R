#' Cleaning numeric values for nonnumeric and large/small values, similarly for dates.
#'
#'
#' @param dat_temp data gathered with selected variables
#' @param binwidth for histogram
#' @param n_table is the number of table values to print
#' @param sw_date is the value a date?
#' @param sw_plot TRUE/FALSE switch to plot
#' @param lab_title title before output and for plot
#'
#' @return invisible(NULL)
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @import ggplot2
#' @export
#'
#' @examples
clean_numeric_nonnum_minmax_plot <-
  function(
    dat_temp
  , binwidth  = 7
  , n_table   = 5
  , sw_date   = FALSE
  , sw_plot   = TRUE
  , lab_title = NULL
  ) {

  # ## requires formatting like this for input
  # library(dplyr)
  # library(tidyr)
  # dat_temp <-
  #   dat_pdp %>%
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

  if(!is.null(lab_title)) {
    print(paste(lab_title, "----------------------------------------"))
  }

  # Smallest values for each variable
  dat_temp %>%
    group_by(variable) %>%
    arrange(value) %>%
    slice(1:n_table) %>%
    print(n = Inf)

  # Largest values for each variable
  dat_temp %>%
    group_by(variable) %>%
    arrange(desc(value)) %>%
    slice(1:n_table) %>%
    print(n = Inf)

  if (sw_plot) {
    #library(ggplot2)
    p1 <- ggplot(dat_temp, aes(x = value))
    p1 <- p1 + geom_histogram(aes()
                              , binwidth = binwidth
                              #, colour="black", fill="white"
                              )
    if (nrow(dat_temp) < 1000) {
      p1 <- p1 + geom_rug()
    }

    if (sw_date) {
      p1 <- p1 + scale_x_date()
    }

    p1 <- p1 + facet_grid(variable ~ .)
    p1 <- p1 + labs(title = lab_title)
    print(p1)
  }

  invisible(NULL)
}
