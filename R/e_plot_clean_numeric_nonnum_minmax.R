#' Cleaning numeric values for nonnumeric and large/small values, similarly for dates.
#'
#' The variables included together are plotted on the same x-axis scale.
#'
#' @param dat_this  data gathered with selected variables
#' @param binwidth  for histogram
#' @param n_table   is the number of table values to print
#' @param sw_date   is the value a date?
#' @param sw_plot   TRUE/FALSE switch to plot
#' @param lab_title title before output and for plot
#'
#' @return invisible(NULL)
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @importFrom lubridate as_date
#' @export
#'
#' @examples
#' ## requires formatting into two columns: "variables" and "values"
#' dat_this <-
#'   dat_mtcars_e |>
#'   dplyr::select(
#'     model
#'   , tidyselect::starts_with("hp")
#'   , where(is.numeric)
#'   ) |>
#'   tidyr::pivot_longer(
#'     cols      = -model
#'   , names_to  = "variable"
#'   , values_to = "value"
#'   ) |>
#'   dplyr::select(
#'     -model
#'   )
#' dat_this |> e_plot_clean_numeric_nonnum_minmax()
e_plot_clean_numeric_nonnum_minmax <-
  function(
    dat_this
  , binwidth  = 7
  , n_table   = 5
  , sw_date   = FALSE
  , sw_plot   = TRUE
  , lab_title = NULL
  ) {

  # find non-numeric values
  if (is.character(dat_this$value)) {
    dat_this_NA <-
      dat_this |>
      dplyr::filter(!(value == "")) |>
      dplyr::mutate(value_num = as.numeric(value)) |>
      dplyr::filter(is.na(value_num))

    print(paste(nrow(dat_this_NA), "non-numeric values"))
    print(dat_this_NA)
  }

  if (sw_date) {
    #library(lubridate)
    # convert to date and remove NAs
    dat_this$value <- lubridate::as_date(dat_this$value)
  } else {
    # convert to numeric and remove NAs
    dat_this$value <- as.numeric(dat_this$value)
  }

  # remove NAs to report smallest and largest values
  dat_this <-
    dat_this |>
    dplyr::filter(!is.na(value))

  if(!is.null(lab_title)) {
    print(paste(lab_title, "----------------------------------------"))
  }

  # Smallest values for each variable
  dat_this |>
    dplyr::group_by(variable) |>
    dplyr::arrange(value) |>
    dplyr::slice(1:n_table) |>
    print(n = Inf)

  # Largest values for each variable
  dat_this |>
    dplyr::group_by(variable) |>
    dplyr::arrange(desc(value)) |>
    dplyr::slice(1:n_table) |>
    print(n = Inf)

  if (sw_plot) {
    #library(ggplot2)
    p1 <- ggplot(dat_this, aes(x = value))
    p1 <- p1 + theme_bw()
    p1 <- p1 + geom_histogram(aes()
                              , binwidth = binwidth
                              #, colour="black", fill="white"
                              )
    if (nrow(dat_this) < 1000) {
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
} # e_plot_clean_numeric_nonnum_minmax
