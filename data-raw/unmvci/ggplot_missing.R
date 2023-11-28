#' ggplot_missing
#'
#' @description A function that plots missing data in ggplot. For a more updated version, see `vis_miss` from visdat - github.com/njtierney/visdat
#'
#' @param x a data frame
#'
#' @return A ggplot of the missing data.
#'
#'@export
ggplot_missing <- function(dat) {
  # https://github.com/njtierney/neato/blob/master/R/ggplot_missing.R

  dat2 <-
    dat |>
    is.na()

  # create a column indicating which rows all have data (no missing)
  NO_MISSING <-
    !(rowSums(!dat2) == ncol(dat2))
  dat2 <-
    cbind(dat2, NO_MISSING) |>
    reshape2::melt()

  ggplot2::ggplot(data = dat2,
         aes(x = Var2,
             y = Var1)) +
  ggplot2::geom_raster(aes(fill = value)) + # , alpha = 0.6
  ggplot2::scale_fill_grey(name = "",
                  labels = c("Present","Missing")) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x  = element_text(angle=90, vjust=0, hjust=0)) +
  ggplot2::labs(x = "Variables in Dataset",
                y = "Rows / observations") +
  #ggplot2::scale_y_continuous(expand = c(0,0)) +
  ggplot2::scale_y_reverse(expand = c(0,0), breaks = c(1, seq(0, 10000, by=20)))
}
