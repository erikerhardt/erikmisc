#' ggplot_missing
#'
#' @description A function that plots missing data in ggplot. For a more updated version, see `vis_miss` from visdat - github.com/njtierney/visdat
#'
#' @param x a data frame
#' @param group_color = FALSE or a variable name
#'
#' @return A ggplot of the missing data.
#'
#'@export
ggplot_missing2 <-
  function(
    dat
  , group_color = FALSE
  ) {
  # https://github.com/njtierney/neato/blob/master/R/ggplot_missing.R

  ## dat2 <-
  ##   dat %>%
  ##   is.na()
  ##
  ## # create a column indicating which rows all have data (no missing)
  ## NO_MISSING <-
  ##   !(rowSums(!dat2) == ncol(dat2))
  ##
  ## dat2_long <-
  ##   dat2 %>%
  ##   dplyr::as_tibble() %>%
  ##   dplyr::bind_cols(NO_MISSING = NO_MISSING) %>%
  ##   dplyr::mutate(
  ##     ID = 1:dplyr::n()
  ##   , group_color = dat[[group_color]]
  ##   ) %>%
  ##   dplyr::select(
  ##     ID, group_color, everything()
  ##   ) %>%
  ##   tidyr::pivot_longer(
  ##     cols      = -dplyr::one_of(ID, dx_current) # everything()  #-one_of(dx_current)
  ##   , names_to  = "names"
  ##   , values_to = "value"
  ##   )
  ##
  ## p <- ggplot2::ggplot(data = dat2_long, aes(x = Var2, y = Var1))
  ##
  ## #if (group_color == FALSE) {
  ## #  p <- p + ggplot2::geom_raster(aes(fill = value))  # , alpha = 0.6
  ## #} else {
  ##   p <- p + ggplot2::geom_raster(aes_string(fill = group_color), alpha = 1)
  ##   p <- p + ggplot2::geom_raster(aes(fill = value), alpha = 0.5)
  ## #}
  ##
  ## p <- p + ggplot2::scale_fill_grey(name = "", labels = c("Present","Missing"))
  ## p <- p + ggplot2::theme_minimal()
  ## p <- p + ggplot2::theme(axis.text.x  = element_text(angle=90, vjust=0, hjust=0))
  ## p <- p + ggplot2::labs(x = "Variables in Dataset",
  ##               y = "Rows / observations")
  ## #ggplot2::scale_y_continuous(expand = c(0,0))
  ## p <- p + ggplot2::scale_y_reverse(expand = c(0,0), breaks = c(1, seq(0, 10000, by=20)))
  ##
  ## return(p)

}
