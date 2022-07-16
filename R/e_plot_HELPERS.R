#' Extract ggplot legend as it's own plot grob object
#'
#' @param p ggplot grob
#'
#' @return p_legend ggplot grob of only the legend
#' @import ggplot2
#' @export
#'
#' @examples
#' \dontrun{
#' p_legend <- e_plot_extract_legend(p)
#' }
e_plot_extract_legend <-
  function(
    p = NULL
  ) {
  table_of_grobs <- ggplot_gtable(ggplot_build(p))
  ind_legend <- which(sapply(table_of_grobs$grobs, function(x) x$name) == "guide-box")
  p_legend <- table_of_grobs$grobs[[ind_legend]]
  return(p_legend)
} # e_plot_extract_legend


#' Create blank ggplot grob plot used as a place holder
#'
#' @param text_label optional text label, such as "N/A"
#'
#' @return p_blank blank ggplot grob, possibly with a text label
#' @import ggplot2
#' @export
#'
#' @examples
#' p_blank <- e_plot_blank(text_label = c("", "N/A")[2])
e_plot_blank <-
  function(
    text_label = c("", "N/A")[1]
  ) {
  p_blank <- ggplot()
  p_blank <- p_blank + theme_void()
  p_blank <- p_blank + geom_text(aes(0, 0, label = text_label))
  return(p_blank)
}



## # Title in cowplot plot_grid
## # https://stackoverflow.com/questions/50973713/ggplot2-creating-themed-title-subtitle-with-cowplot
## e_plot
## f_draw_label_theme <-
##   function(
##     this_label
##   , this_theme   = NULL
##   , this_element = "text"
##   , ...
##   ) {
##
##   if (is.null(this_theme)) {
##     this_theme <- ggplot2::theme_get()
##   }
##   if (!this_element %in% names(this_theme)) {
##     stop("Element must be a valid ggplot theme element name")
##   }
##
##   elements <- ggplot2::calc_element(this_element, this_theme)
##
##   cowplot::draw_label(
##     this_label
##   , fontfamily  = elements$family
##   , fontface    = elements$face
##   , colour      = elements$color
##   , size        = elements$size
##   , ...
##   )
## }
##
## cowplot_title <-
##   cowplot::ggdraw() +
##   f_draw_label_theme(
##     paste0(
##       "Support group: "
##     , n_PSH_ES
##     , ";  "
##     , "Response: "
##     , this_var_list
##     )
##   #, theme = theme_georgia()
##   , this_element = "plot.title"
##   , x = 0.01 #0.05
##   , hjust = 0
##   , vjust = 1
##   )
## cowplot_subtitle <-
##   cowplot::ggdraw() +
##   f_draw_label_theme(
##     "By census tract, 2016"
##   , this_theme = theme_georgia()
##   , this_element = "plot.subtitle"
##   , x = 0.05
##   , hjust = 0
##   , vjust = 1
##   )
##
## p.arranged2 <-
##   cowplot::plot_grid(
##     cowplot_title
##   #, cowplot_subtitle
##   , p.arranged
##   , ncol = 1
##   , rel_heights = c(0.04, 1)
##   )
##
## p.arranged2 %>% print()


