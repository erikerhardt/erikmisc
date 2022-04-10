#' Plot scatterplot of two numeric variables, by up to one color factor variables and two facets factor variables
#'
#' @param dat_plot  data to plot
#' @param var_num   two numeric variable names to plot
#' @param var_color factor or numeric color variable
#' @param var_facet factor varibles (1 or 2) to facet by, (row facets, then column facets),
#' @param sw_print  T/F whether to print table and display plot
#'
#' @return          a ggplot object
#' @import ggplot2
#' @export
#'
#' @examples
#' e_plot_scatterplot(
#'     dat_plot  = dat_mtcars_e
#'   , var_num   = c("mpg", "disp")
#'   )
#' e_plot_scatterplot(
#'     dat_plot  = dat_mtcars_e
#'   , var_num   = c("mpg", "disp")
#'   , var_color = c("cyl")
#'   )
#' e_plot_scatterplot(
#'     dat_plot  = dat_mtcars_e
#'   , var_num   = c("mpg", "disp")
#'   , var_color = c("wt")          # numeric color
#'   )
#' e_plot_scatterplot(
#'     dat_plot  = dat_mtcars_e
#'   , var_num   = c("mpg", "disp")
#'   , var_facet = c("vs", "am")
#'   )
#' e_plot_scatterplot(
#'     dat_plot  = dat_mtcars_e
#'   , var_num   = c("mpg", "disp")
#'   , var_color = c("cyl")
#'   , var_facet = c("vs", "am")
#'   )
e_plot_scatterplot <-
  function(
    dat_plot  = NULL
  , var_num   = NULL
  , var_color = NULL
  , var_facet = NULL
  , sw_print  = FALSE
  ) {

  library(ggplot2)
  text_title <- paste0("Plot of ", var_num[2], " vs ", var_num[1])

  if (is.null(var_color)) {
    p <- ggplot(dat_plot, aes_string(x = var_num[1], y = var_num[2]))
  } else {
    if (class(dat_plot[[ var_color ]]) == "numeric") {
      p <- ggplot(dat_plot, aes_string(x = var_num[1], y = var_num[2], colour = var_color))
    } else {
      p <- ggplot(dat_plot, aes_string(x = var_num[1], y = var_num[2], colour = var_color, shape = var_color))
    }

    text_title <- paste0(text_title, " by ", var_color)
  }
  p <- p + theme_bw()
  p <- p + geom_point()


  if (!is.null(var_facet)) {
    if (length(var_facet) == 1) {
      p <- p + facet_grid(as.formula(paste0(". ~ ", var_facet[1])), scales = "free_y", drop = TRUE)

      text_title <- paste0(text_title, ", by ", var_facet[1])
    }
    if (length(var_facet) == 2) {
      p <- p + facet_grid(as.formula(paste0(var_facet[2], " ~ ", var_facet[1])), scales = "free_y", drop = TRUE)

      text_title <- paste0(text_title, ", by ", var_facet[1], " and ", var_facet[2])
    }
  }

  p <- p + labs(
                  title     = text_title
                #, subtitle  = "subtitle"
                #, x         = "x"
                #, y         = paste0(var_num[1], " proportion")
                #, caption = paste0(  "Caption 1"
                #                  , "\nCaption 2"
                #                  )
                #, colour    = "Class"
                #, shape     = "Class"
                #, linetype  = "General Health"  #"Diagnosis"
                #, fill      = "Diagnosis"
                #, tag = "A"
                )
  #p <- p + theme(legend.position = "bottom") # "none"

  if (sw_print) {
    print(p)
  }

  return(p)
} # e_plot_scatterplot

