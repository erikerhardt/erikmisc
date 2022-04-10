#' Plot scatterplot of a numeric y-variable against a numeric or categorical x-variable, by up to one color factor or numeric variable and two facets factor variables
#'
#' @param dat_plot  data to plot
#' @param var_x     a numeric or categorical variable name
#' @param var_y     a numeric variable name
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
#'   , var_x     = "disp"           # numeric x
#'   , var_y     = "mpg"
#'   )
#' e_plot_scatterplot(
#'     dat_plot  = dat_mtcars_e
#'   , var_x     = "am"             # factor x
#'   , var_y     = "mpg"
#'   )
#' e_plot_scatterplot(
#'     dat_plot  = dat_mtcars_e
#'   , var_x     = "disp"
#'   , var_y     = "mpg"
#'   , var_color = "cyl"            # factor color
#'   )
#' e_plot_scatterplot(
#'     dat_plot  = dat_mtcars_e
#'   , var_x     = "disp"
#'   , var_y     = "mpg"
#'   , var_color = "wt"             # numeric color
#'   )
#' e_plot_scatterplot(
#'     dat_plot  = dat_mtcars_e
#'   , var_x     = "disp"
#'   , var_y     = "mpg"
#'   , var_facet = c("vs")          # one facet
#'   )
#' e_plot_scatterplot(
#'     dat_plot  = dat_mtcars_e
#'   , var_x     = "disp"
#'   , var_y     = "mpg"
#'   , var_color = "cyl"
#'   , var_facet = c("vs", "am")    # two facets
#'   )
#' e_plot_scatterplot(
#'     dat_plot  = dat_mtcars_e
#'   , var_x     = "am"             # factor x
#'   , var_y     = "mpg"
#'   , var_color = "wt"             # numeric color
#'   , var_facet = c("vs")
#'   )
e_plot_scatterplot <-
  function(
    dat_plot  = NULL
  , var_x     = NULL
  , var_y     = NULL
  , var_color = NULL
  , var_facet = NULL
  , sw_print  = FALSE
  ) {

  library(ggplot2)
  text_title <- paste0("Plot of ", var_y, " vs ", var_x)

  position_dodge_val <- position_dodge(0)

  if (is.null(var_color)) {
    p <- ggplot(dat_plot, aes_string(x = var_x, y = var_y))
  } else {
    if (class(dat_plot[[ var_color ]]) == "numeric") {
      p <- ggplot(dat_plot, aes_string(x = var_x, y = var_y, colour = var_color))
    } else {
      p <- ggplot(dat_plot, aes_string(x = var_x, y = var_y, colour = var_color, shape = var_color))
      position_dodge_val <- position_dodge(0.6) # move them .05 to the left and right
    }

    text_title <- paste0(text_title, " by ", var_color)
  }
  p <- p + theme_bw()

  if (class(dat_plot[[ var_x ]]) == "numeric") {
    p <- p + geom_point()
  } else {
    # plot a reference line for the global mean (assuming no groups)
    p <- p + geom_hline(yintercept = mean(dat_plot[[ var_y ]]),
                        colour = "black", linetype = "dashed", size = 0.3, alpha = 0.5)
    # boxplot, size=.75 to stand out behind CI
    p <- p + geom_violin(width = 0.25, alpha = 0.25, position = position_dodge_val)
    p <- p + geom_boxplot(width = 0.25, alpha = 0.25, position = position_dodge_val)
    # points for observed data
    p <- p + geom_point(position = position_jitter(w = 0.1, h = 0), alpha = 1) #, position = position_dodge_val)
    # diamond at mean for each group
    p <- p + stat_summary(fun = mean, geom = "point", shape = 18, size = 5,
                          alpha = 0.8, position = position_dodge_val) # colour = "red",
    # confidence limits based on normal distribution
    p <- p + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",
                          width = 0.2, alpha = 0.8, position = position_dodge_val) # colour = "red",
  }


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
                #, y         = paste0(var_x, " proportion")
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

