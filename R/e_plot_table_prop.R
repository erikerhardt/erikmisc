#' Calculate proportions and plot factor variable by up to two other factor variables
#'
#' @param dat_plot  data to plot
#' @param var_names proportions are calculated for first variable, by up to two other variables (row facets, then column facets), \code{c("y", "a", "b")}
#' @param sw_print  T/F whether to print table and display plot
#'
#' @return          a list including a table and ggplot object
#' @import ggplot2
#' @importFrom tidyr drop_na
#' @importFrom tidyselect any_of
#' @export
#'
#' @examples
#' e_plot_table_prop(
#'     dat_plot  = dat_mtcars_e
#'   , var_names = c("vs")
#'   )
#' e_plot_table_prop(
#'     dat_plot  = dat_mtcars_e
#'   , var_names = c("vs", "am")
#'   )
#' e_plot_table_prop(
#'     dat_plot  = dat_mtcars_e
#'   , var_names = c("vs", "am", "gear")
#'   )
e_plot_table_prop <-
  function(
    dat_plot  = NULL
  , var_names = NULL
  , sw_print  = FALSE
  ) {

  # calculate proportions
  tab_plot <-
    e_table_sum_freq_prop(
      dat       = dat_plot %>% tidyr::drop_na(tidyselect::any_of(var_names))
    , var_names = var_names
    )

  if (sw_print) {
    print(tab_plot)
  }


  x_var_name <- var_names[length(var_names)]


  library(ggplot2)
  p <- ggplot(tab_plot, aes_string(x = x_var_name, y = "prop"))
  p <- p + theme_bw()
  p <- p + geom_col()

  text_title <- paste0("Plot of ", var_names[1])

  if (length(var_names) == 2) {
    p <- p + facet_grid(as.formula(paste0(". ~ ", var_names[1])), scales = "free_y", drop = TRUE)

    text_title <- paste0(text_title, " by ", var_names[2])
  }
  if (length(var_names) == 3) {
    p <- p + facet_grid(as.formula(paste0(var_names[2], " ~ ", var_names[1])), scales = "free_y", drop = TRUE)

    text_title <- paste0(text_title, " by ", var_names[2], " and ", var_names[3])
  }

  p <- p + labs(
                  title     = text_title
                #, subtitle  = "subtitle"
                #, x         = "x"
                , y         = paste0(var_names[1], " proportion")
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

  out <- list(table = tab_plot, plot = p)

  return(out)
} # e_plot_table_prop

