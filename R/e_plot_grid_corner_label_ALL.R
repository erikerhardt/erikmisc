#' For corner labels, remove the labs(tag=) argument and use this
#'
#' See \code{?e_plot_grid_corner_label_list} to use with a list.
#'
#' @param p             ggplot grob
#' @param label_corner  label for top-left corner of plot
#' @param font_color    color
#' @param font_size     point size
#' @param font_face     font face
#'
#' @return grob with top-left corner label
#' @export
e_plot_grid_corner_label <-
  function(
    p
  , label_corner = "A"
  , font_color   = "black"
  , font_size    = 18
  , font_face    = "bold"
  ) {
  # for adding labels to corners of figure panels using grid.arrange()
    # https://stackoverflow.com/questions/17576381/label-individual-panels-in-a-multi-panel-ggplot2

  p <-
    gridExtra::arrangeGrob(
      p
    , top =
        grid::textGrob(
          label_corner
        , x     = grid::unit(0, "npc")
        , y     = grid::unit(1, "npc")
        , just  = c("left", "top")
        , gp    = grid::gpar(
                    col         = font_color
                  , fontsize    = font_size
                  , fontface    = font_face
                  #, fontfamily  = "Times Roman"
                  )
        )
    )

  return(p)
} # e_plot_grid_corner_label



#' Add corner labels to all grobs in a list using \code{e_plot_grid_corner_label()} instead of labs(tag=)
#'
#' @param p_list        list of ggplot grobs
#' @param labels_corner list of labels for top-left corner of plot
#' @param font_color    color
#' @param font_size     point size
#' @param font_face     font face
#'
#' @return list of grobs with top-left corner labels
#' @export
#'
#' @examples
#' # make one example plot
#' library(ggplot2)
#' p <- ggplot(dat_mtcars_e, aes(x = disp, y = mpg, colour = am))
#' p <- p + theme_bw()
#' p <- p + geom_point(alpha = 0.5)
#' p <- p + labs(  x         = labelled::var_label(dat_mtcars_e[["disp"]])
#'               , y         = labelled::var_label(dat_mtcars_e[["mpg"]])
#'               , colour    = labelled::var_label(dat_mtcars_e[["am"]])
#'               #, tag = "A"
#'               )
#' p <- p + theme(legend.position = "bottom") # "none"
#' p <- p + theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1, Caption align left
#' #print(p)
#'
#' # make 3 plots with separate titles
#' p1 <- p + labs(title = "First" )
#' p2 <- p + labs(title = "Second")
#' p3 <- p + labs(title = "Third" )
#'
#' # Make a list of the plots
#' p_list <-
#'   list(
#'     p1
#'   , p2
#'   , p3
#'   )
#'
#' # label all of the plots
#' p_list <- e_plot_grid_corner_label_list(p_list)
#'
#' # Arrange in a grid
#' library(gridExtra)
#' library(grid)
#' lay_grid <-
#'   rbind(
#'     c(1, 3)
#'   , c(2, 3)
#'   )
#' p_arranged <-
#'   gridExtra::arrangeGrob(
#'     grobs         = p_list
#'   , layout_matrix = lay_grid
#'   ) %>%
#'   ggpubr::as_ggplot()
#' # print the plots
#' p_arranged %>% print()
e_plot_grid_corner_label_list <-
  function(
    p_list
  , labels_corner = LETTERS
  , font_color    = "black"
  , font_size     = 18
  , font_face     = "bold"
  ) {
  # for adding labels to corners of figure panels using grid.arrange()
    # https://stackoverflow.com/questions/17576381/label-individual-panels-in-a-multi-panel-ggplot2

  # add plot panel grid corner labels
  for (i_list in 1:length(p_list)) {
    p_list[[i_list]] <-
      e_plot_grid_corner_label(
          p_list[[i_list]]
        , label_corner = labels_corner[[i_list]]
        , font_color   = font_color
        , font_size    = font_size
        , font_face    = font_face
        )
  }

  return(p_list)
} # e_plot_grid_corner_label_list

