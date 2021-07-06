#' A function to plot the "all" column from a Random Forests Variable Importance Plot (VIMP).
#'
#' @param v_imp
#'
#' @return p
#' @import ggplot2
#' @export
#'
#' @examples
f_vimp_plot <- function(v_imp) {

  dat_vimp <-
    v_imp %>%
    select(
      Var
    , "all"
    ) %>%
    arrange(
      desc(all)
    ) %>%
    mutate(
      positive = (all > 0) %>% factor(levels = c(FALSE, TRUE), labels = c("FALSE", "TRUE"))
    )


  # Create a custom color scale
  myColors <- RColorBrewer::brewer.pal(4, "Set1")
  names(myColors) <- levels(dat_vimp$positive)

  #library(ggplot2)
  p <- ggplot(dat_vimp, aes(x = reorder(Var, all), y = all, fill = positive))
  p <- p + theme_bw()
  p <- p + geom_hline(yintercept = 0, alpha = 1/2, size = 1/4)
  p <- p + geom_bar(stat = "identity", colour = "black", width = 1/2, alpha = 3/4)
  p <- p + scale_fill_manual(values = myColors, drop = FALSE)
  p <- p + coord_flip()

  p <- p + labs(  title     = NULL
                , x         = NULL
                , y         = "Variable Importance"
                #, colour    =
                #, shape     =
                #, linetype  =
                #, fill      =
                )
  p <- p + theme(legend.position = "none")

  return(p)

}
