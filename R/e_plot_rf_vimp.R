#' A function to plot the "all" column from a Random Forests Variable Importance Plot (VIMP).
#'
#' @param v_imp
#'
#' @return p
#' @importFrom tibble as_tibble
#' @importFrom RColorBrewer brewer.pal
#' @import dplyr
#' @import ggplot2
#' @export
#'
#' @examples
#' # o_class <- randomForestSRC::rfsrc(...)
#' # e_plot_rf_vimp(o_class$importance)
e_plot_rf_vimp <- function(v_imp) {

  dat_vimp <-
    v_imp %>%
    tibble::as_tibble(
      rownames = "Var"
    ) |>
    dplyr::select(
      Var
    , "all"
    ) %>%
    dplyr::arrange(
      desc(all)
    ) %>%
    dplyr::mutate(
      positive = (all > 0) %>% factor(levels = c(FALSE, TRUE), labels = c("FALSE", "TRUE"))
    )


  # Create a custom color scale
  myColors <- RColorBrewer::brewer.pal(4, "Set1")
  names(myColors) <- levels(dat_vimp$positive)

  #library(ggplot2)
  p <- ggplot(dat_vimp, aes(x = reorder(Var, all), y = all, fill = positive))
  p <- p + theme_bw()
  p <- p + geom_hline(yintercept = 0, alpha = 1/2, linewidth = 1/4)
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

} # e_plot_rf_vimp
