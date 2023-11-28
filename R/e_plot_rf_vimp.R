#' A function to plot the "all" column from a Random Forests Variable Importance Plot (VIMP).
#'
#' @param v_imp       \code{$importance} table from \code{randomForestSRC::rfsrc()}
#' @param targets     a list of column names of \code{v_imp}, such as \code{c("all", "Yes")}.  If \code{NULL}, does "all" and every target.
#'
#' @return p
#' @importFrom tibble as_tibble
#' @importFrom RColorBrewer brewer.pal
#' @importFrom tidyselect all_of
#' @importFrom forcats fct_reorder
#' @importFrom tidyr pivot_longer
#' @import dplyr
#' @import ggplot2
#' @export
#'
#' @examples
#' # o_class <- randomForestSRC::rfsrc(...)
#' # e_plot_rf_vimp(o_class$importance)
e_plot_rf_vimp <-
  function(
    v_imp
  , targets = NULL
  ) {
  ## targets = c("all", "Yes")

  if(is.null(targets)) {
    targets = v_imp |> colnames()
  }

  if (any(targets %notin% colnames(v_imp))) {
    warning("erikmisc::e_plot_rf_vimp(), targets do not match 'all' or individual targets")
    return(NULL)
  }

  dat_vimp <-
    v_imp |>
    tibble::as_tibble(
      rownames = "Var"
    ) |>
    dplyr::mutate(
      Var = Var |> factor() |> forcats::fct_reorder(.x = all)
    ) |>
    dplyr::select(
      Var
    , tidyselect::all_of(targets)
    ) |>
    tidyr::pivot_longer(
      cols = -Var
    ) |>
    dplyr::mutate(
      name = name |> factor(levels = targets)
    ) |>
    dplyr::arrange(
      name
    , desc(Var)
    ) |>
    dplyr::mutate(
      positive = (value > 0) |> factor(levels = c(FALSE, TRUE), labels = c("FALSE", "TRUE"))
    )


  # Create a custom color scale
  myColors <- RColorBrewer::brewer.pal(4, "Set1")
  names(myColors) <- levels(dat_vimp$positive)

  #library(ggplot2)
  p <- ggplot(dat_vimp, aes(x = Var, y = value, fill = positive))
  p <- p + theme_bw()
  p <- p + geom_hline(yintercept = 0, alpha = 1/2, linewidth = 1/4)
  p <- p + geom_bar(stat = "identity", colour = "black", width = 1/2, alpha = 3/4)
  p <- p + facet_grid(. ~ name, drop = TRUE)
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
