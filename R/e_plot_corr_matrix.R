#' Correlation matrix plot with ellipses
#'
#' See \code{corrplot::corrplot()} for creating custom versions of this
#'
#' @param dat               data.frame or tibble to plot a correlation matrix of just the numeric variables.
#' @param sw_type           Pearson or Spearman correlation
#' @param sw_conf_level     Confidence level for significance, typically 0.95.
#' @param sw_plot_type      A full symmetric plot or a mixed plot with different visualizations on upper and lower triangle.
#' @param sw_cluster_rect   Number of cluster rectangles, available for \code{sw_plot_type = "full"}
#' @param sw_hclust_method  Clustering method for sorting variables and grouping with rectangles.
#'
#' @return out        a list, including tables of correlations and p-values from \code{Hmisc::rcorr}, CIs for r from \code{corrplot::cor.mtest}, and a plot from \code{corrplot::corrplot} as a ggplot grob
#' @import dplyr
#' @import tidyselect
#' @import corrplot
#' @importFrom Hmisc rcorr
#' @importFrom cowplot plot_grid
#' @importFrom cowplot as_grob
#' @export
#'
#' @examples
#' erikmisc::dat_mtcars_e |> e_plot_corr_matrix()
#'
#' out <- erikmisc::dat_mtcars_e |> e_plot_corr_matrix(sw_cluster_rect = 3)
#' out$plot |> print()
#'
#' out <- erikmisc::dat_mtcars_e |> e_plot_corr_matrix(sw_plot_type = "mixed")
#' out$plot |> print()
e_plot_corr_matrix <-
  function(
    dat               = NULL
  , sw_type           = c("pearson", "spearman")[1]
  , sw_conf_level     = 0.95
  , sw_plot_type      = c("full", "mixed")[1]
  , sw_cluster_rect   = NULL
  , sw_hclust_method  = c("ward", "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")[3]
  ) {

  ## Restrict to numeric variables, convert to matrix
  dat_mat <-
    dat |>
    dplyr::select(
      tidyselect::where(is.numeric)
    ) |>
    as.matrix()

  if (ncol(dat_mat) < 2) {
    message("e_plot_corr_matrix, fewer than 2 numeric columns, returning NULL")
    return(NULL)
  }

  # Correlation matrix
  tab_cor <-
    dat_mat |>
    Hmisc::rcorr(
      type = sw_type
    )
  #tab_cor

  # Correlation p-values and CIs
  cor_mtest_out <-
    corrplot::cor.mtest(
      mat         = dat_mat
    , conf.level  = sw_conf_level
    , method      = sw_type
    )
  #cor_mtest_out

  # https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

  if(sw_plot_type == c("full", "mixed")[1]) {
    p <-
      cowplot::as_grob(
        ~
        corrplot::corrplot(
          corr            = tab_cor$r
        , is.corr         = TRUE
        , p.mat           = cor_mtest_out$p
        , type            = c("full", "upper", "lower")[1]
        , method          = c("circle", "square", "ellipse", "number", "shade", "color", "pie")[3]
        #, upper          = c("circle", "square", "ellipse", "number", "shade", "color", "pie")[3]
        , tl.pos          = c("lt", "ld", "td", "d", "n")[4]
        , tl.col          = "black"
        , cl.ratio        = 0.1
        , cl.align.text   = c("l", "c", "r")[2]
        , cl.length       = 11
        #, diag           = c("n", "l", "u")[1]
        , bg              = "white"
        , addgrid.col     = "grey"
        , addCoef.col     = "black"
        , number.cex      = 1
        #, lower.col      = "black"
        #, upper.col      = NULL
        #, plotCI         = c("n", "square", "circle", "rect")[1]
        , mar             = c(0, 0, 1, 0)           # c(bottom, left, top, right)
        , title           = "Correlation of numeric predictor variables"
        #, sig.level      = rev(c(1e-04, 0.001, 0.01, 0.05))
        , insig           = c("pch", "p-value", "blank", "n", "label_sig")[1]
        , order           = c("original", "AOE", "FPC", "hclust", "alphabet")[4]
        , addrect         = sw_cluster_rect
        , hclust.method   = sw_hclust_method
        )
      ) |>
      cowplot::plot_grid()
  } # full

  if(sw_plot_type == c("full", "mixed")[2]) {
    p <-
      cowplot::as_grob(
        ~
        corrplot::corrplot.mixed(
          corr            = tab_cor$r
        , is.corr         = TRUE
        , p.mat           = cor_mtest_out$p
        , upper           = c("circle", "square", "ellipse", "number", "shade", "color", "pie")[3]
        , lower           = c("circle", "square", "ellipse", "number", "shade", "color", "pie")[6]
        , outline         = TRUE
        , tl.pos          = c("lt", "ld", "td", "d", "n")[4]
        , tl.col          = "black"
        , cl.ratio        = 0.1
        , cl.align.text   = c("l", "c", "r")[2]
        , cl.length       = 11
        #, col             = COL2('RdYlBu', 100)
        , addCoef.col     = "black" #"grey70"
        , number.digits   = 2
        , diag            = c("n", "l", "u")[1]
        , bg              = "white"
        , addgrid.col     = "grey"
        , lower.col       = NULL
        , upper.col       = NULL
        , plotCI          = c("n", "square", "circle", "rect")[1]
        , mar             = c(0, 0, 1, 0)           # c(bottom, left, top, right)
        , title           = "Correlation of numeric predictor variables"
        #, sig.level      = rev(c(1e-04, 0.001, 0.01, 0.05))
        , insig           = c("pch", "p-value", "blank", "n", "label_sig")[1]
        , order           = c("original", "AOE", "FPC", "hclust", "alphabet")[4]
        , addrect         = sw_cluster_rect
        , hclust.method   = sw_hclust_method
        , rect.col        = "black"
        , rect.lwd        = 3
        )
      ) |>
      cowplot::plot_grid()
  } # mixed

  out <-
    list(
      tab_cor = tab_cor
    , tab_CI  = cor_mtest_out
    , plot    = p
    )

  return(out)
} # e_plot_corr_matrix
