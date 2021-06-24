#' t-SNE plot and clustering plot
#'
#' @param dat
#' @param subtitle
#'
#' @return
#' @importFrom tsne tsne
#' @import ggplot2
#' @export
#'
#' @examples
f_tsne <- function(dat, subtitle = NULL, ellipse_level = 0.5) {
  if (nrow(dat) == 0) {
    warning("WARNING: tSNE data has 0 obs, no output produced")
    return(NULL)
  }
  if (ncol(dat) <= 2) {
    warning("WARNING: tSNE data has fewer than 2 variables, no output produced")
    return(NULL)
  }

  #library(tsne)
  tsne.res <- tsne::tsne(subset(dat, select = -dx_current), max_iter = 2000, perplexity = 5)

  dat$tsne.x <- tsne.res[,1]
  dat$tsne.y <- tsne.res[,2]

  #library(ggplot2)
  p <- ggplot2::ggplot(dat, aes(x = tsne.x, y = tsne.y, colour = dx_current, shape = dx_current, linetype = dx_current))
  p <- p + ggplot2::geom_point(size = 3)
  p <- p + ggplot2::stat_ellipse(level = ellipse_level)
  p <- p + ggplot2::labs(title = "tSNE clustering"
              , subtitle = subtitle)
  p <- p + ggplot2::theme_bw()
  p <- p + ggplot2::theme(legend.position="bottom")
  return(p)
}
