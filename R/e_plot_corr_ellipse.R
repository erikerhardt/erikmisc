#' Correlation plot with ellipses
#'
#' @param corr        correlation matrix
#' @param outline     option
#' @param col         option
#' @param upper.panel option
#' @param lower.panel option
#' @param diag        option
#' @param digits      option
#' @param bty         option
#' @param axes        option
#' @param xlab        option
#' @param ylab        option
#' @param asp         option
#' @param cex.lab     option
#' @param cex         option
#' @param mar         option
#' @param ...         option
#'
#' @return \code{invisible(NULL)}
#' @importFrom ellipse ellipse
#' @importFrom graphics plot.new
#' @importFrom graphics text
#' @importFrom graphics polygon
#' @importFrom graphics lines
#' @importFrom graphics mtext
#' @importFrom graphics strwidth
#' @export
#'
#' @examples
#' datasets::mtcars |> cor() |> e_plot_corr_ellipse()
#'
#' \dontrun{
#' ## Color version
#' # use mtcars since all numeric
#' tab_cor <-
#'   datasets::mtcars |>
#'   as.matrix() |>
#'   Hmisc::rcorr(
#'     type = c("pearson", "spearman")[1]
#'   )
#' tab_cor$r
#'
#' # red neg, white 0, blue pos
#' color_scale <-
#'   c(
#'     rgb(241,  54,  23, maxColorValue = 255)
#'   , "white"
#'   , rgb(  0,  61, 104, maxColorValue = 255)
#'   )
#' # color ramp in "Lab" space
#' color_ramp <- colorRampPalette(color_scale, space = "Lab")
#' # Create scale with 200 points
#' colors_plot <- color_ramp(100)
#'
#' # move correlation range from [-1, 1] to [0, 100] for colors
#' tab_cor$r |>
#' e_plot_corr_ellipse(
#'   col  = colors_plot[round(((tab_cor$r + 1)/2) * 100)]
#' , main = "Colored ellipses"
#' , upper.panel = c("ellipse", "number", "none")[1]
#' , lower.panel = c("ellipse", "number", "none")[2]
#' , diag = c("none", "ellipse", "number")[3]
#' , digits = 3
#' )
#' }
e_plot_corr_ellipse <-
  function(
    corr
  , outline = FALSE
  , col = "grey"
  , upper.panel = c("ellipse", "number", "none")[1]
  , lower.panel = c("ellipse", "number", "none")[2]
  , diag = c("none", "ellipse", "number")[3]
  , digits = 2
  , bty = "n"
  , axes = FALSE
  , xlab = ""
  , ylab = ""
  , asp = 1
  , cex.lab = par("cex.lab")
  , cex = 0.75 * par("cex")
  , mar = c(0, 0, 1, 0)  # 0.1 + c(2, 2, 4, 2)
  , ...
  ) {

  # this is a modified version of the plotcorr function from the ellipse package
  # this prints numbers and ellipses on the same plot but upper.panel and lower.panel changes what is displayed
  # diag now specifies what to put in the diagonal (numbers, ellipses, nothing)
  # digits specifies the number of digits after the . to round to
  # unlike the original, this function will always print x_i by x_i correlation rather than being able to drop it
  # modified by Esteban Buz
  # see http://hlplab.wordpress.com/2012/03/20/correlation-plot-matrices-using-the-ellipse-library/

  #if (!require('ellipse', quietly = TRUE, character = TRUE)) {
  #  stop('Need the ellipse library.  Run: install.packages("ellipse")')
  #}
  savepar <- par(pty = "s", mar = mar)
  on.exit(par(savepar))
  if (is.null(corr))
    return(invisible())
  if ((!is.matrix(corr)) || (round(min(corr, na.rm = TRUE), 6) < -1) || (round(max(corr, na.rm = TRUE), 6) > 1))
    stop("Need a correlation matrix")
  graphics::plot.new()
  par(new = TRUE)
  rowdim <- dim(corr)[1]
  coldim <- dim(corr)[2]
  rowlabs <- dimnames(corr)[[1]]
  collabs <- dimnames(corr)[[2]]
  if (is.null(rowlabs))
    rowlabs <- 1:rowdim
  if (is.null(collabs))
    collabs <- 1:coldim
  rowlabs <- as.character(rowlabs)
  collabs <- as.character(collabs)
  col <- rep(col, length = length(corr))
  dim(col) <- dim(corr)
  #upper.panel <- match.arg(upper.panel)
  #lower.panel <- match.arg(lower.panel)
  #diag <- match.arg(diag)
  cols <- 1:coldim
  rows <- 1:rowdim
  maxdim <- max(length(rows), length(cols))
  plt <- par("plt")
  xlabwidth <- max(strwidth(rowlabs[rows], units = "figure", cex = cex.lab))/(plt[2] - plt[1])
  xlabwidth <- xlabwidth * maxdim/(1 - xlabwidth)
  ylabwidth <- max(strwidth(collabs[cols], units = "figure", cex = cex.lab))/(plt[4] - plt[3])
  ylabwidth <- ylabwidth * maxdim/(1 - ylabwidth)
  plot(c(-xlabwidth - 0.5, maxdim + 0.5), c(0.5, maxdim + 1 + ylabwidth), type = "n", bty = bty, axes = axes, xlab = "", ylab = "", asp = asp, cex.lab = cex.lab, ...)
  graphics::text(rep(0, length(rows)), length(rows):1, labels = rowlabs[rows], adj = 1, cex = cex.lab)
  graphics::text(cols, rep(length(rows) + 1, length(cols)), labels = collabs[cols], srt = 90, adj = 0, cex = cex.lab)
  mtext(xlab, 1, 0)
  mtext(ylab, 2, 0)
  mat <- diag(c(1, 1))
  plotcorrInternal <- function() {
    if (i == j){ #diag behavior
      if (diag == 'none'){
        return()
      } else if (diag == 'number'){
        graphics::text(j + 0.3, length(rows) + 1 - i, round(corr[i, j], digits=digits), adj = 1, cex = cex)
      } else if (diag == 'ellipse') {
        mat[1, 2] <- corr[i, j]
        mat[2, 1] <- mat[1, 2]
        ell <- ellipse::ellipse(mat, t = 0.43)
        ell[, 1] <- ell[, 1] + j
        ell[, 2] <- ell[, 2] + length(rows) + 1 - i
        polygon(ell, col = col[i, j])
        if (outline)
          graphics::lines(ell)
      }
    } else if (i >= j){ #lower half of plot
      if (lower.panel == 'ellipse') { #check if ellipses should go here
        mat[1, 2] <- corr[i, j]
        mat[2, 1] <- mat[1, 2]
        ell <- ellipse::ellipse(mat, t = 0.43)
        ell[, 1] <- ell[, 1] + j
        ell[, 2] <- ell[, 2] + length(rows) + 1 - i
        polygon(ell, col = col[i, j])
        if (outline)
          graphics::lines(ell)
      } else if (lower.panel == 'number') { #check if ellipses should go here
        graphics::text(j + 0.3, length(rows) + 1 - i, round(corr[i, j], digits=digits), adj = 1, cex = cex)
      } else {
        return()
      }
    } else { #upper half of plot
      if (upper.panel == 'ellipse') { #check if ellipses should go here
        mat[1, 2] <- corr[i, j]
        mat[2, 1] <- mat[1, 2]
        ell <- ellipse::ellipse(mat, t = 0.43)
        ell[, 1] <- ell[, 1] + j
        ell[, 2] <- ell[, 2] + length(rows) + 1 - i
        polygon(ell, col = col[i, j])
        if (outline)
          graphics::lines(ell)
      } else if (upper.panel == 'number') { #check if ellipses should go here
        graphics::text(j + 0.3, length(rows) + 1 - i, round(corr[i, j], digits=digits), adj = 1, cex = cex)
      } else {
        return()
      }
    }
  }
  for (i in 1:dim(corr)[1]) {
    for (j in 1:dim(corr)[2]) {
      plotcorrInternal()
    }
  }
  invisible(NULL)
} # e_plot_corr_ellipse
