#' Visual comparison of whether Bootstrap sampling distribution is close to Normal
#'
#' A function to compare the bootstrap sampling distribution with
#'   a normal distribution with mean and SEM estimated from the data
#'
#' @param dat a list of values
#' @param N   number of bootstrap iterations
#'
#' @return \code{invisible(NULL)}
#' @importFrom graphics hist
#' @importFrom graphics par
#' @importFrom graphics points
#' @importFrom graphics rug
#' @importFrom stats density
#' @importFrom stats dnorm
#' @importFrom stats sd
#' @export
#'
#' @examples
#' e_plot_bs_one_samp_dist(dat = runif(6))
e_plot_bs_one_samp_dist <-
  function(
    dat
  , N = 1e4
  ) {

  n <- length(dat);
  # resample from data
  sam <- matrix(sample(dat, size = N * n, replace = TRUE), ncol=N);
  # draw a histogram of the means
  sam_mean <- colMeans(sam);
  # save par() settings
  old_par <- par(no.readonly = TRUE)
  # make smaller margins
  par(mfrow=c(2,1), mar=c(3,2,2,1), oma=c(1,1,1,1))
  # Histogram overlaid with kernel density curve
  graphics::hist(dat, freq = FALSE, breaks = 6
      , main = "Plot of data with smoothed density curve")
  points(density(dat), type = "l")
  rug(dat)

  graphics::hist(sam_mean, freq = FALSE, breaks = 25
      , main = "Bootstrap sampling distribution of the mean"
      , xlab = paste("Data: n =", n
                   , ", mean =", signif(mean(dat), digits = 5)
                   , ", se =", signif(stats::sd(dat)/sqrt(n)), digits = 5))
  # overlay a density curve for the sample means
  points(density(sam_mean), type = "l")
  # overlay a normal distribution, bold and red
  x <- seq(min(sam_mean), max(sam_mean), length = 1000)
  points(x, stats::dnorm(x, mean = mean(dat), sd = stats::sd(dat)/sqrt(n))
       , type = "l", lwd = 2, col = "red")
  # place a rug of points under the plot
  rug(sam_mean)
  # restore par() settings
  par(old_par)

  invisible(NULL)
} # e_plot_bs_one_samp_dist
