#' Visual comparison of whether Bootstrap sampling distribution is close to Normal
#'
#' A function to compare the bootstrap sampling distribution
#'   of the difference of means from two samples with
#'   a normal distribution with mean and SEM estimated from the data
#'
#' @param dat1 a list of values from Sample 1
#' @param dat2 a list of values from Sample 2
#' @param N    number of bootstrap iterations
#'
#' @return
#' @export
#'
#' @examples
#' e_bs_two_samp_diff_dist(dat1 = runif(6), dat2 = runif(6) - 2)
e_bs_two_samp_diff_dist <-
  function(
    dat1
  , dat2
  , N = 1e4
  ) {

  n1 <- length(dat1);
  n2 <- length(dat2);
  # resample from data
  sam1 <- matrix(sample(dat1, size = N * n1, replace = TRUE), ncol=N);
  sam2 <- matrix(sample(dat2, size = N * n2, replace = TRUE), ncol=N);
  # calculate the means and take difference between populations
  sam1_mean <- colMeans(sam1);
  sam2_mean <- colMeans(sam2);
  diff_mean <- sam1_mean - sam2_mean;
  # save par() settings
  old_par <- par(no.readonly = TRUE)
  # make smaller margins
  par(mfrow=c(3,1), mar=c(3,2,2,1), oma=c(1,1,1,1))
  # Histogram overlaid with kernel density curve
  hist(dat1, freq = FALSE, breaks = 6
      , main = paste("Sample 1", "\n"
                    , "n =", n1
                    , ", mean =", signif(mean(dat1), digits = 5)
                    , ", sd =", signif(sd(dat1), digits = 5))
      , xlim = range(c(dat1, dat2)))
  points(density(dat1), type = "l")
  rug(dat1)

  hist(dat2, freq = FALSE, breaks = 6
      , main = paste("Sample 2", "\n"
                    , "n =", n2
                    , ", mean =", signif(mean(dat2), digits = 5)
                    , ", sd =", signif(sd(dat2), digits = 5))
      , xlim = range(c(dat1, dat2)))
  points(density(dat2), type = "l")
  rug(dat2)

  hist(diff_mean, freq = FALSE, breaks = 25
      , main = paste("Bootstrap sampling distribution of the difference in means", "\n"
                   , "mean =", signif(mean(diff_mean), digits = 5)
                   , ", se =", signif(sd(diff_mean), digits = 5)))
  # overlay a density curve for the sample means
  points(density(diff_mean), type = "l")
  # overlay a normal distribution, bold and red
  x <- seq(min(diff_mean), max(diff_mean), length = 1000)
  points(x, dnorm(x, mean = mean(diff_mean), sd = sd(diff_mean))
       , type = "l", lwd = 2, col = "red")
  # place a rug of points under the plot
  rug(diff_mean)
  # restore par() settings
  par(old_par)

  invisible(NULL)
} # e_bs_two_samp_diff_dist
