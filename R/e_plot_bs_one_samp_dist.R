#' Visual comparison of whether Bootstrap sampling distribution of the mean is close to Normal
#'
#' A function to compare the bootstrap sampling distribution with
#'   a normal distribution with mean and SEM estimated from the data
#'
#' @param dat             a list of values
#' @param N               number of bootstrap iterations
#' @param sw_graphics     use either ggplot or R base graphics
#' @param sw_ggplot_print if ggplot, print the plot or just return the grob
#' @param conf_level      0.95 for a 95% bootstrap CI
#'
#' @return \code{invisible(NULL) or ggplot grob}
#' @import dplyr
#' @import ggplot2
#' @importFrom cowplot plot_grid
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
#' e_plot_bs_one_samp_dist(dat = runif(15), sw_graphics = "base")
#' e_plot_bs_one_samp_dist(dat = runif(15), sw_graphics = "ggplot")
e_plot_bs_one_samp_dist <-
  function(
    dat
  , N = 1e4
  , sw_graphics = c("ggplot", "base")[1]
  , sw_ggplot_print = c(TRUE, FALSE)[2]
  , conf_level = 0.95
  ) {
  ## dat = runif(6)

  n <- length(dat)
  # resample from data
  dat_sam <- matrix(sample(dat, size = N * n, replace = TRUE), ncol=N)
  # draw a histogram of the means
  dat_sam_mean <- colMeans(dat_sam)

  # obs mean
  obs_mean <- mean(dat, na.rm = TRUE)
  # obs CI
  prob_CI <- c(lower = (1 - conf_level) / 2, upper = 1 - (1 - conf_level) / 2)
  ind_CI <- prob_CI * N
  ind_CI[1] <- floor(ind_CI[1])
    if (ind_CI[1] == 0) { ind_CI[1] <- 1 }
  ind_CI[2] <- ceiling(ind_CI[2])
  CI_limits <- sort(dat_sam_mean)[ind_CI]

  if (sw_graphics == c("ggplot", "base")[2]) {
    # save par() settings
    old_par <- graphics::par(no.readonly = TRUE)
    # make smaller margins
    graphics::par(mfrow=c(2,1), mar=c(3,2,2,1), oma=c(1,1,1,1))
    # Histogram overlaid with kernel density curve
    graphics::hist(dat, freq = FALSE, breaks = ceiling(log(n, base = 1.2))
        , main = "Data with smoothed density curve")
    graphics::points(density(dat), type = "l")
    graphics::rug(dat)

    graphics::hist(dat_sam_mean, freq = FALSE, breaks = ceiling(log(N, base = 1.2))
        , main = "Bootstrap sampling distribution of the mean"
        , xlab = paste("Data: n =", n
                     , ", mean =", signif(mean(dat), digits = 3)
                     , ", se =", signif(stats::sd(dat)/sqrt(n)), digits = 3))
    # overlay a density curve for the sample means
    graphics::points(density(dat_sam_mean), type = "l")
    # overlay a normal distribution, bold and red
    x <- seq(min(dat_sam_mean), max(dat_sam_mean), length = 1000)
    graphics::points(x, stats::dnorm(x, mean = mean(dat), sd = stats::sd(dat)/sqrt(n))
         , type = "l", lwd = 2, col = "red")
    # place a rug of points under the plot
    graphics::rug(dat_sam_mean)
    # restore par() settings
    graphics::par(old_par)

    invisible(NULL)
  } # base

  if (sw_graphics == c("ggplot", "base")[1]) {
    dat_all <-
      dplyr::bind_rows(
        tibble::tibble(
          val   = dat
        , group = "Data"
        )
      , tibble::tibble(
          val   = dat_sam_mean
        , group = "BS"
        )
      ) %>%
      dplyr::mutate(
        group = group %>% factor(levels = c("Data", "BS"))
      )

    p1 <- ggplot(dat_all %>% dplyr::filter(group == "Data"), aes(x = val))
    p1 <- p1 + theme_bw()
    p1 <- p1 + geom_histogram(aes(y = after_stat(density)), boundary = 0, bins = ceiling(log(n, base = 1.2)))
    p1 <- p1 + geom_density(alpha = 0.2, fill = "gray50", colour = "black", adjust = 2)
    p1 <- p1 + labs(
                  title = "Data with smoothed density curve"
                , x     = NULL
                , caption =
                    paste0(
                      "Data: n = ", n
                    , " ,  mean = ", signif(mean(dat), digits = 3)
                    , " ,  se = ", signif(stats::sd(dat)/sqrt(n), digits = 3)
                    )
                )

    p2 <- ggplot(dat_all %>% dplyr::filter(group == "BS"), aes(x = val))
    p2 <- p2 + theme_bw()
    p2 <- p2 + geom_histogram(aes(y = after_stat(density)), boundary = 0, bins = ceiling(log(N, base = 1.2)), alpha = 1/2)
    p2 <- p2 + stat_function(
                  fun = dnorm
                , args = list(mean = mean(dat_sam_mean), sd = sd(dat_sam_mean))
                , col = "red"
                , size = 2
                , alpha = 3/4
                )
    p2 <- p2 + geom_density(fill = NA, colour = "black", adjust = 2, size = 2, alpha = 0.5)
    p2 <- p2 + labs(
                  title = "Bootstrap sampling distribution of the mean"
                , x     = NULL
                , caption =
                    paste0(
                      "Black is smoothed density histogram.  Red is normal distribution."
                    , "\nN = ", N, " bootstrap resamples"
                    , " ,  mean = ", signif(obs_mean, 4)
                    , " ,  95% CI: ("
                    , signif(CI_limits[1], 4)
                    , ", "
                    , signif(CI_limits[2], 4)
                    , ")"
                    )
                )

    p_arranged <-
      cowplot::plot_grid(
        plotlist = list(p1, p2)
      , nrow = NULL
      , ncol = 1
      , labels = "auto"
      , rel_heights = c(1, 1.5)
      )

    if (sw_ggplot_print) {
      p_arranged %>% print()
    }

    return(p_arranged)
  } # ggplot

} # e_plot_bs_one_samp_dist
