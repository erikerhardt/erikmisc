#' Visual comparison of whether Bootstrap sampling distribution of the difference in means is close to Normal
#'
#' A function to compare the bootstrap sampling distribution
#'   of the difference of means from two samples with
#'   a normal distribution with mean and SEM estimated from the data
#'
#' @param dat1            a list of values from Sample 1
#' @param dat2            a list of values from Sample 2
#' @param N               number of bootstrap iterations
#' @param sw_graphics     use either ggplot or R base graphics
#' @param sw_ggplot_print if ggplot, print the plot or just return the grob
#' @param conf_level      0.95 for a 95% bootstrap CI
#'
#' @return \code{invisible(NULL)}
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
#' e_plot_bs_two_samp_diff_dist(dat1 = runif(15), dat2 = runif(15) - 2, sw_graphics = "base")
#' e_plot_bs_two_samp_diff_dist(dat1 = runif(15), dat2 = runif(15) - 2, sw_graphics = "ggplot")
e_plot_bs_two_samp_diff_dist <-
  function(
    dat1
  , dat2
  , N = 1e4
  , sw_graphics = c("ggplot", "base")[1]
  , sw_ggplot_print = c(TRUE, FALSE)[1]
  , conf_level = 0.95
  ) {

  n1 <- length(dat1)
  n2 <- length(dat2)
  # resample from data
  dat_sam1 <- matrix(sample(dat1, size = N * n1, replace = TRUE), ncol=N)
  dat_sam2 <- matrix(sample(dat2, size = N * n2, replace = TRUE), ncol=N)
  # calculate the means and take difference between populations
  dat_sam1_mean <- colMeans(dat_sam1)
  dat_sam2_mean <- colMeans(dat_sam2)
  dat_diff_mean <- dat_sam1_mean - dat_sam2_mean

  # obs mean
  obs_mean <- mean(dat1, na.rm = TRUE) - mean(dat2, na.rm = TRUE)
  # obs CI
  prob_CI <- c(lower = (1 - conf_level) / 2, upper = 1 - (1 - conf_level) / 2)
  ind_CI <- prob_CI * N
  ind_CI[1] <- floor(ind_CI[1])
    if (ind_CI[1] == 0) { ind_CI[1] <- 1 }
  ind_CI[2] <- ceiling(ind_CI[2])
  CI_limits <- sort(dat_diff_mean)[ind_CI]

  if (sw_graphics == c("ggplot", "base")[2]) {
    # save par() settings
    old_par <- graphics::par(no.readonly = TRUE)
    # make smaller margins
    graphics::par(mfrow=c(3,1), mar=c(3,2,2,1), oma=c(1,1,1,1))
    # Histogram overlaid with kernel density curve
    graphics::hist(dat1, freq = FALSE, breaks = ceiling(log(n1, base = 1.15))
        , main = paste("Data 1 with smoothed density curve", "\n"
                      , "n =", n1
                      , ", mean =", signif(mean(dat1), digits = 3)
                      , ", sd =", signif(stats::sd(dat1), digits = 3))
        , xlim = range(c(dat1, dat2)))
    graphics::points(density(dat1), type = "l")
    graphics::rug(dat1)

    graphics::hist(dat2, freq = FALSE, breaks = ceiling(log(n2, base = 1.15))
        , main = paste("Data 2 with smoothed density curve", "\n"
                      , "n =", n2
                      , ", mean =", signif(mean(dat2), digits = 3)
                      , ", sd =", signif(stats::sd(dat2), digits = 3))
        , xlim = range(c(dat1, dat2)))
    graphics::points(density(dat2), type = "l")
    graphics::rug(dat2)

    graphics::hist(dat_diff_mean, freq = FALSE, breaks = ceiling(log(N, base = 1.2))
        , main = paste("Bootstrap sampling distribution of the difference in means", "\n"
                     , "mean =", signif(mean(dat_diff_mean), digits = 5)
                     , ", se =", signif(stats::sd(dat_diff_mean), digits = 5)))
    # overlay a density curve for the sample means
    graphics::points(density(dat_diff_mean), type = "l")
    # overlay a normal distribution, bold and red
    x <- seq(min(dat_diff_mean), max(dat_diff_mean), length = 1000)
    graphics::points(x, stats::dnorm(x, mean = mean(dat_diff_mean), sd = stats::sd(dat_diff_mean))
         , type = "l", lwd = 2, col = "red")
    # place a rug of points under the plot
    graphics::rug(dat_diff_mean)
    # restore par() settings
    graphics::par(old_par)

    invisible(NULL)
  } # base

  if (sw_graphics == c("ggplot", "base")[1]) {
    dat_all <-
      dplyr::bind_rows(
        tibble::tibble(
          val   = dat1
        , group = "Data1"
        )
      , tibble::tibble(
          val   = dat2
        , group = "Data2"
        )
      , tibble::tibble(
          val   = dat_diff_mean
        , group = "BS"
        )
      ) %>%
      dplyr::mutate(
        group = group %>% factor(levels = c("Data1", "Data2", "BS"))
      )

    p1 <- ggplot(dat_all %>% dplyr::filter(group == "Data1"), aes(x = val))
    p1 <- p1 + theme_bw()
    p1 <- p1 + geom_histogram(aes(y = ..density..), boundary = 0, bins = ceiling(log(n1, base = 1.2)))
    p1 <- p1 + geom_density(alpha = 0.2, fill = "gray50", colour = "black", adjust = 2)
    p1 <- p1 + xlim(min(c(dat1, dat2), na.rm = TRUE), max(c(dat1, dat2), na.rm = TRUE))
    p1 <- p1 + labs(
                  title = "Data 1 with smoothed density curve"
                , x     = NULL
                , caption =
                    paste0(
                      "Data: n = ", n1
                    , " ,  mean = ", signif(mean(dat1), digits = 3)
                    , " ,  se = ", signif(stats::sd(dat1)/sqrt(n1), digits = 3)
                    )
                )

    p2 <- ggplot(dat_all %>% dplyr::filter(group == "Data2"), aes(x = val))
    p2 <- p2 + theme_bw()
    p2 <- p2 + geom_histogram(aes(y = ..density..), boundary = 0, bins = ceiling(log(n2, base = 1.2)))
    p2 <- p2 + geom_density(alpha = 0.2, fill = "gray50", colour = "black", adjust = 2)
    p2 <- p2 + xlim(min(c(dat1, dat2), na.rm = TRUE), max(c(dat1, dat2), na.rm = TRUE))
    p2 <- p2 + labs(
                  title = "Data 2 with smoothed density curve"
                , x     = NULL
                , caption =
                    paste0(
                      "Data: n = ", n2
                    , " ,  mean = ", signif(mean(dat2), digits = 3)
                    , " ,  se = ", signif(stats::sd(dat2)/sqrt(n2), digits = 3)
                    )
                )

    p3 <- ggplot(dat_all %>% dplyr::filter(group == "BS"), aes(x = val))
    p3 <- p3 + theme_bw()
    p3 <- p3 + geom_histogram(aes(y = ..density..), boundary = 0, bins = ceiling(log(N, base = 1.2)))
    p3 <- p3 + geom_density(fill = NA, colour = "black", adjust = 2, size = 2, alpha = 0.5)
    p3 <- p3 + stat_function(
                  fun = dnorm
                , args = list(mean = mean(dat_diff_mean), sd = sd(dat_diff_mean))
                , col = "red"
                , size = 2
                , alpha = 0.5
                )
    p3 <- p3 + labs(
                  title = "Bootstrap sampling distribution of the difference in means"
                , x     = NULL
                , caption =
                    paste0(
                      "Black is smoothed density histogram.  Red is normal distribution."
                    , "\nDiff = ", signif(obs_mean, 4)
                    , " ,  95% CI: ("
                    , signif(CI_limits[1], 4)
                    , ", "
                    , signif(CI_limits[2], 4)
                    , ")"
                    )
                )

    p_arranged <-
      cowplot::plot_grid(
        plotlist = list(p1, p2, p3)
      , nrow = NULL
      , ncol = 1
      , labels = "auto"
      , rel_heights = c(1, 1, 1.5)
      )

    if (sw_ggplot_print) {
      p_arranged %>% print()
    }

    return(p_arranged)
  } # ggplot

} # e_plot_bs_two_samp_diff_dist
