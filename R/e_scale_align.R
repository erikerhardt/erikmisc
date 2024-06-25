#' Align the distribution of one empirical distribution to an empirical reference distribution
#'
#' @param var_to_scale       Variable to rescale
#' @param var_ref            Reference variable distribution
#' @param scale_method       Method of rescaling.  \code{"quantile"} with \code{c(0.1, 0.9)} is remarkably good.
#' @param quantiles_to_match For \code{"quantile"}, which quantiles to match.
#' @param sw_plot            T/F to plot Reference, Original, and Scaled variables.
#'
#' @return var_scaled        Scaled vector (NAs have been removed)
#' @import ggplot2
#' @import stats
#' @importFrom tibble tibble
#' @importFrom ggbeeswarm geom_beeswarm
#' @export
#'
#' @examples
#' set.seed(76543)
#' e_scale_align(
#'   var_to_scale        = c(NA, rgamma( 40, shape = 3, rate = 2) + 4, NA)
#' , var_ref             = rgamma(100, shape = 3, rate = 1)
#' , scale_method        = c("quantile", "zscore")[1]
#' , quantiles_to_match  = c(0.1, 0.9)
#' , sw_plot             = c(TRUE, FALSE)[1]
#' )
#' set.seed(76543)
#' e_scale_align(
#'   var_to_scale        = c(NA, rgamma( 40, shape = 3, rate = 2) + 4, NA)
#' , var_ref             = rgamma(100, shape = 3, rate = 1)
#' , scale_method        = c("quantile", "zscore")[2]
#' , sw_plot             = c(TRUE, FALSE)[1]
#' )
#' # Scale to c(0, 1) bounded
#' e_scale_align(
#'   var_to_scale        = rnorm(100) |> sort()
#' , var_ref             = c(0, 1)
#' , scale_method        = c("quantile", "zscore")[1]
#' , quantiles_to_match  = c(0, 1)
#' , sw_plot             = c(TRUE, FALSE)[2]
#' )
e_scale_align <-
  function(
    var_to_scale        = NULL
  , var_ref             = NULL
  , scale_method        = c("quantile", "zscore")[1]
  , quantiles_to_match  = c(0.1, 0.9)
  , sw_plot             = c(TRUE, FALSE)[2]
  ) {
  ### @param sw_mfrow
  ### sw_mfrow            = c(1, 1)

  # length and indexes of non-NA values
  n_var_to_scale  <- length(var_to_scale)
  ind_non_NA      <- which(!is.na(var_to_scale))

  # remove NAs
  var_to_scale  = var_to_scale |> na.omit() |> as.numeric()
  var_ref       = var_ref      |> na.omit() |> as.numeric()

  if (scale_method == "quantile") {
    #message("Applying Quantile matching scaling")
    quantile_scale <- quantile(var_to_scale, probs = quantiles_to_match, na.rm = TRUE)
    quantile_ref   <- quantile(var_ref     , probs = quantiles_to_match, na.rm = TRUE)

    # Calculate the quantile range the two distributions
    quantile_range_ref   <- diff(quantile_ref  )
    quantile_range_scale <- diff(quantile_scale)

    # Scale the difference by the ratio of the quantile ranges
    var_scaled <-
      (var_to_scale - quantile_scale[1]) *
      (quantile_range_ref / quantile_range_scale) + quantile_ref[1]

    #var_scaled <- as.numeric(unlist(var_scaled))
  }

  if (scale_method == "zscore") {
    #message("Applying Z-score scaling")
    m_sd_scale <- c(mean = mean(var_to_scale, na.rm = TRUE), sd = sd(var_to_scale, na.rm = TRUE))
    m_sd_ref   <- c(mean = mean(var_ref     , na.rm = TRUE), sd = sd(var_ref     , na.rm = TRUE))

    var_scaled <- ((var_to_scale - m_sd_scale["mean"]) / m_sd_scale["sd"]) * m_sd_ref["sd"] + m_sd_ref["mean"]

    #var_scaled <- as.numeric(val_scale_z)
  }

  # plot
  if (sw_plot) {
    dat_plot <-
      tibble::tibble(
        values =
          c(
            var_ref
          , var_to_scale
          , var_scaled
          )
      , labels =
          c(
            rep("Ref"   , length(var_ref      ))
          , rep("Org"   , length(var_to_scale ))
          , rep("Scaled", length(var_scaled   ))
          ) |>
          factor(levels = c("Ref", "Scaled", "Org"))
      )

    # Plot with facets stacked vertically
    p <- ggplot(dat_plot, aes(x = values, y = labels, fill = labels, color = labels))
    p <- p + theme_bw()
    p <- p + geom_boxplot(alpha = 0.5, width = 0.25)
    p <- p + ggbeeswarm::geom_beeswarm()
    p <- p + theme(legend.position = "none")
    #p <- p + guides(colour = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE))
    p <- p + labs(title     = "Scaled comparison")
    print(p)

    # op <- par(mfrow = sw_mfrow)
    # boxplot(
    #   values ~ labels
    # , data = dat_plot
    # , horizontal = TRUE
    # , main = "Scaled comparison"
    # )
    # beeswarm::beeswarm(
    #   values ~ labels
    # , data = dat_plot
    # , method = c("swarm", "compactswarm", "center", "hex", "square")[4]
    # , horizontal = TRUE
    # , add = TRUE
    # )
    # par(op)
  } # sw_plot

  # place scaled values into list of original length in non-NA positions
  var_return <- rep(NA, n_var_to_scale)
  var_return[ind_non_NA] <- var_scaled

  return(var_return)

} # e_scale_align
