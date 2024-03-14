#' Time series lowpass filter using iFFT harmonics
#'
#' @param x                 List of numbers
#' @param n_harmonics       Number of harmonics
#' @param upsample_multiple To upscale number of points.
#'
#' @return x                If \code{upsample_multiple = 1}, then a list of numbers.  If \code{upsample_multiple > 1}, then a dataframe with time points.
#' @import stats
#' @export
#'
#' @examples
#' dat <- log(AirPassengers) |> as.numeric()
#' dat2 <- e_ts_lowpass_filter_ifft_harmonics(x = dat, n_harmonics = 12)
#' dat3 <- e_ts_lowpass_filter_ifft_harmonics(x = dat, n_harmonics = 24)
#' plot(dat |> scale(), type = "l")
#' points(dat2 |> scale(), type = "l", col = "red")
#' points(dat3 |> scale(), type = "l", col = "blue")
#'
e_ts_lowpass_filter_ifft_harmonics <-
  function(
    x                 = NULL
  , n_harmonics       = NULL
  , upsample_multiple = 1

  ) {
  # https://stackoverflow.com/questions/41435777/perform-fourier-analysis-to-a-time-series-in-r/41465250

  # The direct transformation
  # The first frequency is DC, the rest are duplicated
  dff <- stats::fft(x)

  # The time
  #t_time <- seq(from = 1, to = length(x))

  # Upsampled time (optional)
  t_time_upsample <-
    seq(
      from  = 1
    , to    = length(x) + 1 - 1 / upsample_multiple
    , by    = 1 / upsample_multiple
    )

  # New spectrum
  ndff <- array(data = 0, dim = c(length(t_time_upsample), 1))

  ndff[1] <- dff[1] # Always, it's the DC component

  if(n_harmonics != 0){
    # Positive frequencies
    ndff[2:(n_harmonics + 1)] <- dff[2:(n_harmonics + 1)]
    # Negative frequencies
    ndff[length(ndff):(length(ndff) - n_harmonics + 1)] <- dff[length(x):(length(x) - n_harmonics + 1)]
  }

  # The inverses
  indff <- stats::fft(ndff/73, inverse = TRUE)
  idff  <- stats::fft( dff/73, inverse = TRUE)

  #if(plot){
  #  if(!add){
  #    plot(x = t_time, y = x, pch = 16L, xlab = "Time", ylab = "Measurement",
  #      main = ifelse(is.null(main), paste(n_harmonics, "harmonics"), main))
  #    lines(y = Mod(idff), x = t_time, col = adjustcolor(1L, alpha = 0.5))
  #  }
  #  lines(y = Mod(indff), x = t_time_upsample, ...)
  #}

  if (upsample_multiple == 1) {
    out <- Mod(indff) |> as.numeric()
  } else {
    out <- data.frame(time = t_time_upsample, y = Mod(indff))
  }

  return(out)

} # e_ts_lowpass_filter_ifft_harmonics

