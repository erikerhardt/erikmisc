#' Time series bootstrap scramble phase
#'
#' @param x         time series object
#'
#' @return x        time series, bootstrapped
#' @export
#'
#' @examples
#' e_tc_bootstrap_phase(
#'    x = datasets::AirPassengers
#'  )
#'
e_tc_bootstrap_phase <-
  function(
    x = NULL
  ) {
                                          ## MATLAB equivalent steps
  # fft amplitude and phase               # function yboot = bootstrap_phase(y),
  x_fft       <- fft(x)                   # Y      = fft(y);
  x_fft_amp   <- abs(x_fft)               # Yamp   = abs(Y);
  x_fft_phase <- Arg(x_fft)               # Yphase = angle(Y);
  # permute phase                         #
  ind <- sample.int(length(x_fft_phase))  # rp     = randperm(length(Yphase));
  # inverse fft keeping Real part         # yboot  = real(ifft(Yamp.*exp(i*(Yphase(rp)))));
  x_boot <- Re(fft(x_fft_amp * exp(complex(imaginary = 1) * x_fft_phase[ind]), inverse = TRUE))

  ## XXX Scale is very different in result

  return(x_boot)
} # e_tc_bootstrap_phase

