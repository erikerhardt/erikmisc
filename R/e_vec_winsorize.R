#' Winsorize a numeric list or vector by replacing extreme values with the less extreme values
#'
#' @param x        numeric list or vector
#' @param fraction fraction between 0 and 0.5 from both sides to Winsorize
#' @param type     type from `quantile()`
#'
#' @return same numeric vector with extreme values replaced
#' @export
#'
#' @examples
#'
#' x <- 1:100
#' y <- e_vec_winsorize(x, fraction = 0.25)
#' plot(x, y)
e_vec_winsorize <-
  function(
    x
  , fraction = 0.05
  , type = 1
  ) {

  if(length(fraction) != 1 || fraction < 0 || fraction > 0.5) {
    stop("e_vec_winsorize: fraction, bad value")
  }

  # determine limits
  lim <- quantile(x, probs=c(fraction, 1 - fraction), type = type)
  # replace more extreme values with limits
  x[ x < lim[1] ] <- lim[1]
  x[ x > lim[2] ] <- lim[2]

  return(x)
}
