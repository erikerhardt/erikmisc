#' Log2 nonnegative numbers after offsetting 0 by minimum non-zero value.
#'
#' Default \code{min_add} values:
#'   If specified, use that.
#'   All positive, and the minimum value is less than 1, add the minimum value, else add 0.
#'   Min is 0, add the minimum non-0 value.
#'   Min is negative and max is positive, add the minimum non-0 value (so minimum is now the previous non-0 value).
#'   Max is negative, add the minimum value plus 1 so \code{log2(x)=0} is the minimum value.
#'
#' @param x         numeric vector
#' @param min_add   value to add to x before taking log, if not specified and min(x) <= 0, then this is determined automatically
#' @param base_log  base for log
#' @param sw_symmetric overrides \code{min_add} and chooses constant to give most symmetric distribution after \code{log(x+c)}
#'
#' @return numeric vector with attributes
#' @export
#'
#' @examples
#'
#' f_print_example <- function(x, x2 = e_log_shift(x)) {
#'   print(x)
#'   print(x2)
#'   par(mfrow = c(1, 2))
#'   hist(x)
#'   hist(x2)
#'   par(mfrow = c(1, 1))
#'   invisible(NULL)
#' }
#'
#'
#' e_log_shift(x = c(0, 1, 2, 3))
#' e_log_shift(x = c(0, 10, 100, 1000), base = 10)
#' e_log_shift(x = c(-4, -2, 0, 2, 4))
#'
#' # Symmetric, positive with right skew
#' x <- rgamma(100, 1, 1) |> sort()
#' x_log <- e_log_shift(x, sw_symmetric = TRUE)
#'   print(x)
#'   print(x_log)
#'   par(mfrow = c(1, 3))
#'   hist(x)
#'   hist(x_log)
#'   dat_skew <- tibble::tibble(min_add = seq(0, 1, by = 0.01), skewness = NA)
#'   for (i_row in seq_len(nrow(dat_skew))) {
#'     ## i_row = 1
#'     dat_skew$skewness[i_row] <-
#'       e_log_shift(x, min_add = dat_skew$min_add[i_row]) |>
#'       moments::skewness()
#'   } # i_row
#'   plot(dat_skew$min_add, dat_skew$skewness, type = "l")
#'   abline(h = 0, col = "gray50")
#'   abline(v = attr(x_log, "e_log_shift")["min_add"], col = "gray50")
#'   par(mfrow = c(1, 1))
#'   print(c(skew_x = moments::skewness(x), skew_x_log = moments::skewness(x_log)))
#'
#' # Symmetric, positive with right skew, less than 1
#' x <- rgamma(100, 1, 1) |> sort()
#' x <- x / max(x + 1)
#' x_log <- e_log_shift(x, sw_symmetric = TRUE)
#'   print(x)
#'   print(x_log)
#'   par(mfrow = c(1, 2))
#'   hist(x)
#'   hist(x_log)
#'   par(mfrow = c(1, 1))
#'   print(c(skew_x = moments::skewness(x), skew_x_log = moments::skewness(x_log)))
#'
#' # Symmetric, negative with left skew
#' x <- -rgamma(100, 1, 1) |> sort()
#' x_log <- e_log_shift(x, sw_symmetric = TRUE)
#'   print(x)
#'   print(x_log)
#'   par(mfrow = c(1, 2))
#'   hist(x)
#'   hist(x_log)
#'   par(mfrow = c(1, 1))
#'   print(c(skew_x = moments::skewness(x), skew_x_log = moments::skewness(x_log)))
#'
#' # Symmetric, positive with left skew
#' x <- -rgamma(100, 1, 1) |> sort()
#' x <- x + min(x)
#' x_log <- e_log_shift(x, sw_symmetric = TRUE)
#'   print(x)
#'   print(x_log)
#'   par(mfrow = c(1, 2))
#'   hist(x)
#'   hist(x_log)
#'   par(mfrow = c(1, 1))
#'   print(c(skew_x = moments::skewness(x), skew_x_log = moments::skewness(x_log)))
e_log_shift <-
  function(
    x
  , min_add       = NULL
  , base_log      = 2
  , sw_symmetric  = c(FALSE, TRUE)[1]
  ) {

  # keep attributes
  x_attr <- attributes(x)

  # if passed as a column from a tibble, need to make it a list
  x <- as.numeric(unlist(x))

  if(all(is.na(x))) {
    warning("erikmisc::e_log_shift, all values NA")
    return(x)
  }

  # constant for most symmetric distribution
  if (sw_symmetric) {
    f_log_optim <-
      function(
        min_add       = 0
      , f_in_x        = x
      , f_in_base_log = base_log
      ) {

      x_new <- x + min_add

      x_log <-
        log(
          x_new
        , base = base_log
        )

      val_skewness <-
        moments::skewness(
          x_log
        , na.rm = TRUE
        )

      if (any(x_new < 0)) {
        return(1e3 * abs(val_skewness))
      }

      return(abs(val_skewness))
    } # f_log_optim

    #out_optim <-
    #  optim(
    #    par     = min_add
    #  , fn      = f_log_optim
    #  , method  = "Brent"
    #  , lower   = -min(x, na.rm = TRUE) + 1e-12
    #  , upper   = +max(x, na.rm = TRUE)
    #  , f_in_x         = x
    #  , f_in_base_log  = base_log
    #  )

    #out_optim <-
    #  stats::uniroot(
    #    f       = f_log_optim
    #  #, par     = min_add
    #  , interval = c(lower   = -min(x, na.rm = TRUE) + 1e-12, upper   = +max(x, na.rm = TRUE) + 1e1)
    #  #, lower   = -min(x, na.rm = TRUE) + 1e-12
    #  #, upper   = +max(x, na.rm = TRUE) + 1e1
    #  , f_in_x         = x
    #  , f_in_base_log  = base_log
    #  )

    out_optim <-
      stats::optimize(
        f       = f_log_optim
      #, par     = min_add
      , interval =
          c(
            lower   = -min(abs(x), na.rm = TRUE) + .Machine$double.eps
          , upper   = +max(abs(x), na.rm = TRUE) + range(x, na.rm = TRUE) + 1e4
          )
      , f_in_x         = x
      , f_in_base_log  = base_log
      , tol         = .Machine$double.eps^0.5  # 0.25
      )


    #min_add <- out_optim$par
    min_add <- out_optim$minimum

    x_log <-
      log(
        x + min_add
      , base = base_log
      )

    if (abs(moments::skewness(x)) < abs(moments::skewness(x_log))) {
      message("erikmisc::e_log_shift, sw_symmetric=TRUE, skewness of x less than skewness of log(x)")
    }

    # keep attributes
    attributes(x_log) <- x_attr
    attr(x_log, "e_log_shift") <- c(min_add = min_add, base_log = base_log)

    return(x_log)
  } # if sw_symmetric

  # if specified min_add, then do it
  if(!is.null(min_add)) {
    x_log <-
      log(
        x + min_add
      , base = base_log
      )
    # keep attributes
    attributes(x_log) <- x_attr
    attr(x_log, "e_log_shift") <- c(min_add = min_add, base_log = base_log)

    return(x_log)
  }

  # all positive
  # if min(x) < 1, add x, else add 0
  if (min(x, na.rm = TRUE) > 0) {
    if (min(x, na.rm = TRUE) < 1) {
      min_add <- min(x, na.rm = TRUE)
    } else {
      min_add <- 0
    }
  }

  # min is 0
  # add the minimum non-zero value
  if (min(x, na.rm = TRUE) == 0) {
    min_add <- min(x[(x > 0)], na.rm = TRUE)
  }

  # min is negative, max is positive
  # add the minimum non-zero value minus the minimum value
  #   (make the minimum value equal the minimum non-zero value)
  if ((min(x, na.rm = TRUE) < 0) & (max(x, na.rm = TRUE) > 0)) {
    min_add <- min(x[(x > 0)], na.rm = TRUE) - min(x, na.rm = TRUE)
  }

  # max is negative
  # add the minimum value plus 1 so log2(x)=0 is the minimum value
  if (max(x, na.rm = TRUE) <= 0) {
    min_add <- -min(x[(x > 0)], na.rm = TRUE) + 1
  }

  x_log <-
    log(
      x + min_add
    , base = base_log
    )
  # keep attributes
  attributes(x_log) <- x_attr
  attr(x_log, "e_log_shift") <- c(min_add = min_add, base_log = base_log)

  return(x_log)
}
