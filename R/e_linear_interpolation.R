#' Linear interpolation and extrapolation of y conditional on spacing of x
#'
#' If all y are NA, then return y.
#' If all non-NA y values are equal, then impute all y values equal.
#' Otherwise, find at least two non-NA missing values to impute internal, starting, or ending NAs, in that order.
#'
#' @param y response values to impute via linear interpolation
#' @param x spacing between y values
#' @param sw_which switch to indicate which missing values to replace: "all", "internal", "head" (starting), or "tail" (ending)
#' @param sw_extrapolation switch to indicate how to extrapolate to head and tail missing values.  "martingale" sets NAs to closest non-NA value, while "linear" performs a linear extrapolation from the two closest non-NA values.
#'
#' @return y Completed list of numbers
#' @export
#'
#' @examples
#' e_linear_interpolation(y = c(NA, NA, NA, NA, NA))
#' e_linear_interpolation(y = c(NA, NA, NA, 4, NA))
#' e_linear_interpolation(y = c(1, NA, 3, NA, 5))
#' e_linear_interpolation(y = c(NA, NA, NA, 4, 5), x = c(1, 2, 4, 8, 16))
#' e_linear_interpolation(y = c(NA, NA, 3, 4, NA), x = c(1, 2, 4, 8, 16))
#' e_linear_interpolation(y = c("a", NA, 3, 4, NA)) # warning for not numeric

e_linear_interpolation <-
  function(
    y
  , x = NULL
  , sw_which = c("all", "internal", "head", "tail")[1]
  , sw_extrapolation = c("martingale", "linear")[1]
  ) {

  # Parameter checking
  `%notin%` <- Negate(`%in%`)
  if (sw_which %notin% c("all", "internal", "head", "tail")) {
    warning("sw_which not an admissible option.  Returning y as is.")
    return(y)
  }
  if (sw_extrapolation %notin% c("martingale", "linear")) {
    warning("sw_extrapolation not an admissible option.  Returning y as is.")
    return(y)
  }

  # if x is NULL, then assume uniform spacing
  if (is.null(x)) {
    x = seq_along(y)
  }

  if(any(!is.numeric(y), !is.numeric(x))) {
    warning("Either y or x is not numeric, can not impute.  Returning y as is.")
    return(y)
  }

  ## special cases missing values
  ind_NA    <- which( is.na(y))
  ind_nonNA <- which(!is.na(y))

  # if no values are NA, then exit
  if (length(ind_nonNA) == length(y)) {
    return(y)
  }

  # if all values are NA, then exit
  if (length(ind_NA) == length(y)) {
    return(y)
  }

  # if all non-NAs are equal, then set all NAs to same value
  if (all(diff(y[ind_nonNA]) == 0)) {
    y[ind_NA] <- y[ind_nonNA[1]]
    return(y)
  }


  ## internal missing values

  if(sw_which %in% c("all", "internal")) {
    ind_NA    <- which( is.na(y))
    ind_nonNA <- which(!is.na(y))

    # internal missing values
    if (any(diff(ind_nonNA) > 1)) {
      list_first_ind_of_pairs <- which(diff(ind_nonNA) > 1)

      for (i_list in list_first_ind_of_pairs) {
        ## i_list = list_first_ind_of_pairs[1]

        ind_first <- ind_nonNA[i_list]
        ind_last  <- ind_nonNA[i_list + 1]

        dat <-
          data.frame(
            y = y[ind_first:ind_last]
          , x = x[ind_first:ind_last]
          )

        fit_lm <-
          lm(
            formula = y ~ x
          , data    = dat
          )

        pred_lm <-
          predict(
            object  = fit_lm
          , newdata = dat
          )

        y[ind_first:ind_last] <-
          as.numeric(pred_lm)
      }
    }
  } # sw_which


  ## head missing values
  if(sw_which %in% c("all", "head")) {
    # update missing since internal NA were completed
    ind_NA    <- which( is.na(y))
    ind_nonNA <- which(!is.na(y))

    # starting missing values
    if (length(ind_NA) > 0) {
      if (ind_NA[1] == 1) {
        if (sw_extrapolation == "martingale") {
          ind_first <- ind_nonNA[1]
          y[1:ind_first] <- y[ind_first]
        }

        if (sw_extrapolation == "linear") {
          ind_second <- ind_nonNA[2]

          dat <-
            data.frame(
              y = y[1:ind_second]
            , x = x[1:ind_second]
            )

          fit_lm <-
            lm(
              formula = y ~ x
            , data    = dat
            )

          pred_lm <-
            predict(
              object  = fit_lm
            , newdata = dat
            )

          y[1:ind_second] <-
            as.numeric(pred_lm)
        }
      }
    }
  } # sw_which


  ## tail missing values
  if(sw_which %in% c("all", "tail")) {
    # update missing since internal NA were completed
    ind_NA    <- which( is.na(y))
    ind_nonNA <- which(!is.na(y))

    # ending missing values
    if (length(ind_NA) > 0) {
      if (ind_NA[length(ind_NA)] == length(y)) {
        if (sw_extrapolation == "martingale") {
          ind_first <- ind_nonNA[length(ind_nonNA)]
          y[ind_first:length(y)] <- y[ind_first]
        }

        if (sw_extrapolation == "linear") {
          ind_second <- ind_nonNA[length(ind_nonNA) - 1]

          dat <-
            data.frame(
              y = y[ind_second:length(y)]
            , x = x[ind_second:length(y)]
            )

          fit_lm <-
            lm(
              formula = y ~ x
            , data    = dat
            )

          pred_lm <-
            predict(
              object  = fit_lm
            , newdata = dat
            )

          y[ind_second:length(y)] <-
            as.numeric(pred_lm)
        }
      }
    }
  } # sw_which

  return(y)
} # e_linear_interpolation
