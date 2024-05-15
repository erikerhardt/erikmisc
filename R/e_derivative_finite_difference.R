#' Derivatives of time series using finite difference coefficients
#'
#' To approximate a derivative to an arbitrary order of accuracy, it is
#' possible to use the finite difference. A finite difference can be central,
#' forward or backward. \url{https://en.wikipedia.org/wiki/Finite_difference_coefficient}
#' Applies highest possible accuracy given the location in the vector;
#' end points have lower accuracy and use the 1-width forward/backward method.
#'
#' @param x             numeric vector
#' @param sw_derivative order of the derivative, 1 = slope, up to 6
#'
#' @return dx           derivative
#' @import dplyr
#' @importFrom tibble tibble
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @import datasets
#' @export
#'
#' @examples
#' dat <-
#'   tibble::tibble(
#'     x  = datasets::lh |> as.numeric()
#'   , dx = e_derivative_finite_difference(x)
#'   , i  = 1:length(x)
#'   )
#' dat |> print(n = 10)
#' # reshape to plot
#' dat_long <-
#'   dat |>
#'   tidyr::pivot_longer(
#'     cols = c(x, dx)
#'   ) |>
#'   dplyr::mutate(
#'     name = name |> factor(levels = c("x", "dx"))
#'   )
#' library(ggplot2)
#' p <- ggplot(dat_long, aes(x = i, y = value))
#' p <- p + theme_bw()
#' p <- p + geom_hline(aes(yintercept = 0), colour = "black"
#'                    , linetype = "solid", linewidth = 0.2, alpha = 0.3)
#' p <- p + geom_line()
#' p <- p + facet_grid(name ~ ., scales = "free_y", drop = TRUE)
#' p <- p + scale_x_continuous(breaks = seq(0, max(dat$i), by = 2))
#' print(p)
e_derivative_finite_difference <-
  function (
    x
  , sw_derivative = 1
  ) {
  # options for later
  sw_accuracy   = 8
  sw_method     = c("all", "central", "forward", "backward")[1]
  # "all" does central everywhere except the first and last point, where the "forward" and "backward" is performed

  if (length(x) == 0) {
    return(c())
  }
  if (!is.numeric(x)) {
    stop("Argument 'x' must be a number or a numeric vector.")
  }
  if (sw_derivative %notin% 1:6) {
    stop("The order of the derivative, 'derivative', can only be between 0 and 6.")
  }
  if (sw_derivative == 0) {
    return(x)
  }
  if (sw_method %notin% c("all", "central", "forward", "backward")) {
    stop("Unknown 'method'; use 'all', 'central', 'forward', or 'backward' instead.")
  }

  # initialize the derivative vector
  dx <- rep(NA, length(x))

  # https://en.wikipedia.org/wiki/Finite_difference_coefficient
  # This table contains the coefficients of the central differences,
  #   for several orders of accuracy and with uniform grid spacing.
  ## I added the n_width column
  tab_central_finite_difference <-
    tibble::tribble(
      ~derivative, ~accuracy, ~n_width, ~n5, ~n4, ~n3, ~n2, ~n1, ~zero, ~p1, ~p2, ~p3, ~p4, ~p5
    ,  1, 2, 1, 0, 0, 0, 0, -1/2, 0, 1/2, 0, 0, 0, 0
    ,  1, 4, 2, 0, 0, 0, 1/12, -2/3, 0, 2/3, -1/12, 0, 0, 0
    ,  1, 6, 3, 0, 0, -1/60, 3/20, -3/4, 0, 3/4, -3/20, 1/60, 0, 0
    ,  1, 8, 4, 0, 1/280, -4/105, 1/5, -4/5, 0, 4/5, -1/5, 4/105, -1/280, 0
    ,  2, 2, 1, 0, 0, 0, 0, 1, -2, 1, 0, 0, 0, 0
    ,  2, 4, 2, 0, 0, 0, 1/12, 4/3, -5/2, 4/3, -1/12, 0, 0, 0
    ,  2, 6, 3, 0, 0, 1/90, -3/20, 3/2, -49/18, 3/2, -3/20, 1/90, 0, 0
    ,  2, 8, 4, 0, -1/560, 8/315, -1/5, 8/5, -205/72, 8/5, -1/5, 8/315, -1/560, 0
    ,  3, 2, 2, 0, 0, 0, -1/2, 1, 0, -1, 1/2, 0, 0, 0
    ,  3, 4, 3, 0, 0, 1/8, -1, 13/8, 0, -13/8, 1, -1/8, 0, 0
    ,  3, 6, 4, 0, -7/240, 3/10, -169/120, 61/30, 0, -61/30, 169/120, -3/10, 7/240, 0
    ,  4, 2, 2, 0, 0, 0, 1, -4, 6, -4, 1, 0, 0, 0
    ,  4, 4, 3, 0, 0, -1/6, 2, -13/2, 28/3, -13/2, 2, -1/6, 0, 0
    ,  4, 6, 4, 0, 7/240, -2/5, 169/60, -122/15, 91/8, -122/15, 169/60, -2/5, 7/240, 0
    ,  5, 2, 3, 0, 0, -1/2, 2, -5/2, 0, 5/2, -2, 1/2, 0, 0
    ,  5, 4, 4, 0, 1/6, -3/2, 13/3, -29/6, 0, 29/6, -13/3, 3/2, -1/6, 0
    ,  5, 6, 5, -13/288, 19/36, -87/32, 13/2, -323/48, 0, 323/48, -13/2, 87/32, -19/36, 13/288
    ,  6, 2, 3, 0, 0, 1, -6, 15, -20, 15, -6, 1, 0, 0
    ,  6, 4, 4, 0, -1/4, 3, -13, 29, -75/2, 29, -13, 3, -1/4, 0
    ,  6, 6, 5, 13/240, -19/24, 87/16, -39/2, 323/8, -1023/20, 323/8, -39/2, 87/16, -19/24, 13/240
    ) |>
    dplyr::filter(
      derivative == sw_derivative
    )

  # https://en.wikipedia.org/wiki/Finite_difference_coefficient
  # This table contains the coefficients of the forward differences,
  #   for several orders of accuracy and with uniform grid spacing
  tab_forward_finite_difference <-
    tibble::tribble(
      ~derivative, ~accuracy, ~n_width, ~zero, ~p1, ~p2, ~p3, ~p4, ~p5, ~p6, ~p7, ~p8
    , 1, 1, 1, -1, 1, 0, 0, 0, 0, 0, 0, 0
    , 1, 2, 2, -3/2, 2, -1/2, 0, 0, 0, 0, 0, 0
    , 1, 3, 3, -11/6, 3, -3/2, 1/3, 0, 0, 0, 0, 0
    , 1, 4, 4, -25/12, 4, -3, 4/3, -1/4, 0, 0, 0, 0
    , 1, 5, 5, -137/60, 5, -5, 10/3, -5/4, 1/5, 0, 0, 0
    , 1, 6, 6, -49/20, 6, -15/2, 20/3, -15/4, 6/5, -1/6, 0, 0
    , 2, 1, 2, 1, -2, 1, 0, 0, 0, 0, 0, 0
    , 2, 2, 3, 2, -5, 4, -1, 0, 0, 0, 0, 0
    , 2, 3, 4, 35/12, -26/3, 19/2, -14/3, 11/12, 0, 0, 0, 0
    , 2, 4, 5, 15/4, -77/6, 107/6, -13, 61/12, -5/6, 0, 0, 0
    , 2, 5, 6, 203/45, -87/5, 117/4, -254/9, 33/2, -27/5, 137/180, 0, 0
    , 2, 6, 7, 469/90, -223/10, 879/20, -949/18, 41, -201/10, 1019/180, -7/10, 0
    , 3, 1, 3, -1, 3, -3, 1, 0, 0, 0, 0, 0
    , 3, 2, 4, -5/2, 9, -12, 7, -3/2, 0, 0, 0, 0
    , 3, 3, 5, -17/4, 71/4, -59/2, 49/2, -41/4, 7/4, 0, 0, 0
    , 3, 4, 6, -49/8, 29, -461/8, 62, -307/8, 13, -15/8, 0, 0
    , 3, 5, 7, -967/120, 638/15, -3929/40, 389/3, -2545/24, 268/5, -1849/120, 29/15, 0
    , 3, 6, 8, -801/80, 349/6, -18353/120, 2391/10, -1457/6, 4891/30, -561/8, 527/30, -469/240
    , 4, 1, 4, 1, -4, 6, -4, 1, 0, 0, 0, 0
    , 4, 2, 5, 3, -14, 26, -24, 11, -2, 0, 0, 0
    , 4, 3, 6, 35/6, -31, 137/2, -242/3, 107/2, -19, 17/6, 0, 0
    , 4, 4, 7, 28/3, -111/2, 142, -1219/6, 176, -185/2, 82/3, -7/2, 0
    , 4, 5, 8, 1069/80, -1316/15, 15289/60, -2144/5, 10993/24, -4772/15, 2803/20, -536/15, 967/240
    ) |>
    dplyr::filter(
      derivative == sw_derivative
    )

  # https://en.wikipedia.org/wiki/Finite_difference_coefficient
  # To get the coefficients of the backward approximations from those of the forward ones,
  #   give all odd derivatives listed in the table in the previous section the opposite sign,
  #   whereas for even derivatives the signs stay the same.


  if (sw_method %in% c("all", "central")) {

    beta_width <-
      tab_central_finite_difference |>
      dplyr::select(n5:p5) |>
      as.matrix() |>
      t()
    colnames(beta_width) <- paste0("w", tab_central_finite_difference$n_width)

    x_lag_lead <-
      matrix(
        c(
          dplyr::lag (x, n = 5, default = 0)
        , dplyr::lag (x, n = 4, default = 0)
        , dplyr::lag (x, n = 3, default = 0)
        , dplyr::lag (x, n = 2, default = 0)
        , dplyr::lag (x, n = 1, default = 0)
        , x
        , dplyr::lead(x, n = 1, default = 0)
        , dplyr::lead(x, n = 2, default = 0)
        , dplyr::lead(x, n = 3, default = 0)
        , dplyr::lead(x, n = 4, default = 0)
        , dplyr::lead(x, n = 5, default = 0)
        )
      , ncol = 11
      )

    # used to select the beta for maximum accuracy
    x_width <-
      pmin(
        seq(0, length(x) - 1, by = 1)
      , seq(length(x) - 1, 0, by = -1)
      , max(tab_central_finite_difference$n_width)
      )

    # derivative vector
    # given by dx = x_lag_lead' * beta

    # compute the middle

    for (i_dx in sort(unique(x_width))) {
      ## i_dx = 0
      ## i_dx = 1
      ## i_dx = 2
      ## i_dx = 3
      ## i_dx = 4

      name_beta_width <- paste0("w", i_dx)

      if (name_beta_width %notin% colnames(beta_width)) { next }

      x_ind <- which(x_width == i_dx)

      dx[x_ind] <-
        x_lag_lead[x_ind, ] %*%
        matrix(
          beta_width[, name_beta_width]
        , ncol = 1
        )

    } # i_dx

  } # central

  if (sw_method %in% c("all", "forward")) {
    if (sw_method == "forward") {
      stop("forward not implemented")
    }

    beta_width <-
      tab_forward_finite_difference |>
      dplyr::select(zero:p8) |>
      as.matrix() |>
      t()
    colnames(beta_width) <- paste0("w", tab_forward_finite_difference$n_width)

    x_lag_lead <-
      matrix(
        c(
          x[1:9]
        , dplyr::lead(x[1:9], n = 1, default = 0)
        , dplyr::lead(x[1:9], n = 2, default = 0)
        , dplyr::lead(x[1:9], n = 3, default = 0)
        , dplyr::lead(x[1:9], n = 4, default = 0)
        , dplyr::lead(x[1:9], n = 5, default = 0)
        , dplyr::lead(x[1:9], n = 6, default = 0)
        , dplyr::lead(x[1:9], n = 7, default = 0)
        , dplyr::lead(x[1:9], n = 8, default = 0)
        )
      , ncol = 9
      )

    # used to select the beta for maximum accuracy
    x_width <-
      pmin(
      #  seq(0, length(x) - 1, by = 1)
        seq(length(x) - 1, 0, by = -1)
      , max(tab_forward_finite_difference$n_width)
      , 1
      )

    # derivative vector
    # given by dx = x_lag_lead' * beta

    # compute the forward

    if (sw_method == "all") {

      name_beta_width <- paste0("w", x_width[1])

      dx[1] <-
        x_lag_lead[1, ] %*%
        matrix(
          beta_width[, name_beta_width]
        , ncol = 1
        )

    } # i_dx

  } # forward

  if (sw_method %in% c("all", "backward")) {
    if (sw_method == "backward") {
      stop("backward not implemented")
    }

    # https://en.wikipedia.org/wiki/Finite_difference_coefficient
    # To get the coefficients of the backward approximations from those of the forward ones,
    #   give all odd derivatives listed in the table in the previous section the opposite sign,
    #   whereas for even derivatives the signs stay the same. The following table illustrates this

    beta_width <-
      tab_forward_finite_difference |>
      dplyr::select(p8:zero) |>        # reversed order
      as.matrix() |>
      t()
    colnames(beta_width) <- paste0("w", tab_forward_finite_difference$n_width)

    # https://en.wikipedia.org/wiki/Finite_difference_coefficient
    # To get the coefficients of the backward approximations from those of the forward ones,
    #   give all odd derivatives listed in the table in the previous section the opposite sign,
    #   whereas for even derivatives the signs stay the same.
    if ((sw_derivative %% 2) == 1) {
      beta_width = -beta_width
    }

    x_lag_lead <-
      matrix(
        c(
          dplyr::lag (x[(length(x) - 8):length(x)], n = 8, default = 0)
        , dplyr::lag (x[(length(x) - 8):length(x)], n = 7, default = 0)
        , dplyr::lag (x[(length(x) - 8):length(x)], n = 6, default = 0)
        , dplyr::lag (x[(length(x) - 8):length(x)], n = 5, default = 0)
        , dplyr::lag (x[(length(x) - 8):length(x)], n = 4, default = 0)
        , dplyr::lag (x[(length(x) - 8):length(x)], n = 3, default = 0)
        , dplyr::lag (x[(length(x) - 8):length(x)], n = 2, default = 0)
        , dplyr::lag (x[(length(x) - 8):length(x)], n = 1, default = 0)
        , x[(length(x) - 8):length(x)]
        )
      , ncol = 9
      )

    # used to select the beta for maximum accuracy
    x_width <-
      pmin(
        seq(0, length(x) - 1, by = 1)
      #  seq(length(x) - 1, 0, by = -1)
      , max(tab_forward_finite_difference$n_width)
      , 1
      )

    # derivative vector
    # given by dx = x_lag_lead' * beta

    # compute the backward

    if (sw_method == "all") {

      name_beta_width <- paste0("w", x_width[length(x)])

      dx[length(x)] <-
        x_lag_lead[nrow(x_lag_lead), ] %*%
        matrix(
          beta_width[, name_beta_width]
        , ncol = 1
        )

    } # i_dx

  } # backward

  return(dx)
} # e_derivative_finite_difference
