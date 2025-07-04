#' Distribution tests
#'
#' @param x       single list of numeric values
#' @param dist    distribution
#'
#' @return out      list of tests and results
#' @import nortest
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr rename mutate select
#' @export
#'
#' @examples
#' rnorm(30) |> e_dist_test()
#' rexp(30) |> e_dist_test()
#'
e_dist_test <-
  function(
    x     = NULL
  , dist  = c("normal")[1]
  ) {

  out <- list()

  if (dist == "normal") {

    # Anderson-Darling test for normality
    out[[ "nortest__ad_test" ]] <-
      nortest::ad.test(x) |>
      unlist() |>
      tibble::as_tibble(
        rownames = "Var"
      ) |>
      tidyr::pivot_wider(
        names_from  = "Var"
      , values_from = "value"
      ) |>
      dplyr::rename(
        statistic = statistic.A
      ) |>
      dplyr::mutate(
        statistic = statistic |> as.numeric()
      , p.value   = p.value   |> as.numeric()
      , sig       = p.value   |> e_pval_stars()
      , dist      = "normal"
      , method_short = "AD norm test"
      ) |>
      dplyr::select(
        dist
      , method
      , method_short
      , statistic
      , p.value
      , sig
      )

    # Cramer-von Mises test for normality
    out[[ "nortest__cvm_test" ]] <-
      nortest::cvm.test(x) |>
      unlist() |>
      tibble::as_tibble(
        rownames = "Var"
      ) |>
      tidyr::pivot_wider(
        names_from  = "Var"
      , values_from = "value"
      ) |>
      dplyr::rename(
        statistic = statistic.W
      ) |>
      dplyr::mutate(
        statistic = statistic |> as.numeric()
      , p.value   = p.value   |> as.numeric()
      , sig       = p.value   |> e_pval_stars()
      , dist      = "normal"
      , method_short = "CvM norm test"
      ) |>
      dplyr::select(
        dist
      , method
      , method_short
      , statistic
      , p.value
      , sig
      )

    # Lilliefors (Kolmogorov-Smirnov) test for normality
    out[[ "nortest__ks_test" ]] <-
      nortest::lillie.test(x) |>
      unlist() |>
      tibble::as_tibble(
        rownames = "Var"
      ) |>
      tidyr::pivot_wider(
        names_from  = "Var"
      , values_from = "value"
      ) |>
      dplyr::rename(
        statistic = statistic.D
      ) |>
      dplyr::mutate(
        statistic = statistic |> as.numeric()
      , p.value   = p.value   |> as.numeric()
      , sig       = p.value   |> e_pval_stars()
      , dist      = "normal"
      , method_short = "KS norm test"
      ) |>
      dplyr::select(
        dist
      , method
      , method_short
      , statistic
      , p.value
      , sig
      )

    # Pearson chi-square test for normality
    out[[ "nortest__PX2_test" ]] <-
      nortest::pearson.test(x) |>
      unlist() |>
      tibble::as_tibble(
        rownames = "Var"
      ) |>
      tidyr::pivot_wider(
        names_from  = "Var"
      , values_from = "value"
      ) |>
      dplyr::rename(
        statistic = statistic.P
      ) |>
      dplyr::mutate(
        statistic = statistic |> as.numeric()
      , p.value   = p.value   |> as.numeric()
      , sig       = p.value   |> e_pval_stars()
      , dist      = "normal"
      , method_short = "PX2 norm test"
      ) |>
      dplyr::select(
        dist
      , method
      , method_short
      , statistic
      , p.value
      , sig
      )


    # Shapiro-Francia test for normality
    out[[ "nortest__SF_test" ]] <-
      nortest::sf.test(x) |>
      unlist() |>
      tibble::as_tibble(
        rownames = "Var"
      ) |>
      tidyr::pivot_wider(
        names_from  = "Var"
      , values_from = "value"
      ) |>
      dplyr::rename(
        statistic = statistic.W
      ) |>
      dplyr::mutate(
        statistic = statistic |> as.numeric()
      , p.value   = p.value   |> as.numeric()
      , sig       = p.value   |> e_pval_stars()
      , dist      = "normal"
      , method_short = "SF norm test"
      ) |>
      dplyr::select(
        dist
      , method
      , method_short
      , statistic
      , p.value
      , sig
      )

    out <-
      out |>
      dplyr::bind_rows()

    return(out)

  }

} # e_dist_test
