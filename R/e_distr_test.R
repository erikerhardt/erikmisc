#' Distribution tests
#'
#' @param x       single list of numeric values
#' @param distr   distribution
#'
#' @return out      list of tests and results
#' @import nortest
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr rename mutate select
#' @export
#'
#' @examples
#' rnorm(30) |> e_distr_test()
#' rexp(30) |> e_distr_test()
#'
e_distr_test <-
  function(
    x     = NULL
  , distr  = c("normal")[1]
  ) {

  out <- list()

  if (distr == "normal") {

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
        statistic     = statistic |> as.numeric()
      , p.value       = p.value   |> as.numeric()
      , sig           = p.value   |> e_pval_stars()
      , distr          = "normal"
      , stat_label    = "A"
      , method_short  = "AD"
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
        statistic     = statistic |> as.numeric()
      , p.value       = p.value   |> as.numeric()
      , sig           = p.value   |> e_pval_stars()
      , distr          = "normal"
      , stat_label    = "W"
      , method_short  = "CvM"
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
        statistic     = statistic |> as.numeric()
      , p.value       = p.value   |> as.numeric()
      , sig           = p.value   |> e_pval_stars()
      , distr          = "normal"
      , stat_label    = "D"
      , method_short  = "KS"
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
        statistic     = statistic |> as.numeric()
      , p.value       = p.value   |> as.numeric()
      , sig           = p.value   |> e_pval_stars()
      , distr          = "normal"
      , stat_label    = "P"
      , method_short  = "PX2"
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
        statistic     = statistic |> as.numeric()
      , p.value       = p.value   |> as.numeric()
      , sig           = p.value   |> e_pval_stars()
      , distr          = "normal"
      , stat_label    = "W"
      , method_short  = "SF"
      )

    out <-
      out |>
      dplyr::bind_rows()

    out <-
      out |>
      dplyr::mutate(
        #text = paste0(method_short, ": ", stat_label, " = ", signif(statistic, 3), ", p-val = ", round(p.value, 4), " ", sig)
        text = paste0(method_short, " (p = ", sprintf("%04.4f", p.value), ") ", sig)
      )

    out <-
      out |>
      dplyr::bind_rows() |>
      dplyr::select(
        distr
      , text
      , method_short
      , p.value
      , sig
      , stat_label
      , statistic
      , method
      )

    return(out)

  } # normal

} # e_distr_test
