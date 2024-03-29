#' Multiple regression power analysis
#'
#' @param dat           observed effect size data set
#' @param formula_full  observed effect size full model formula
#' @param formula_red   observed effect size reduced model formula
#' @param n_total       a total sample size value or list of values
#' @param n_groups      number of groups for degrees-of-freedom adjustment for Cohen effect sizes
#' @param sig_level     Type-I error rate
#' @param weights       observed effect size model fit, if it should be weighted regression
#' @param sw_print      print results
#' @param sw_plot       create plot
#' @param n_plot_ref    a sample size reference line for the plot; if null, then uses size of data, otherwise uses median of n_total
#'
#' @return list with both table and plots of power analysis results
#' @importFrom pwr pwr.f2.test
#' @importFrom pwr cohen.ES
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr drop_na
#' @importFrom scales percent
#' @import dplyr
#' @import ggplot2
#' @export
#'
#' @examples
#'
#' # without data, single n
#' n_total  <- 100
#' n_groups <- 3
#' out <-
#'   e_lm_power_ORIGINAL(
#'     dat           = NULL
#'   , formula_full  = NULL
#'   , formula_red   = NULL
#'   , n_total       = n_total
#'   , n_groups      = n_groups
#'   , sig_level     = 0.05
#'   , weights       = NULL
#'   , sw_print      = TRUE
#'   , sw_plot       = TRUE
#'   , n_plot_ref    = NULL
#'   )
#'
#' # without data, sequence of n for power curve
#' n_total <- seq(10, 300, by = 5)
#' n_groups <- 3
#' out <-
#'   e_lm_power_ORIGINAL(
#'     dat           = NULL
#'   , formula_full  = NULL
#'   , formula_red   = NULL
#'   , n_total       = n_total
#'   , n_groups      = n_groups
#'   , sig_level     = 0.05
#'   , weights       = NULL
#'   , sw_print      = TRUE
#'   , sw_plot       = TRUE
#'   , n_plot_ref    = NULL
#'   )
#'
#'
#' # with data
#' data(mtcars, package="datasets")
#' str(mtcars)
#'
#' yvar      <- "mpg"
#' xvar_full <- c("cyl", "disp", "hp", "drat", "wt", "qsec")
#' xvar_red  <- c(               "hp", "drat", "wt", "qsec")
#'
#' formula_full <-
#'   as.formula(
#'     paste0(
#'       yvar
#'     , " ~ "
#'     , paste(
#'         xvar_full
#'       , collapse= "+"
#'       )
#'     )
#'   )
#'
#' formula_red <-
#'   as.formula(
#'     paste0(
#'       yvar
#'     , " ~ "
#'     , paste(
#'         xvar_red
#'       , collapse= "+"
#'       )
#'     )
#'   )
#'
#'
#' # with data, single n
#' n_total  <- 100
#' n_groups <- 3
#' out <-
#'   e_lm_power_ORIGINAL(
#'     dat           = datasets::mtcars
#'   , formula_full  = formula_full
#'   , formula_red   = formula_red
#'   , n_total       = n_total
#'   , n_groups      = n_groups
#'   , sig_level     = 0.05
#'   , weights       = NULL
#'   , sw_print      = TRUE
#'   , sw_plot       = TRUE
#'   , n_plot_ref    = NULL
#'   )
#'
#' # without data, sequence of n for power curve
#' n_total <- seq(10, 300, by = 5)
#' n_groups <- 3
#' out <-
#'   e_lm_power_ORIGINAL(
#'     dat           = datasets::mtcars
#'   , formula_full  = formula_full
#'   , formula_red   = formula_red
#'   , n_total       = n_total
#'   , n_groups      = n_groups
#'   , sig_level     = 0.05
#'   , weights       = NULL
#'   , sw_print      = TRUE
#'   , sw_plot       = TRUE
#'   , n_plot_ref    = 100
#'   )
#'
#' ### RMarkdown results reporting
#' # The results above indicate the following.
#' #
#' # 1. With the number of observations
#' #      $n = `r out[["tab_power"]] |> dplyr::filter(n_total == 100) |> dplyr::pull(n_total)`$
#' #    and the number of groups
#' #      $k = `r out[["tab_power"]] |> dplyr::filter(n_total == 100) |> dplyr::pull(n_groups)`$
#' # 2. Observed (preliminary) power:
#' #     * `r out[["tab_power"]] |> dplyr::filter(n_total == 100) |>
#' #            dplyr::pull(obs_power         ) |> signif(digits = 2)`.
#' # 3. Cohen small, medium, and large power:
#' #     * `r out[["tab_power"]] |> dplyr::filter(n_total == 100) |>
#' #            dplyr::pull(Cohen_small_power ) |> signif(digits = 2)`,
#' #     * `r out[["tab_power"]] |> dplyr::filter(n_total == 100) |>
#' #            dplyr::pull(Cohen_medium_power) |> signif(digits = 2)`, and
#' #     * `r out[["tab_power"]] |> dplyr::filter(n_total == 100) |>
#' #            dplyr::pull(Cohen_large_power ) |> signif(digits = 2)`.
e_lm_power_ORIGINAL <-
  function(
    dat           = NULL
  , formula_full  = NULL
  , formula_red   = NULL
  , n_total
  , n_groups
  , sig_level     = 0.05
  , weights       = NULL
  , sw_print      = TRUE
  , sw_plot       = TRUE
  , n_plot_ref    = NULL
  ) {

  if (!is.null(dat)) {

    if(!is.null(weights)) {
      lm_summary_AB <- lm(formula_full, data = dat, weights = weights)
      lm_summary_A  <- lm(formula_red , data = dat, weights = weights)
    } else {
      lm_summary_AB <- lm(formula_full, data = dat)
      lm_summary_A  <- lm(formula_red , data = dat)
    }

    if(sw_print) {
      cat("Full Model ========================================================\n")
      print(formula_full)
      print(summary(lm_summary_AB))
      cat("Reduced Model =====================================================\n")
      print(formula_red)
      print(summary(lm_summary_A ))
    }


    ## power analysis
    # http://www.statmethods.net/stats/power.html
    #library(pwr)

    # observed effect size and df
    # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3328081/
    #   https://dx.doi.org/10.3389%2Ffpsyg.2012.00111
    #   Equation 2
    f2 <- (summary(lm_summary_AB)$r.squared - summary(lm_summary_A)$r.squared) /
          (1 - summary(lm_summary_AB)$r.squared)
    # f2
    df_full  <- summary(lm_summary_AB)$fstatistic[c("numdf", "dendf")]
    df_red   <- summary(lm_summary_A )$fstatistic[c("numdf", "dendf")]

    n_total <- c(n_total, nrow(dat))

    if (is.null(n_plot_ref)) {
      message("Setting n_plot_ref = the number of observations in the data for reference")
      n_plot_ref <- nrow(dat)
    }

  }

  n_total <- c(n_total, n_plot_ref) |> unique() |> sort()

  if (length(n_total) == 1) {
    message("Setting n_plot_ref = n_total for reference")
    n_plot_ref <- n_total
  }

  if (is.null(n_plot_ref)) {
    message("Setting n_plot_ref = median(n_total) for reference")
    n_plot_ref <- quantile(n_total, probs = 0.5, type = 1)
  }


  sw_print_message <- TRUE
  tab_power <- list()
  for (i_n_total in 1:length(n_total)) {

    # degrees of freedom
    if (!is.null(dat)) {
      df_f2    <- c(df_red[2] - df_full[2], n_total[i_n_total] - df_red[1] - n_groups)  # n_groups includes the -1
    } else {
      df_f2    <- c(n_groups - 1, n_total[i_n_total] - n_groups)
    }


    # Observed power with observed effect size given our data
    if (!is.null(dat)) {
      pwr_summary_dat <-
        pwr::pwr.f2.test(
          u         = df_f2[1]  # numerator df
        , v         = df_f2[2]  # denominator df
        , f2        = f2        # observed effect size
        , sig.level = sig_level # Type-I error rate
        , power     = NULL      # leaving blank to calculate the power
        )
    }

    # Cohen small, medium, and large effect sizes
    #cohen.ES(test = "f2", size = "small" )$effect.size
    #cohen.ES(test = "f2", size = "medium")$effect.size
    #cohen.ES(test = "f2", size = "large" )$effect.size
    pwr_summary_s <-
      pwr::pwr.f2.test(
        u         = df_f2[1]  # numerator df
      , v         = df_f2[2]  # denominator df
      , f2        = pwr::cohen.ES(test = "f2", size = "small")$effect.size
      , sig.level = sig_level # Type-I error rate
      , power     = NULL      # leaving blank to calculate the power
      )

    pwr_summary_m <-
      pwr::pwr.f2.test(
        u         = df_f2[1]  # numerator df
      , v         = df_f2[2]  # denominator df
      , f2        = pwr::cohen.ES(test = "f2", size = "medium")$effect.size
      , sig.level = sig_level # Type-I error rate
      , power     = NULL      # leaving blank to calculate the power
      )

    pwr_summary_l <-
      pwr::pwr.f2.test(
        u         = df_f2[1]  # numerator df
      , v         = df_f2[2]  # denominator df
      , f2        = pwr::cohen.ES(test = "f2", size = "large")$effect.size
      , sig.level = sig_level # Type-I error rate
      , power     = NULL      # leaving blank to calculate the power
      )

    if(sw_print) {
      if(length(n_total) == 1) {
        if (!is.null(dat)) {
          cat("Observed effect size and power ====================================\n")
          cat("  Observed ---------------\n")
          print(pwr_summary_dat)
        }
        cat("Cohen reference effect size and power =============================\n")
        cat("  Small ---------------\n")
        print(pwr_summary_s)
        cat("  Medium --------------\n")
        print(pwr_summary_m)
        cat("  Large ---------------\n")
        print(pwr_summary_l)
      } else {
        if(sw_print_message) {
          warning("e_lm_power_ORIGINAL, not printing results when n_total > 1")
          sw_print_message <- FALSE
        }
      }
    }

    tab_power[[i_n_total]] <-
      tibble::tibble(
        n_total                   = n_total[i_n_total]
      , n_groups                  = n_groups
      , df_num                    = pwr_summary_s  $u
      , df_den                    = pwr_summary_s  $v
      , sig_level                 = pwr_summary_s  $sig.level
      , method                    = pwr_summary_s  $method
      , Cohen_small_effect_size   = pwr_summary_s  $f2
      , Cohen_small_power         = pwr_summary_s  $power
      , Cohen_medium_effect_size  = pwr_summary_m  $f2
      , Cohen_medium_power        = pwr_summary_m  $power
      , Cohen_large_effect_size   = pwr_summary_l  $f2
      , Cohen_large_power         = pwr_summary_l  $power
      )

    if (!is.null(dat)) {
      tab_power[[i_n_total]] <-
        tab_power[[i_n_total]] |>
        dplyr::bind_cols(
          tibble::tibble(
            obs_effect_size           = pwr_summary_dat$f2
          , obs_power                 = pwr_summary_dat$power
          )
        )
    }
  } # i_n_total

  tab_power <-
    tab_power |>
    dplyr::bind_rows()


  if(sw_plot) {

    if (is.null(dat)) {
      tab_power <-
        tab_power |>
        dplyr::bind_cols(
          tibble::tibble(
            obs_effect_size           = NA
          , obs_power                 = NA
          )
        )
    }


    # reshape for plotting
    dat_power_curve_long <-
      tab_power |>
      dplyr::select(
        n_total
      #, n_groups
      #, df_num
      #, df_den
      #, sig_level
      #, method
      #, Cohen_small_effect_size
      , Cohen_small_power
      #, Cohen_medium_effect_size
      , Cohen_medium_power
      #, Cohen_large_effect_size
      , Cohen_large_power
      #, obs_effect_size
      , obs_power
      ) |>
      dplyr::rename(
        `Observed`     = obs_power
      , `Cohen Small`  = Cohen_small_power
      , `Cohen Medium` = Cohen_medium_power
      , `Cohen Large`  = Cohen_large_power
      ) |>
      tidyr::pivot_longer(
        cols =
          c(
            `Observed`
          , `Cohen Small`
          , `Cohen Medium`
          , `Cohen Large`
          )
      , names_to = "Effect_Size"
      , values_to = "Power"
      ) |>
      dplyr::rename(
        Sample_Size = n_total
      ) |>
      dplyr::select(
        Sample_Size, everything()
      ) |>
      dplyr::arrange(
        Power, Sample_Size, Effect_Size
      ) |>
      dplyr::mutate(
        Effect_Size = Effect_Size |> factor(levels =
                                                c(
                                                  "Observed"
                                                , "Cohen Small"
                                                , "Cohen Medium"
                                                , "Cohen Large"
                                                )
                                              , ordered = TRUE
                                              )
      ) |>
      tidyr::drop_na()


    text_caption <-
      paste0(
          "Power at a sample size of n = ", n_plot_ref, ":\n"
      )
    if (!is.null(dat)) {
      text_caption <-
        paste0(
          text_caption
        , "Observed: ", dat_power_curve_long |> filter(Sample_Size == n_plot_ref, Effect_Size == "Observed"    ) |> pull(Power) |> round(3)
        , ";  "
        )
    }
    text_caption <-
      paste0(
        text_caption
      , "Cohen Small: ", dat_power_curve_long |> filter(Sample_Size == n_plot_ref, Effect_Size == "Cohen Small" ) |> pull(Power) |> round(3)
      , ";  "
      , "Cohen Medium: ", dat_power_curve_long |> filter(Sample_Size == n_plot_ref, Effect_Size == "Cohen Medium") |> pull(Power) |> round(3)
      , ";  "
      , "Cohen Large: ", dat_power_curve_long |> filter(Sample_Size == n_plot_ref, Effect_Size == "Cohen Large" ) |> pull(Power) |> round(3)
      )

    if (length(n_total) == 1) {

      #library(ggplot2)
      p <- ggplot(dat_power_curve_long, aes(x = Effect_Size, y = Power))
      p <- p + theme_bw()
      #if (!is.null(n_plot_ref)) {
      #  p <- p + geom_vline(xintercept = n_plot_ref , linetype = 3, size = 1/2, alpha = 1/2)
      #}
      p <- p + geom_hline(yintercept = c(0.80), linetype = 3, size = 1/2, alpha = 1/2)
      p <- p + geom_hline(yintercept = c(0, 1), alpha = 0.15)
      p <- p + geom_bar(stat = "identity")
      #if (!is.null(n_plot_ref)) {
      #  p <- p + geom_hline(data = dat_power_curve_long |> filter(Sample_Size == n_plot_ref)
      #                    , aes(yintercept = Power, colour = Effect_Size, linetype = Effect_Size), size = 0.5, alpha = 1/2)
      #}

      p <- p + scale_y_continuous(breaks = seq(0, 1, by = 0.2), labels = scales::percent)
      #p <- p + scale_x_continuous(breaks = c(seq(0, 1000, by = 50), n_total), minor_breaks = seq(0, 1000, by = 10))
      p <- p + labs(  title = "Power"
                    #, subtitle = "Progress and Starting Current"
                    #, x = stringr::str_wrap(labelled::var_label(dat_pdp$a1c_baseline) |> as.character(), width = text_width)
                    #, y = labelled::var_label(dat_pdp$phq9_all_total_score_log2) |> as.character()
                    #, colour    = "Effect Size"
                    #, shape     = "General Health"  # "Imputed"
                    , linetype  = "Effect Size"  #"Diagnosis"
                    , fill      = "Effect Size"
                    )
      if (!is.null(n_plot_ref)) {
        p <- p + labs(caption = text_caption)
      }
      #p <- p + theme(legend.position = "bottom")
      #p <- p + theme(legend.position = "none")
      #p <- p + guides(colour = guide_legend(nrow = 2), shape = guide_legend(nrow = 2))
      #p <- p + theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1
      #p <- p + facet_grid(surv_prog ~ pdi_diagnosis)

    } else {

      if (length(n_total) <= 5) {
         warning("e_lm_power_ORIGINAL, add more values to n_total for a smoother and more accurate curve")
      }

      #library(ggplot2)
      p <- ggplot(dat_power_curve_long, aes(x = Sample_Size, y = Power, colour = Effect_Size, linetype = Effect_Size, group = Effect_Size))
      p <- p + theme_bw()
      if (!is.null(n_plot_ref)) {
        p <- p + geom_vline(xintercept = n_plot_ref , linetype = 3, size = 1/2, alpha = 1/2)
      }
      p <- p + geom_hline(yintercept = c(0.80), linetype = 3, size = 1/2, alpha = 1/2)
      p <- p + geom_hline(yintercept = c(0, 1), alpha = 0.15)
      p <- p + geom_line(alpha = 1, size = 1)
      if (!is.null(n_plot_ref)) {
        p <- p + geom_hline(data = dat_power_curve_long |> filter(Sample_Size == n_plot_ref)
                          , aes(yintercept = Power, colour = Effect_Size, linetype = Effect_Size), size = 0.5, alpha = 1/2)
      }
      p <- p + scale_y_continuous(breaks = seq(0, 1, by = 0.2), labels = scales::percent)
      #p <- p + scale_x_continuous(breaks = c(seq(0, 1000, by = 50), n_total), minor_breaks = seq(0, 1000, by = 10))
      p <- p + labs(  title = "Power curves"
                    #, subtitle = "Progress and Starting Current"
                    #, x = stringr::str_wrap(labelled::var_label(dat_pdp$a1c_baseline) |> as.character(), width = text_width)
                    #, y = labelled::var_label(dat_pdp$phq9_all_total_score_log2) |> as.character()
                    , colour    = "Effect Size"
                    #, shape     = "General Health"  # "Imputed"
                    , linetype  = "Effect Size"  #"Diagnosis"
                    #, fill      = "Diagnosis"
                    )
      if (!is.null(n_plot_ref)) {
        p <- p + labs(caption = text_caption)
        # p <- p + labs(
        #               caption = paste0(  "Power at a sample size of n = ", n_plot_ref, ":"
        #                               , "\nObserved: ", dat_power_curve_long |> filter(Sample_Size == n_plot_ref, Effect_Size == "Observed"    ) |> pull(Power) |> round(3)
        #                               , ";  Cohen Large: ", dat_power_curve_long |> filter(Sample_Size == n_plot_ref, Effect_Size == "Cohen Large" ) |> pull(Power) |> round(3)
        #                               , ";  Cohen Medium: ", dat_power_curve_long |> filter(Sample_Size == n_plot_ref, Effect_Size == "Cohen Medium") |> pull(Power) |> round(3)
        #                               , ";  Cohen Small: ", dat_power_curve_long |> filter(Sample_Size == n_plot_ref, Effect_Size == "Cohen Small" ) |> pull(Power) |> round(3)
        #                                )
        #             )
      }
      p <- p + theme(legend.position = "bottom")
      #p <- p + theme(legend.position = "none")
      #p <- p + guides(colour = guide_legend(nrow = 2), shape = guide_legend(nrow = 2))
      p <- p + theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1
      #p <- p + facet_grid(surv_prog ~ pdi_diagnosis)

      p <- p + scale_colour_brewer(palette = "Dark2") # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

    }

    if(sw_print) {
      print(p)
    }

    plot_power <- p

  } else {
    plot_power = NULL
  }

  # return a table and plot
  out <-
    list(
      tab_power  = tab_power
    , plot_power = plot_power
    )

  return(out)
} # e_lm_power_ORIGINAL
