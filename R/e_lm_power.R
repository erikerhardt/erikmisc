#' Multiple regression power analysis
#'
#' @param dat           observed effect size data set
#' @param formula_full  observed effect size full model formula, used with dat
#' @param formula_red   observed effect size reduced model formula, used with dat
#' @param fit_model_type "lm" or "lmer", used with dat to specify how formulas should be fit
#' @param n_total       a total sample size value or list of values, used for power curve
#' @param n_param_full  number of parameters in full model, only used if dat is not specified
#' @param n_param_red   number of parameters in reduced model, only used if dat is not specified; must be fewer than n_param_full
#' @param sig_level     Type-I error rate
#' @param weights       observed effect size model fit, if it should be weighted regression
#' @param sw_print      print results
#' @param sw_plots      create histogram and power curve plots
#' @param n_plot_ref    a sample size reference line for the plot; if null, then uses size of data, otherwise uses median of n_total.  Histogram is created for first reference value in the list.
#'
#' @return list with tables and plots of power analysis results
#' @importFrom pwr pwr.f2.test
#' @importFrom pwr cohen.ES
#' @importFrom modelr rsquare
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr drop_na
#' @importFrom scales percent
#' @importFrom stats as.formula
#' @importFrom stats lm
#' @importFrom stats quantile
#' @importFrom RColorBrewer brewer.pal
#' @import dplyr
#' @import ggplot2
#' @export
#'
#' @examples
#'
#' # without data, single n
#' out <-
#'   e_lm_power(
#'     dat           = NULL
#'   , formula_full  = NULL
#'   , formula_red   = NULL
#'   , n_total       = 100
#'   , n_param_full  = 10
#'   , n_param_red   = 5
#'   , sig_level     = 0.05
#'   , weights       = NULL
#'   , sw_print      = TRUE
#'   , sw_plots      = TRUE
#'   , n_plot_ref    = NULL
#'   )
#'
#' # without data, sequence of n for power curve
#' out <-
#'   e_lm_power(
#'     dat           = NULL
#'   , formula_full  = NULL
#'   , formula_red   = NULL
#'   , n_total       = seq(20, 300, by = 5)
#'   , n_param_full  = 10
#'   , n_param_red   = 5
#'   , sig_level     = 0.05
#'   , weights       = NULL
#'   , sw_print      = TRUE
#'   , sw_plots      = TRUE
#'   , n_plot_ref    = NULL
#'   )
#'
#'
#' # with data
#' str(dat_mtcars_e)
#'
#' yvar      <- "mpg"
#' xvar_full <- c("cyl", "disp", "hp", "drat", "wt", "qsec")
#' xvar_red  <- c(               "hp", "drat", "wt", "qsec")
#'
#' formula_full <-
#'   stats::as.formula(
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
#'   stats::as.formula(
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
#' out <-
#'   e_lm_power(
#'     dat           = dat_mtcars_e
#'   , formula_full  = formula_full
#'   , formula_red   = formula_red
#'   , n_total       = 100
#'   , n_param_full  = NULL
#'   , n_param_red   = NULL
#'   , sig_level     = 0.05
#'   , weights       = NULL
#'   , sw_print      = TRUE
#'   , sw_plots      = TRUE
#'   , n_plot_ref    = NULL
#'   )
#'
#' # with data, sequence of n for power curve, multiple reference sample sizes
#' out <-
#'   e_lm_power(
#'     dat           = dat_mtcars_e
#'   , formula_full  = formula_full
#'   , formula_red   = formula_red
#'   , n_total       = seq(10, 300, by = 5)
#'   , n_param_full  = NULL
#'   , n_param_red   = NULL
#'   , sig_level     = 0.05
#'   , weights       = NULL
#'   , sw_print      = TRUE
#'   , sw_plots      = TRUE
#'   , n_plot_ref    = c(100, 120, 150)
#'   )
#' out$tab_power_ref |> print(width = Inf)
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
e_lm_power <-
  function(
    dat           = NULL
  , formula_full  = NULL
  , formula_red   = NULL
  , fit_model_type = c("lm", "lmer")[1]
  , n_total
  , n_param_full  = NULL
  , n_param_red   = NULL
  , sig_level     = 0.05
  , weights       = NULL
  , sw_print      = TRUE
  , sw_plots      = TRUE
  , n_plot_ref    = NULL
  ) {

  if (!is.null(dat)) {


    if(fit_model_type == c("lm", "lmer")[1]) {
      if(!is.null(weights)) {
        lm_summary_AB <- lm(formula_full, data = dat, weights = weights)
        lm_summary_A  <- lm(formula_red , data = dat, weights = weights)
      } else {
        lm_summary_AB <- lm(formula_full, data = dat)
        lm_summary_A  <- lm(formula_red , data = dat)
      }
    } # if lm
    if(fit_model_type == c("lm", "lmer")[2]) {
      if(!is.null(weights)) {
        lm_summary_AB <- lme4::lmer(formula_full, data = dat, weights = weights, REML = TRUE)
        lm_summary_A  <- lme4::lmer(formula_red , data = dat, weights = weights, REML = TRUE)
      } else {
        lm_summary_AB <- lme4::lmer(formula_full, data = dat, REML = TRUE)
        lm_summary_A  <- lme4::lmer(formula_red , data = dat, REML = TRUE)
      }
    } # if lmer

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

    if(fit_model_type == c("lm", "lmer")[1]) {
      f2 <- (summary(lm_summary_AB)$r.squared - summary(lm_summary_A)$r.squared) /
            (1 - summary(lm_summary_AB)$r.squared)
      # f2
      df_full  <- summary(lm_summary_AB)$fstatistic[c("numdf", "dendf")]
      df_red   <- summary(lm_summary_A )$fstatistic[c("numdf", "dendf")]
      if (is.null(df_red)) {
        df_red = c(numdf = 0, dendf = sum(df_full))
      }

      n_param_full <- df_full[1]
      n_param_red  <- df_red [1]
    } # if lm
    if(fit_model_type == c("lm", "lmer")[2]) {
      f2 <- (modelr::rsquare(lm_summary_AB, data = dat) - modelr::rsquare(lm_summary_A, data = dat)) /
            (1 - modelr::rsquare(lm_summary_AB, data = dat))
      # f2
      df_full  <- summary(lm_summary_AB)$devcomp$dims[c("p", "q")]
      df_red   <- summary(lm_summary_A )$devcomp$dims[c("p", "q")]
      if (is.null(df_red)) {
        df_red = c(numdf = 0, dendf = sum(df_full))
      }

      n_param_full <- df_full[1]
      n_param_red  <- df_red [1]
    } # if lmer

    n_total <- c(n_total, nrow(dat))

    if (is.null(n_plot_ref)) {
      message("Setting n_plot_ref = the number of observations in the data for reference")
      n_plot_ref <- nrow(dat)
    }

  } # dat

  n_total <- c(n_total, n_plot_ref) |> unique() |> sort()

  if (length(n_total) == 1) {
    message("Setting n_plot_ref = n_total for reference")
    n_plot_ref <- n_total
  }

  if (is.null(n_plot_ref)) {
    message("Setting n_plot_ref = median(n_total) for reference")
    n_plot_ref <- as.numeric(stats::quantile(n_total, probs = 0.5, type = 1))
  }


  sw_print_message <- TRUE
  tab_power <- list()
  for (i_n_total in 1:length(n_total)) {

    # degrees of freedom
    df_f2    <- c(n_param_full - n_param_red, n_total[i_n_total] - n_param_full)


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
        cat("Cohen reference effect size and power =============================\n")
        cat("  Small ---------------\n")
        print(pwr_summary_s)
        cat("  Medium --------------\n")
        print(pwr_summary_m)
        cat("  Large ---------------\n")
        print(pwr_summary_l)
        if (!is.null(dat)) {
          cat("Observed effect size and power ====================================\n")
          cat("  Observed ---------------\n")
          print(pwr_summary_dat)
        }
      } else {
        if(sw_print_message) {
          message("e_lm_power, not printing results when n_total > 1")
          sw_print_message <- FALSE
        }
      }
    } # sw_print

    tab_power[[i_n_total]] <-
      tibble::tibble(
        n_total                   = n_total[i_n_total]
      , n_param_full              = n_param_full
      , n_param_red               = n_param_red
      , df_num                    = pwr_summary_s  $u
      , df_den                    = pwr_summary_s  $v
      , sig_level                 = pwr_summary_s  $sig.level
      , method                    =
          dplyr::case_when(
            fit_model_type == c("lm", "lmer")[1] ~ pwr_summary_s  $method  # "Multiple regression power calculation"
          , fit_model_type == c("lm", "lmer")[2] ~ "Longitudinal multiple regression power calculation"
          , TRUE                                 ~ NA |> as.character()
          )
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


  if(sw_plots) {

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
      dplyr::left_join(
        ## power
        tab_power |>
        dplyr::select(
          n_total
        #, n_param_full
        #, n_param_red
        #, df_num
        #, df_den
        #, sig_level
        #, method
        #, obs_effect_size
        , obs_power
        #, Cohen_small_effect_size
        , Cohen_small_power
        #, Cohen_medium_effect_size
        , Cohen_medium_power
        #, Cohen_large_effect_size
        , Cohen_large_power
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
          Effect_Size =
            Effect_Size |>
            factor(
              levels =
                c(
                  "Observed"
                , "Cohen Small"
                , "Cohen Medium"
                , "Cohen Large"
                )
            , ordered = TRUE
            )
        )
        ## f2 effect size
      , tab_power |>
        dplyr::select(
          #n_total
        #, n_param_full
        #, n_param_red
        #, df_num
        #, df_den
        #, sig_level
        #, method
          Cohen_small_effect_size
        #, Cohen_small_power
        , Cohen_medium_effect_size
        #, Cohen_medium_power
        , Cohen_large_effect_size
        #, Cohen_large_power
        , obs_effect_size
        #, obs_power
        ) |>
        dplyr::rename(
          `Cohen Small`  = Cohen_small_effect_size
        , `Cohen Medium` = Cohen_medium_effect_size
        , `Cohen Large`  = Cohen_large_effect_size
        , `Observed`     = obs_effect_size
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
        , values_to = "f2"
        ) |>
        dplyr::arrange(
          f2, Effect_Size
        ) |>
        dplyr::mutate(
          Effect_Size =
            Effect_Size |>
            factor(
              levels =
                c(
                  "Observed"
                , "Cohen Small"
                , "Cohen Medium"
                , "Cohen Large"
                )
            , ordered = TRUE
            )
        )
      ) |>
      dplyr::distinct() |>
      dplyr::mutate(
        # plot size used in ggplot, Observed is larger
        plot_size =
          dplyr::case_when(
            Effect_Size |> stringr::str_detect(pattern = fixed("Cohen ")) ~ 1
          , TRUE ~ 1.5
          ) |> as.factor()
      ) |>
      tidyr::drop_na()




    text_caption <- NULL
    ## Effect size
    text_caption <-
      paste0(
        text_caption
      , "Effect size (f2): "
      )

    # observed
    if (!is.null(dat)) {
      text_caption <-
        paste0(
          text_caption
        , "Observed: ", dat_power_curve_long |> dplyr::filter(Sample_Size == n_plot_ref[1], Effect_Size == "Observed" ) |> pull(f2) |> round(3)
        , ";  "
        )
    }

    text_caption <-
      paste0(
        text_caption
      , "Small: ", dat_power_curve_long |> dplyr::filter(Sample_Size == n_plot_ref[1], Effect_Size == "Cohen Small" ) |> pull(f2) |> round(3)
      , ";  "
      , "Medium: ", dat_power_curve_long |> dplyr::filter(Sample_Size == n_plot_ref[1], Effect_Size == "Cohen Medium" ) |> pull(f2) |> round(3)
      , ";  "
      , "Large: ", dat_power_curve_long |> dplyr::filter(Sample_Size == n_plot_ref[1], Effect_Size == "Cohen Large" ) |> pull(f2) |> round(3)
      )
    # new line
    text_caption <-
      paste0(
        text_caption
      , "\n"
      )
    for (i_n_plot_ref in seq_along(n_plot_ref)) {
      ## i_n_plot_ref = 1
      # next line of powers
      if (i_n_plot_ref > 1) {
        text_caption <-
          paste0(
            text_caption
          , "\n"
          )
      }
      ## Power
      text_caption <-
        paste0(
          text_caption
        , "Power at a sample size of n = ", n_plot_ref[i_n_plot_ref], ":\n"
        )
      # spaces before power line
      text_caption <-
        paste0(
          text_caption
        , "  "
        )
      # observed
      if (!is.null(dat)) {
        text_caption <-
          paste0(
            text_caption
          , "Observed: ", dat_power_curve_long |> dplyr::filter(Sample_Size == n_plot_ref[i_n_plot_ref], Effect_Size == "Observed"    ) |> pull(Power) |> round(3)
          , ";  "
          )
      }
      # Cohen
      text_caption <-
        paste0(
          text_caption
        , "Cohen Small: ", dat_power_curve_long |> dplyr::filter(Sample_Size == n_plot_ref[i_n_plot_ref], Effect_Size == "Cohen Small" ) |> pull(Power) |> round(3)
        , ";  "
        , "Cohen Medium: ", dat_power_curve_long |> dplyr::filter(Sample_Size == n_plot_ref[i_n_plot_ref], Effect_Size == "Cohen Medium") |> pull(Power) |> round(3)
        , ";  "
        , "Cohen Large: ", dat_power_curve_long |> dplyr::filter(Sample_Size == n_plot_ref[i_n_plot_ref], Effect_Size == "Cohen Large" ) |> pull(Power) |> round(3)
        )
    }

    ## Histogram plot for the first reference

    #library(ggplot2)
    p <- ggplot(dat_power_curve_long |> dplyr::filter(Sample_Size == n_plot_ref[1]), aes(x = Effect_Size, y = Power))
    p <- p + theme_bw()
    #if (!is.null(n_plot_ref)) {
    #  p <- p + geom_vline(xintercept = n_plot_ref , linetype = 3, size = 1/2, alpha = 1/2)
    #}
    p <- p + geom_hline(yintercept = c(0.80), linetype = 3, size = 1/2, alpha = 1/2)
    p <- p + geom_hline(yintercept = c(0, 1), alpha = 0.15)
    p <- p + geom_bar(stat = "identity")
    #if (!is.null(n_plot_ref)) {
    #  p <- p + geom_hline(data = dat_power_curve_long |> dplyr::filter(Sample_Size == n_plot_ref)
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
    p <- p + theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1
    #p <- p + facet_grid(surv_prog ~ pdi_diagnosis)

    plot_power_hist <- p


    ## Power curves for all n_total values
    if (length(n_total) > 1) {
      if (length(n_total) <= 5) {
         message("e_lm_power, add more values to n_total for a smoother and more accurate curve")
      }

      #library(ggplot2)
      p <- ggplot(dat_power_curve_long, aes(x = Sample_Size, y = Power, colour = Effect_Size, linetype = Effect_Size, group = Effect_Size))
      p <- p + theme_bw()
      if (!is.null(n_plot_ref)) {
        p <- p + geom_vline(xintercept = n_plot_ref, linetype = 3, size = 1/2, alpha = 1/2)
      }
      p <- p + geom_hline(yintercept = c(0.80), linetype = 3, size = 1/2, alpha = 1/2)
      p <- p + geom_hline(yintercept = c(0, 1), alpha = 0.15)

      # observed
      if (!is.null(dat)) {
        p <- p + geom_line(aes(size = plot_size), alpha = 1)
        p <- p + scale_size_manual(values = c(1, 1.5), breaks = c(1, 1.5))
        p <- p + guides(size = "none")
      } else {
        p <- p + geom_line(size = 1, alpha = 1)
      }


      if (!is.null(n_plot_ref)) {
        p <- p + geom_hline(data = dat_power_curve_long |> dplyr::filter(Sample_Size %in% n_plot_ref)
                          , aes(yintercept = Power, colour = Effect_Size, linetype = Effect_Size), size = 1/2, alpha = 1/2)
      }
      p <- p + scale_y_continuous(breaks = seq(0, 1, by = 0.2), labels = scales::percent)
      p <- p + scale_x_continuous(breaks = c(seq(0, max(n_total), by = e_plot_calc_break_interval(n_total, num_intervals = 3)), n_plot_ref)) #, minor_breaks = seq(0, 1000, by = 10))
      p <- p + labs(  title = "Power curves"
                    #, subtitle =
                    #, x =
                    #, y =
                    , colour    = "Effect Size"
                    #, shape     =
                    , linetype  = "Effect Size"  #"Diagnosis"
                    #, fill      =
                    )
      if (!is.null(n_plot_ref)) {
        p <- p + labs(caption = text_caption)
        # p <- p + labs(
        #               caption = paste0(  "Power at a sample size of n = ", n_plot_ref, ":"
        #                               , "\nObserved: ", dat_power_curve_long |> dplyr::filter(Sample_Size == n_plot_ref, Effect_Size == "Observed"    ) |> pull(Power) |> round(3)
        #                               , ";  Cohen Large: ", dat_power_curve_long |> dplyr::filter(Sample_Size == n_plot_ref, Effect_Size == "Cohen Large" ) |> pull(Power) |> round(3)
        #                               , ";  Cohen Medium: ", dat_power_curve_long |> dplyr::filter(Sample_Size == n_plot_ref, Effect_Size == "Cohen Medium") |> pull(Power) |> round(3)
        #                               , ";  Cohen Small: ", dat_power_curve_long |> dplyr::filter(Sample_Size == n_plot_ref, Effect_Size == "Cohen Small" ) |> pull(Power) |> round(3)
        #                                )
        #             )
      }
      p <- p + theme(legend.position = "bottom")
      #p <- p + theme(legend.position = "none")
      #p <- p + guides(colour = guide_legend(nrow = 2), shape = guide_legend(nrow = 2))
      p <- p + theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1
      #p <- p + facet_grid(surv_prog ~ pdi_diagnosis)

      # RColorBrewer::brewer.pal(4,"Dark2")
      custom_color <-
        c(
          "Observed"     = RColorBrewer::brewer.pal(4,"Dark2")[4]
        , "Cohen Small"  = RColorBrewer::brewer.pal(4,"Dark2")[1]
        , "Cohen Medium" = RColorBrewer::brewer.pal(4,"Dark2")[2]
        , "Cohen Large"  = RColorBrewer::brewer.pal(4,"Dark2")[3]
        )
      custom_linetype <-
        c(
          "Observed"     = "solid"
        , "Cohen Small"  = "dashed"
        , "Cohen Medium" = "dotdash"
        , "Cohen Large"  = "longdash"
        )

      p <- p + scale_colour_brewer(palette = "Dark2") # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
      p <- p + scale_colour_manual(values = custom_color) # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
      p <- p + scale_linetype_manual(values = custom_linetype)

      plot_power_curve <- p
    } else {
      message("e_lm_power, power curve only available when length of n_total > 1")
      plot_power_curve  = NULL
    } # length(n_total)

  } else {
    plot_power_hist   = NULL
    plot_power_curve  = NULL
  } # sw_plots

  if(sw_print) {
    print(plot_power_hist )
    print(plot_power_curve)
  }


  # table of powers at reference sizes
  tab_power_ref <-
    tab_power |>
    dplyr::filter(
      n_total %in% n_plot_ref
    ) |>
    dplyr::select(
      n_total
    , n_param_full
    , n_param_red
    , ends_with("_power")
    , ends_with("_effect_size")
    )

  # return a table and plot
  out <-
    list(
      tab_power         = tab_power
    , tab_power_ref     = tab_power_ref
    , plot_power_hist   = plot_power_hist
    , plot_power_curve  = plot_power_curve
    )

  return(out)
} # e_lm_power
