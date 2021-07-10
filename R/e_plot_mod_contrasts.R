#' Compute and plot all contrasts and test results from a linear model by automating the use of emmeans tables and plots.
#'
#' Variable labels can be provided by labelling your data with the labelled::var_label() command.
#'
#' @param fit                       lm object
#' @param dat_cont                  data used for the lm object (only used for variable labels using labelled::var_label()
#' @param choose_contrasts          is a list of effects to plot, such as c("hp", "vs:wt"); NULL does all in model.
#' @param sw_table_in_plot          T/F put table of results in caption of plot
#' @param adjust_method see         `?emmeans::summary.emmGrid`
#' @param CI_level                  level from `?emmeans::emmeans`
#' @param sw_print                  T/F whether to print results as this function runs
#' @param sw_marginal_even_if_interaction T/F whether to also calculate marginal results when involved in interaction(s)
#' @param sw_TWI_plots_keep         two-way interaction plots are plotted for each variable conditional on the other.  Plots are created separately ("singles") or together in a grid ("both"), and "all" keeps the singles and the grid version.
#' @param sw_TWI_both_orientation   "tall" or "wide" orientation for when both two-way interaction plots are combined in a grid
#' @param plot_quantiles            quantiles plotted for numeric:numeric interaction plots
#'
#' @return out                      a list of two lists: "tables" and "plots", each have results for each contrast that was computed.  "tables" is a list of emmeans tables to print.  "plots" is a list of ggplot objects to plot separately or arrange in a grid.
#' @import dplyr
#' @import ggplot2
#' @import emmeans
#' @importFrom labelled var_label
#' @importFrom stringr str_split
#' @importFrom gridExtra arrangeGrob
#' @importFrom ggpubr as_ggplot
#' @export
#'
#' @examples
#' # Data for testing
#' dat_cont <-
#'   datasets::mtcars %>%
#'   tibble::as_tibble(
#'     rownames = "model"
#'   ) %>%
#'   dplyr::mutate(
#'     cyl = cyl %>% factor(levels = c(4, 6, 8), labels = c("four", "six", "eight"))
#'   , vs  = vs  %>% factor(levels = c(0, 1), labels = c("V-shaped", "straight"))
#'   , am  = am  %>% factor(levels = c(0, 1), labels = c("automatic", "manual"))
#'   )
#'
#' # Label columns
#' dat_labels <-
#'   tibble::tribble(
#'     ~var    , ~label
#'   , "model" , "Model"
#'   , "mpg"   , "Miles/(US) gallon"
#'   , "cyl"   , "Number of cylinders"
#'   , "disp"  , "Displacement (cu.in.)"
#'   , "hp"    , "Gross horsepower"
#'   , "drat"  , "Rear axle ratio"
#'   , "wt"    , "Weight (1000 lbs)"
#'   , "qsec"  , "1/4 mile time"
#'   , "vs"    , "Engine"                     # (0 = V-shaped, 1 = straight)"
#'   , "am"    , "Transmission"               # (0 = automatic, 1 = manual)"
#'   , "gear"  , "Number of forward gears"
#'   , "carb"  , "Number of carburetors"
#'   )
#'
#' for (i_row in 1:nrow(dat_labels)) {
#'   labelled::var_label(dat_cont[[dat_labels[["var"]][i_row] ]]) <- dat_labels[["label"]][i_row]
#' }
#'
#'  # Set specific model with some interactions
#' form_model <-
#'   mpg ~ cyl + disp + hp + wt + vs + am + cyl:vs + disp:hp + hp:vs
#'
#' fit <-
#'   lm(
#'     formula = form_model
#'   , data    = dat_cont
#'   )
#'
#' anova(fit)
#' summary(fit)
#'
#' fit_contrasts <-
#'   e_plot_model_contrasts(
#'     fit                = fit
#'   , dat_cont           = dat_cont
#'   , sw_print           = FALSE
#'   , sw_table_in_plot   = FALSE
#'   , sw_TWI_plots_keep  = "both"
#'   )
#' fit_contrasts$tables  # to print tables
#' fit_contrasts$plots   # to print plots
#' fit_contrasts$text    # to print caption text
e_plot_model_contrasts <-
  function(
    fit                     = NULL
  , dat_cont                = NULL
  , choose_contrasts        = NULL
  , sw_table_in_plot        = TRUE
  , adjust_method           = c("none", "tukey", "scheffe", "sidak", "bonferroni", "dunnettx", "mvt")[4]  # see ?emmeans::summary.emmGrid
  , CI_level                = 0.95
  , sw_print                = TRUE
  , sw_marginal_even_if_interaction = FALSE
  , sw_TWI_plots_keep       = c("singles", "both", "all")[3]
  , sw_TWI_both_orientation = c("wide", "tall")[1]
  , plot_quantiles          = c(0.05, 0.25, 0.50, 0.75, 0.95)  # for numeric:numeric plots
  ) {
  ###### START Example dataset for testing
  ##
  ## library(tidyverse)
  ##
  ## choose_contrasts        = NULL
  ## sw_table_in_plot        = TRUE
  ## adjust_method           = c("none", "tukey", "scheffe", "sidak", "bonferroni", "dunnettx", "mvt")[4]  # see ?emmeans::summary.emmGrid
  ## CI_level                = 0.95
  ## sw_print                = TRUE
  ## sw_marginal_even_if_interaction = TRUE
  ## sw_TWI_plots_keep       = c("singles", "both", "all")[3]
  ## sw_TWI_both_orientation = c("tall", "wide")[1]
  ## plot_quantiles = c(0.05, 0.25, 0.50, 0.75, 0.95)  # for numeric:numeric plots
  ##
  ## # Data for testing
  ## dat_cont <-
  ##   mtcars %>%
  ##   as_tibble(
  ##     rownames = "model"
  ##   ) %>%
  ##   mutate(
  ##     cyl = cyl %>% factor(levels = c(4, 6, 8), labels = c("four", "six", "eight"))
  ##   , vs  = vs  %>% factor(levels = c(0, 1), labels = c("V-shaped", "straight"))
  ##   , am  = am  %>% factor(levels = c(0, 1), labels = c("automatic", "manual"))
  ##   )
  ##
  ## # Label columns
  ## dat_labels <-
  ##   tribble(
  ##     ~var, ~label
  ##   , "model" , "Model"
  ##   , "mpg"   , "Miles/(US) gallon"
  ##   , "cyl"   , "Number of cylinders"
  ##   , "disp"  , "Displacement (cu.in.)"
  ##   , "hp"    , "Gross horsepower"
  ##   , "drat"  , "Rear axle ratio"
  ##   , "wt"    , "Weight (1000 lbs)"
  ##   , "qsec"  , "1/4 mile time"
  ##   , "vs"    , "Engine"                     # (0 = V-shaped, 1 = straight)"
  ##   , "am"    , "Transmission"               # (0 = automatic, 1 = manual)"
  ##   , "gear"  , "Number of forward gears"
  ##   , "carb"  , "Number of carburetors"
  ##   )
  ##
  ## for (i_row in 1:nrow(dat_labels)) {
  ##   labelled::var_label(dat_cont[[dat_labels[["var"]][i_row] ]]) <- dat_labels[["label"]][i_row]
  ## }
  ##
  ## ## form_model <-
  ## ##   as.formula(
  ## ##     paste0(
  ## ##       "mpg"
  ## ##     , " ~ "
  ## ##     , "("
  ## ##     , "   cyl"
  ## ##     , " + disp"
  ## ##     , " + hp"
  ## ##     #, " + drat"
  ## ##     , " + wt"
  ## ##     #, " + qsec"
  ## ##     , " + vs"
  ## ##     , " + am"
  ## ##     , " + gear"
  ## ##     , " + carb"
  ## ##     , ")^2"  # all two-way interaction pairs
  ## ##     )
  ## ##   )
  ## ##
  ## ## fit <-
  ## ##   lm(
  ## ##     formula = form_model
  ## ##   , data    = dat_cont
  ## ##   )
  ## ##
  ## ## ## AIC
  ## ## # option: test="F" includes additional information
  ## ## #           for parameter estimate tests that we're familiar with
  ## ## # option: for BIC, include k=log(nrow( [data.frame name] ))
  ## ## fit <-
  ## ##   step(
  ## ##     fit
  ## ##   , direction = "both"
  ## ##   , test = "F"
  ## ##   , k = log(nrow( dat_cont ))
  ## ##   , trace = 0
  ## ##   )
  ##
  ##  # Set specific model with some interactions
  ## form_model <-
  ##   mpg ~ cyl + disp + hp + wt + vs + am + cyl:vs + disp:hp + hp:vs
  ##
  ##
  ## fit <-
  ##   lm(
  ##     formula = form_model
  ##   , data    = dat_cont
  ##   )
  ## anova(fit)
  ## summary(fit)
  ##
  ##
  ## e_plot_model_contrasts(
  ##   fit                     = fit
  ## , dat_cont                = dat_cont
  ## , choose_contrasts        = NULL
  ## , sw_table_in_plot        = TRUE
  ## , adjust_method           = c("none", "tukey", "scheffe", "sidak", "bonferroni", "dunnettx", "mvt")[4]  # see ?emmeans::summary.emmGrid
  ## , CI_level                = 0.95
  ## , sw_print                = TRUE
  ## , sw_marginal_even_if_interaction = TRUE
  ## , sw_TWI_plots_keep       = c("singles", "both", "all")[3]
  ## , sw_TWI_both_orientation = c("tall", "wide")[1]
  ## , plot_quantiles = c(0.05, 0.25, 0.50, 0.75, 0.95)  # for numeric:numeric plots
  ## )
  ##
  ##
  ###### END Example dataset for testing

  # BEGIN Capture warnings to take actions
    message_involve_interaction <-
      "NOTE: Results may be misleading due to involvement in interactions\n"
  # END Capture warnings to take actions


  # check for label, create label if unlabelled
  for (n_col in names(dat_cont)) {
    ## n_col = names(dat_cont)[1]
    if (is.null(labelled::var_label(dat_cont[[n_col]]))) {
      labelled::var_label(dat_cont[[n_col]]) <- n_col
    }
  }

  # labelled::var_label(fit$model[[1]])

  # extract variables
  var_name_y <-
    as.character(formula(fit))[2]
  var_name_x <-
    str_split(
      string = attr(terms.formula(formula(fit)), "term.labels")   #as.character(formula(fit))[3]
    , pattern = fixed(" + ")
    ) %>%
    unlist()


  # restrict to chosen contrasts
  if (!is.null(choose_contrasts)) {

    # for chosen contrasts, copy and reverse order of two-way interactions
    # to make sure there's a match
      ## https://stackoverflow.com/questions/49141253/how-to-reverse-the-delimited-parts-of-a-string

    choose_contrasts_rev <-
      sapply(
        stringr::str_split(
          string  = choose_contrasts
        , pattern = fixed(":")
        )
      , function(x) {
          paste0(rev(x), collapse = ":")
        }
      )

    # restrict to chosen
    var_name_x <-
      var_name_x[which(var_name_x %in% c(choose_contrasts, choose_contrasts_rev))]
  }

  # output list of tables and plots
  out <- list()
  out[["tables"]] <- list()
  out[["plots" ]] <- list()
  out[["text"  ]] <- list()

  ## For each covariate, compute contrasts
  for (i_var_x in seq_along(var_name_x)) {
    #### "cyl"     "disp"    "hp"      "wt"      "vs"      "am"      "cyl:vs"  "disp:hp" "hp:vs"
    ## i_var_x = 1                    # factor , has interaction
    ## i_var_x = 2                    # numeric, has interaction
    ## i_var_x = 3                    # numeric, has interaction
    ## i_var_x = 4                    # numeric
    ## i_var_x = 5                    # factor , has interaction
    ## i_var_x = 6                    # factor
    ## i_var_x = 7                    # factor:factor     one pair NA, cyleight:vsstraight
    ## i_var_x = 8                    # numeric:numeric
    ## i_var_x = 9                    # numeric:factor
    ## i_var_x = length(var_name_x)

    # get x variable(s)
    var_xs <-
      stringr::str_split(
        string = var_name_x[i_var_x]
      , pattern = fixed(":")
      ) %>%
      unlist()

    # Main effect
    if (length(var_xs) == 1) {

      text_marginal_even_if_interaction <- NULL

      # First check if effect is involved in interactions
      check_message <-
        e_message_capture(
          emmeans::emmeans(
            object  = fit
          , specs   = var_xs
          )
        )(1)
      if (check_message$logs[[1]]$message == message_involve_interaction) {

        if(sw_marginal_even_if_interaction) {
          text_marginal_even_if_interaction <- paste0(check_message$logs[[1]]$message, "\n")
          message(paste0("e_plot_model_contrasts: Continuing with \"", var_xs, "\" even though involved in interactions."))
        } else {
          message(paste0("e_plot_model_contrasts: Skipping \"", var_xs, "\" since involved in interactions."))
          next
        }
      }

      # if numeric
      if ( class(dat_cont[[var_xs]]) == "numeric" ) {

        ## Table
        cont_fit <-
          emmeans::emtrends(
            object  = fit
          , specs   = var_xs
          , var     = var_xs
          )

        out[["tables"]][[ var_name_x[i_var_x] ]][["est"  ]] <- cont_fit
        out[["tables"]][[ var_name_x[i_var_x] ]][["cont" ]] <- NULL


        ## Plot
        # CI text
            # is.null is for when no values coulbe be estimated
        if(!is.null(summary(cont_fit)[["lower.CL"]])) {
          text_cont <- summary(cont_fit)[[ var_xs[1] ]]
          text_est  <- summary(cont_fit)[[2]]
          text_LCL  <- summary(cont_fit)[["lower.CL"]]
          text_UCL  <- summary(cont_fit)[["upper.CL"]]
          text_CI  <-
            paste0(
              "Estimate: "
            , text_cont
            , " = "
            , signif(text_est, 3)
            , ",  "
            , round(CI_level * 100, 1), "% CI: "
            , "("
            , signif(text_LCL, 3)
            , ", "
            , signif(text_UCL, 3)
            , ")"
            , collapse = "\n"
            )
        } else {
          text_CI <- NULL
        }

        text_averaged <-
          paste0("Tables: ", attributes(summary(cont_fit))$mesg)

        form_var <- as.formula(paste0("~", var_xs))

        p <-
          emmeans::emmip(
            object     = fit
          , formula    = form_var
          , cov.reduce = range
          )
        text_averaged_plot <-
          paste0("Plot: ", attributes(p$data)$mesg)


        text_long <-
          paste0(
            text_marginal_even_if_interaction
          , text_CI
          , "\n"
          #, text_diff
          #, "\n"
          , text_averaged
          , "\n"
          , text_averaged_plot
          )
        text_short <-
          paste0(
            text_marginal_even_if_interaction
          #  text_CI
          #, "\n"
          #, text_diff
          #, "\n"
          #, text_averaged
          #, "\n"
          , text_averaged_plot
          )
        if (sw_table_in_plot) {
          text_caption <- text_long
        } else {
          text_caption <- text_short
        }

        p <- p + labs(
            title     = paste0("Main effect of ", labelled::var_label(dat_cont[[var_xs]]) %>% as.character())
          , subtitle  = var_name_x[i_var_x]
          , x         = labelled::var_label(dat_cont[[var_xs]]) %>% as.character()
          , y         = paste0("Linear prediction of:\n", labelled::var_label(dat_cont[[var_name_y]]) %>% as.character() )
          , caption   = text_caption
          )
        p <- p + theme_bw()
        p <- p + theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1
        #p <- p + theme(axis.text.x = element_text(angle = 15, vjust = 1, hjust = 1))

        if(sw_print) {
          print(p)
        }

        out[["plots" ]][[ var_name_x[i_var_x] ]] <- p
        out[["text"  ]][[ var_name_x[i_var_x] ]] <- text_long %>% stringr::str_split(pattern = "\n")

      } # numeric




      # if factor
      if ( class(dat_cont[[var_xs]]) == "factor" ) {

        ## Table
        # if a factor, then compute the contrast and statistics and create plot
        cont_fit <-
          emmeans::emmeans(
            object  = fit
          , specs   = var_xs
          #, by    =
          #, simple = "each", combine = FALSE
          , adjust  = adjust_method
          , level   = CI_level
          )
        cont_pairs <- cont_fit %>% pairs(adjust = adjust_method)

        if(sw_print) {
          cont_fit   %>% print()
          cont_pairs %>% print()
        }

        out[["tables"]][[ var_name_x[i_var_x] ]][["est"  ]] <- cont_fit
        out[["tables"]][[ var_name_x[i_var_x] ]][["cont" ]] <- cont_pairs


        ## Plot
        # CI text
        text_cont <- summary(cont_fit)[[var_xs]]
        text_est  <- summary(cont_fit)[["emmean"]]
        text_LCL  <- summary(cont_fit)[["lower.CL"]]
        text_UCL  <- summary(cont_fit)[["upper.CL"]]
        text_CI  <-
          paste0(
            "Estimate: "
          , text_cont
          , " = "
          , signif(text_est, 3)
          , ",  "
          , round(CI_level * 100, 1), "% CI: "
          , "("
          , signif(text_LCL, 3)
          , ", "
          , signif(text_UCL, 3)
          , ")"
          , collapse = "\n"
          )

        # Contrast text
        text_cont <- summary(cont_pairs)[["contrast"]]
        text_est  <- summary(cont_pairs)[["estimate"]]
        text_pval <- summary(cont_pairs)[["p.value"]]
        text_diff  <-
          paste0(
            "Contrast: "
          , text_cont
          , " = "
          , signif(text_est, 3)
          , ", p-value = "
          , round(text_pval, 4)
          , collapse = "\n"
          )

        text_averaged <-
          paste0(attributes(summary(cont_pairs))$mesg, collapse = "\n")

        text_long <-
          paste0(
            text_marginal_even_if_interaction
          , text_CI
          , "\n"
          , text_diff
          , "\n"
          , text_averaged
          )
        text_short <-
          paste0(
            text_marginal_even_if_interaction
          #  text_CI
          #, "\n"
          #, text_diff
          #, "\n"
          , text_averaged
          )
        if (sw_table_in_plot) {
          text_caption <- text_long
        } else {
          text_caption <- text_short
        }

        p <-
          plot(
            cont_fit
          , comparisons = TRUE
          , adjust      = adjust_method
          , horizontal  = TRUE #FALSE
          #, by          = "surv_prog.factor"
          )
        p <- p + labs(
            title     = paste0("Main effect of ", labelled::var_label(dat_cont[[var_xs]]) %>% as.character())
          , subtitle  = var_name_x[i_var_x]
          , x         = paste0("Estimate of:\n", labelled::var_label(dat_cont[[var_name_y]]) %>% as.character())
          , y         = labelled::var_label(dat_cont[[var_xs]]) %>% as.character()
          , caption   = text_caption
          )
        p <- p + theme_bw()
        p <- p + theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1
        #p <- p + theme(axis.text.x = element_text(angle = 15, vjust = 1, hjust = 1))

        if(sw_print) {
          print(p)
        }

        out[["plots" ]][[ var_name_x[i_var_x] ]] <- p
        out[["text"  ]][[ var_name_x[i_var_x] ]] <- text_long %>% stringr::str_split(pattern = "\n")

      } # factor

    } # 1 Main effect




    # Two-way interaction
    if (length(var_xs) == 2) {

      ### if factor:factor
      if (all(c(class(dat_cont[[ var_xs[1] ]]), class(dat_cont[[ var_xs[2] ]])) == "factor")) {

        # do this twice, reversing the order of the factors
        for (i_repeat in 1:2) {

          ## Repeat, but reverse factors
          if (i_repeat == 2) {
            var_xs <- rev(var_xs)
          }

          ## Table
          # if a factor, then compute the contrast and statistics and create plot
          cont_fit <-
            emmeans::emmeans(
              object  = fit
            , specs   = var_xs[1]
            , by      = var_xs[2]
            #, simple = "each", combine = FALSE
            , adjust  = adjust_method
            , level   = CI_level
            )
          cont_pairs <- cont_fit %>% pairs(adjust = adjust_method)

          if(sw_print) {
            cont_fit   %>% print()
            cont_pairs %>% print()
          }

          out[["tables"]][[ var_name_x[i_var_x] ]][["est"  ]] <- cont_fit
          out[["tables"]][[ var_name_x[i_var_x] ]][["cont" ]] <- cont_pairs

          # levels of variables
          levels_specs <- levels(cont_fit)[[ var_xs[1] ]]
          levels_by    <- levels(cont_fit)[[ var_xs[2] ]]


          ## Plot
          # CI text
          text_CI  <- NULL
          i_row_cont_fit = 0
          for (i_by in seq_along(levels_by)) {
            text_CI  <-
              paste0(
                text_CI
              , paste0(var_xs[2], " = ", levels_by[i_by], ":\n")
              )
            for (i_specs in seq_along(levels_specs)) {
              i_row_cont_fit = i_row_cont_fit + 1

              text_cont <- summary(cont_fit)[[ var_xs[1] ]][i_row_cont_fit]
              text_est  <- summary(cont_fit)[["emmean"]]   [i_row_cont_fit]
              text_LCL  <- summary(cont_fit)[["lower.CL"]] [i_row_cont_fit]
              text_UCL  <- summary(cont_fit)[["upper.CL"]] [i_row_cont_fit]
              text_CI  <-
                paste0(
                  text_CI
                , "Estimate: "
                , text_cont
                , " = "
                , signif(text_est, 3)
                , ",  "
                , round(CI_level * 100, 1), "% CI: "
                , "("
                , signif(text_LCL, 3)
                , ", "
                , signif(text_UCL, 3)
                , ")"
                , "\n"
                #, collapse = "\n"
                )
            }
          }

          # # CI text
          # text_cont <- summary(cont_fit)[[ var_xs[1] ]]
          # text_est  <- summary(cont_fit)[["emmean"]]
          # text_LCL  <- summary(cont_fit)[["lower.CL"]]
          # text_UCL  <- summary(cont_fit)[["upper.CL"]]
          # text_CI  <-
          #   paste0(
          #     "Estimate: "
          #   , text_cont
          #   , " = "
          #   , signif(text_est, 3)
          #   , ",  "
          #   , round(CI_level * 100, 1), "% CI: "
          #   , "("
          #   , signif(text_LCL, 3)
          #   , ", "
          #   , signif(text_UCL, 3)
          #   , ")"
          #   , collapse = "\n"
          #   )

          # Contrast text
          text_diff  <- NULL
          i_row_cont_fit = 0
          for (i_by in seq_along(levels_by)) {
            text_diff  <-
              paste0(
                text_diff
              , paste0(var_xs[2], " = ", levels_by[i_by], ":\n")
              )
            for (i_specs in seq_along(levels_specs)) {
              i_row_cont_fit = i_row_cont_fit + 1

              text_cont <- summary(cont_pairs)[["contrast"]][i_row_cont_fit]
              text_est  <- summary(cont_pairs)[["estimate"]][i_row_cont_fit]
              text_pval <- summary(cont_pairs)[["p.value"]] [i_row_cont_fit]
              text_diff  <-
                paste0(
                  text_diff
                , "Contrast: "
                , text_cont
                , " = "
                , signif(text_est, 3)
                , ", p-value = "
                , round(text_pval, 4)
                , "\n"
                #, collapse = "\n"
                )
            }
          }

          # # Contrast text
          # text_cont <- summary(cont_pairs)[["contrast"]]
          # text_est  <- summary(cont_pairs)[["estimate"]]
          # text_pval <- summary(cont_pairs)[["p.value"]]
          # text_diff  <-
          #   paste0(
          #     "Contrast: "
          #   , text_cont
          #   , " = "
          #   , signif(text_est, 3)
          #   , ", p-value = "
          #   , round(text_pval, 4)
          #   , collapse = "\n"
          #   )

          text_averaged <-
            paste0(attributes(summary(cont_pairs))$mesg, collapse = "\n")

          text_long <-
            paste0(
              text_CI
            , "\n"
            , text_diff
            , "\n"
            , text_averaged
            )
          text_short <-
            paste0(
            #  text_CI
            #, "\n"
            #, text_diff
            #, "\n"
              text_averaged
            )
          if (sw_table_in_plot) {
            text_caption <- text_long
          } else {
            text_caption <- text_short
          }


          # if error: "Error: Aborted -- Some comparison arrows have negative length!"
          # then remove comprisons
          sw_try_ok <-
            !e_is_error(
              try(
                plot(
                  cont_fit
                , comparisons = TRUE
                )
              )
            )
          if(sw_try_ok) {
            p <-
              plot(
                cont_fit
              , comparisons = TRUE
              , adjust      = adjust_method
              , horizontal  = TRUE #FALSE
              #, by          = "surv_prog.factor"
              )
          } else {
            message(paste0("  e_plot_model_contrasts: Due to error, no comparison arrows for: ", var_xs))
            text_caption <-
              paste0(
                text_caption
              , "\n"
              , "Due to error, no comparison arrows are plotted"
              )

            p <-
              plot(
                cont_fit
              #, comparisons = TRUE
              , adjust      = adjust_method
              , horizontal  = TRUE #FALSE
              #, by          = "surv_prog.factor"
              )
          }

          p <- p + labs(
              title     = paste0("Interaction of ", labelled::var_label(dat_cont[[ var_xs[1] ]]) %>% as.character(), " and ", labelled::var_label(dat_cont[[ var_xs[2] ]]) %>% as.character())
            , subtitle  = var_name_x[i_var_x]
            , x         = paste0("Estimate of:\n", labelled::var_label(dat_cont[[var_name_y]]) %>% as.character())
            , y         = labelled::var_label(dat_cont[[ var_xs[1] ]]) %>% as.character()
            , caption   = text_caption
            )
          p <- p + theme_bw()
          p <- p + theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1
          #p <- p + theme(axis.text.x = element_text(angle = 15, vjust = 1, hjust = 1))

          if(sw_print) {
            print(p)
          }

          if (i_repeat == 1) {
            p1 <- p
          }
          if (i_repeat == 2) {
            p2 <- p
          }
          if(sw_TWI_plots_keep %in% c("singles", "both", "all")[c(1, 3)]) {
            out[["plots" ]][[ var_name_x[i_var_x] ]][[i_repeat]] <- p
          }
          out[["text"  ]][[ var_name_x[i_var_x] ]][[i_repeat]] <- text_long %>% stringr::str_split(pattern = "\n")

        } # i_repeat

        if(sw_TWI_plots_keep %in% c("singles", "both", "all")[c(2, 3)]) {
          # prepare for group plot
          p1 <- p1 +
            labs(title = NULL, tag = "A")
          p2 <- p2 +
            labs(title = NULL, tag = "B")

          ## Arrange in a grid
          #library(gridExtra)
          #library(grid)
          if (sw_TWI_both_orientation == c("tall", "wide")[1]) {
            lay_grid <-
              rbind(
                c(1)
              , c(2)
              )

            #p2 <- p2 + labs(title = NULL)

            # p_arranged <-
            #   #gridExtra::grid.arrange(
            #   gridExtra::arrangeGrob(
            #     grobs         = list(p1, p2)
            #   , layout_matrix = lay_grid
            #   #, top           = paste0("Interaction of ", labelled::var_label(dat_cont[[ var_xs[1] ]]) %>% as.character(), " and ", labelled::var_label(dat_cont[[ var_xs[2] ]]) %>% as.character())
            #   #, bottom        = "bottom\ntitle"
            #   #, left          = "left label"
            #   #, right         = "right label"
            #   ) %>%
            #   ggpubr::as_ggplot()

          }
          if (sw_TWI_both_orientation == c("tall", "wide")[2]) {
            lay_grid <-
              rbind(
                c(1, 2)
              #, c(2)
              )

            #p1 <- p1 + labs(title = NULL)
            #p2 <- p2 + labs(title = NULL)

            # p_arranged <-
            #   #gridExtra::grid.arrange(
            #   gridExtra::arrangeGrob(
            #     grobs         = list(p1, p2)
            #   , layout_matrix = lay_grid
            #   , top           = paste0("Interaction of ", labelled::var_label(dat_cont[[ var_xs[1] ]]) %>% as.character(), " and ", labelled::var_label(dat_cont[[ var_xs[2] ]]) %>% as.character())
            #   #, bottom        = "bottom\ntitle"
            #   #, left          = "left label"
            #   #, right         = "right label"
            #   ) %>%
            #   ggpubr::as_ggplot()
          }

          p_arranged <-
            #gridExtra::grid.arrange(
            gridExtra::arrangeGrob(
              grobs         = list(p1, p2)
            , layout_matrix = lay_grid
            , top           = paste0("Interaction of ", labelled::var_label(dat_cont[[ var_xs[1] ]]) %>% as.character(), " and ", labelled::var_label(dat_cont[[ var_xs[2] ]]) %>% as.character())
            #, bottom        = "bottom\ntitle"
            #, left          = "left label"
            #, right         = "right label"
            ) %>%
            ggpubr::as_ggplot()

          if(sw_print) {
            print(p_arranged)
          }

          out[["plots" ]][[ var_name_x[i_var_x] ]][["both"]] <- p_arranged

        } # sw_TWI_plots_keep

      } # factor:factor





      ### if factor:numeric
      if (any(c(class(dat_cont[[ var_xs[1] ]]), class(dat_cont[[ var_xs[2] ]])) == "factor" ) &
          any(c(class(dat_cont[[ var_xs[1] ]]), class(dat_cont[[ var_xs[2] ]])) == "numeric")
        ) {

        # make first variable the factor and second numeric
        if(class(dat_cont[[ var_xs[2] ]]) == "factor") {
          var_xs <- rev(var_xs)
        }

        ## Table
        form_var_fac <- as.formula(paste0("pairwise", " ~ ", var_xs[1]))

        cont_fit <-
          emmeans::emtrends(
            object  = fit
          , specs   = form_var_fac
          , var     = var_xs[2]
          #, mult.name = "variety"
          )

        out[["tables"]][[ var_name_x[i_var_x] ]][["est"  ]] <- cont_fit$emtrends
        out[["tables"]][[ var_name_x[i_var_x] ]][["cont" ]] <- cont_fit$contrasts

        ## Plot
        # CI text
            # is.null is for when no values coulbe be estimated
        if(!is.null(summary(cont_fit$emtrends)[["lower.CL"]])) {
          text_cont <- summary(cont_fit$emtrends)[[ var_xs[1] ]]
          text_est  <- summary(cont_fit$emtrends)[[2]]
          text_LCL  <- summary(cont_fit$emtrends)[["lower.CL"]]
          text_UCL  <- summary(cont_fit$emtrends)[["upper.CL"]]
          text_CI  <-
            paste0(
              "Estimate: "
            , text_cont
            , " = "
            , signif(text_est, 3)
            , ",  "
            , round(CI_level * 100, 1), "% CI: "
            , "("
            , signif(text_LCL, 3)
            , ", "
            , signif(text_UCL, 3)
            , ")"
            , collapse = "\n"
            )
        } else {
          text_CI <- NULL
        }
        # Contrast text
        text_cont <- summary(cont_fit$contrasts)[["contrast"]]
        text_est  <- summary(cont_fit$contrasts)[["estimate"]]
        text_pval <- summary(cont_fit$contrasts)[["p.value"]]
        text_diff  <-
          paste0(
            "Contrast: "
          , text_cont
          , " = "
          , signif(text_est, 3)
          , ", p-value = "
          , round(text_pval, 4)
          , collapse = "\n"
          )

        text_averaged <-
          attributes(summary(cont_fit$contrasts))$mesg

        text_long <-
          paste0(
            text_CI
          , "\n"
          , text_diff
          , "\n"
          , text_averaged
          )
        text_short <-
          paste0(
          #  text_CI
          #, "\n"
          #, text_diff
          #, "\n"
            text_averaged
          )
        if (sw_table_in_plot) {
          text_caption <- text_long
        } else {
          text_caption <- text_short
        }

        p1 <- plot(cont_fit)
        p1 <- p1 + theme_bw()
        p1 <- p1 + labs(
            title     = paste0("Interaction of ", labelled::var_label(dat_cont[[ var_xs[1] ]]) %>% as.character(), " and ", labelled::var_label(dat_cont[[ var_xs[2] ]]) %>% as.character())
          , subtitle  = var_name_x[i_var_x]
          , x         = paste0("Estimated slope for:\n", labelled::var_label(dat_cont[[ var_xs[2] ]]) %>% as.character() )
          , y         = labelled::var_label(dat_cont[[ var_xs[1] ]]) %>% as.character()
          #, colour    = labelled::var_label(dat_cont[[ var_xs[1] ]]) %>% as.character()
          , caption   = text_caption
          #, tag       = "A"
          )
        p1 <- p1 + theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1

        if(sw_TWI_plots_keep %in% c("singles", "both", "all")[c(1, 3)]) {
          out[["plots" ]][[ var_name_x[i_var_x] ]][[1]] <- p1
        }
        out[["text"  ]][[ var_name_x[i_var_x] ]][[1]] <- text_long %>% stringr::str_split(pattern = "\n")



        form_var_fac_num <- as.formula(paste0(var_xs[1], " ~ ", var_xs[2]))

        text_long <-
          paste0(
            text_averaged
          )
        text_short <-
          paste0(
            text_averaged
          )
        if (sw_table_in_plot) {
          text_caption <- text_long
        } else {
          text_caption <- text_short
        }


        p2 <- emmeans::emmip(
            object     = fit
          , formula    = form_var_fac_num
          , cov.reduce = range
          )
        p2 <- p2 + theme_bw()
        p2 <- p2 + labs(
            title     = paste0("Interaction of ", labelled::var_label(dat_cont[[ var_xs[1] ]]) %>% as.character(), " and ", labelled::var_label(dat_cont[[ var_xs[2] ]]) %>% as.character())
          , subtitle  = var_name_x[i_var_x]
          , x         = labelled::var_label(dat_cont[[ var_xs[2] ]]) %>% as.character()
          , y         = paste0("Linear prediction of:\n", labelled::var_label(dat_cont[[var_name_y]]) %>% as.character() )
          , colour    = labelled::var_label(dat_cont[[ var_xs[1] ]]) %>% as.character()
          , caption   = text_caption
          #, tag       = "B"
          )
        p2 <- p2 + theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1


        if(sw_TWI_plots_keep %in% c("singles", "both", "all")[c(1, 3)]) {
          out[["plots" ]][[ var_name_x[i_var_x] ]][[2]] <- p2
        }
        out[["text"  ]][[ var_name_x[i_var_x] ]][[2]] <- text_long %>% stringr::str_split(pattern = "\n")

        if(sw_TWI_plots_keep %in% c("singles", "both", "all")[c(2, 3)]) {
          # prepare for group plot
          p1 <- p1 +
            labs(title = NULL, tag = "A")
          p2 <- p2 +
            labs(title = NULL, tag = "B")

          ## Arrange in a grid
          #library(gridExtra)
          #library(grid)
          if (sw_TWI_both_orientation == c("tall", "wide")[1]) {
            lay_grid <-
              rbind(
                c(1)
              , c(2)
              )

            #p2 <- p2 + labs(title = NULL)

            # p_arranged <-
            #   #gridExtra::grid.arrange(
            #   gridExtra::arrangeGrob(
            #     grobs         = list(p1, p2)
            #   , layout_matrix = lay_grid
            #   #, top           = paste0("Interaction of ", labelled::var_label(dat_cont[[ var_xs[1] ]]) %>% as.character(), " and ", labelled::var_label(dat_cont[[ var_xs[2] ]]) %>% as.character())
            #   #, bottom        = "bottom\ntitle"
            #   #, left          = "left label"
            #   #, right         = "right label"
            #   ) %>%
            #   ggpubr::as_ggplot()

          }
          if (sw_TWI_both_orientation == c("tall", "wide")[2]) {
            lay_grid <-
              rbind(
                c(1, 2)
              #, c(2)
              )

            #p1 <- p1 + labs(title = NULL)
            #p2 <- p2 + labs(title = NULL)

            # p_arranged <-
            #   #gridExtra::grid.arrange(
            #   gridExtra::arrangeGrob(
            #     grobs         = list(p1, p2)
            #   , layout_matrix = lay_grid
            #   , top           = paste0("Interaction of ", labelled::var_label(dat_cont[[ var_xs[1] ]]) %>% as.character(), " and ", labelled::var_label(dat_cont[[ var_xs[2] ]]) %>% as.character())
            #   #, bottom        = "bottom\ntitle"
            #   #, left          = "left label"
            #   #, right         = "right label"
            #   ) %>%
            #   ggpubr::as_ggplot()
          }

          p_arranged <-
            #gridExtra::grid.arrange(
            gridExtra::arrangeGrob(
              grobs         = list(p1, p2)
            , layout_matrix = lay_grid
            , top           = paste0("Interaction of ", labelled::var_label(dat_cont[[ var_xs[1] ]]) %>% as.character(), " and ", labelled::var_label(dat_cont[[ var_xs[2] ]]) %>% as.character())
            #, bottom        = "bottom\ntitle"
            #, left          = "left label"
            #, right         = "right label"
            ) %>%
            ggpubr::as_ggplot()

          if(sw_print) {
            print(p_arranged)
          }

          out[["plots" ]][[ var_name_x[i_var_x] ]][["both"]] <- p_arranged
        } # sw_TWI_plots_keep
      } # factor:numeric




      ### if numeric:numeric
      if (all(c(class(dat_cont[[ var_xs[1] ]]), class(dat_cont[[ var_xs[2] ]])) == "numeric")) {

        # do this twice, reversing the order of the factors
        for (i_repeat in 1:2) {
          ## i_repeat = 2

          ## Repeat, but reverse factors
          if (i_repeat == 2) {
            var_xs <- rev(var_xs)
          }

          form_var_num_num <- as.formula(paste0(var_xs[1], " ~ ", var_xs[2]))

          # values of numeric variables for plotting
          at_list <- list()
          at_list[[ var_xs[1] ]] <-
            dat_cont[[ var_xs[1] ]] %>% quantile(probs = plot_quantiles) %>% unique()
          at_list[[ var_xs[2] ]] <-
            dat_cont[[ var_xs[2] ]] %>% quantile(probs = plot_quantiles) %>% unique()

          fit_emm_at <-
            emmeans::ref_grid(
              object  = fit
            , at      = at_list
            )

          ## Table

          #form_var_fac <- as.formula(paste0("pairwise", " ~ ", var_xs[1]))

          out[["tables"]][[ var_name_x[i_var_x] ]][["est"  ]] <- NULL
          out[["tables"]][[ var_name_x[i_var_x] ]][["cont" ]] <- NULL


          ## Plot
          p <- emmeans::emmip(
              object  = fit_emm_at
            , formula = form_var_num_num
            )

          text_averaged_plot <-
            attributes(p$data)$mesg

          text_long <-
            paste0(
              paste0("Quantiles plotted: ", paste(plot_quantiles, collapse = ", "), "; may be fewer if quantiles are not unique values.")
            , "\n"
            , text_averaged_plot
            )
          text_short <-
            paste0(
              paste0("Quantiles plotted: ", paste(plot_quantiles, collapse = ", "), "; may be fewer if quantiles are not unique values.")
            , "\n"
            , text_averaged_plot
            )
          if (sw_table_in_plot) {
            text_caption <- text_long
          } else {
            text_caption <- text_short
          }

          p <- p + labs(
              title     = paste0("Interaction of ", labelled::var_label(dat_cont[[ var_xs[1] ]]) %>% as.character(), " and ", labelled::var_label(dat_cont[[ var_xs[2] ]]) %>% as.character())
            , subtitle  = var_name_x[i_var_x]
            , x         = labelled::var_label(dat_cont[[ var_xs[2] ]]) %>% as.character()
            , y         = paste0("Linear prediction of:\n", labelled::var_label(dat_cont[[var_name_y]]) %>% as.character() )
            , colour    = labelled::var_label(dat_cont[[ var_xs[1] ]]) %>% as.character()
            , caption   = text_caption
            #, tag       = "A"
            )
          p <- p + theme_bw()
          p <- p + theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1
          #p <- p + theme(axis.text.x = element_text(angle = 15, vjust = 1, hjust = 1))

          if(sw_print) {
            print(p)
          }

          if (i_repeat == 1) {
            p1 <- p
          }
          if (i_repeat == 2) {
            p2 <- p
          }
          if(sw_TWI_plots_keep %in% c("singles", "both", "all")[c(1, 3)]) {
            out[["plots" ]][[ var_name_x[i_var_x] ]][[i_repeat]] <- p
          }
          out[["text"  ]][[ var_name_x[i_var_x] ]][[i_repeat]] <- text_long %>% stringr::str_split(pattern = "\n")

        } # i_repeat

        if(sw_TWI_plots_keep %in% c("singles", "both", "all")[c(2, 3)]) {
          # prepare for group plot
          p1 <- p1 +
            labs(title = NULL, tag = "A")
          p2 <- p2 +
            labs(title = NULL, tag = "B")

          ## Arrange in a grid
          #library(gridExtra)
          #library(grid)
          if (sw_TWI_both_orientation == c("tall", "wide")[1]) {
            lay_grid <-
              rbind(
                c(1)
              , c(2)
              )

            #p2 <- p2 + labs(title = NULL)

            # p_arranged <-
            #   #gridExtra::grid.arrange(
            #   gridExtra::arrangeGrob(
            #     grobs         = list(p1, p2)
            #   , layout_matrix = lay_grid
            #   #, top           = paste0("Interaction of ", labelled::var_label(dat_cont[[ var_xs[1] ]]) %>% as.character(), " and ", labelled::var_label(dat_cont[[ var_xs[2] ]]) %>% as.character())
            #   #, bottom        = "bottom\ntitle"
            #   #, left          = "left label"
            #   #, right         = "right label"
            #   ) %>%
            #   ggpubr::as_ggplot()

          }
          if (sw_TWI_both_orientation == c("tall", "wide")[2]) {
            lay_grid <-
              rbind(
                c(1, 2)
              #, c(2)
              )

            #p1 <- p1 + labs(title = NULL)
            #p2 <- p2 + labs(title = NULL)

            # p_arranged <-
            #   #gridExtra::grid.arrange(
            #   gridExtra::arrangeGrob(
            #     grobs         = list(p1, p2)
            #   , layout_matrix = lay_grid
            #   , top           = paste0("Interaction of ", labelled::var_label(dat_cont[[ var_xs[1] ]]) %>% as.character(), " and ", labelled::var_label(dat_cont[[ var_xs[2] ]]) %>% as.character())
            #   #, bottom        = "bottom\ntitle"
            #   #, left          = "left label"
            #   #, right         = "right label"
            #   ) %>%
            #   ggpubr::as_ggplot()
          }

          p_arranged <-
            #gridExtra::grid.arrange(
            gridExtra::arrangeGrob(
              grobs         = list(p1, p2)
            , layout_matrix = lay_grid
            , top           = paste0("Interaction of ", labelled::var_label(dat_cont[[ var_xs[1] ]]) %>% as.character(), " and ", labelled::var_label(dat_cont[[ var_xs[2] ]]) %>% as.character())
            #, bottom        = "bottom\ntitle"
            #, left          = "left label"
            #, right         = "right label"
            ) %>%
            ggpubr::as_ggplot()

          if(sw_print) {
            print(p_arranged)
          }

          out[["plots" ]][[ var_name_x[i_var_x] ]][["both"]] <- p_arranged
        } # sw_TWI_plots_keep
      } # numeric:numeric

    } # 2 Two-way interaction

  } # i_var_x

  invisible(out)

} # end of e_plot_model_contrasts()

