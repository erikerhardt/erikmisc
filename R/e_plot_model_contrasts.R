#' Compute and plot all contrasts and test results from a linear model by automating the use of emmeans tables and plots.
#'
#' Variable labels can be provided by labelling your data with the labelled::var_label() command.
#'
#' Plot interpretation:
#' This EMM plot (Estimated Marginal Means, aka Least-Squares Means) is only
#' available when conditioning on one variable. The blue bars are confidence
#' intervals for the EMMs; don't ever use confidence intervals for EMMs to perform
#' comparisons --- they can be very misleading. The red arrows are for the
#' comparisons between means; the degree to which the "comparison arrows" overlap
#' reflects as much as possible the significance of the comparison of the two
#' estimates. If an arrow from one mean overlaps an arrow from another group, the
#' difference is not significant, based on the adjust setting (which defaults to
#' "tukey").
#'
#' @param fit                       (required) model object: lm, glm, lmerModLmerTest (from \code{lme4::lmer}, or lmerMod (from \code{lmerTest::as_lmerModLmerTest})
#' @param dat_cont                  (required) data used for the lm object (only used for variable labels using labelled::var_label()
#' @param choose_contrasts          is a list of effects to plot, such as c("hp", "vs:wt"); NULL does all in model.
#' @param sw_table_in_plot          T/F put table of results in caption of plot
#' @param adjust_method see         `?emmeans::summary.emmGrid`
#' @param CI_level                  level from `?emmeans::emmeans`
#' @param sw_glm_scale              for glm fit, choose results on the "link" (default) or "response" scale
#' @param sw_print                  T/F whether to print results as this function runs
#' @param sw_marginal_even_if_interaction T/F whether to also calculate marginal results when involved in interaction(s)
#' @param sw_TWI_plots_keep         two-way interaction plots are plotted for each variable conditional on the other.  Plots are created separately ("singles") or together in a grid ("both"), and "all" keeps the singles and the grid version.
#' @param sw_TWI_both_orientation   "tall" or "wide" orientation for when both two-way interaction plots are combined in a grid
#' @param sw_plot_quantiles_values  "quantiles" or "values" to specify whether to plot quantiles of the numeric variable or specified values
#' @param plot_quantiles            quantiles plotted for numeric:numeric interaction plots, if \code{sw_plot_quantiles_values} is "quantiles"
#' @param sw_quantile_type          quantile type as specified in \code{?stats::quantile}.
#' @param plot_values               a named list for values plotted for a single specified numeric:numeric interaction plot, if \code{sw_plot_quantiles_values} is "values".  Specify a specific contrast, for example: \code{choose_contrasts = "disp:hp"}.  Then specify the values for each variable, for example: \code{plot_values = list(hp = c(75, 100, 150, 200, 250), disp = c(80, 120, 200, 350, 450))}
#' @param emmip_rg.limit            from emmeans package, increase from 10000 if "Error: The rows of your requested reference grid would be 10000, which exceeds the limit of XXXXX (not including any multivariate responses)."
#'
#' @return out                      a list of three lists: "plots", "tables" , and "text", each have results for each contrast that was computed.  "plots" is a list of ggplot objects to plot separately or arrange in a grid.  "tables" is a list of emmeans tables to print.  "text" is the caption text for the plots.
#' @import dplyr
#' @import ggplot2
#' @import emmeans
#' @importFrom labelled var_label
#' @importFrom stringr fixed
#' @importFrom stringr str_detect
#' @importFrom stringr str_split
#' @importFrom stringr str_split_fixed
#' @importFrom gridExtra arrangeGrob
#' @importFrom ggpubr as_ggplot
#' @importFrom purrr pluck
#' @importFrom stats as.formula
#' @importFrom stats quantile
#' @importFrom stats anova
#' @importFrom stats terms.formula
#' @importFrom cowplot plot_grid
#' @export
#'
#' @examples
#' # Data for testing
#' dat_cont <-
#'   dat_mtcars_e
#'
#' # Set specific model with some interactions
#' form_model <-
#'   mpg ~ cyl + disp + hp + wt + vs + am + cyl:vs + disp:hp + hp:vs
#'
#' fit <-
#'   lm(
#'     formula = form_model
#'   , data    = dat_cont
#'   )
#'
#' car::Anova(fit)  #, type = 3)
#' summary(fit)
#'
#' # all contrasts from model
#' fit_contrasts <-
#'   e_plot_model_contrasts(
#'     fit                = fit
#'   , dat_cont           = dat_cont
#'   , sw_print           = FALSE
#'   , sw_table_in_plot   = FALSE
#'   , sw_TWI_plots_keep  = "singles"
#'   , sw_quantile_type   = 1
#'   )
#' fit_contrasts$tables  # to print tables
#' fit_contrasts$plots   # to print plots
#' fit_contrasts$text    # to print caption text
#'
#'
#' ## To plot all contrasts in a plot grid:
#' # Since plot interactions have sublists of plots, we want to pull those out
#' #   into a one-level plot list.
#' # The code here works for sw_TWI_plots_keep = "singles"
#' #   which will make each plot the same size in the plot_grid() below.
#' # For a publications, you'll want to manually choose which plots to show.
#'
#' # index for plot list,
#' #   needed since interactions add 2 plots to the list, so the number of terms
#' #   is not necessarily the same as the number of plots.
#' i_list <- 0
#' # initialize a list of plots
#' p_list <- list()
#'
#' for (i_term in 1:length(fit_contrasts$plots)) {
#'   ## i_term = 1
#'
#'   # extract the name of the plot
#'   n_list <- names(fit_contrasts$plots)[i_term]
#'
#'   # test whether the name has a colon ":"; if so, it's an interaction
#'   if (stringr::str_detect(string = n_list, pattern = stringr::fixed(":"))) {
#'     # an two-way interaction has two plots
#'
#'     # first plot
#'     i_list <- i_list + 1
#'     p_list[[ i_list ]] <- fit_contrasts$plots[[ i_term ]][[ 1 ]]
#'
#'     # second plot
#'     i_list <- i_list + 1
#'     p_list[[ i_list ]] <- fit_contrasts$plots[[ i_term ]][[ 2 ]]
#'
#'   } else {
#'     # not an interaction, only one plot
#'
#'     i_list <- i_list + 1
#'     p_list[[ i_list ]] <- fit_contrasts$plots[[ i_term ]]
#'
#'   } # if
#' } # for
#'
#' p_arranged <-
#'   cowplot::plot_grid(
#'     plotlist  = p_list
#'   , nrow      = NULL
#'   , ncol      = 3
#'   , labels    = "AUTO"
#'   )
#'
#' p_arranged %>% print()
#'
#'
#'
#'
#'
#' # one specific numeric:numeric contrast with specified values
#' fit_contrasts <-
#'   e_plot_model_contrasts(
#'     fit                     = fit
#'   , dat_cont                = dat_cont
#'   , choose_contrasts        = "disp:hp"
#'   , sw_table_in_plot        = TRUE
#'   , adjust_method           = c("none", "tukey", "scheffe", "sidak", "bonferroni"
#'                                 , "dunnettx", "mvt")[4]  # see ?emmeans::summary.emmGrid
#'   , CI_level                = 0.95
#'   , sw_glm_scale            = c("link", "response")[1]
#'   , sw_print                = FALSE
#'   , sw_marginal_even_if_interaction = TRUE
#'   , sw_TWI_plots_keep       = c("singles", "both", "all")[3]
#'   , sw_TWI_both_orientation = c("tall", "wide")[1]
#'   , sw_plot_quantiles_values = c("quantiles", "values")[2]    # for numeric:numeric plots
#'   , plot_quantiles          = c(0.05, 0.25, 0.50, 0.75, 0.95) # for numeric:numeric plots
#'   , sw_quantile_type        = 1
#'   , plot_values             = list(hp = c(75, 100, 150, 200, 250)
#'                                  , disp = c(80, 120, 200, 350, 450)) # for numeric:numeric plots
#'   )
#' fit_contrasts$plots$`disp:hp`$both
#'
#'
#' \dontrun{
#' ## GLM on logit and probability scales
#'
#' dat_cont <-
#'   dat_cont %>%
#'   dplyr::mutate(
#'     am_01 =
#'       dplyr::case_when(
#'         am == "manual"    ~ 0
#'       , am == "automatic" ~ 1
#'       )
#'   )
#' labelled::var_label(dat_cont[["am_01"]]) <- labelled::var_label(dat_cont[["am"]])
#'
#' # numeric:factor interaction
#' fit_glm <-
#'   glm(
#'     cbind(am_01, 1 - am_01) ~
#'       vs + hp + vs:hp
#'   , family  = binomial
#'   , data    = dat_cont
#'   )
#'
#' car::Anova(fit_glm, type = 3)
#' summary(fit_glm)
#'
#' # all contrasts from model, logit scale
#' fit_contrasts <-
#'   e_plot_model_contrasts(
#'     fit                = fit_glm
#'   , dat_cont           = dat_cont
#'   , sw_glm_scale       = c("link", "response")[1]
#'   , sw_print           = FALSE
#'   , sw_marginal_even_if_interaction = TRUE
#'   , sw_TWI_plots_keep  = "both"
#'   )
#' fit_contrasts
#'
#' # all contrasts from model, probability scale
#' fit_contrasts <-
#'   e_plot_model_contrasts(
#'     fit                = fit_glm
#'   , dat_cont           = dat_cont
#'   , sw_glm_scale       = c("link", "response")[2]
#'   , sw_print           = FALSE
#'   , sw_marginal_even_if_interaction = TRUE
#'   , sw_TWI_plots_keep  = "both"
#'   )
#' fit_contrasts
#'
#'
#' # numeric:numeric interaction
#' fit_glm <-
#'   glm(
#'     cbind(am_01, 1 - am_01) ~
#'       disp + hp + disp:hp
#'   , family  = binomial
#'   , data    = dat_cont
#'   )
#'
#' car::Anova(fit_glm, type = 3)
#' summary(fit_glm)
#'
#' fit_contrasts <-
#'   e_plot_model_contrasts(
#'     fit                = fit_glm
#'   , dat_cont           = dat_cont
#'   , sw_glm_scale       = c("link", "response")[2]
#'   , sw_print           = FALSE
#'   , sw_TWI_plots_keep  = "both"
#'   )
#' fit_contrasts
#'
#'
#' # factor:factor interaction
#' fit_glm <-
#'   glm(
#'     cbind(am_01, 1 - am_01) ~
#'       vs + cyl + vs:cyl
#'   , family  = binomial
#'   , data    = dat_cont
#'   )
#'
#' car::Anova(fit_glm) #, type = 3)
#' summary(fit_glm)
#'
#' fit_contrasts <-
#'   e_plot_model_contrasts(
#'     fit                = fit_glm
#'   , dat_cont           = dat_cont
#'   , sw_glm_scale       = c("link", "response")[2]
#'   , sw_print           = FALSE
#'   , sw_TWI_plots_keep  = "both"
#'   )
#' fit_contrasts
#'
#'
#' ## lme4::lmer mixed-effects model
#' fit_lmer <-
#'   lme4::lmer(
#'     formula = Reaction ~ Days + (Days | Subject)
#'   , data    = lme4::sleepstudy
#'   )
#'
#' fit_lmer_contrasts <-
#'   e_plot_model_contrasts(
#'     fit                = fit_lmer
#'   , dat_cont           = lme4::sleepstudy
#'   , sw_print           = FALSE
#'   , sw_table_in_plot   = FALSE
#'   )
#' fit_lmer_contrasts
#'
#'
#' ## lmerTest::as_lmerModLmerTest mixed-effects model
#'
#' fit_lmer <-
#'   lmerTest::as_lmerModLmerTest(fit_lmer)
#'
#' fit_lmer_contrasts <-
#'   e_plot_model_contrasts(
#'     fit                = fit_lmer
#'   , dat_cont           = lme4::sleepstudy
#'   , sw_print           = FALSE
#'   , sw_table_in_plot   = FALSE
#'   )
#' fit_lmer_contrasts
#' }
#'
e_plot_model_contrasts <-
  function(
    fit                     = NULL
  , dat_cont                = NULL
  , choose_contrasts        = NULL
  , sw_table_in_plot        = TRUE
  , adjust_method           = c("none", "tukey", "scheffe", "sidak", "bonferroni", "dunnettx", "mvt")[4]  # see ?emmeans::summary.emmGrid
  , CI_level                = 0.95
  , sw_glm_scale            = c("link", "response")[1]
  , sw_print                = TRUE
  , sw_marginal_even_if_interaction = FALSE
  , sw_TWI_plots_keep       = c("singles", "both", "all")[3]
  , sw_TWI_both_orientation = c("wide", "tall")[1]
  , sw_plot_quantiles_values = c("quantiles", "values")[1]    # for numeric:numeric plots
  , plot_quantiles          = c(0.05, 0.25, 0.50, 0.75, 0.95) # for numeric:numeric plots
  , sw_quantile_type        = 7
  , plot_values             = NULL                            # for numeric:numeric plots
  , emmip_rg.limit          = 10000
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
  ## sw_plot_quantiles_values = c("quantiles", "values")[1] # for numeric:numeric plots
  ## plot_quantiles = c(0.05, 0.25, 0.50, 0.75, 0.95)  # for numeric:numeric plots
  ## plot_values             = c(-1, 0, 1)             # for numeric:numeric plots
  ##
  ## # Data for testing
  ## dat_cont <-
  ##   dat_mtcars_e %>%
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
  ## ##   stats::as.formula(
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
  ## stats::anova(fit)
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
  ## , plot_quantiles          = c(0.05, 0.25, 0.50, 0.75, 0.95)  # for numeric:numeric plots
  ## )
  ##
  ##
  ## e_plot_model_contrasts(
  ##   fit                     = fit
  ## , dat_cont                = dat_cont
  ## , choose_contrasts        = "disp:hp"
  ## , sw_table_in_plot        = TRUE
  ## , adjust_method           = c("none", "tukey", "scheffe", "sidak", "bonferroni", "dunnettx", "mvt")[4]  # see ?emmeans::summary.emmGrid
  ## , CI_level                = 0.95
  ## , sw_print                = TRUE
  ## , sw_marginal_even_if_interaction = TRUE
  ## , sw_TWI_plots_keep       = c("singles", "both", "all")[1]
  ## , sw_TWI_both_orientation = c("tall", "wide")[1]
  ## , sw_plot_quantiles_values = c("quantiles", "values")[2]    # for numeric:numeric plots
  ## , plot_quantiles          = c(0.05, 0.25, 0.50, 0.75, 0.95) # for numeric:numeric plots
  ## , plot_values             = list(hp = c(75, 100, 150, 200, 250), disp = c(80, 120, 200, 350, 450)) # for numeric:numeric plots
  ## )
  ##
  ##
  ###### END Example dataset for testing

  # give error if two main inputs aren't specified
  if (is.null(fit) | is.null(dat_cont)) {
    stop("erikmisc::e_plot_model_contrasts() \"fit\" and \"dat_cont\" arguments are required.")
    return(NULL)
  }

  # indicates, "lm", "glm", or "lmerMod" (future, another method based on the call)
  fit_model_type <-
    #as.character(fit$call)[1]
    class(fit)[1]

  if (fit_model_type %notin% c("lm", "glm", "lmerModLmerTest", "lmerMod")) {
    message(paste0("e_plot_model_contrasts: fit class\"", fit_model_type, "\" not tested."))
  }

  # BEGIN Capture warnings to take actions
    message_involve_interaction <-
      "NOTE: Results may be misleading due to involvement in interactions\n"
  # END Capture warnings to take actions

  # confidence limits (CL) column names differ depending on method and rank deficiency
  # provide a list of possibilities to look for from the summary tables
  col_names_LCL <- c("lower.CL", "asymp.LCL")
  col_names_UCL <- c("upper.CL", "asymp.UCL")


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
    stringr::str_split(
      string = attr(terms.formula(formula(fit)), "term.labels")   #as.character(formula(fit))[3]
    , pattern = stringr::fixed(" + ")
    ) %>%
    unlist()

  if (fit_model_type == "glm" & stringr::str_detect(var_name_y, pattern = stringr::fixed("cbind("))) {
    # the y-variable usually looks like "cbind(y, 1-y)", so strip out the variable name
    var_name_y <-
      var_name_y %>%
      stringr::str_split_fixed(
        pattern = stringr::fixed(",")
      , n = 2
      ) %>%
      as.character() %>%
      purrr::pluck(1) %>%
      stringr::str_split_fixed(
        pattern = stringr::fixed("cbind(")
      , n = 2
      ) %>%
      as.character() %>%
      purrr::pluck(2)
  }


  # restrict to chosen contrasts
  if (!is.null(choose_contrasts)) {

    # for chosen contrasts, copy and reverse order of two-way interactions
    # to make sure there's a match
      ## https://stackoverflow.com/questions/49141253/how-to-reverse-the-delimited-parts-of-a-string

    choose_contrasts_rev <-
      sapply(
        stringr::str_split(
          string  = choose_contrasts
        , pattern = stringr::fixed(":")
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
      , pattern = stringr::fixed(":")
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

      ### if numeric
      if ( inherits(dat_cont[[var_xs]], c("numeric", "integer")) ) {

        ## Table
        if (fit_model_type == "glm" & sw_glm_scale == "response") {
          # response scale
          cont_fit <-
            emmeans::emtrends(
              object  = fit
            , specs   = var_xs
            , var     = var_xs
            #, transform = "response" # updated in emmeans 1.7.3
            , regrid  = "response"
            )
        } else {
          # default scale
          cont_fit <-
            emmeans::emtrends(
              object  = fit
            , specs   = var_xs
            , var     = var_xs
            ##, transform = "response" # updated in emmeans 1.7.3
            #, regrid  = "response"
            )
        }

        # confidence limit (CL) column names
        col_name_LCL <- names(summary(cont_fit))[which(names(summary(cont_fit)) %in% col_names_LCL)]
        col_name_UCL <- names(summary(cont_fit))[which(names(summary(cont_fit)) %in% col_names_UCL)]

        out[["tables"]][[ var_name_x[i_var_x] ]][["est"  ]] <- cont_fit
        out[["tables"]][[ var_name_x[i_var_x] ]][["cont" ]] <- NULL


        ## Plot
        # CI text
            # is.null is for when no values could be estimated
        #CLs#if(!is.null(summary(cont_fit)[["lower.CL"]])) {
        if(!is.null(summary(cont_fit)[[col_name_LCL]])) {
          text_cont <- summary(cont_fit)[[ var_xs[1] ]]
          ind_trend <- which(stringr::str_detect(names(summary(cont_fit)), stringr::fixed(".trend")))
          text_est  <- summary(cont_fit)[[ind_trend]]  # 2
          text_LCL  <- summary(cont_fit)[[col_name_LCL]]
          text_UCL  <- summary(cont_fit)[[col_name_UCL]]
          text_CI  <-
            paste0(
              "Estimate: "
            , "(at mean = ", signif(text_cont, 3), ")"
            , ": "
            , signif(text_est, 3)
            , ", "
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

        form_var <- stats::as.formula(paste0("~", var_xs))

        if (fit_model_type == "glm" & sw_glm_scale == "response") {
          ## ?emmeans::ref_grid for cov.reduce option
          # response scale
          at_list <- list()
          at_list[[var_xs]] <- unique(quantile(dat_cont[[var_xs]], probs = seq(0, 1, by = 0.01)))

          p <-
            emmeans::emmip(
              object     = fit
            , formula    = form_var
            #, cov.reduce = range
            , at         = at_list
            , rg.limit   = emmip_rg.limit
            #, transform = "response" # updated in emmeans 1.7.3
            , regrid  = "response"
            )

        } else {
          # default scale
          p <-
            emmeans::emmip(
              object     = fit
            , formula    = form_var
            , cov.reduce = range
            , rg.limit   = emmip_rg.limit
            )
        }
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

        if (!(fit_model_type == "glm")) {
          y_label <- paste0("Linear prediction of:\n", labelled::var_label(dat_cont[[var_name_y]]) %>% as.character() )
        } else {
          if (sw_glm_scale == "response") {
            # response scale
            y_label <- paste0("(Response-scale) Linear prediction of:\n", labelled::var_label(dat_cont[[var_name_y]]) %>% as.character())
          } else {
            # default scale
            y_label <- paste0("(Link-scale) Linear prediction of:\n", labelled::var_label(dat_cont[[var_name_y]]) %>% as.character())
          }
        }

        p <- p + labs(
            title     = paste0("Main effect of ", labelled::var_label(dat_cont[[var_xs]]) %>% as.character())
          , subtitle  = var_name_x[i_var_x]
          , x         = labelled::var_label(dat_cont[[var_xs]]) %>% as.character()
          , y         = y_label
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




      ### if factor
      if ( inherits(dat_cont[[var_xs]], "factor") ) {

        ## Table
        # if a factor, then compute the contrast and statistics and create plot
        if (fit_model_type == "glm" & sw_glm_scale == "response") {
          # response scale
          cont_fit <-
            emmeans::emmeans(
              object  = fit
            , specs   = var_xs
            #, by    =
            #, simple = "each", combine = FALSE
            , adjust  = adjust_method
            , level   = CI_level
            , type = "response"
            )
        } else {
          # default scale
          cont_fit <-
            emmeans::emmeans(
              object  = fit
            , specs   = var_xs
            #, by    =
            #, simple = "each", combine = FALSE
            , adjust  = adjust_method
            , level   = CI_level
            #, type = "response"
            )
        }
        cont_pairs <- cont_fit %>% pairs(adjust = adjust_method)

        # confidence limit (CL) column names
        col_name_LCL <- names(summary(cont_fit))[which(names(summary(cont_fit)) %in% col_names_LCL)]
        col_name_UCL <- names(summary(cont_fit))[which(names(summary(cont_fit)) %in% col_names_UCL)]

        if(sw_print) {
          cont_fit   %>% print()
          cont_pairs %>% print()
        }

        out[["tables"]][[ var_name_x[i_var_x] ]][["est"  ]] <- cont_fit
        out[["tables"]][[ var_name_x[i_var_x] ]][["cont" ]] <- cont_pairs

        #CLs#col_name_LCL <- "lower.CL"
        #CLs#col_name_UCL <- "upper.CL"
        #CLs#
        #CLs## ## lmer fit issue can cause a cont_fit to not provide an estimate
        #CLs## #     fit warnings:
        #CLs## #     fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
        #CLs## # if the estimate is NA, then the column headers differ, too.
        #CLs#if (!(col_name_LCL %in% names(summary(cont_fit)))) {
        #CLs#  col_name_LCL <- "asymp.LCL"
        #CLs#  col_name_UCL <- "asymp.UCL"
        #CLs#}

        ## Plot
        # CI text
        text_cont <- summary(cont_fit)[[var_xs]]
        if (fit_model_type == "glm" & sw_glm_scale == "response") {
          # response scale
          text_est  <- summary(cont_fit)[["prob"]]
        } else {
          # default scale
          text_est  <- summary(cont_fit)[["emmean"]]
        }
        text_LCL  <- summary(cont_fit)[[col_name_LCL]]
        text_UCL  <- summary(cont_fit)[[col_name_UCL]]
        text_CI  <-
          paste0(
            "Estimate: "
          , text_cont
          , " = "
          , signif(text_est, 3)
          , ", "
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
        if (fit_model_type == "glm" & sw_glm_scale == "response") {
          # response scale
          text_est  <- summary(cont_pairs)[["odds.ratio"]]
        } else {
          # default scale
          text_est  <- summary(cont_pairs)[["estimate"]]
        }
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
          #, type = "scale" #"response"
          )

        if (!(fit_model_type == "glm")) {
          x_label <- paste0("Estimate of:\n", labelled::var_label(dat_cont[[var_name_y]]) %>% as.character())
        } else {
          if (sw_glm_scale == "response") {
            # response scale
            x_label <- paste0("(Response-scale) Estimate of:\n", labelled::var_label(dat_cont[[var_name_y]]) %>% as.character())
          } else {
            # default scale
            x_label <- paste0("(Link-scale) Estimate of:\n", labelled::var_label(dat_cont[[var_name_y]]) %>% as.character())
          }
        }

        p <- p + labs(
            title     = paste0("Main effect of ", labelled::var_label(dat_cont[[var_xs]]) %>% as.character())
          , subtitle  = var_name_x[i_var_x]
          , x         = x_label
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
      if (all(inherits(dat_cont[[ var_xs[1] ]], "factor"), inherits(dat_cont[[ var_xs[2] ]], "factor"))) {

        # do this twice, reversing the order of the factors
        for (i_repeat in 1:2) {
          ## i_repeat = 1

          ## Repeat, but reverse factors
          if (i_repeat == 2) {
            var_xs <- rev(var_xs)
          }

          ## Table
          # if a factor, then compute the contrast and statistics and create plot
          if (fit_model_type == "glm" & sw_glm_scale == "response") {
            # response scale
            cont_fit <-
              emmeans::emmeans(
                object  = fit
              , specs   = var_xs[1]
              , by      = var_xs[2]
              #, simple = "each", combine = FALSE
              , adjust  = adjust_method
              , level   = CI_level
              , type = "response"
              )
          } else {
            # default scale
            cont_fit <-
              emmeans::emmeans(
                object  = fit
              , specs   = var_xs[1]
              , by      = var_xs[2]
              #, simple = "each", combine = FALSE
              , adjust  = adjust_method
              , level   = CI_level
              )
          }
          cont_pairs <- cont_fit %>% pairs(adjust = adjust_method)

          # confidence limit (CL) column names
          col_name_LCL <- names(summary(cont_fit))[which(names(summary(cont_fit)) %in% col_names_LCL)]
          col_name_UCL <- names(summary(cont_fit))[which(names(summary(cont_fit)) %in% col_names_UCL)]

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
          # CI and Contrast text
          #fix20230214# text_CI   <- NULL
          #fix20230214# text_diff <- NULL
          #fix20230214# i_row_cont_fit = 0
          #fix20230214# for (i_by in seq_along(levels_by)) {
          #fix20230214#   ## i_by = 1
          #fix20230214#
          #fix20230214#   text_CI  <-
          #fix20230214#     paste0(
          #fix20230214#       text_CI
          #fix20230214#     , paste0(var_xs[2], " = ", levels_by[i_by], ":\n")
          #fix20230214#     )
          #fix20230214#   text_diff  <-
          #fix20230214#     paste0(
          #fix20230214#       text_diff
          #fix20230214#     , paste0(var_xs[2], " = ", levels_by[i_by], ":\n")
          #fix20230214#     )
          #fix20230214#
          #fix20230214#   for (i_specs in seq_along(levels_specs)) {
          #fix20230214#     ## i_spects = 1
          #fix20230214#
          #fix20230214#     i_row_cont_fit = i_row_cont_fit + 1
          #fix20230214#
          #fix20230214#     # CI text
          #fix20230214#     text_cont <- summary(cont_fit)[[ var_xs[1] ]] [i_row_cont_fit]
          #fix20230214#     if (fit_model_type == "glm" & sw_glm_scale == "response") {
          #fix20230214#       # response scale
          #fix20230214#       text_est  <- summary(cont_fit)[["prob"]]      [i_row_cont_fit]
          #fix20230214#     } else {
          #fix20230214#       # default scale
          #fix20230214#       text_est  <- summary(cont_fit)[["emmean"]]    [i_row_cont_fit]
          #fix20230214#     }
          #fix20230214#     text_LCL  <- summary(cont_fit)[[col_name_LCL]][i_row_cont_fit]
          #fix20230214#     text_UCL  <- summary(cont_fit)[[col_name_UCL]][i_row_cont_fit]
          #fix20230214#     text_CI  <-
          #fix20230214#       paste0(
          #fix20230214#         text_CI
          #fix20230214#       , "Estimate: "
          #fix20230214#       , text_cont
          #fix20230214#       , " = "
          #fix20230214#       , signif(text_est, 3)
          #fix20230214#       , ", "
          #fix20230214#       , round(CI_level * 100, 1), "% CI: "
          #fix20230214#       , "("
          #fix20230214#       , signif(text_LCL, 3)
          #fix20230214#       , ", "
          #fix20230214#       , signif(text_UCL, 3)
          #fix20230214#       , ")"
          #fix20230214#       , "\n"
          #fix20230214#       #, collapse = "\n"
          #fix20230214#       )
          #fix20230214#
          #fix20230214#     # Contrast text
          #fix20230214#     text_cont <- summary(cont_pairs)[["contrast"]][i_row_cont_fit]
          #fix20230214#     if (fit_model_type == "glm" & sw_glm_scale == "response") {
          #fix20230214#       # response scale
          #fix20230214#       text_est  <- summary(cont_pairs)[["odds.ratio"]][i_row_cont_fit]
          #fix20230214#     } else {
          #fix20230214#       # default scale
          #fix20230214#       text_est  <- summary(cont_pairs)[["estimate"]][i_row_cont_fit]
          #fix20230214#     }
          #fix20230214#     text_pval <- summary(cont_pairs)[["p.value"]] [i_row_cont_fit]
          #fix20230214#     text_diff  <-
          #fix20230214#       paste0(
          #fix20230214#         text_diff
          #fix20230214#       , "Contrast: "
          #fix20230214#       , text_cont
          #fix20230214#       , " = "
          #fix20230214#       , signif(text_est, 3)
          #fix20230214#       , ", p-value = "
          #fix20230214#       , round(text_pval, 4)
          #fix20230214#       , "\n"
          #fix20230214#       #, collapse = "\n"
          #fix20230214#       )
          #fix20230214#
          #fix20230214#   } # i_specs
          #fix20230214# } # i_by

          text_CI   <- NULL
          text_diff <- NULL
          i_row_cont_fit = 0
          for (i_by in seq_along(levels_by)) {
            ## i_by = 1

            text_CI  <-
              paste0(
                text_CI
              , paste0(var_xs[2], " = ", levels_by[i_by], ":\n")
              )
            text_diff  <-
              paste0(
                text_diff
              , paste0(var_xs[2], " = ", levels_by[i_by], ":\n")
              )

            ## Estimates
            # CI text
            summary_cont_fit <-
              cont_fit %>%
              summary() %>%
              as.data.frame()
            # only rows of this "by" variable
            summary_cont_fit_by <-
              summary_cont_fit[summary_cont_fit[[ var_xs[2] ]] == levels_by[i_by], ]

            for (i_row in seq_len(nrow(summary_cont_fit_by))) {
              # i_row = 1

              text_cont <- summary_cont_fit_by[[ var_xs[1] ]] [i_row]
              if (fit_model_type == "glm" & sw_glm_scale == "response") {
                # response scale
                text_est  <- summary_cont_fit_by[["prob"]]      [i_row]
              } else {
                # default scale
                text_est  <- summary_cont_fit_by[["emmean"]]    [i_row]
              }
              text_LCL  <- summary_cont_fit_by[[col_name_LCL]][i_row]
              text_UCL  <- summary_cont_fit_by[[col_name_UCL]][i_row]
              text_CI  <-
                paste0(
                  text_CI
                , "Estimate: "
                , text_cont
                , " = "
                , signif(text_est, 3)
                , ", "
                , round(CI_level * 100, 1), "% CI: "
                , "("
                , signif(text_LCL, 3)
                , ", "
                , signif(text_UCL, 3)
                , ")"
                , "\n"
                #, collapse = "\n"
                )
            } # i_row


            ## Contrasts
            # CI text
            summary_cont_pairs <-
              cont_pairs %>%
              summary() %>%
              as.data.frame()
            # only rows of this "by" variable
            summary_cont_pairs_by <-
              summary_cont_pairs[summary_cont_pairs[[ var_xs[2] ]] == levels_by[i_by], ]

            for (i_row in seq_len(nrow(summary_cont_pairs_by))) {
              # i_row = 1

              # Contrast text
              text_cont <- summary_cont_pairs_by[["contrast"]][i_row]
              if (fit_model_type == "glm" & sw_glm_scale == "response") {
                # response scale
                text_est  <- summary_cont_pairs_by[["odds.ratio"]][i_row]
              } else {
                # default scale
                text_est  <- summary_cont_pairs_by[["estimate"]][i_row]
              }
              text_pval <- summary_cont_pairs_by[["p.value"]] [i_row]
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
            } # i_row

          } # i_by

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
          } # if sw_try_ok

          if (!(fit_model_type == "glm")) {
            x_label <- paste0("Estimate of:\n", labelled::var_label(dat_cont[[var_name_y]]) %>% as.character())
          } else {
            if (sw_glm_scale == "response") {
              # response scale
              x_label <- paste0("(Response-scale) Estimate of:\n", labelled::var_label(dat_cont[[var_name_y]]) %>% as.character())
            } else {
              # default scale
              x_label <- paste0("(Link-scale) Estimate of:\n", labelled::var_label(dat_cont[[var_name_y]]) %>% as.character())
            }
          }

          p <- p + labs(
              title     = paste0("Interaction of ", labelled::var_label(dat_cont[[ var_xs[1] ]]) %>% as.character(), " and ", labelled::var_label(dat_cont[[ var_xs[2] ]]) %>% as.character())
            , subtitle  = var_name_x[i_var_x]
            , x         = x_label
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
      if (any(inherits(dat_cont[[ var_xs[1] ]], "factor"), inherits(dat_cont[[ var_xs[2] ]], "factor") ) &
          any(inherits(dat_cont[[ var_xs[1] ]], c("numeric", "integer")), inherits(dat_cont[[ var_xs[2] ]], c("numeric", "integer")) )
        ) {

        # make first variable the factor and second numeric
        if(inherits(dat_cont[[ var_xs[2] ]], "factor")) {
          var_xs <- rev(var_xs)
        }

        ## Table
        form_var_fac <- stats::as.formula(paste0("pairwise", " ~ ", var_xs[1]))

        if (fit_model_type == "glm" & sw_glm_scale == "response") {
          # response scale
          cont_fit <-
            emmeans::emtrends(
              object  = fit
            , specs   = form_var_fac
            , var     = var_xs[2]
            , adjust  = adjust_method
            , level   = CI_level
            #, transform = "response" # updated in emmeans 1.7.3
            , regrid  = "response"
            )
        } else {
          # default scale
          cont_fit <-
            emmeans::emtrends(
              object  = fit
            , specs   = form_var_fac
            , var     = var_xs[2]      ## 8/18/2022 1:29PM XXX Should this be "3"
            , adjust  = adjust_method
            , level   = CI_level
            ##, transform = "response" # updated in emmeans 1.7.3
            #, regrid  = "response"
            )
        }

        # confidence limit (CL) column names
        col_name_LCL <- names(summary(cont_fit)$emtrends)[which(names(summary(cont_fit)$emtrends) %in% col_names_LCL)]
        col_name_UCL <- names(summary(cont_fit)$emtrends)[which(names(summary(cont_fit)$emtrends) %in% col_names_UCL)]

        out[["tables"]][[ var_name_x[i_var_x] ]][["est"  ]] <- cont_fit$emtrends
        out[["tables"]][[ var_name_x[i_var_x] ]][["cont" ]] <- cont_fit$contrasts

        ## Plot
        # CI text
            # is.null is for when no values could be estimated
        #CLs#if(!is.null(summary(cont_fit$emtrends)[["lower.CL"]])) {
        if(!is.null(summary(cont_fit$emtrends)[[col_name_LCL]])) {
          text_cont <- summary(cont_fit$emtrends)[[ var_xs[1] ]]
          ind_trend <- which(stringr::str_detect(names(summary(cont_fit$emtrends)), stringr::fixed(".trend")))
          text_est  <- summary(cont_fit$emtrends)[[ind_trend]]  # 2
          text_LCL  <- summary(cont_fit$emtrends)[[col_name_LCL]]
          text_UCL  <- summary(cont_fit$emtrends)[[col_name_UCL]]
          text_CI  <-
            paste0(
              "Estimate: "
            , text_cont
            , " = "
            , signif(text_est, 3)
            , ", "
            , round(CI_level * 100, 1), "% CI: "
            , "("
            , signif(text_LCL, 3)
            , ", "
            , signif(text_UCL, 3)
            , ")"
            , collapse = "\n"
            )

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

        } else {
          text_CI <- NULL
        }

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



        form_var_fac_num <- stats::as.formula(paste0(var_xs[1], " ~ ", var_xs[2]))

        # 12/20/2021 removed because it was removing the table in plot
        # text_long <-
        #   paste0(
        #     text_averaged
        #   )
        # text_short <-
        #   paste0(
        #     text_averaged
        #   )
        # if (sw_table_in_plot) {
        #   text_caption <- text_long
        # } else {
        #   text_caption <- text_short
        # }



        if (fit_model_type == "glm" & sw_glm_scale == "response") {
          ## ?emmeans::ref_grid for cov.reduce option
          # response scale
          at_list <- list()
          at_list[[var_xs[2]]] <- unique(quantile(dat_cont[[var_xs[2]]], probs = seq(0, 1, by = 0.01)))

          p2 <- emmeans::emmip(
              object     = fit
            , formula    = form_var_fac_num
            #, cov.reduce = range
            , at = at_list
            , rg.limit   = emmip_rg.limit
            #, transform = "response" # updated in emmeans 1.7.3
            , regrid  = "response"
            )

        } else {
          # default scale
          p2 <- emmeans::emmip(
              object     = fit
            , formula    = form_var_fac_num
            , cov.reduce = range
            , rg.limit   = emmip_rg.limit
            )

        }

        if (!(fit_model_type == "glm")) {
          y_label <- paste0("Linear prediction of:\n", labelled::var_label(dat_cont[[var_name_y]]) %>% as.character() )
        } else {
          if (sw_glm_scale == "response") {
            # response scale
            y_label <- paste0("(Response-scale) Linear prediction of:\n", labelled::var_label(dat_cont[[var_name_y]]) %>% as.character())
          } else {
            # default scale
            y_label <- paste0("(Link-scale) Linear prediction of:\n", labelled::var_label(dat_cont[[var_name_y]]) %>% as.character())
          }
        }

        p2 <- p2 + theme_bw()
        p2 <- p2 + labs(
            title     = paste0("Interaction of ", labelled::var_label(dat_cont[[ var_xs[1] ]]) %>% as.character(), " and ", labelled::var_label(dat_cont[[ var_xs[2] ]]) %>% as.character())
          , subtitle  = var_name_x[i_var_x]
          , x         = labelled::var_label(dat_cont[[ var_xs[2] ]]) %>% as.character()
          , y         = y_label
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
      if ( all(inherits(dat_cont[[ var_xs[1] ]], c("numeric", "integer")), inherits(dat_cont[[ var_xs[2] ]], c("numeric", "integer"))) ) {

        # do this twice, reversing the order of the factors
        for (i_repeat in 1:2) {
          ## i_repeat = 2

          ## Repeat, but reverse factors
          if (i_repeat == 2) {
            var_xs <- rev(var_xs)
          }

          form_var_num_num <- stats::as.formula(paste0(var_xs[1], " ~ ", var_xs[2]))

          # this is the common every-variable situation
          if (sw_plot_quantiles_values == "quantiles" | is.null(plot_values)) {
            # values of numeric variables for plotting
            at_list <- list()
            at_list[[ var_xs[1] ]] <-
              dat_cont[[ var_xs[1] ]] %>% stats::quantile(probs = plot_quantiles, type = sw_quantile_type) %>% unique()
            at_list[[ var_xs[2] ]] <-
              #dat_cont[[ var_xs[2] ]] %>% stats::quantile(probs = plot_quantiles, type = sw_quantile_type) %>% unique()
              dat_cont[[ var_xs[2] ]] %>% stats::quantile(probs = seq(0, 1, by = 0.01)) %>% unique()
          }
          # this is for a specific numeric:numeric interaction
          if (sw_plot_quantiles_values == "values") {
            # values of numeric variables for plotting
            at_list <- list()
            at_list[[ var_xs[1] ]] <-
              plot_values[[ var_xs[1] ]]
            at_list[[ var_xs[2] ]] <-
              plot_values[[ var_xs[2] ]]
          }


          if (fit_model_type == "glm" & sw_glm_scale == "response") {
            ## ?emmeans::ref_grid for cov.reduce option
            # response scale
            fit_emm_at <-
              emmeans::ref_grid(
                object  = fit
              , at      = at_list
              #, type    = "response"
              #, transform = "response" # updated in emmeans 1.7.3
              , regrid  = "response"
              )

          } else {
            # default scale
            fit_emm_at <-
              emmeans::ref_grid(
                object  = fit
              , at      = at_list
              )
          }


          ## Table

          #form_var_fac <- stats::as.formula(paste0("pairwise", " ~ ", var_xs[1]))

          out[["tables"]][[ var_name_x[i_var_x] ]][["est"  ]] <- NULL
          out[["tables"]][[ var_name_x[i_var_x] ]][["cont" ]] <- NULL


          ## Plot
          p <- emmeans::emmip(
              object    = fit_emm_at
            , formula   = form_var_num_num
            #, cov.reduce = range  #not originally here, but maybe belongs
            , rg.limit  = emmip_rg.limit
            )

          text_averaged_plot <-
            attributes(p$data)$mesg

          # this is the common every-variable situation
          if (sw_plot_quantiles_values == "quantiles" | is.null(plot_values)) {
            text_long <-
              paste0(
                paste0("Quantiles plotted: ", paste(plot_quantiles, collapse = ", "), ";\n  may be fewer if quantiles are not unique values.")
              , "\n"
              , text_averaged_plot
              )
            text_short <-
              paste0(
                paste0("Quantiles plotted: ", paste(plot_quantiles, collapse = ", "), ";\n  may be fewer if quantiles are not unique values.")
              , "\n"
              , text_averaged_plot
              )
          }
          # this is for a specific numeric:numeric interaction
          if (sw_plot_quantiles_values == "values") {
            text_long <-
              paste0(
                paste0("Specified values plotted.")
              , "\n"
              , text_averaged_plot
              )
            text_short <-
              paste0(
                paste0("Specified values plotted.")
              , "\n"
              , text_averaged_plot
              )
          }

          if (sw_table_in_plot) {
            text_caption <- text_long
          } else {
            text_caption <- text_short
          }

          if (!(fit_model_type == "glm")) {
            y_label <- paste0("Linear prediction of:\n", labelled::var_label(dat_cont[[var_name_y]]) %>% as.character() )
          } else {
            if (sw_glm_scale == "response") {
              # response scale
              y_label <- paste0("(Response-scale) Linear prediction of:\n", labelled::var_label(dat_cont[[var_name_y]]) %>% as.character())
            } else {
              # default scale
              y_label <- paste0("(Link-scale) Linear prediction of:\n", labelled::var_label(dat_cont[[var_name_y]]) %>% as.character())
            }
          }

          p <- p + labs(
              title     = paste0("Interaction of ", labelled::var_label(dat_cont[[ var_xs[1] ]]) %>% as.character(), " and ", labelled::var_label(dat_cont[[ var_xs[2] ]]) %>% as.character())
            , subtitle  = var_name_x[i_var_x]
            , x         = labelled::var_label(dat_cont[[ var_xs[2] ]]) %>% as.character()
            , y         = y_label
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

