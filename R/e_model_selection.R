#' Model selection function for lm and glm, using \code{step} via AIC/BIC or best subset.
#'
#' (Best subset not yet implemented.)
#'
#' @param form                formula for a model
#' @param dat                 data to use
#' @param sw_model            type of regression model, \code{"lm"} or \code{"glm"}
#' @param sw_sel_type         type of model selection.  \code{step} starts with specified \code{form} model with upper = two-way effects model and lower = 1 (intercept-only) model
#' @param sw_step_direction   for \code{step}, the \code{direction} argument from the \code{stats::step()} function
#' @param sw_step_k           for \code{step} determined by "AIC" (2), "BIC" (log(n)), or specified numerically; the \code{k} argument from the \code{stats::step()} function for penalty df
#' @param sw_glm_scale        Scale for glm contrasts, \code{"link"} or \code{"response"}, when \code{sw_model} is \code{"glm"}
#' @param sw_plot_missing     output plots from \code{e_plot_missing}
#' @param sw_plot_y_covar     output plots from \code{e_plot_lm_y_covar}
#' @param sw_plot_x_corr      output correlation matrix plot
#' @param sw_print_results    print results before returning object
#' @param ...                 options sent to \code{e_model_all_subsets_formula} for subsets to consider
#'
#' @return out                list object with all results organized by \code{init} and \code{sel} for initial and selected models
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom tidyselect one_of all_of where
#' @importFrom forcats fct_drop
#' @importFrom patchwork wrap_plots plot_annotation
#' @importFrom Hmisc rcorr
#' @importFrom corrplot cor.mtest corrplot
#' @importFrom cowplot as_grob plot_grid
#' @importFrom car Anova
#' @importFrom stats step
#' @import dplyr
#' @import ggplot2
#' @import stats
#' @export
#'
#' @examples
#' \dontrun{
#' ## lm example
#' dat_sel <-
#'    erikmisc::dat_mtcars_e
#' form_model <-
#'   mpg ~ cyl + disp + hp + wt + vs + am + cyl:vs + disp:hp + hp:vs
#'
#' out <-
#'   e_model_selection(
#'     form = form_model
#'   , dat  = dat_sel
#'   )
#' # out |> print()
#'
#'
#' ## glm example
#' dat_sel <-
#'   erikmisc::dat_mtcars_e |>
#'   dplyr::mutate(
#'     am_01 =
#'       dplyr::case_when(
#'         am == "manual"    ~ 0
#'       , am == "automatic" ~ 1
#'       )
#'   )
#' labelled::var_label(dat_sel[["am_01"]]) <- labelled::var_label(dat_sel[["am"]])
#' form_model <-
#'   cbind(am_01, 1 - am_01) ~ cyl + disp + hp + wt + vs + hp:vs
#'
#' out <-
#'   e_model_selection(
#'     form       = form_model
#'   , dat        = dat_sel
#'   , sw_model   = c("lm", "glm")[2]
#'   , sw_step_k  = c("AIC", "BIC", 2)[2]
#'   )
#' # out |> print()
#' }
e_model_selection <-
  function(
    form              = NULL
  , dat               = NULL
  , sw_model          = c("lm", "glm")[1]
  , sw_sel_type       = c("step", "bestsubset")[1]
  , sw_step_direction = c("both", "backward", "forward")[1]
  , sw_step_k         = c("AIC", "BIC", 2)[1]
  , sw_glm_scale      = c("link", "response")[2]
  , sw_plot_missing   = c(TRUE, FALSE)[1]
  , sw_plot_y_covar   = c(TRUE, FALSE)[1]
  , sw_plot_x_corr    = c(TRUE, FALSE)[1]
  , sw_print_results  = c(TRUE, FALSE)[1]
  , ...
  ) {
  #### lm example
  ## form              = mpg ~ cyl + disp + hp + wt + vs + am + cyl:vs + disp:hp + hp:vs
  ## dat               = erikmisc::dat_mtcars_e
  ## sw_model          = c("lm", "glm")[1]
  ## sw_sel_type       = c("step", "bestsubset")[1]
  ## sw_step_direction = c("both", "backward", "forward")[1]
  ## sw_step_k         = "BIC"
  ## sw_glm_scale      = c("link", "response")[2]
  ## sw_plot_missing   = c(TRUE, FALSE)[1]
  ## sw_plot_y_covar   = c(TRUE, FALSE)[1]
  ## sw_plot_x_corr    = c(TRUE, FALSE)[1]
  ## sw_print_results  = c(TRUE, FALSE)[1]
  ##
  #### glm example
  ## dat_sel <-
  ##    erikmisc::dat_mtcars_e
  ## dat_sel <-
  ##   dat_sel |>
  ##   dplyr::mutate(
  ##     am_01 =
  ##       dplyr::case_when(
  ##         am == "manual"    ~ 0
  ##       , am == "automatic" ~ 1
  ##       )
  ##   )
  ## labelled::var_label(dat_sel[["am_01"]]) <- labelled::var_label(dat_sel[["am"]])
  ## form              = cbind(am_01, 1 - am_01) ~ cyl + disp + hp + wt + vs + hp:vs
  ## dat               = dat_sel
  ## sw_model          = c("lm", "glm")[2]
  ## sw_sel_type       = c("step", "bestsubset")[1]
  ## sw_step_direction = c("both", "backward", "forward")[1]
  ## sw_step_k         = "BIC"
  ## sw_glm_scale      = c("link", "response")[2]
  ## sw_plot_missing   = c(TRUE, FALSE)[1]
  ## sw_plot_y_covar   = c(TRUE, FALSE)[1]
  ## sw_plot_x_corr    = c(TRUE, FALSE)[1]
  ## sw_print_results  = c(TRUE, FALSE)[1]

  # store results in a list
  out <- list()

  # if formula is specified
  if (!is.null(form)){
    # decompose formula into each covariate
    # identify response and main effect terms from the formula
    form_terms <-
      form |>
      stats::terms() |>
      attr("variables") |>
      as.character()
    # remove "list" artifact element from as.character() and the random effect
    ind_form_terms <-
      stringr::str_detect(string = form_terms, pattern = "list", negate = TRUE)
    form_terms <-
      form_terms[ind_form_terms]
    ind_form_terms_covar <-
      stringr::str_detect(string = form_terms, pattern = stringr::fixed(form_terms[1]), negate = TRUE)

    y_var_name <-
      form_terms[1]
    x_var_names <-
      form_terms[ind_form_terms_covar]

    if (sw_model == c("lm", "glm")[2]) {
      y_var_name_components <-
        stringr::str_split(
          string    = y_var_name
        , pattern   = "\\(|,"
        ) |>
        unlist()
      if (y_var_name_components[1] == "cbind") {
        y_var_name <-
          y_var_name_components[2]
      }
    } # glm

  } # form

  dat_sel__ <-
    dat |>
    # subset to these columns
    dplyr::select(
      tidyselect::all_of(y_var_name)
    , tidyselect::all_of(x_var_names)
    ) |>
    # convert character to factor
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.character)
      , as.factor
      )
    # Drop unused factor levels
    , dplyr::across(
        tidyselect::where(is.factor)
      , forcats::fct_drop
      )
    )

  # remove factors with only one level
  for (i_covar in seq_along(x_var_names)) {
    ## i_covar = 5
    if (is.factor (dat_sel__[[ x_var_names[i_covar] ]]) |
        is.ordered(dat_sel__[[ x_var_names[i_covar] ]])) {

      if (length(levels(dat_sel__[[ x_var_names[i_covar] ]])) <= 1) {
        warning(paste0("erikmisc::e_model_selection, removing x-var with only 1 level: ", x_var_names[i_covar]))

        dat_sel__[[ x_var_names[i_covar] ]] <- NULL
        x_var_names[i_covar] <- NA
      }
    }
  }
  x_var_names <- x_var_names[!is.na(x_var_names)]


  # Plot missing values
  if (sw_plot_missing) {
    out[["plot_missing"]] <-
      e_plot_missing(
        dat_sel__
      )
  } # sw_plot_missing


  # Drop missing values
  dat_sel__ <-
    dat_sel__ |>
    tidyr::drop_na()

  out[["data"]] <-
    dat_sel__

  if (length(x_var_names) == 0) {
    warning(paste0("erikmisc::e_model_selection, No predictors (x) with more than 1 level -- skip analysis."))
    return(NULL)
  }

  # Plot y vs each x
  if (sw_plot_y_covar) {
    # Simple method
    out[["plot_covar"]] <-
      e_plot_lm_y_covar(
        form  = form
      , dat   = dat_sel__
      , sw_version    = c("simple", "detail")[1]
      )

    # Detailed method
    out[["plot_scatterplots"]] <-
      e_plot_lm_y_covar(
        form  = form
      , dat   = dat_sel__
      , sw_version    = c("simple", "detail")[2]
      )

  } # sw_plot_y_covar

  # Plot correlations of xs
  if (sw_plot_x_corr) {

    out[["plot_corrplot"]] <-
      dat_sel__ |>
      e_plot_corr_matrix(sw_plot_type = "mixed")

  } # sw_plot_x_corr


  # Full models
  form_init__ <-
    form
    ## this code is main-effects model
    # paste0(
    #   y_var_name
    # , " ~ "
    # , paste(
    #     x_var_names
    #   , collapse = " + "
    #   )
    # ) |>
    # stats::formula()

  # Needs to be in global environment
  assign(x = "form_init__", value = form_init__, envir = .GlobalEnv)


  # Fit initial model
  if (sw_model == c("lm", "glm")[1]) {
    out[["init"]][["fit"]] <-
      stats::lm(
        formula = form_init__
      , data    = dat_sel__
      )
  } # sw_model "lm"
  if (sw_model == c("lm", "glm")[2]) {
    out[["init"]][["fit"]] <-
      stats::glm(
        formula = form_init__
      , data    = dat_sel__
      , family  = binomial(link = logit)
      )
  } # sw_model "glm"

  out[["init"]][["anova"]] <-
    car::Anova(
      out[["init"]][["fit"]]
    , type = 3
    , singular.ok = TRUE
    )
  out[["init"]][["summary"]] <-
    summary(
      out[["init"]][["fit"]]
    )
  if (sw_model == c("lm", "glm")[1]) {
    out[["init"]][["criteria"]] <-
      e_lm_model_criteria(
        lm_fit  = out[["init"]][["fit"]]
      , dat_fit = dat_sel__
      )
    out[["init"]][["plot_diagnostics"]] <-
      cowplot::as_grob(
        ~e_plot_lm_diagnostics(
            fit         = out[["init"]][["fit"]]
          , rc_mfrow    = c(2 + ((2 + length(x_var_names)) %/% 3), 3)
          , which_plot  = c(4, 6, 1)
          , n_outliers  = 3
          , sw_qqplot   = TRUE
          , sw_boxcox   = TRUE
          , sw_constant_var   = TRUE
          , sw_collinearity   = TRUE
          , sw_order_of_data  = TRUE
          , sw_addedvar = TRUE
          , sw_plot_set = c(NA, "simple", "simpleAV", "all")[2]
          )
      ) |>
      cowplot::plot_grid() +
      patchwork::plot_annotation(
        title       = "Diagnostics: Initial model"
      #, subtitle    = ""
      #, caption     =
      #, tag_levels  = "A"
      , theme = theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1, Caption align left
      )
  } # sw_model "lm"
  out[["init"]][["contrasts"]] <-
    e_plot_model_contrasts(
      fit                     = out[["init"]][["fit"]]
    , dat_cont                = dat_sel__
    , choose_contrasts        = NULL
    , sw_table_in_plot        = TRUE #FALSE
    , adjust_method           = c("none", "tukey", "scheffe", "sidak", "bonferroni", "dunnettx", "mvt")[1]  # see ?emmeans::summary.emmGrid
    , sw_glm_scale            = sw_glm_scale
    , CI_level                = 0.95
    , sw_print                = FALSE
    , sw_marginal_even_if_interaction = TRUE  # FALSE
    , sw_TWI_plots_keep       = c("singles", "both", "all")[1]
    , sw_TWI_both_orientation = c("wide", "tall")[1]
    , sw_plot_quantiles_values = c("quantiles", "values")[1]    # for numeric:numeric plots
    , plot_quantiles          = c(0.05, 0.25, 0.50, 0.75, 0.95) # for numeric:numeric plots
    , sw_quantile_type        = 7
    , plot_values             = NULL                            # for numeric:numeric plots
    )


  # Stepwise upper and lower models
  if(sw_sel_type == c("step", "bestsubset")[1]) {

    if (sw_model == c("lm", "glm")[1]) {
      form_init_upper <-
        paste0(
          y_var_name
        , " ~ "
        , " ( "
        , paste(
            x_var_names
          , collapse = " + "
          )
        , " )^2"
        ) |>
        stats::formula()

      form_init_lower <-
        paste0(
          y_var_name
        , " ~ "
        , "1"
        ) |>
        stats::formula()
    } # sw_model "lm"
    if (sw_model == c("lm", "glm")[2]) {
      form_init_upper <-
        paste0(
          paste0("cbind(", y_var_name, ", 1 - ", y_var_name, ")")
        , " ~ "
        , " ( "
        , paste(
            x_var_names
          , collapse = " + "
          )
        , " )^2"
        ) |>
        stats::formula()

      form_init_lower <-
        paste0(
          paste0("cbind(", y_var_name, ", 1 - ", y_var_name, ")")
        , " ~ "
        , "1"
        ) |>
        stats::formula()
    } # sw_model "glm"

    # determine stepwise "AIC" penalty
    sw_step_k <-
      ifelse(
        is.numeric(sw_step_k)
      , sw_step_k
      , dplyr::case_when(
          sw_step_k == "AIC"    ~ 2
        , sw_step_k == "BIC"    ~ log(nrow(out[["data"]]))
        #,
        , .default              = as.numeric(NA)
        )
      )


    ### XXX error here, dat_sel__ can't be found when stats::step is called.

    ## AIC/BIC
    # option: test="F" includes additional information
    #           for parameter estimate tests that we're familiar with
    # option: for BIC, include k=log(nrow( [data.frame name] ))

    # assign is required for stats::step to find "dat_sel__" in .GlobalEnv
    # Needs to be in global environment
    assign(x = "dat_sel__", value = dat_sel__, envir = .GlobalEnv)
    # XXX # exists_dat_sel__ <- FALSE
    # XXX # if (exists(x = "dat_sel__", envir = .GlobalEnv)) {
    # XXX #   exists_dat_sel__ <- TRUE
    # XXX #   dat_sel___TEMP__ <-
    # XXX #     get(x = dat_sel__, envir = .GlobalEnv)
    # XXX #   assign(x = "dat_sel__", value = dat_sel__, envir = .GlobalEnv)
    # XXX # }

    form_sel__ <-
      stats::step(
        object    = out[["init"]][["fit"]]
      , scope     = list(upper = form_init_upper, lower = form_init_lower)
      , direction = sw_step_direction
      , test      = "F"
      , trace     = 0
      , k         = sw_step_k  # 2 #log(nrow( dat_sel__ ))
      ) |>
      stats::formula()

    # Needs to be in global environment
    assign(x = "form_sel__", value = form_sel__, envir = .GlobalEnv)

    # XXX # if (exists_dat_sel__) {
    # XXX #   assign(x = "dat_sel__", value = dat_sel___TEMP__, envir = .GlobalEnv)
    # XXX # }


    if (sw_model == c("lm", "glm")[1]) {
      out[["sel"]][["fit"]] <-
        stats::lm(
          formula = form_sel__
        , data    = dat_sel__
        )
    } # sw_model "lm"
    if (sw_model == c("lm", "glm")[2]) {
      out[["sel"]][["fit"]] <-
        stats::glm(
          formula = form_sel__
        , data    = dat_sel__
        , family  = binomial(link = logit)
        )
    } # sw_model "glm"

    out[["sel"]][["anova"]] <-
      car::Anova(
        out[["sel"]][["fit"]]
      , type = 3
      , singular.ok = TRUE
      )
    out[["sel"]][["summary"]] <-
      summary(
        out[["sel"]][["fit"]]
      )
    if (sw_model == c("lm", "glm")[1]) {
      out[["sel"]][["criteria"]] <-
        e_lm_model_criteria(
          lm_fit  = out[["sel"]][["fit"]]
        , dat_fit = dat_sel__
        )
      out[["sel"]][["plot_diagnostics"]] <-
        cowplot::as_grob(
          ~e_plot_lm_diagnostics(
              fit         = out[["sel"]][["fit"]]
            , rc_mfrow    = c(2 + ((2 + length(x_var_names)) %/% 3), 3)
            , which_plot  = c(4, 6, 1)
            , n_outliers  = 3
            , sw_qqplot   = TRUE
            , sw_boxcox   = TRUE
            , sw_constant_var   = TRUE
            , sw_collinearity   = TRUE
            , sw_order_of_data  = TRUE
            , sw_addedvar = TRUE
            , sw_plot_set = c(NA, "simple", "simpleAV", "all")[2]
            )
        ) |>
        cowplot::plot_grid() +
      patchwork::plot_annotation(
        title       = "Diagnostics: Selected model"
      #, subtitle    = ""
      #, caption     =
      #, tag_levels  = "A"
      , theme = theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1, Caption align left
      )
    }
    out[["sel"]][["contrasts"]] <-
      e_plot_model_contrasts(
        fit                     = out[["sel"]][["fit"]]
      , dat_cont                = dat_sel__
      , choose_contrasts        = NULL
      , sw_table_in_plot        = TRUE #FALSE
      , adjust_method           = c("none", "tukey", "scheffe", "sidak", "bonferroni", "dunnettx", "mvt")[1]  # see ?emmeans::summary.emmGrid
      , CI_level                = 0.95
      , sw_print                = FALSE
      , sw_marginal_even_if_interaction = TRUE  # FALSE
      , sw_TWI_plots_keep       = c("singles", "both", "all")[1]
      , sw_TWI_both_orientation = c("wide", "tall")[1]
      , sw_plot_quantiles_values = c("quantiles", "values")[1]    # for numeric:numeric plots
      , plot_quantiles          = c(0.05, 0.25, 0.50, 0.75, 0.95) # for numeric:numeric plots
      , sw_quantile_type        = 7
      , plot_values             = NULL                            # for numeric:numeric plots
      )

  } # sw_sel_type step



  if (sw_print_results) {
    print("=================================================")
    print("-  BEGIN  ---------------------------------------")
    print("=================================================")

    print(paste("=====", "data dim"))
    dim(dat_sel__) |> print()

    print(paste("=====", "missing data plots"))
    out[["plot_missing"]] |> print()

    print(paste("=====", "resp vs covar plots"))
    out[["plot_covar"]]   |> print()

    print(paste("=====", "resp vs covar scatterplots"))
    out[["plot_scatterplots"]]   |> print()

    print(paste("=====", "predictor correlation matrix"))
    out[["plot_corrplot"]]   |> print()


    print(paste(y_var_name))
    print("____init____")
    print(form_init__)
    print("____sel____")
    print(form_sel__)

    if (sw_model == c("lm", "glm")[1]) {
      print("")
      print(paste("=====", "Model criteria"))
      print("____init____")
      out[["init"]][["criteria"      ]] |> print()
      print("____sel____")
      out[["sel"]][["criteria"      ]] |> print()
    } # sw_model "lm"

    print("")
    print("")
    print("")
    print(paste("=====", "anova/ranova"))
    print("____init____")
    out[["init"]][["anova"     ]] |> print()
    print("____sel____")
    out[["sel"]][["anova"     ]] |> print()

    print("")
    print("")
    print("")
    print(paste("=====", "summary"))
    print("____init____")
    out[["init"]][["summary"   ]] |> print()
    print("____sel____")
    out[["sel"]][["summary"   ]] |> print()

    if (sw_model == c("lm", "glm")[1]) {
      print("")
      print("")
      print("")
      print(paste("=====", "Diagnostic plots"))
      print("____init____")
      out[["init"]][["plot_diagnostics"   ]] |> print()
      #plot(out[["init"]][["fit"   ]], which = 1) |> print()
      print("____sel____")
      out[["sel"]][["plot_diagnostics"   ]] |> print()
      #plot(out[["sel"]][["fit"   ]], which = 1) |> print()
    } # sw_model "lm"

    print("")
    print("")
    print("")
    print(paste("=====", "Contrasts and model interpretations"))
    #print("____init____")
    #out[["init"]][["contrasts"]]   |> print()
    #out[["init"]][["contrasts"]]$plots   |> print()
    #out[["init"]][["contrasts"]]$tables  |> print()
    #out[["init"]][["contrasts"]]$text    |> print()
    print("____sel____")
    #out[["sel"]][["contrasts"]]  |> print()
    out[["sel"]][["contrasts"]]$plots  |> print()
    out[["sel"]][["contrasts"]]$tables |> print()
    #out[["sel"]][["contrasts"]]$text   |> print()
    out[["sel"]][["contrasts"]]$interp |> print()

    print("=================================================")
    print("--  END    --------------------------------------")
    print("=================================================")

  } # sw_print_results

  # Clean up global environment
  rm("dat_sel__", envir = .GlobalEnv)
  rm("form_init__", envir = .GlobalEnv)
  rm("form_sel__", envir = .GlobalEnv)


  return(out)
} # e_model_selection
