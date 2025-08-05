#' Model selection function for lm and glm, using \code{step} via AIC/BIC or best subset.
#'
#' (Best subset not yet implemented.)
#'
#' @param form                      formula for a model
#' @param dat                       data to use
#' @param sw_model                  type of regression model, \code{"lm"} or \code{"glm"}
#' @param sw_sel_type               type of model selection.  \code{step} starts with specified \code{form} model with upper = two-way effects model and lower = 1 (intercept-only) model
#' @param sw_step_direction         for \code{step}, the \code{direction} argument from the \code{stats::step()} function
#' @param sw_step_k                 for \code{step} determined by "AIC" (2), "BIC" (log(n)), or specified numerically; the \code{k} argument from the \code{stats::step()} function for penalty df
#' @param sw_glm_scale              Scale for glm contrasts, \code{"link"} or \code{"response"}, when \code{sw_model} is \code{"glm"}
#' @param sw_plot_missing           output plots from \code{e_plot_missing}
#' @param sw_plot_y_covar           output plots from \code{e_plot_lm_y_covar}
#' @param sw_plot_x_corr            output correlation matrix plot
#' @param sw_contrasts              which contrasts to plot, \code{"both"} is full and selected model, \code{"sel"} is only selected model, \code{"none"} produces blank plots (but this may still take a while due to large grid with many interactions), and \code{"skip"} doesn't run \code{e_plot_model_contrasts()}.
#' @param sw_print_results          print results before returning object
#' @param emmip_rg.limit            passed to \code{e_plot_model_contrasts}
#' @param sw_write_output           T/F for whether to save plots and text to a path
#' @param sw_write_output_path      path to save results
#' @param sw_write_output_prefix    filename prefix for results
#' @param sw_write_output_plot_fmt  plot filename extension that determs the \code{ggsave} \code{device} argument
#' @param ...                 options sent to \code{e_model_all_subsets_formula} for subsets to consider
#'
#' @return out                list object with all results organized by \code{init} and \code{sel} for initial and selected models
#' @importFrom stringr str_detect str_split
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom tidyselect one_of all_of where
#' @importFrom forcats fct_drop
#' @importFrom patchwork wrap_plots plot_annotation
#' @importFrom Hmisc rcorr
#' @importFrom corrplot cor.mtest corrplot
#' @importFrom cowplot as_grob plot_grid
#' @importFrom car Anova
#' @importFrom stats step
#' @importFrom broom tidy
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
    form                      = NULL
  , dat                       = NULL
  , sw_model                  = c("lm", "glm")[1]
  , sw_sel_type               = c("step", "bestsubset")[1]
  , sw_step_direction         = c("both", "backward", "forward")[1]
  , sw_step_k                 = c("AIC", "BIC", 2)[1]
  , sw_glm_scale              = c("link", "response")[2]
  , sw_plot_missing           = c(TRUE, FALSE)[1]
  , sw_plot_y_covar           = c(TRUE, FALSE)[1]
  , sw_plot_x_corr            = c(TRUE, FALSE)[1]
  , sw_contrasts              = c("both", "sel", "none", "skip")[2]
  , sw_print_results          = c(TRUE, FALSE)[1]
  , emmip_rg.limit            = 10000
  , sw_write_output           = c(TRUE, FALSE)[2]
  , sw_write_output_path      = "."
  , sw_write_output_prefix    = "prefix_"
  , sw_write_output_plot_fmt  = c("png", "pdf", "jpeg", "eps", "ps", "tex", "tiff", "bmp", "svg", "wmf")[1]
  , ...
  ) {
  #### lm example
  ## form                     = mpg ~ cyl + disp + hp + wt + vs + am + cyl:vs + disp:hp + hp:vs
  ## dat                      = erikmisc::dat_mtcars_e
  ## sw_model                 = c("lm", "glm")[1]
  ## sw_sel_type              = c("step", "bestsubset")[1]
  ## sw_step_direction        = c("both", "backward", "forward")[1]
  ## sw_step_k                = "BIC"
  ## sw_glm_scale             = c("link", "response")[2]
  ## sw_plot_missing          = c(TRUE, FALSE)[1]
  ## sw_plot_y_covar          = c(TRUE, FALSE)[1]
  ## sw_plot_x_corr           = c(TRUE, FALSE)[1]
  ## sw_contrasts             = c("both", "sel", "none", "skip")[2]
  ## sw_print_results         = c(TRUE, FALSE)[1]
  ## emmip_rg.limit           = 10000
  ## sw_write_output          = c(TRUE, FALSE)[2]
  ## sw_write_output_path     = "out/XXX"
  ## sw_write_output_prefix   = "TEST_"
  ## sw_write_output_plot_fmt = c("png", "pdf", "jpeg", "eps", "ps", "tex", "tiff", "bmp", "svg", "wmf")[1]
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
  ## form                     = cbind(am_01, 1 - am_01) ~ cyl + disp + hp + wt + vs + hp:vs
  ## dat                      = dat_sel
  ## sw_model                 = c("lm", "glm")[2]
  ## sw_sel_type              = c("step", "bestsubset")[1]
  ## sw_step_direction        = c("both", "backward", "forward")[1]
  ## sw_step_k                = "BIC"
  ## sw_glm_scale             = c("link", "response")[2]
  ## sw_plot_missing          = c(TRUE, FALSE)[1]
  ## sw_plot_y_covar          = c(TRUE, FALSE)[1]
  ## sw_plot_x_corr           = c(TRUE, FALSE)[1]
  ## sw_contrasts             = c("both", "sel", "none", "skip")[2]
  ## sw_print_results         = c(TRUE, FALSE)[1]
  ## emmip_rg.limit           = 10000
  ## sw_write_output          = c(TRUE, FALSE)[2]
  ## sw_write_output_path     = "."
  ## sw_write_output_prefix   = ""
  ## sw_write_output_plot_fmt = c("png", "pdf", "jpeg", "eps", "ps", "tex", "tiff", "bmp", "svg", "wmf")[1]

  if (sw_write_output) {
    # create folder if it doesn't already exist
    dir.create(sw_write_output_path, recursive = TRUE, showWarnings = FALSE)
  } # sw_write_output


  # store results in a list
  out <- list()

  # if formula is specified
  if (!is.null(form)) {

    xy_var_names_list <- e_model_extract_var_names(form)
    #xy_var_names_list
    y_var_name                <- xy_var_names_list$y_var_name
    y_var_name_glm            <- xy_var_names_list$y_var_name_glm
    x_var_names               <- xy_var_names_list$x_var_names
    x_var_names_interactions  <- xy_var_names_list$x_var_names_interactions

  } # form

  dat_sel__ <-
    dat |>
    # subset to these columns
    dplyr::select(
      tidyselect::all_of(y_var_name )
    , tidyselect::all_of(x_var_names)
    ) |>
    # convert character to factor
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.character)
      , as.factor
      )
    )

  # if lm, ensure response y is numeric
  if (sw_model == c("lm", "glm")[1]) {
    temp_y_label <-
      dat_sel__[[ y_var_name ]] |>
      labelled::var_label()
    dat_sel__[[ y_var_name ]]  <-
      dat_sel__[[ y_var_name ]] |>
      as.numeric()
    labelled::var_label(dat_sel__[[ y_var_name ]]) <-
      temp_y_label
  } # sw_model "lm"


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

  x_var_removed <- NULL
  # remove factors with only one level after dropping missing
  for (i_covar in seq_along(x_var_names)) {
    ## i_covar = 5
    if (is.factor (dat_sel__[[ x_var_names[i_covar] ]]) |
        is.ordered(dat_sel__[[ x_var_names[i_covar] ]])) {

      # drop unused factor levels
      dat_sel__[[ x_var_names[i_covar] ]] <-
        dat_sel__[[ x_var_names[i_covar] ]] |>
        forcats::fct_drop()

      if (length(levels(dat_sel__[[ x_var_names[i_covar] ]])) <= 1) {
        warning(paste0("erikmisc::e_model_selection, removing x-var with only 1 level: ", x_var_names[i_covar]))

        x_var_removed <- c(x_var_removed, x_var_names[i_covar])

        # remove data column
        dat_sel__[[ x_var_names[i_covar] ]] <- NULL

        # remove associated interactions
        x_int_split <-
          x_var_names_interactions |>
          stringr::str_split(
            pattern = stringr::fixed(":")
          , simplify = TRUE
          )

        ind_int_remove <-
          (x_int_split == x_var_names[i_covar]) |>
          rowSums() |>
          as.logical() |>
          which()

        if (length(ind_int_remove)) {
          x_var_names_interactions[ind_int_remove] <- NA
        }

        # remove main effect
        x_var_names[i_covar] <- NA

      }
    }
  }
  x_var_names               <- x_var_names              [!is.na(x_var_names             )]
  x_var_names_interactions  <- x_var_names_interactions [!is.na(x_var_names_interactions)]

  if (length(x_var_names) == 0) {
    warning(paste0("erikmisc::e_model_selection, No predictors (x) with more than 1 level -- skip analysis."))
    return(NULL)
  }

  # if a variable was removed, update formula
  if (length(x_var_removed)) {
    if (sw_model == c("lm", "glm")[1]) {
      form <-
        paste0(
          y_var_name
        , " ~ "
        , paste0(
            c(
              x_var_names
            , x_var_names_interactions
            )
          , collapse = " + "
          )
        ) |>
        as.formula()
    } # sw_model "lm"
    if (sw_model == c("lm", "glm")[2]) {
      form <-
        paste0(
          y_var_name_glm
        , " ~ "
        , paste0(
            c(
              x_var_names
            , x_var_names_interactions
            )
          , collapse = " + "
          )
        ) |>
        as.formula()
    } # sw_model "glm"
  } # x_var_removed


  out[["data"]] <-
    dat_sel__

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


  # skip car::Anova if only intercept
  if (sum(names(out[["sel"]][["fit"]]$coefficients) %notin% "(Intercept)")) {
    out[["init"]][["anova"]] <-
      car::Anova(
        out[["init"]][["fit"]]
      , type = 3
      , singular.ok = TRUE
      )
  } else {
    out[["init"]][["anova"]] <-
      NULL
  }
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
      , theme = ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0)) # Default is hjust=1, Caption align left
      )
  } # sw_model "lm"
  if (!(sw_contrasts      == c("both", "sel", "none", "skip")[4])) {
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
      , sw_produce_plots        = (sw_contrasts %in% c("both", "sel", "none", "skip")[c(1)])
      , sw_TWI_plots_keep       = c("singles", "both", "all")[1]
      , sw_TWI_both_orientation = c("wide", "tall")[1]
      , sw_plot_quantiles_values = c("quantiles", "values")[1]    # for numeric:numeric plots
      , plot_quantiles          = c(0.05, 0.25, 0.50, 0.75, 0.95) # for numeric:numeric plots
      , sw_quantile_type        = 7
      , plot_values             = NULL                            # for numeric:numeric plots
      , emmip_rg.limit          = emmip_rg.limit
      )
  } else {
    out[["init"]][["contrasts"]] <- "skip"
  } # sw_contrasts !skip

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
        # test if numeric, if numeric, then as.numeric does not return NA
        suppressWarnings(ifelse(is.na(sw_step_k), NA, !is.na(as.numeric(sw_step_k))))
      , as.numeric(sw_step_k)
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

    # skip car::Anova if only intercept
    if (sum(names(out[["sel"]][["fit"]]$coefficients) %notin% "(Intercept)")) {
      out[["sel"]][["anova"]] <-
        car::Anova(
          out[["sel"]][["fit"]]
        , type = 3
        , singular.ok = TRUE
        )
    } else {
      out[["sel"]][["anova"]] <-
        NULL
    }
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
        , theme = ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0)) # Default is hjust=1, Caption align left
        )
    }
    if (!(sw_contrasts      == c("both", "sel", "none", "skip")[4])) {
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
        , sw_produce_plots        = (sw_contrasts %in% c("both", "sel", "none", "skip")[c(1, 2)])
        , sw_TWI_plots_keep       = c("singles", "both", "all")[1]
        , sw_TWI_both_orientation = c("wide", "tall")[1]
        , sw_plot_quantiles_values = c("quantiles", "values")[1]    # for numeric:numeric plots
        , plot_quantiles          = c(0.05, 0.25, 0.50, 0.75, 0.95) # for numeric:numeric plots
        , sw_quantile_type        = 7
        , plot_values             = NULL                            # for numeric:numeric plots
        , emmip_rg.limit          = emmip_rg.limit
        )
    } else {
      out[["sel"]][["contrasts"]] <- "skip"
    } # sw_contrasts !skip
  } # sw_sel_type step


  # print results
  if (sw_print_results) {
    print("=================================================")
    print("--  BEGIN  --------------------------------------")
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

    if (sw_contrasts %in% c("both", "sel", "none")[c(1, 2)]) {
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
    }

    print("=================================================")
    print("--  END    --------------------------------------")
    print("=================================================")

  } # sw_print_results

  # write output
  if (sw_write_output) {
    # create folder if it doesn't already exist
    dir.create(sw_write_output_path, recursive = TRUE, showWarnings = FALSE)

    fn_output <-
      file.path(
            sw_write_output_path
          , paste0(
              sw_write_output_prefix
            , "model_text.txt"
            )
          )

    ggplot2::ggsave(
        file.path(
          sw_write_output_path
        , paste0(
            sw_write_output_prefix
          #, "_"
          , "plot_missing."
          , sw_write_output_plot_fmt
          )
        )
      , plot   = out[["plot_missing"]]
      , width  = 10
      , height = 10
      , units  = "in"
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      #, useDingbats = FALSE
      )

    ggplot2::ggsave(
        file.path(
          sw_write_output_path
        , paste0(
            sw_write_output_prefix
          #, "_"
          , "plot_covar."
          , sw_write_output_plot_fmt
          )
        )
      , plot   = out[["plot_covar"]]
      , width  = 10
      , height = 6
      , units  = "in"
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      #, useDingbats = FALSE
      )

    ggplot2::ggsave(
        file.path(
          sw_write_output_path
        , paste0(
            sw_write_output_prefix
          #, "_"
          , "plot_scatterplots."
          , sw_write_output_plot_fmt
          )
        )
      , plot   = out[["plot_scatterplots"]]
      , width  = 10
      , height = 10
      , units  = "in"
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      #, useDingbats = FALSE
      )

    ggplot2::ggsave(
        file.path(
          sw_write_output_path
        , paste0(
            sw_write_output_prefix
          #, "_"
          , "plot_corrplot."
          , sw_write_output_plot_fmt
          )
        )
      , plot   = out[["plot_corrplot"]]$plot
      , width  = 10
      , height = 10
      , units  = "in"
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      #, useDingbats = FALSE
      )

    output_write <-
      c(
        fn_output
      , "\n\n"
      )
    readr::write_lines(x = output_write, file = fn_output, append = FALSE)

    output_write <-
      c(
        y_var_name
      , "____init____"
      , form_init__     |> deparse()
      , "____sel____"
      , form_sel__      |> deparse()
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)


    if (sw_model == c("lm", "glm")[1]) {
      output_write <-
        c(
          "\n\n"
        , paste("=====", "Model criteria")
        , "____init____"
        )
      readr::write_lines(x = output_write, file = fn_output, append = TRUE)

      output_write <-
        out[["init"]][["criteria"      ]] |>
        signif(3)
      write.table(x = output_write, file = fn_output, append = TRUE)


      output_write <-
        c(
          "____sel____"
        )
      readr::write_lines(x = output_write, file = fn_output, append = TRUE)

      output_write <-
        out[["sel"]][["criteria"      ]] |>
        signif(3) |>
        knitr::kable() |>
        as.character()
      write.table(x = output_write, file = fn_output, append = TRUE)

    } # sw_model "lm"

    output_write <-
      c(
        "\n\n"
      , paste("=====", "anova/ranova")
      , "____init____"
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)

    if (!is.null(out[["init"]][["anova"     ]])) {
      output_write <-
        out[["init"]][["anova"     ]] |>
        broom::tidy() |>
        dplyr::mutate(
          dplyr::across(tidyselect::where(is.numeric), ~signif(.x, digits = 3))
        , sig = p.value |> e_pval_stars()
        ) |>
        knitr::kable() |>
        as.character()
      write.table(x = output_write, file = fn_output, append = TRUE)
    }

    output_write <-
      c(
        "____sel____"
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)

    if (!is.null(out[["sel"]][["anova"     ]])) {
      output_write <-
        out[["sel"]][["anova"     ]] |>
        broom::tidy() |>
        dplyr::mutate(
          dplyr::across(tidyselect::where(is.numeric), ~signif(.x, digits = 3))
        , sig = p.value |> e_pval_stars()
        ) |>
        knitr::kable() |>
        as.character()
      write.table(x = output_write, file = fn_output, append = TRUE)
    }

    output_write <-
      c(
        "\n\n"
      , paste("=====", "summary")
      , "____init____"
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)

    if (!is.null(out[["init"]][["fit"     ]])) {
      # summary table via tidy
      output_write <-
        out[["init"]][["fit"   ]] |>
        broom::tidy() |>
        dplyr::mutate(
          dplyr::across(tidyselect::where(is.numeric), ~signif(.x, digits = 3))
        , sig = p.value |> e_pval_stars()
        ) |>
        knitr::kable() |>
        as.character()
      write.table(x = output_write, file = fn_output, append = TRUE)
    }

    output_write <-
      c(
        "____sel____"
      )
    readr::write_lines(x = output_write, file = fn_output, append = TRUE)

    if (!is.null(out[["sel"]][["fit"     ]])) {
      # summary table via tidy
      output_write <-
        out[["sel"]][["fit"   ]] |>
        broom::tidy() |>
        dplyr::mutate(
          dplyr::across(tidyselect::where(is.numeric), ~signif(.x, digits = 3))
        , sig = p.value |> e_pval_stars()
        ) |>
        knitr::kable() |>
        as.character()
      write.table(x = output_write, file = fn_output, append = TRUE)
    }


    if (sw_model == c("lm", "glm")[1]) {
      # print("")
      # print("")
      # print("")
      # print(paste("=====", "Diagnostic plots"))
      # print("____init____")
      # out[["init"]][["plot_diagnostics"   ]] |> print()
      # #plot(out[["init"]][["fit"   ]], which = 1) |> print()
      # print("____sel____")
      # out[["sel"]][["plot_diagnostics"   ]] |> print()
      # #plot(out[["sel"]][["fit"   ]], which = 1) |> print()

      ggplot2::ggsave(
          file.path(
            sw_write_output_path
          , paste0(
              sw_write_output_prefix
            #, "_"
            , "plot_diagnostics__init."
            , sw_write_output_plot_fmt
            )
          )
        , plot   = out[["init"]][["plot_diagnostics"   ]]
        , width  = 10
        , height = 12
        , units  = "in"
        ## png, jpeg
        , dpi    = 300
        , bg     = "white"
        ## pdf
        #, useDingbats = FALSE
        )

      ggplot2::ggsave(
          file.path(
            sw_write_output_path
          , paste0(
              sw_write_output_prefix
            #, "_"
            , "plot_diagnostics__sel."
            , sw_write_output_plot_fmt
            )
          )
        , plot   = out[["sel"]][["plot_diagnostics"   ]]
        , width  = 10
        , height = 12
        , units  = "in"
        ## png, jpeg
        , dpi    = 300
        , bg     = "white"
        ## pdf
        #, useDingbats = FALSE
        )

    } # sw_model "lm"

    if ((sw_contrasts %in% c("both", "sel", "none")[c(1, 2)]) &
        (length(out[["sel"]][["contrasts"]]$plots) > 0)
      ) {
      # print("")
      # print("")
      # print("")
      # print(paste("=====", "Contrasts and model interpretations"))
      # #print("____init____")
      # #out[["init"]][["contrasts"]]   |> print()
      # #out[["init"]][["contrasts"]]$plots   |> print()
      # #out[["init"]][["contrasts"]]$tables  |> print()
      # #out[["init"]][["contrasts"]]$text    |> print()
      # print("____sel____")
      # #out[["sel"]][["contrasts"]]  |> print()
      # out[["sel"]][["contrasts"]]$plots  |> print()
      # out[["sel"]][["contrasts"]]$tables |> print()
      # #out[["sel"]][["contrasts"]]$text   |> print()
      # out[["sel"]][["contrasts"]]$interp |> print()


      ## To plot all contrasts in a plot grid:
      # Since plot interactions have sublists of plots, we want to pull those out
      #   into a one-level plot list.
      # The code here works for sw_TWI_plots_keep = "singles"
      #   which will make each plot the same size in the plot_grid() below.
      # For a publications, you'll want to manually choose which plots to show.

      # index for plot list,
      #   needed since interactions add 2 plots to the list, so the number of terms
      #   is not necessarily the same as the number of plots.
      i_list <- 0
      # initialize a list of plots
      p_list <- list()

      for (i_term in 1:length(out[["sel"]][["contrasts"]]$plots)) {
        ## i_term = 1

        # extract the name of the plot
        n_list <- names(out[["sel"]][["contrasts"]]$plots)[i_term]

        # test whether the name has a colon ":"; if so, it's an interaction
        if (stringr::str_detect(string = n_list, pattern = stringr::fixed(":"))) {
          # an two-way interaction has two plots

          # first plot
          i_list <- i_list + 1
          p_list[[ i_list ]] <- out[["sel"]][["contrasts"]]$plots[[ i_term ]][[ 1 ]]

          # second plot
          i_list <- i_list + 1
          p_list[[ i_list ]] <- out[["sel"]][["contrasts"]]$plots[[ i_term ]][[ 2 ]]

        } else {
          # not an interaction, only one plot

          i_list <- i_list + 1
          p_list[[ i_list ]] <- out[["sel"]][["contrasts"]]$plots[[ i_term ]]


        } # if
      } # for

      p_arranged <-
        cowplot::plot_grid(
          plotlist  = p_list
        , nrow      = NULL
        , ncol      = 3
        , labels    = "AUTO"
        ) +
        patchwork::plot_annotation(
          title       = paste0("Selected model contrasts")
        #, subtitle    = text_formula_sel
        #, caption     = paste0(
        #                  "Caption down here."
        #                )
        #, tag_levels  = "A"
        , theme = ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0)) # Default is hjust=1, Caption align left
        )

      ggplot2::ggsave(
          file.path(
            sw_write_output_path
          , paste0(
              sw_write_output_prefix
            #, "_"
            , "plot_contrasts."
            , sw_write_output_plot_fmt
            )
          )
        , plot   = p_arranged
        , width  = 12
        , height = 12
        , units  = "in"
        ## png, jpeg
        , dpi    = 300
        , bg     = "white"
        ## pdf
        #, useDingbats = FALSE
        )


      output_write <-
        c(
          "\n\n"
        , "====="
        , "\n\nCONTRASTS:\n"
        )
      for (i_cont in seq_len(length(out[["sel"]][["contrasts"]]$text))) {
        ## i_cont=1
        output_write <-
          c(
            output_write
          , "\n"
          , names(out[["sel"]][["contrasts"]]$text)[ i_cont ]
          , out[["sel"]][["contrasts"]]$text[[ i_cont ]] |> unlist()
          )
      } # i_cont
      readr::write_lines(x = output_write, file = fn_output, append = TRUE)


      output_write <-
        c(
          "\n\n"
        , "====="
        , "\n\nINTERPRETATIONS:\n"
        , paste(
            out[["sel"]][["contrasts"]]$interp |> names()
          , out[["sel"]][["contrasts"]]$interp |> unlist()
          , sep = "\n"
          , collapse = "\n\n"
          )
        )
      readr::write_lines(x = output_write, file = fn_output, append = TRUE)

    } # sw_contrasts

  } # sw_write_output


  # Clean up global environment
  rm("dat_sel__"  , envir = .GlobalEnv)
  rm("form_init__", envir = .GlobalEnv)
  rm("form_sel__" , envir = .GlobalEnv)


  return(out)
} # e_model_selection
