#' Random Forests classification workflow using \code{randomForestSRC}
#'
#' @param dat_rf_class    data.frame with data
#' @param rf_y_var        y factor variable, if NULL then assumed to be the first column of \code{dat_rf_class}
#' @param rf_x_var        x variables, if NULL then assumed to be all except the first column of \code{dat_rf_class}
#' @param sw_rfsrc_ntree  \code{ntree} argument for \code{randomForestSRC::rfsrc()}
#' @param sw_alpha        \code{alpha} argument for \code{randomForestSRC::plot.subsample()} and \code{randomForestSRC::extract.bootsample()}
#'
#' @return list with many RF objects, summaries, and plots
#' @import parallel
#' @import dplyr
#' @import ggplot2
#' @import randomForestSRC
#' @import patchwork
#' @importFrom lubridate duration
#' @importFrom purrr keep
#' @importFrom tidyselect all_of
#' @importFrom cowplot as_grob
#' @export
#'
#' @examples
#' dat_rf_class <-
#'   erikmisc::dat_mtcars_e |>
#'   dplyr::select(
#'     -model
#'   ) |>
#'   dplyr::select(
#'     cyl
#'   , tidyselect::everything()
#'   )
#'
#' out_e_rf <-
#'   e_rfsrc_classification(
#'     dat_rf_class    = dat_rf_class
#'   , rf_y_var        = NULL
#'   , rf_x_var        = NULL
#'   , sw_rfsrc_ntree  = 200
#'   , sw_alpha        = 0.05
#'   )
#'
e_rfsrc_classification <-
  function(
    dat_rf_class    = NULL
  , rf_y_var        = NULL
  , rf_x_var        = NULL
  , sw_rfsrc_ntree  = 2000
  , sw_alpha        = 0.05
  # Add options for creating certain output
  ) {

  # patchwork model selection plot design
  #   appears: after model selection if no x variables
  #            at end to summarize ROC with model selection
  plot_design <-
    "AAAABB
     AAAABB
     CCCCDD
     CCCCDD
     EEEEEE
     EEEEEE"

  # Start timer
  time_start <- proc.time()

  print(paste0("erikmisc::e_rfsrc_classification, "
    , round(lubridate::duration((proc.time() - time_start)["elapsed"], units="seconds"), 2) |> as.character()
    , ":  Process input"
  ))

  #library(parallel)
  n_cores <- max(1, parallel::detectCores() - 2)
  options(rf.cores = n_cores) # OpenMP Parallel Processing
  options(mc.cores = n_cores) # R-side Parallel Processing

  # list for output objects
  out <- list()

  # Data
  if (is.null(dat_rf_class)) {
    warning("erikmisc::e_rfsrc_classification, dat_rf_class is NULL")
  }
  dat_rf_data <-
    dat_rf_class |>
    as.data.frame()

  # Variables
  if (is.null(rf_y_var)) {
    rf_y_var <- names(dat_rf_data)[ 1]
  } else {
    if(!all(rf_y_var %in% names(dat_rf_data))) {
      warning("erikmisc::e_rfsrc_classification, rf_y_var var not in dat_rf_class")
    }
  }
  if (is.null(rf_x_var)) {
    rf_x_var <- names(dat_rf_data)[-1]
  } else {
    if(!all(rf_x_var %in% names(dat_rf_data))) {
      warning("erikmisc::e_rfsrc_classification, rf_x_var vars not in dat_rf_class")
    }
  }

  # check for all NA columns
  all_na_columns <-
    dat_rf_data |>
    purrr::keep(~all(is.na(.x))) |>
    names()

  if (length(all_na_columns)) {
    # check if y var
    if(any(all_na_columns %in% rf_y_var)) {
      warning(paste0("erikmisc::e_rfsrc_classification, returning NULL: dat_rf_class y-variable is all NA:  ", rf_y_var))
      return(NULL)
    }

    warning(paste0("erikmisc::e_rfsrc_classification, dat_rf_class removing column(s) that are all NA:  ", paste(all_na_columns, collapse = ", ")))

    # remove column from data
    dat_rf_data <-
      dat_rf_data |>
      dplyr::select(
        -tidyselect::all_of(all_na_columns)
      )
    # remove variable from x var list
    rf_x_var <- rf_x_var[rf_x_var %notin% all_na_columns]
  }

  # check for at least 2 y var levels
  if (length(unique(dat_rf_data[[ rf_y_var ]])) < 2) {
    warning(paste0("erikmisc::e_rfsrc_classification, returning NULL: rf_y_var (", rf_y_var, ") needs at least two levels:  ", paste(unique(dat_rf_data[[ rf_y_var ]]), collapse = ", ")))
    return(NULL)
  }


  rf_formula <-
    paste(
      rf_y_var
    , " ~ "
    , paste(
        rf_x_var
      , collapse= " + "
      )
    ) |>
    as.formula()

  #out[[ "dat_rf_data" ]] <- dat_rf_data
  out[[ "rf_y_var"    ]] <-
    rf_y_var
  out[[ "rf_x_var"    ]] <-
    rf_x_var
  out[[ "rf_formula"  ]] <-
    rf_formula

  text_formula <-
    paste(as.character(out$rf_formula)[c(2, 1, 3)], collapse = " ") |>
    stringr::str_wrap(width = 120, exdent = 4)


  ### Grow forest
  print(paste0("erikmisc::e_rfsrc_classification, "
    , round(lubridate::duration((proc.time() - time_start)["elapsed"], units="seconds"), 2) |> as.character()
    , ":  Grow forest"
  ))

  o_class <-
    randomForestSRC::rfsrc(
      formula     = rf_formula
    , data        = dat_rf_data
    , ntree       = sw_rfsrc_ntree
    #, mtry        = NULL             # tune()$optimal["mtry"]
    #, ytry        = NULL
    #, nodesize    = NULL             # tune()$optimal["nodesize"]
    #, nodedepth   = NULL
    #, splitrule   = NULL
    #, splitrule   = c("mse", "quantile.regr")[1]       # regression
    , splitrule   = c("gini", "auc", "entropy")[2]     # classification
    , nsplit      = 0 # 10
    , importance  = c("anti", "permute", "random", "none")[1]
                      # "permute" yields permutation VIMP (Breiman-Cutler importance) by permuting OOB cases.
                      # "random"  uses random left/right assignments whenever a split is encountered for the target variable.
                      # "anti"    (default) assigns cases to the anti (opposite) split.
    , block.size  = 1 # 10 #if (any(is.element(as.character(importance), c("none", "FALSE")))) NULL else 10
    , bootstrap   = c("by.root", "none", "by.user")[1]
    , samptype    = c("swor", "swr")[1]
    #, samp        = NULL
    #, membership  = FALSE
    #, sampsize    = if (samptype == "swor") function(x){x * .632} else function(x){x}
    , na.action   = c("na.omit", "na.impute")[2]
    #, nimpute     = 1
    #, ntime       = 150    # survival
    #, cause       = NULL   # competing risks
    , perf.type   = c("none", "misclass", "brier", "gmean")[2]  # classification
    , proximity   = c("inbag", "oob", "all", TRUE, FALSE)[2]    # TRUE = "inbag"
    , distance    = c("inbag", "oob", "all", TRUE, FALSE)[2]    # TRUE = "inbag"
    , forest.wt   = c("inbag", "oob", "all", TRUE, FALSE)[1]    # TRUE = "inbag"
    #, xvar.wt     = NULL
    #, yvar.wt     = NULL
    #, split.wt    = NULL
    #, case.wt     = NULL
    , forest      = TRUE
    #, save.memory = FALSE
    , var.used    = c(FALSE, "all.trees", "by.tree")[2]
    , split.depth = c(FALSE, "all.trees", "by.tree")[2]
    #, seed        = NULL
    , do.trace    = 1
    , statistics  = TRUE # FALSE
    )

  out[[ "o_class" ]] <-
    o_class

  ### Plot convergence
  out[[ "plot_o_class" ]] <-
    cowplot::as_grob(
      ~plot(o_class)
    )
  # cowplot::plot_grid(out[[ "plot_o_class" ]])

  ### Variable Importance (also for variable selection)
  # VIMP
  out[[ "o_class_importance" ]] <-
    o_class$importance            # Variable importance (VIMP) for each x-variable.

  out[[ "plot_o_class_importance" ]] <-
    e_plot_rf_vimp(o_class$importance)

  # obtains the value of AUC (area under the ROC curve)
  o_class_AUC <-
    randomForestSRC::get.auc(
      y    = dat_rf_data[[ rf_y_var ]]
    , prob = o_class$predicted.oob
    )
  out[[ "o_class_AUC" ]] <-
    o_class_AUC


  ### Confidence intervals and standard errors for VIMP (variable importance)
  print(paste0("erikmisc::e_rfsrc_classification, "
    , round(lubridate::duration((proc.time() - time_start)["elapsed"], units="seconds"), 2) |> as.character()
    , ":  Subsample for VIMP and model selection"
  ))

  o_class_subsample <-
    randomForestSRC::subsample(
      obj               = o_class
    , B                 = 100           # Number of subsamples (or number of bootstraps).
    , block.size        = 1             # 1 = VIMP is calculated for each tree. "ntree" =  VIMP is calculated for the entire forest, ensemble VIMP.
    , importance        = c("anti", "permute", "random")[1]
    , subratio          = NULL
    , stratify          = TRUE
    , performance       = FALSE         # Generalization error? User can also request standard error and confidence regions for generalization error.
    , performance.only  = FALSE         # Only calculate standard error and confidence region for the generalization error (no VIMP).
    , joint             = FALSE         # Joint VIMP for all variables? Users can also request joint VIMP for specific variables using xvar.names.
    , xvar.names        = NULL          # Specifies variables for calculating joint VIMP. By default all variables are used.
    , bootstrap         = TRUE #FALSE  # Use double bootstrap approach in place of subsampling? Much slower, but potentially more accurate.
    , verbose           = TRUE          # Provide verbose output?
    )
  out[[ "o_class_subsample" ]] <-
    o_class_subsample

  out[[ "o_class_subsample_extract_subsample" ]] <-
    randomForestSRC::extract.subsample (o_class_subsample, alpha = sw_alpha)

  # Use double bootstrap approach in place of subsampling? Much slower, but potentially more accurate.
  out[[ "o_class_subsample_extract_bootsample" ]] <-
    randomForestSRC::extract.bootsample(o_class_subsample, alpha = sw_alpha)

  ### VIMP plot out to a selected alpha level
  out[[ "plot_o_class_subsample" ]] <-
    cowplot::as_grob(
      ~randomForestSRC::plot.subsample(
        x           = o_class_subsample
      , alpha       = sw_alpha
      #, xvar.names
      , standardize = TRUE
      , normal      = TRUE
      , jknife      = FALSE
      #, target
      , m.target    = NULL
      , pmax        = 75
      , main        = ""
      , sorted      = TRUE
      )
    )
  # cowplot::plot_grid(out[[ "plot_o_class_subsample" ]])


  ## Select variables from randomForestSRC::extract.bootsample(o_class_subsample)
  print(paste0("erikmisc::e_rfsrc_classification, "
    , round(lubridate::duration((proc.time() - time_start)["elapsed"], units="seconds"), 2) |> as.character()
    , ":  Model selection"
  ))

  rf_x_var_sel <-
    rownames(
      randomForestSRC::extract.bootsample(o_class_subsample, alpha = sw_alpha)$var.sel.Z
    )[randomForestSRC::extract.bootsample(o_class_subsample, alpha = sw_alpha)$var.sel.Z$signif]

  out[[ "rf_x_var_sel" ]] <- rf_x_var_sel

  if (length(rf_x_var_sel) == 0) {

    # Compile training summary plot (also at very end when x var)
    # If selected model has NO x variables
    out[[ "plot_rf_train_all_summary" ]] <-
      cowplot::plot_grid(out$plot_o_class              ) +
      cowplot::plot_grid(out$plot_o_class_subsample    ) +
      #cowplot::plot_grid(out$plot_o_class_sel          ) +
      #cowplot::plot_grid(out$plot_o_class_sel_subsample) +
      #cowplot::plot_grid(plotlist = out$plot_o_class_sel_ROC$plot_roc, nrow = 1) +
      patchwork::plot_spacer() +
      patchwork::plot_spacer() +
      patchwork::plot_spacer() +
      patchwork::plot_layout(design = plot_design) +
      patchwork::plot_annotation(
        title       = text_formula
      , subtitle    = "No x variables selected"
      , caption     = paste0(
                        "Full model AUC = "
                      , round(out$o_class_AUC, 3)
                      )
      , tag_levels  = "A"
      ) +
      theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1, Caption align left

    warning("erikmisc::e_rfsrc_classification, model selection has 0 x variables")
    return(out)
  }

  ### Prepare data and formula

  rf_formula_sel <-
    paste(
      rf_y_var
    , " ~ "
    , paste(
        rf_x_var_sel
      , collapse= " + "
      )
    ) |>
    as.formula()

  out[[ "rf_formula_sel" ]] <-
    rf_formula_sel

  text_formula_sel <-
    paste(as.character(out$rf_formula_sel)[c(2, 1, 3)], collapse = " ") |>
    stringr::str_wrap(width = 120, exdent = 4)


  ### Grow forest
  print(paste0("erikmisc::e_rfsrc_classification, "
    , round(lubridate::duration((proc.time() - time_start)["elapsed"], units="seconds"), 2) |> as.character()
    , ":  Grow forest (selected model)"
  ))

  o_class_sel <-
    randomForestSRC::rfsrc(
      formula     = rf_formula_sel
    , data        = dat_rf_data
    , ntree       = sw_rfsrc_ntree
    #, mtry        = NULL             # tune()$optimal["mtry"]
    #, ytry        = NULL
    #, nodesize    = NULL             # tune()$optimal["nodesize"]
    #, nodedepth   = NULL
    #, splitrule   = NULL
    #, splitrule   = c("mse", "quantile.regr")[1]       # regression
    , splitrule   = c("gini", "auc", "entropy")[2]     # classification
    , nsplit      = 0 # 10
    , importance  = c("anti", "permute", "random", "none")[1]
                      # "permute" yields permutation VIMP (Breiman-Cutler importance) by permuting OOB cases.
                      # "random"  uses random left/right assignments whenever a split is encountered for the target variable.
                      # "anti"    (default) assigns cases to the anti (opposite) split.
    , block.size  = 1 # 10 #if (any(is.element(as.character(importance), c("none", "FALSE")))) NULL else 10
    , bootstrap   = c("by.root", "none", "by.user")[1]
    , samptype    = c("swor", "swr")[1]
    #, samp        = NULL
    #, membership  = FALSE
    #, sampsize    = if (samptype == "swor") function(x){x * .632} else function(x){x}
    , na.action   = c("na.omit", "na.impute")[2]
    #, nimpute     = 1
    #, ntime       = 150    # survival
    #, cause       = NULL   # competing risks
    , perf.type   = c("none", "misclass", "brier", "gmean")[2]  # classification
    , proximity   = c("inbag", "oob", "all", TRUE, FALSE)[2]    # TRUE = "inbag"
    , distance    = c("inbag", "oob", "all", TRUE, FALSE)[2]    # TRUE = "inbag"
    , forest.wt   = c("inbag", "oob", "all", TRUE, FALSE)[1]    # TRUE = "inbag"
    #, xvar.wt     = NULL
    #, yvar.wt     = NULL
    #, split.wt    = NULL
    #, case.wt     = NULL
    , forest      = TRUE
    #, save.memory = FALSE
    , var.used    = c(FALSE, "all.trees", "by.tree")[2]
    , split.depth = c(FALSE, "all.trees", "by.tree")[2]
    #, seed        = NULL
    , do.trace    = 1
    , statistics  = TRUE # FALSE
    )

  out[[ "o_class_sel" ]] <-
    o_class_sel

  ### Plot convergence
  out[[ "plot_o_class_sel" ]] <-
    cowplot::as_grob(
      ~plot(o_class_sel)
    )
  # cowplot::plot_grid(out[[ "plot_o_class_sel" ]])

  ### Variable Importance (also for variable selection)
  # VIMP
  out[[ "o_class_sel_importance" ]] <-
    o_class_sel$importance            # Variable importance (VIMP) for each x-variable.

  out[[ "plot_o_class_sel_importance" ]] <-
    e_plot_rf_vimp(o_class_sel$importance)

  ### Confidence intervals and standard errors for VIMP (variable importance)
  print(paste0("erikmisc::e_rfsrc_classification, "
    , round(lubridate::duration((proc.time() - time_start)["elapsed"], units="seconds"), 2) |> as.character()
    , ":  Subsample for VIMP with CIs and SEs (selected model)"
  ))

  o_class_sel_subsample <-
    randomForestSRC::subsample(
      obj               = o_class_sel
    , B                 = 100           # Number of subsamples (or number of bootstraps).
    , block.size        = 1             # 1 = VIMP is calculated for each tree. "ntree" =  VIMP is calculated for the entire forest, ensemble VIMP.
    , importance        = c("anti", "permute", "random")[1]
    , subratio          = NULL
    , stratify          = TRUE
    , performance       = FALSE         # Generalization error? User can also request standard error and confidence regions for generalization error.
    , performance.only  = FALSE         # Only calculate standard error and confidence region for the generalization error (no VIMP).
    , joint             = FALSE         # Joint VIMP for all variables? Users can also request joint VIMP for specific variables using xvar.names.
    , xvar.names        = NULL          # Specifies variables for calculating joint VIMP. By default all variables are used.
    , bootstrap         = TRUE #FALSE  # Use double bootstrap approach in place of subsampling? Much slower, but potentially more accurate.
    , verbose           = TRUE          # Provide verbose output?
    )
  out[[ "o_class_sel_subsample" ]] <-
    o_class_sel_subsample

  out[[ "o_class_sel_subsample_extract_subsample" ]] <-
    randomForestSRC::extract.subsample(o_class_sel_subsample, alpha = sw_alpha)

  # Use double bootstrap approach in place of subsampling? Much slower, but potentially more accurate.
  out[[ "o_class_sel_subsample_extract_bootsample" ]] <-
    randomForestSRC::extract.bootsample(o_class_sel_subsample, alpha = sw_alpha)

  ### VIMP plot out to a selected alpha level
  out[[ "plot_o_class_sel_subsample" ]] <-
    cowplot::as_grob(
      ~randomForestSRC::plot.subsample(
        x           = o_class_sel_subsample
      , alpha       = sw_alpha
      #, xvar.names
      , standardize = TRUE
      , normal      = TRUE
      , jknife      = FALSE
      #, target
      , m.target    = NULL
      , pmax        = 75
      , main        = ""
      , sorted      = TRUE
      )
    )
  # cowplot::plot_grid(out[[ "plot_o_class_sel_subsample" ]])


  ### Marginal/Partial effects plots
  print(paste0("erikmisc::e_rfsrc_classification, "
    , round(lubridate::duration((proc.time() - time_start)["elapsed"], units="seconds"), 2) |> as.character()
    , ":  Partial effects plots (selected model)"
  ))

  out[[ "plot_o_class_sel_marginal_effects" ]] <- list()
  for (i_level in seq_along(levels(o_class_sel$class))) {

    # Partial effects plots
    out[[ "plot_o_class_sel_marginal_effects" ]][[ levels(o_class_sel$class)[i_level] ]] <-
      cowplot::as_grob(
        ~randomForestSRC::plot.variable(
          x               = o_class_sel
        #, xvar.names
        , target          = levels(o_class_sel$class)[i_level]   # classification: first event type
        #, m.target        = NULL
        #, time
        #, surv.type       = c("mort", "rel.freq", "surv", "years.lost", "cif", "chf")
        , class.type      = c("prob", "bayes")[1]
        , partial         = TRUE # FALSE = Marginal plots, TRUE = Partial plots
        , oob             = TRUE
        , show.plots      = TRUE
        , plots.per.page  = 4
        , granule         = 5
        , sorted          = FALSE #TRUE
        #, nvar
        , npts            = 25
        , smooth.lines    = FALSE  # when partial = TRUE, use lowess smoothed lines (too smooth)
        #, subset
        , main            = paste0("Partial plot, target: ", levels(o_class_sel$class)[i_level])
        )
      )
  }
  # cowplot::plot_grid(out[[ "plot_o_class_sel_marginal_effects" ]][[ levels(o_class_sel$class)[1] ]])


  ### ROC Curve
  print(paste0("erikmisc::e_rfsrc_classification, "
    , round(lubridate::duration((proc.time() - time_start)["elapsed"], units="seconds"), 2) |> as.character()
    , ":  ROC Curve (selected model)"
  ))

  # obtains the value of AUC (area under the ROC curve)
  o_class_sel_AUC <-
    randomForestSRC::get.auc(
      y    = dat_rf_data[[ rf_y_var ]]
    , prob = o_class_sel$predicted.oob
    )
  out[[ "o_class_sel_AUC" ]] <-
    o_class_sel_AUC


  ## ROC via erikmisc
  out_roc_temp <- list()
  for (n_target in levels(dat_rf_data[[ rf_y_var ]])) {
    out_roc <-
      e_plot_roc(
        labels_true     = ifelse(dat_rf_data[[ rf_y_var ]] == n_target, 1, 0)
      , pred_values_pos = o_class_sel$predicted.oob[, n_target]
      , label_neg_pos   = c(0, 1)
      , sw_plot         = TRUE
      )
    #p <- out$plot_roc
    out_roc$plot_roc <- out_roc$plot_roc + labs(title = paste0("ROC Curve, Target:  ", n_target))
    out_roc$plot_roc <- out_roc$plot_roc + coord_fixed(ratio = 1) # equal axes

    out_roc_temp[[ n_target ]] <- out_roc
  } # n_target

  # hierarchy: reorder ROC objects by type (rather than target)
  out[[ "plot_o_class_sel_ROC" ]] <- list()
  for (n_object in names(out_roc_temp[[ 1 ]])) {
    ## n_object = names(out_roc_temp[[ 1 ]])[1]
    out[[ "plot_o_class_sel_ROC" ]][[ n_object ]] <- list()

    for (n_target in names(out_roc_temp)) {
      ## n_target = names(out_roc_temp)[1]
      out[[ "plot_o_class_sel_ROC" ]][[ n_object ]][[ n_target ]] <-
        out_roc_temp[[ n_target ]][[ n_object ]]

      if (n_object == "roc_curve_best") {
        out[[ "plot_o_class_sel_ROC" ]][[ n_object ]][[ n_target ]] <-
          out[[ "plot_o_class_sel_ROC" ]][[ n_object ]][[ n_target ]] |>
          dplyr::mutate(
            Group = n_target
          ) |>
          dplyr::relocate(Group)
      }
      if (n_object == "roc_curve") {
        out[[ "plot_o_class_sel_ROC" ]][[ n_object ]][[ n_target ]] <-
          out[[ "plot_o_class_sel_ROC" ]][[ n_object ]][[ n_target ]] |>
          dplyr::mutate(
            Group = n_target
          ) |>
          dplyr::relocate(Group)
      }

    }
  }

  #out[[ "plot_o_class_sel_ROC" ]] <- p_list

  # p_arranged <-
  #   cowplot::plot_grid(
  #     plotlist = out[[ "plot_o_class_sel_ROC" ]]
  #   , nrow = 1
  #   #, ncol = 2
  #   )
  # p_arranged |> print()


  # Compile training summary plot (also after selection if no X var)
  out[[ "plot_rf_train_all_summary" ]] <-
    cowplot::plot_grid(out$plot_o_class              ) +
    cowplot::plot_grid(out$plot_o_class_subsample    ) +
    cowplot::plot_grid(out$plot_o_class_sel          ) +
    cowplot::plot_grid(out$plot_o_class_sel_subsample) +
    cowplot::plot_grid(plotlist = out$plot_o_class_sel_ROC$plot_roc, nrow = 1) +
    patchwork::plot_layout(design = plot_design) +
    patchwork::plot_annotation(
      title       = text_formula
    , subtitle    = text_formula_sel
    , caption     = paste0(
                      "Full model AUC = "
                    , round(out$o_class_AUC, 3)
                    , ";  "
                    , "Selected model AUC = "
                    , round(out$o_class_sel_AUC, 3)
                    )
    , tag_levels  = "A"
    ) +
    theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1, Caption align left

    # ggsave(
    #     paste0("out/plot_rf_train_all_summary__", n_var_group, "_-_", n_dx_group, ".png")
    #   , plot   = out[[ "plot_rf_train_all_summary" ]]
    #   , width  = 16
    #   , height = 16
    #   ## png, jpeg
    #   , dpi    = 300
    #   , bg     = "white"
    #   ## pdf
    #   #, units  = "in"
    #   #, useDingbats = FALSE
    #   )



  print(paste0("erikmisc::e_rfsrc_classification, "
    , round(lubridate::duration((proc.time() - time_start)["elapsed"], units="seconds"), 2) |> as.character()
    , ":  Complete"
  ))

  return(out)

} # e_rfsrc_classification
