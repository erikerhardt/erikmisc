#' Random Forests classification workflow using \code{randomForestSRC}
#'
#' @param dat_rf_class    data.frame with data
#' @param rf_y_var        y factor variable, if NULL then assumed to be the first column of \code{dat_rf_class}
#' @param rf_x_var        x variables, if NULL then assumed to be all except the first column of \code{dat_rf_class}
#' @param rf_id_var       ID variable, removed from dataset prior to analysis
#' @param sw_rfsrc_ntree  \code{ntree} argument for \code{randomForestSRC::rfsrc()}
#' @param sw_alpha        \code{alpha} argument for \code{randomForestSRC::plot.subsample()} and \code{randomForestSRC::extract.bootsample()}
#' @param sw_select_full  run with model selection, or only fit full model
#' @param sw_save_model   T/F to save model (XXX, not yet implemented)
#' @param plot_title      title for plots
#' @param out_path        path to save output
#' @param file_prefix     file prefix for saved output
#' @param plot_format     plot format supported by \code{ggplot2::ggsave()}
#' @param n_marginal_plot_across for partial and marginal plots, number of plots per row (increase if not all plots are displayed)
#'
#' @return list with many RF objects, summaries, and plots
#' @import parallel
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import randomForestSRC
#' @import patchwork
#' @importFrom tidyselect all_of
#' @importFrom lubridate duration
#' @importFrom purrr keep
#' @importFrom tidyselect all_of
#' @importFrom cowplot as_grob
#' @importFrom cowplot plot_grid
#' @importFrom readr write_lines
#' @importFrom readr write_csv
#' @importFrom tibble as_tibble
#' @importFrom stringr str_wrap
#' @export
#'
#' @examples
#' dat_rf_class <-
#'   erikmisc::dat_mtcars_e |>
#'   #dplyr::select(
#'   #  -model
#'   #) |>
#'   #dplyr::select(
#'   #  cyl
#'   #, tidyselect::everything()
#'   #) |>
#'   dplyr::mutate(
#'   #  ID    = 1:dplyr::n() # ID number
#'     disp  =              # add missing values
#'       dplyr::case_when(
#'         disp |> dplyr::between(160, 170) ~ NA
#'       , TRUE ~ disp
#'       )
#'   , X1 = rnorm(dplyr::n())
#'   , X2 = rnorm(dplyr::n())
#'   , X3 = rnorm(dplyr::n())
#'   , X4 = rnorm(dplyr::n())
#'   , X5 = rnorm(dplyr::n())
#'   , X6 = rnorm(dplyr::n())
#'   , X7 = rnorm(dplyr::n())
#'   , X8 = rnorm(dplyr::n())
#'   , X9 = rnorm(dplyr::n())
#'   )
#'
#' out_e_rf <-
#'   e_rfsrc_classification(
#'     dat_rf_class    = dat_rf_class
#'   , rf_y_var        = "cyl"    # NULL
#'   , rf_x_var        = NULL # c("mpg", "disp", "hp", "drat", "wt", "qsec", "vs", "am") # NULL
#'   , rf_id_var       = "model"
#'   , sw_rfsrc_ntree  = 200
#'   , sw_alpha        = 0.05
#'   , sw_select_full  = c("select", "full")[1]
#'   , sw_save_model   = c(TRUE, FALSE)[1]
#'   , plot_title      = "Random Forest Title SEL"
#'   , out_path        = "./out_sel"
#'   , file_prefix     = "out_e_rf"
#'   , plot_format     = c("png", "pdf")[1]
#'   , n_marginal_plot_across = 4
#'   )
#'
#' \dontrun{
#'
#' ## Overall summaries
#'
#' # Summary of Full and reduced model fits with ROC curves
#' out_e_rf[[ "plot_rf_train_all_summary" ]]
#'
#' # Selected model: ROC object from e_plot_roc()
#' out_e_rf[[ "plot_o_class_sel_ROC" ]]$plot_roc |>
#'   cowplot::plot_grid(plotlist = _, nrow = 1)
#'
#' # Selected model: Marginal/Partial effects plots
#'   # for each level of the response variable
#' for (i_level in seq_along(levels(dat_rf_class[[ out_e_rf[[ "rf_y_var" ]] ]]))) {
#'   out_e_rf[[ "plot_o_class_sel_marginal_effects" ]][[ i_level ]] |>
#'     cowplot::plot_grid() |>
#'     print()
#' }
#'
#'
#' ## Full model summaries
#' # y response variable
#' out_e_rf[[ "rf_y_var" ]]
#' # Full model: x predictor variables
#' out_e_rf[[ "rf_x_var_full" ]]
#' # Full model: formula
#' out_e_rf[[ "rf_formula_full" ]]
#' # Full model: rfsrc classification object
#' out_e_rf[[ "o_class_full" ]]
#' # Full model: convergence
#' out_e_rf[[ "plot_o_class_full" ]] |>
#'   cowplot::plot_grid()
#' # Full model: variable importance (VIMP) table
#' out_e_rf[[ "o_class_full_importance" ]]
#' # Full model: variable importance (VIMP) plot
#' out_e_rf[[ "plot_o_class_full_importance" ]]
#' # Full model: ROC AUC (area under the curve)
#' out_e_rf[[ "o_class_full_AUC" ]]
#' # Full model: subsample iterates for VIMP and model selection
#' out_e_rf[[ "o_class_full_subsample" ]]
#' # Full model: subsample VIMP model selection
#' out_e_rf[[ "o_class_full_subsample_extract_subsample" ]]
#' # Full model: subsample double bootstrap VIMP model selection
#' out_e_rf[[ "o_class_full_subsample_extract_bootsample" ]]
#' # Full model: variable importance (VIMP) plot boxplots
#' out_e_rf[[ "plot_o_class_full_subsample" ]] |>
#'   cowplot::plot_grid()
#'
#' ## Selected model summaries
#' # Selected model: x predictor variables
#' out_e_rf[[ "rf_x_var_sel" ]]
#' # Selected model: formula
#' out_e_rf[[ "rf_formula_sel" ]]
#' # Selected model: rfsrc classification object
#' out_e_rf[[ "o_class_sel" ]]
#' # Selected model: convergence
#' out_e_rf[[ "plot_o_class_sel" ]] |>
#'   cowplot::plot_grid()
#' # Selected model: variable importance (VIMP) table
#' out_e_rf[[ "o_class_sel_importance" ]]
#' # Selected model: variable importance (VIMP) plot
#' out_e_rf[[ "plot_o_class_sel_importance" ]]
#' # Selected model: ROC AUC (area under the curve)
#' out_e_rf[[ "o_class_sel_AUC" ]]
#' # Selected model: subsample iterates for VIMP and model selection
#' out_e_rf[[ "o_class_sel_subsample" ]]
#' # Selected model: subsample VIMP model selection
#' out_e_rf[[ "o_class_sel_subsample_extract_subsample" ]]
#' # Selected model: subsample double bootstrap VIMP model selection
#' out_e_rf[[ "o_class_sel_subsample_extract_bootsample" ]]
#' # Selected model: variable importance (VIMP) plot boxplots
#' out_e_rf[[ "plot_o_class_sel_subsample" ]] |>
#'   cowplot::plot_grid()
#'
#' }
e_rfsrc_classification <-
  function(
    dat_rf_class    = NULL
  , rf_y_var        = NULL
  , rf_x_var        = NULL
  , rf_id_var       = NULL
  , sw_rfsrc_ntree  = 2000
  , sw_alpha        = 0.05
  , sw_select_full  = c("select", "full")[1]
  , sw_save_model   = c(TRUE, FALSE)[1]
  , plot_title      = "Random Forest Title"
  , out_path        = "out"
  , file_prefix     = "out_e_rf"
  , plot_format     = c("png", "pdf")[1]
  , n_marginal_plot_across = 6
  ) {
  ## dat_rf_class <-
  ##   erikmisc::dat_mtcars_e |>
  ##   dplyr::select(
  ##     -model
  ##   ) |>
  ##   dplyr::select(
  ##     cyl
  ##   , tidyselect::everything()
  ##   ) |>
  ##   dplyr::mutate(
  ##     ID    = 1000 + 1:dplyr::n() # ID number
  ##   , disp  =              # add missing values
  ##       dplyr::case_when(
  ##         disp |> dplyr::between(160, 170) ~ NA
  ##       , TRUE ~ disp
  ##       )
  ##   )
  ##   #|>
  ##   #dplyr::relocate(
  ##   #  ID
  ##   #)
  ##
  ## out_e_rf <-
  ##   e_rfsrc_classification(
  ##     dat_rf_class    = dat_rf_class
  ##   , rf_y_var        = NULL # "cyl"    # NULL
  ##   , rf_x_var        = NULL # c("mpg", "disp", "hp", "drat", "wt", "qsec", "vs", "am") # NULL
  ##   , rf_id_var       = "ID"
  ##   , sw_rfsrc_ntree  = 100
  ##   , sw_alpha        = 0.05
  ##   , sw_save_model   = c(TRUE, FALSE)[1]
  ##   , plot_title      = "Random Forest Title"
  ##   , out_path        = "C:/Users/erike/Desktop/temp"
  ##   , file_prefix     = "out_e_rf"
  ##   , plot_format     = c("png", "pdf")[1]
  ##   , n_marginal_plot_across = 6
  ##   )

  f_write_this_text <-
    function(
      this_text
    , sw_append = TRUE
    ) {
      readr::write_lines(
        x    = this_text
      , file =
          file.path(
            out_path
          , paste0(
              file_prefix
            , "__"
            , "text_summary.txt"
            )
          )
      , append = sw_append
      )
  }

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

  this_text <-
    paste0("erikmisc::e_rfsrc_classification, "
      , round(lubridate::duration((proc.time() - time_start)["elapsed"], units="seconds"), 2) |> as.character()
      , ":  Create output directory"
    )

  print(this_text)

  # create output directory, warning if already exists
  dir.create(out_path, showWarnings = FALSE, recursive = TRUE)
  f_write_this_text(this_text, sw_append = FALSE)

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

  # ID label
  if (is.null(rf_id_var)) {
    rownames(dat_rf_data) <- 1:nrow(dat_rf_data)
  } else {
    rownames(dat_rf_data) <- dat_rf_data[[ rf_id_var ]]
    dat_rf_data <-
      dat_rf_data |>
      dplyr::select(
        -tidyselect::all_of(rf_id_var)
      )
  }

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

  rf_x_var_full <-
    rf_x_var


  this_text <-
    paste0("erikmisc::e_rfsrc_classification, "
      , round(lubridate::duration((proc.time() - time_start)["elapsed"], units="seconds"), 2) |> as.character()
      , ":  Removing rows with NAs (later versions will allow imputations)"
    )
  print(this_text)
  f_write_this_text(this_text)

  dat_rf_data <-
    dat_rf_data |>
    tidyr::drop_na()


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
    rf_x_var_full <- rf_x_var_full[rf_x_var_full %notin% all_na_columns]
  }

  # check for at least 2 y var levels
  if (length(unique(dat_rf_data[[ rf_y_var ]])) < 2) {
    warning(paste0("erikmisc::e_rfsrc_classification, returning NULL: rf_y_var (", rf_y_var, ") needs at least two levels:  ", paste(unique(dat_rf_data[[ rf_y_var ]]), collapse = ", ")))
    return(NULL)
  }


  rf_formula_full <-
    paste(
      rf_y_var
    , " ~ "
    , paste(
        rf_x_var_full
      , collapse= " + "
      )
    ) |>
    as.formula()

  #out[[ "dat_rf_data" ]] <- dat_rf_data
  out[[ "rf_y_var"    ]] <-
    rf_y_var
  out[[ "rf_x_var_full"    ]] <-
    rf_x_var_full
  out[[ "rf_formula_full"  ]] <-
    rf_formula_full

  text_formula <-
    paste(as.character(out$rf_formula_full)[c(2, 1, 3)], collapse = " ") |>
    stringr::str_wrap(width = 120, exdent = 4)

  this_text <-
    paste0("Full model: ", text_formula)
  f_write_this_text(this_text)



  ### Grow forest
  this_text <-
    paste0("erikmisc::e_rfsrc_classification, "
      , round(lubridate::duration((proc.time() - time_start)["elapsed"], units="seconds"), 2) |> as.character()
      , ":  Grow forest"
    )
  print(this_text)
  f_write_this_text(this_text)

  o_class_full <-
    randomForestSRC::rfsrc(
      formula     = rf_formula_full
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
    , forest.wt   = c("inbag", "oob", "all", TRUE, FALSE)[5]    # TRUE = "inbag", 11/2/2023 changed from [1], crashes sometimes
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

  out[[ "o_class_full" ]] <-
    o_class_full


  # save model (to be used for prediction)
  if(sw_save_model) {
    this_text <-
      paste0("erikmisc::e_rfsrc_classification, "
        , round(lubridate::duration((proc.time() - time_start)["elapsed"], units="seconds"), 2) |> as.character()
        , ":  Write model object to .RData file"
      )
    print(this_text)
    f_write_this_text(this_text)

    save(
      o_class_full
    , file =
        file.path(
          out_path
        , paste0(
            file_prefix
          , "__"
          , "model_o_class_full"
          , ".RData"
          )
        )
    )
  }


  ### Plot convergence
  out[[ "plot_o_class_full" ]] <-
    #cowplot::as_grob(
    #  ~plot(o_class_full)
    #)
    patchwork::wrap_elements(
      full =
        cowplot::as_grob(
          ~ randomForestSRC::plot.rfsrc(
            x               = o_class_full
          , m.target        = NULL
          , plots.one.page  = TRUE
          , sorted          = TRUE
          , verbose         = FALSE
          )
        )
    ) +
    labs(
      title     = plot_title
    , subtitle  = "Full model, Error Rate and Variable Importance"
    , caption   = paste0(
                    "Left: Prediction error is calculated using OOB.  Error rates should (roughly) converge, otherwise increase number of trees."
                  , "\n"
                  , "Right: Variable Importance is the prediction error attributable to excluding the variable."
                  )
    ) +
    theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)

  ggplot2::ggsave(
      filename =
        file.path(
          out_path
        , paste0(
            file_prefix
          , "__"
          , "plot_o_class_full"
          , "."
          , plot_format
          )
        )
    , plot   =
        out[[ "plot_o_class_full" ]]
    , width  = 12
    , height = 8
    ## png, jpeg
    , dpi    = 300
    , bg     = "white"
    ## pdf
    , units  = "in"
    #, useDingbats = FALSE
    )



  # cowplot::plot_grid(out[[ "plot_o_class_full" ]])

  ### Variable Importance (also for variable selection)
  # VIMP
  out[[ "o_class_full_importance" ]] <-
    o_class_full$importance            # Variable importance (VIMP) for each x-variable.

  readr::write_csv(
    x = tibble::as_tibble(o_class_full$importance, rownames = "Var")
  , file =
      file.path(
        out_path
      , paste0(
          file_prefix
        , "__"
        , "o_class_full_importance"
        , ".csv"
        )
      )
  )

  out[[ "plot_o_class_full_importance" ]] <-
    e_plot_rf_vimp(o_class_full$importance) +
    labs(
      title     = plot_title
    , subtitle  = "Full model, Variable Importance"
    , caption   = paste0(
                    "Variable Importance is the prediction error attributable to excluding the variable.\nA large positive value indicates a variable with predictive importance."
                  )
    ) +
    theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)

  ggplot2::ggsave(
      filename =
        file.path(
          out_path
        , paste0(
            file_prefix
          , "__"
          , "plot_o_class_full_importance"
          , "."
          , plot_format
          )
        )
    , plot   =
        out[[ "plot_o_class_full_importance" ]]
    , width  = 4 + 2 * ncol(o_class_full$importance)
    , height = 2 + 0.25 * length(rf_x_var_full)
    ## png, jpeg
    , dpi    = 300
    , bg     = "white"
    ## pdf
    , units  = "in"
    #, useDingbats = FALSE
    )

  out_vimp_temp <- list()
  for (i_level in seq_along(levels(dat_rf_data[[ rf_y_var ]]))) {
    ## i_level = 1

    n_target <- levels(dat_rf_data[[ rf_y_var ]])[i_level]

    out_vimp_temp[[ n_target ]] <-
      e_plot_rf_vimp(o_class_full$importance, targets = c("all", n_target)) +
      labs(
        title     = plot_title
      , subtitle  = "Full model, Variable Importance"
      , caption   = paste0(
                      "Variable Importance is the prediction error attributable to excluding the variable.\nA large positive value indicates a variable with predictive importance."
                    )
      ) +
      theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)

    ggplot2::ggsave(
        filename =
          file.path(
            out_path
          , paste0(
              file_prefix
            , "__"
            , "plot_o_class_full_importance"
            , "_"
            , i_level
            , "-"
            , levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]])[i_level]
            , "."
            , plot_format
            )
          )
      , plot   =
          out_vimp_temp[[ levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]])[i_level] ]]
      , width  = 4 + 2 * 2
      , height = 2 + 0.25 * length(rf_x_var_full)
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      , units  = "in"
      #, useDingbats = FALSE
      )

  } # i_level




  # obtains the value of AUC (area under the ROC curve)
  o_class_full_AUC <-
    randomForestSRC::get.auc(
      y    = dat_rf_data[[ rf_y_var ]]
    , prob = o_class_full$predicted.oob
    )
  out[[ "o_class_full_AUC" ]] <-
    o_class_full_AUC

  this_text <-
    paste0("o_class_full_AUC: ", o_class_full_AUC)
  f_write_this_text(this_text)


  ## ROC via erikmisc
  out_roc_temp <- list()
  for (n_target in levels(dat_rf_data[[ rf_y_var ]])) {
    out_roc <-
      e_plot_roc(
        labels_true     = ifelse(dat_rf_data[[ rf_y_var ]] == n_target, 1, 0)
      , pred_values_pos = o_class_full$predicted.oob[, n_target]
      , label_neg_pos   = c(0, 1)
      , sw_plot         = TRUE
      )
    #p <- out$plot_roc
    out_roc$plot_roc <- out_roc$plot_roc + labs(title = paste0("ROC Curve, Target:  ", n_target))
    out_roc$plot_roc <- out_roc$plot_roc + coord_fixed(ratio = 1) # equal axes

    out_roc_temp[[ n_target ]] <- out_roc
  } # n_target

  # hierarchy: reorder ROC objects by type (rather than target)
  out[[ "o_class_full_ROC" ]] <- list()
  for (n_object in names(out_roc_temp[[ 1 ]])) {
    ## n_object = names(out_roc_temp[[ 1 ]])[1]
    out[[ "o_class_full_ROC" ]][[ n_object ]] <- list()

    for (n_target in names(out_roc_temp)) {
      ## n_target = names(out_roc_temp)[1]
      out[[ "o_class_full_ROC" ]][[ n_object ]][[ n_target ]] <-
        out_roc_temp[[ n_target ]][[ n_object ]]

      if (n_object == "roc_curve_best") {
        out[[ "o_class_full_ROC" ]][[ n_object ]][[ n_target ]] <-
          out[[ "o_class_full_ROC" ]][[ n_object ]][[ n_target ]] |>
          dplyr::mutate(
            Group = n_target
          ) |>
          dplyr::relocate(Group)
      }
      if (n_object == "roc_curve") {
        out[[ "o_class_full_ROC" ]][[ n_object ]][[ n_target ]] <-
          out[[ "o_class_full_ROC" ]][[ n_object ]][[ n_target ]] |>
          dplyr::mutate(
            Group = n_target
          ) |>
          dplyr::relocate(Group)
      }

    }
  }

  readr::write_csv(
    x = out[[ "o_class_full_ROC" ]]$roc_curve_best |> dplyr::bind_rows()
  , file =
      file.path(
        out_path
      , paste0(
          file_prefix
        , "__"
        , "o_class_full_ROC"
        , ".csv"
        )
      )
  )

  out[[ "plot_o_class_full_ROC" ]] <-
    patchwork::wrap_elements(
      full =
        cowplot::plot_grid(plotlist = out$o_class_full_ROC$plot_roc, nrow = 1)
    ) +
    patchwork::plot_annotation(
    #labs(
      title     = plot_title
    , subtitle  = "Full model, ROC Curves"
    #, caption   = paste0(
    #                ""
    #              )
    , theme = theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)
    )
    # +
    #theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)

  ggplot2::ggsave(
      filename =
        file.path(
          out_path
        , paste0(
            file_prefix
          , "__"
          , "plot_o_class_full_ROC"
          , "."
          , plot_format
          )
        )
    , plot   =
        out[[ "plot_o_class_full_ROC" ]]
    , width  = 5 * length(levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]]))
    , height = 6
    ## png, jpeg
    , dpi    = 300
    , bg     = "white"
    ## pdf
    , units  = "in"
    #, useDingbats = FALSE
    )

  for (i_level in seq_along(levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]]))) {
    ggplot2::ggsave(
        filename =
          file.path(
            out_path
          , paste0(
              file_prefix
            , "__"
            , "plot_o_class_full_ROC"
            , "_"
            , i_level
            , "-"
            , levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]])[i_level]
            , "."
            , plot_format
            )
          )
      , plot   =
          out$o_class_full_ROC$plot_roc[[ i_level ]] +
          patchwork::plot_annotation(
          #labs(
            title     = plot_title
          , subtitle  = "Full model, ROC Curves"
          #, caption   = paste0(
          #                ""
          #              )
          , theme = theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)
          )
      , width  = 5
      , height = 6
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      , units  = "in"
      #, useDingbats = FALSE
      )
  } # i_level

  # VIMP and ROC for each target
  for (i_level in seq_along(levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]]))) {
    ## i_level = 1

    plot_temp <-
      patchwork::wrap_plots(
        out_vimp_temp[[ i_level ]]
      , out$o_class_full_ROC$plot_roc[[ i_level ]] +
        patchwork::plot_annotation(
        #labs(
          title     = plot_title
        , subtitle  = "Full model, ROC Curves"
        #, caption   = paste0(
        #                ""
        #              )
        , theme = theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)
        )
      )

    ggplot2::ggsave(
        filename =
          file.path(
            out_path
          , paste0(
              file_prefix
            , "__"
            , "plot_o_class_full_VIMP_ROC"
            , "_"
            , i_level
            , "-"
            , levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]])[i_level]
            , "."
            , plot_format
            )
          )
      , plot   =
          plot_temp
      , width  = (4 + 2 * 2) + 5
      , height = 6
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      , units  = "in"
      #, useDingbats = FALSE
      )
  } # i_level



  ### Confidence intervals and standard errors for VIMP (variable importance)
  this_text <-
    paste0("erikmisc::e_rfsrc_classification, "
      , round(lubridate::duration((proc.time() - time_start)["elapsed"], units="seconds"), 2) |> as.character()
      , ":  Subsample for VIMP and model selection"
    )
  print(this_text)
  f_write_this_text(this_text)

  o_class_full_subsample <-
    randomForestSRC::subsample(
      obj               = o_class_full
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
  out[[ "o_class_full_subsample" ]] <-
    o_class_full_subsample

  out[[ "o_class_full_subsample_extract_subsample" ]] <-
    randomForestSRC::extract.subsample (o_class_full_subsample, alpha = sw_alpha)

  # Use double bootstrap approach in place of subsampling? Much slower, but potentially more accurate.
  out[[ "o_class_full_subsample_extract_bootsample" ]] <-
    randomForestSRC::extract.bootsample(o_class_full_subsample, alpha = sw_alpha)

  ### VIMP plot out to a selected alpha level
  out[[ "plot_o_class_full_subsample" ]] <-
    patchwork::wrap_elements(
      full =
        cowplot::as_grob(
          ~ randomForestSRC::plot.subsample(
            x           = o_class_full_subsample
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
    ) +
    labs(
      title     = plot_title
    , subtitle  = "Full model, Subsampled VIMP Confidence Intervals"
    , caption   = paste0(
                    "Variable Importance is the prediction error attributable to excluding the variable."
                  )
    ) +
    theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)

  ggplot2::ggsave(
      filename =
        file.path(
          out_path
        , paste0(
            file_prefix
          , "__"
          , "plot_o_class_full_subsample"
          , "."
          , plot_format
          )
        )
    , plot   =
        out[[ "plot_o_class_full_subsample" ]]
    , width  = 8
    , height = 2 + 0.25 * length(rf_x_var_full)
    ## png, jpeg
    , dpi    = 300
    , bg     = "white"
    ## pdf
    , units  = "in"
    #, useDingbats = FALSE
    )

  # cowplot::plot_grid(out[[ "plot_o_class_full_subsample" ]])



  ## Select variables from randomForestSRC::extract.bootsample(o_class_full_subsample)
  this_text <-
    paste0("erikmisc::e_rfsrc_classification, "
      , round(lubridate::duration((proc.time() - time_start)["elapsed"], units="seconds"), 2) |> as.character()
      , ":  Model selection"
    )
  print(this_text)
  f_write_this_text(this_text)

  rf_x_var_sel <-
    rownames(
      randomForestSRC::extract.bootsample(o_class_full_subsample, alpha = sw_alpha)$var.sel.Z
    )[randomForestSRC::extract.bootsample(o_class_full_subsample, alpha = sw_alpha)$var.sel.Z$signif]

  out[[ "rf_x_var_sel" ]] <-
    rf_x_var_sel


  # ----------------------------------------
  # Full model, only output

  if (length(rf_x_var_sel) == 0) {
    sw_select_full <- c("select", "full")[2]
    warning("erikmisc::e_rfsrc_classification, model selection has 0 x variables")
  }

  if (sw_select_full  == c("select", "full")[2]) {

    ### Marginal/Partial effects plots
    this_text <-
      paste0("erikmisc::e_rfsrc_classification, "
        , round(lubridate::duration((proc.time() - time_start)["elapsed"], units="seconds"), 2) |> as.character()
        , ":  Partial effects plots (full model)"
      )
    print(this_text)
    f_write_this_text(this_text)

    # 11/2/2023 See the lapply() code below which overcomes lazy evaluation in the cowplot ggplot code,
    #   which was resulting in all plots in the list being the last plot
    ## out[[ "plot_o_class_full_marginal_effects" ]] <- list()
    ## for (i_level in seq_along(levels(o_class_full$class))) {
    ##   print(i_level)
    ##   print(levels(o_class_full$class)[i_level])
    ##
    ##   # Partial effects plots
    ##   out[[ "plot_o_class_full_marginal_effects" ]][[ levels(o_class_full$class)[i_level] ]] <-
    ##     cowplot::as_grob(
    ##       ~randomForestSRC::plot.variable(
    ##         x               = o_class_full
    ##       #, xvar.names
    ##       , target          = levels(o_class_full$class)[i_level]   # classification: first event type
    ##       #, m.target        = NULL
    ##       #, time
    ##       #, surv.type       = c("mort", "rel.freq", "surv", "years.lost", "cif", "chf")
    ##       , class.type      = c("prob", "bayes")[1]
    ##       , partial         = TRUE # FALSE = Marginal plots, TRUE = Partial plots
    ##       , oob             = TRUE
    ##       , show.plots      = TRUE
    ##       , plots.per.page  = 4
    ##       , granule         = 5
    ##       , sorted          = FALSE #TRUE
    ##       #, nvar
    ##       , npts            = 25
    ##       , smooth.lines    = FALSE  # when partial = TRUE, use lowess smoothed lines (too smooth)
    ##       #, subset
    ##       , main            = paste0("Partial plot, target: ", levels(o_class_full$class)[i_level])
    ##       )
    ##     )
    ## }
    # cowplot::plot_grid(out[[ "plot_o_class_full_marginal_effects" ]][[ levels(o_class_full$class)[1] ]])
    # cowplot::plot_grid(out[[ "plot_o_class_full_marginal_effects" ]][[ 2 ]])

    out[[ "plot_o_class_full_partial_effects" ]] <-
      lapply(
        seq_along(levels(o_class_full$class))
      , function(i_level) {
          cowplot::as_grob(
            ~randomForestSRC::plot.variable(
              x               = o_class_full
            #, xvar.names      = o_class_full$importance |> tibble::as_tibble(rownames = "rf_x_var") |> dplyr::arrange(dplyr::desc(all)) |> dplyr::pull(rf_x_var)
            , target          = levels(o_class_full$class)[i_level]   # classification: first event type
            #, m.target        = NULL
            #, time
            #, surv.type       = c("mort", "rel.freq", "surv", "years.lost", "cif", "chf")
            , class.type      = c("prob", "bayes")[1]
            , partial         = TRUE # FALSE = Marginal plots, TRUE = Partial plots
            , oob             = TRUE
            , show.plots      = TRUE
            , plots.per.page  = n_marginal_plot_across # 4
            , granule         = 5
            , sorted          = TRUE #FALSE
            #, nvar
            , npts            = 25
            , smooth.lines    = FALSE  # when partial = TRUE, use lowess smoothed lines (too smooth)
            #, subset
            , main            = paste0("Partial plot, target: ", levels(o_class_full$class)[i_level])
            )
          )
        }
      )

    for (i_level in seq_along(levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]]))) {
      out[[ "plot_o_class_full_partial_effects" ]][[ i_level ]] <-
        patchwork::wrap_elements(
          full =
            #cowplot::as_grob(
              out[[ "plot_o_class_full_partial_effects" ]][[ i_level ]] |>
                  cowplot::plot_grid()
            #)
        ) +
        patchwork::plot_annotation(
        #labs(
          title     = plot_title
        , subtitle  = "Full model, Partial plot, predicted marginal value (adjusted) for variable"
        , caption   = paste0(
                        "For continuous variables, red points are used to indicate partial values (adjusted, integrate out other variables) and"
                      #, "\n"
                      , "  dashed red lines indicate a smoothed error bar of +/- two standard errors. "
                      #, "\n"
                      , "Black dashed line are the partial values."
                      , "\n"
                      , "For discrete variables, partial values are indicated using boxplots with whiskers extending out approximately two standard errors from the mean."
                      , "\n"
                      , "Standard errors are meant only to be a guide and should be interpreted with caution."
                     )
        , theme = theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)
        )

      ggplot2::ggsave(
          filename =
            file.path(
              out_path
            , paste0(
                file_prefix
              , "__"
              , "plot_o_class_full_partial_effects"
              , "_"
              , i_level
              , "-"
              , levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]])[i_level]
              , "."
              , plot_format
              )
            )
        , plot   =
            out[[ "plot_o_class_full_partial_effects" ]][[ i_level ]]
        , width  = 3 * n_marginal_plot_across
        , height = 4 * ceiling(length(out[[ "rf_x_var_full" ]]) / n_marginal_plot_across)
        ## png, jpeg
        , dpi    = 300
        , bg     = "white"
        ## pdf
        , units  = "in"
        #, useDingbats = FALSE
        )
    } # i_level

      ## for (i_level in seq_along(levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]]))) {
      ##   out[[ "plot_o_class_full_partial_effects" ]][[ i_level ]] |>
      ##     cowplot::plot_grid() |>
      ##     print()
      ## }


    out[[ "plot_o_class_full_marginal_effects" ]] <-
      lapply(
        seq_along(levels(o_class_full$class))
      , function(i_level) {
          cowplot::as_grob(
            ~randomForestSRC::plot.variable(
              x               = o_class_full
            #, xvar.names      = o_class_full$importance |> tibble::as_tibble(rownames = "rf_x_var") |> dplyr::arrange(dplyr::desc(all)) |> dplyr::pull(rf_x_var)
            , target          = levels(o_class_full$class)[i_level]   # classification: first event type
            #, m.target        = NULL
            #, time
            #, surv.type       = c("mort", "rel.freq", "surv", "years.lost", "cif", "chf")
            , class.type      = c("prob", "bayes")[1]
            , partial         = FALSE # FALSE = Marginal plots, TRUE = Partial plots
            , oob             = TRUE
            , show.plots      = TRUE
            , plots.per.page  = n_marginal_plot_across # 4
            , granule         = 5
            , sorted          = TRUE #FALSE
            #, nvar
            , npts            = 25
            , smooth.lines    = FALSE  # when partial = TRUE, use lowess smoothed lines (too smooth)
            #, subset
            , main            = paste0("Marginal plot, target: ", levels(o_class_full$class)[i_level])
            )
          )
        }
      )

    for (i_level in seq_along(levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]]))) {
      out[[ "plot_o_class_full_marginal_effects" ]][[ i_level ]] <-
        patchwork::wrap_elements(
          full =
            #cowplot::as_grob(
              out[[ "plot_o_class_full_marginal_effects" ]][[ i_level ]] |>
                  cowplot::plot_grid()
            #)
        ) +
        patchwork::plot_annotation(
        #labs(
          title     = plot_title
        , subtitle  = "Full model, Marginal plot, predicted marginal value (unadjusted) for variable"
        , caption   = paste0(
                        "For continuous variables, red points are used to indicate marginal values (unadjusted for other variables) and"
                      #, "\n"
                      , "  dashed red lines indicate a smoothed error bar of +/- two standard errors. "
                      #, "\n"
                      , "Black dashed line are the marginal values."
                      , "\n"
                      , "For discrete variables, marginal values are indicated using boxplots with whiskers extending out approximately two standard errors from the mean."
                      , "\n"
                      , "Standard errors are meant only to be a guide and should be interpreted with caution."
                     )
        , theme = theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)
        )

      ggplot2::ggsave(
          filename =
            file.path(
              out_path
            , paste0(
                file_prefix
              , "__"
              , "plot_o_class_full_marginal_effects"
              , "_"
              , i_level
              , "-"
              , levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]])[i_level]
              , "."
              , plot_format
              )
            )
        , plot   =
            out[[ "plot_o_class_full_marginal_effects" ]][[ i_level ]]
        , width  = 3 * n_marginal_plot_across
        , height = 4 * ceiling(length(out[[ "rf_x_var_full" ]]) / n_marginal_plot_across)
        ## png, jpeg
        , dpi    = 300
        , bg     = "white"
        ## pdf
        , units  = "in"
        #, useDingbats = FALSE
        )
    } # i_level


    ## out[[ "plot_o_class_full_marginal_effects" ]] <-
    ##   lapply(
    ##     seq_along(levels(o_class_full$class))
    ##   , function(i_level) {
    ##       cowplot::as_grob(
    ##         ~randomForestSRC::plot.variable(
    ##           x               = o_class_full
    ##         #, xvar.names      = o_class_full$importance |> tibble::as_tibble(rownames = "rf_x_var") |> dplyr::arrange(dplyr::desc(all)) |> dplyr::pull(rf_x_var)
    ##         , target          = levels(o_class_full$class)[i_level]   # classification: first event type
    ##         #, m.target        = NULL
    ##         #, time
    ##         #, surv.type       = c("mort", "rel.freq", "surv", "years.lost", "cif", "chf")
    ##         , class.type      = c("prob", "bayes")[1]
    ##         , partial         = FALSE # FALSE = Marginal plots, TRUE = Partial plots
    ##         , oob             = TRUE
    ##         , show.plots      = TRUE
    ##         , plots.per.page  = 4
    ##         , granule         = 5
    ##         , sorted          = TRUE #FALSE
    ##         #, nvar
    ##         , npts            = 25
    ##         , smooth.lines    = FALSE  # when partial = TRUE, use lowess smoothed lines (too smooth)
    ##         #, subset
    ##         , main            = paste0("Marginal plot, target: ", levels(o_class_full$class)[i_level])
    ##         )
    ##       )
    ##     }
    ##   )

      ## for (i_level in seq_along(levels(dat_rf_class[[ out_e_rf[[ "rf_y_var" ]] ]]))) {
      ##   out[[ "plot_o_class_full_marginal_effects" ]][[ i_level ]] |>
      ##     cowplot::plot_grid() |>
      ##     print()
      ## }





    # Compile training summary plot (also at very end when x var)
    # If selected model has NO x variables
    ## out[[ "plot_rf_train_all_summary" ]] <-
    ##   cowplot::plot_grid(out$plot_o_class_full              ) +
    ##   cowplot::plot_grid(out$plot_o_class_full_subsample    ) +
    ##   #cowplot::plot_grid(out$plot_o_class_sel          ) +
    ##   #cowplot::plot_grid(out$plot_o_class_sel_subsample) +
    ##   #cowplot::plot_grid(plotlist = out$plot_o_class_sel_ROC$plot_roc, nrow = 1) +
    ##   patchwork::plot_spacer() +
    ##   patchwork::plot_spacer() +
    ##   patchwork::plot_spacer() +
    ##   patchwork::plot_layout(design = plot_design) +
    ##   patchwork::plot_annotation(
    ##     title       = text_formula
    ##   , subtitle    = "No x variables selected"
    ##   , caption     = paste0(
    ##                     "Full model AUC = "
    ##                   , round(out$o_class_full_AUC, 3)
    ##                   )
    ##   , tag_levels  = "A"
    ##   ) +
    ##   theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)
    out[[ "plot_rf_train_all_summary" ]] <-
      out[[ "plot_o_class_full" ]]               + labs(title = NULL) +
      out[[ "plot_o_class_full_subsample" ]]     + labs(title = NULL) +
      #cowplot::plot_grid(out$plot_o_class_sel          ) +
      #cowplot::plot_grid(out$plot_o_class_sel_subsample) +
      #cowplot::plot_grid(plotlist = out$plot_o_class_sel_ROC$plot_roc, nrow = 1) +
      patchwork::plot_spacer() +
      patchwork::plot_spacer() +
      out[[ "plot_o_class_full_ROC" ]] +
      #cowplot::plot_grid(plotlist = out$plot_o_class_full_ROC$plot_roc, nrow = 1) +
      patchwork::plot_layout(design = plot_design) +
      patchwork::plot_annotation(
        title       = plot_title
      , subtitle    =
                      paste0(
                        "Full model (AUC = ", round(out$o_class_full_AUC, 3), "): "
                      , text_formula
                      , "\n"
                      , "Selected model: "
                      , "No x variables selected"
                      )
      #, caption     = paste0(
      #                  "Full model AUC = "
      #                , round(out$o_class_full_AUC, 3)
      #                )
      , theme = theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)
      , tag_levels  = "A"
      )

    ggplot2::ggsave(
        filename =
          file.path(
            out_path
          , paste0(
              file_prefix
            , "__"
            , "plot_rf_train_all_summary"
            , "."
            , plot_format
            )
          )
      , plot   =
          out[[ "plot_rf_train_all_summary" ]]
      , width  = 18
      , height = 18
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      , units  = "in"
      #, useDingbats = FALSE
      )

    this_text <-
      paste0("erikmisc::e_rfsrc_classification, "
        , round(lubridate::duration((proc.time() - time_start)["elapsed"], units="seconds"), 2) |> as.character()
        , ":  Complete"
      )
    print(this_text)
    f_write_this_text(this_text)

    return(out)
  } # full model only, stop here



  ########################################
  # Selected model

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

  this_text <-
    paste0("Selected model: ", text_formula_sel)
  f_write_this_text(this_text)


  ### Grow forest
  this_text <-
    paste0("erikmisc::e_rfsrc_classification, "
      , round(lubridate::duration((proc.time() - time_start)["elapsed"], units="seconds"), 2) |> as.character()
      , ":  Grow forest (selected model)"
    )
  print(this_text)
  f_write_this_text(this_text)

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
    , forest.wt   = c("inbag", "oob", "all", TRUE, FALSE)[5]    # TRUE = "inbag", 11/2/2023 changed from [1], crashes sometimes
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


  # save model (to be used for prediction)
  if(sw_save_model) {
    this_text <-
      paste0("erikmisc::e_rfsrc_classification, "
        , round(lubridate::duration((proc.time() - time_start)["elapsed"], units="seconds"), 2) |> as.character()
        , ":  Write model object to .RData file"
      )
    print(this_text)
    f_write_this_text(this_text)

    save(
      o_class_sel
    , file =
        file.path(
          out_path
        , paste0(
            file_prefix
          , "__"
          , "model_o_class_sel"
          , ".RData"
          )
        )
    )
  }


  ### Plot convergence
  out[[ "plot_o_class_sel" ]] <-
    #cowplot::as_grob(
    #  ~plot(o_class_sel)
    #)
    patchwork::wrap_elements(
      full =
        cowplot::as_grob(
          ~ randomForestSRC::plot.rfsrc(
            x               = o_class_sel
          , m.target        = NULL
          , plots.one.page  = TRUE
          , sorted          = TRUE
          , verbose         = FALSE
          )
        )
    ) +
    labs(
      title     = plot_title
    , subtitle  = "Selected model, Error Rate and Variable Importance"
    , caption   = paste0(
                    "Left: Prediction error is calculated using OOB.  Error rates should (roughly) converge, otherwise increase number of trees."
                  , "\n"
                  , "Right: Variable Importance is the prediction error attributable to excluding the variable."
                  )
    ) +
    theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)

  ggplot2::ggsave(
      filename =
        file.path(
          out_path
        , paste0(
            file_prefix
          , "__"
          , "plot_o_class_sel"
          , "."
          , plot_format
          )
        )
    , plot   =
        out[[ "plot_o_class_sel" ]]
    , width  = 12
    , height = 8
    ## png, jpeg
    , dpi    = 300
    , bg     = "white"
    ## pdf
    , units  = "in"
    #, useDingbats = FALSE
    )
  # cowplot::plot_grid(out[[ "plot_o_class_sel" ]])


  ### Variable Importance (also for variable selection)
  # VIMP
  out[[ "o_class_sel_importance" ]] <-
    o_class_sel$importance            # Variable importance (VIMP) for each x-variable.

  readr::write_csv(
    x = tibble::as_tibble(o_class_sel$importance, rownames = "Var")
  , file =
      file.path(
        out_path
      , paste0(
          file_prefix
        , "__"
        , "o_class_sel_importance"
        , ".csv"
        )
      )
  )

  out[[ "plot_o_class_sel_importance" ]] <-
    e_plot_rf_vimp(o_class_sel$importance) +
    labs(
      title     = plot_title
    , subtitle  = "Selected model, Variable Importance"
    , caption   = paste0(
                    "Variable Importance is the prediction error attributable to excluding the variable.\nA large positive value indicates a variable with predictive importance."
                  )
    ) +
    theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)

  ggplot2::ggsave(
      filename =
        file.path(
          out_path
        , paste0(
            file_prefix
          , "__"
          , "plot_o_class_sel_importance"
          , "."
          , plot_format
          )
        )
    , plot   =
        out[[ "plot_o_class_sel_importance" ]]
    , width  = 8
    , height = 2 + 0.25 * length(rf_x_var_sel)
    ## png, jpeg
    , dpi    = 300
    , bg     = "white"
    ## pdf
    , units  = "in"
    #, useDingbats = FALSE
    )


# BEGIN ###############################################################################

  out_vimp_sel_temp <- list()
  for (i_level in seq_along(levels(dat_rf_data[[ rf_y_var ]]))) {
    ## i_level = 1

    n_target <- levels(dat_rf_data[[ rf_y_var ]])[i_level]

    out_vimp_sel_temp[[ n_target ]] <-
      e_plot_rf_vimp(o_class_sel$importance, targets = c("all", n_target)) +
      labs(
        title     = plot_title
      , subtitle  = "Selected model, Variable Importance"
      , caption   = paste0(
                      "Variable Importance is the prediction error attributable to excluding the variable.\nA large positive value indicates a variable with predictive importance."
                    )
      ) +
      theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)

    ggplot2::ggsave(
        filename =
          file.path(
            out_path
          , paste0(
              file_prefix
            , "__"
            , "plot_o_class_sel_importance"
            , "_"
            , i_level
            , "-"
            , levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]])[i_level]
            , "."
            , plot_format
            )
          )
      , plot   =
          out_vimp_sel_temp[[ levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]])[i_level] ]]
      , width  = 4 + 2 * 2
      , height = 2 + 0.25 * length(rf_x_var)
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      , units  = "in"
      #, useDingbats = FALSE
      )

  } # i_level




  # obtains the value of AUC (area under the ROC curve)
  o_class_sel_AUC <-
    randomForestSRC::get.auc(
      y    = dat_rf_data[[ rf_y_var ]]
    , prob = o_class_sel$predicted.oob
    )
  out[[ "o_class_sel_AUC" ]] <-
    o_class_sel_AUC

  this_text <-
    paste0("o_class_sel_AUC: ", o_class_sel_AUC)
  f_write_this_text(this_text)


  ## ROC via erikmisc
  out_roc_sel_temp <- list()
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

    out_roc_sel_temp[[ n_target ]] <- out_roc
  } # n_target

  # hierarchy: reorder ROC objects by type (rather than target)
  out[[ "o_class_sel_ROC" ]] <- list()
  for (n_object in names(out_roc_sel_temp[[ 1 ]])) {
    ## n_object = names(out_roc_sel_temp[[ 1 ]])[1]
    out[[ "o_class_sel_ROC" ]][[ n_object ]] <- list()

    for (n_target in names(out_roc_sel_temp)) {
      ## n_target = names(out_roc_sel_temp)[1]
      out[[ "o_class_sel_ROC" ]][[ n_object ]][[ n_target ]] <-
        out_roc_sel_temp[[ n_target ]][[ n_object ]]

      if (n_object == "roc_curve_best") {
        out[[ "o_class_sel_ROC" ]][[ n_object ]][[ n_target ]] <-
          out[[ "o_class_sel_ROC" ]][[ n_object ]][[ n_target ]] |>
          dplyr::mutate(
            Group = n_target
          ) |>
          dplyr::relocate(Group)
      }
      if (n_object == "roc_curve") {
        out[[ "o_class_sel_ROC" ]][[ n_object ]][[ n_target ]] <-
          out[[ "o_class_sel_ROC" ]][[ n_object ]][[ n_target ]] |>
          dplyr::mutate(
            Group = n_target
          ) |>
          dplyr::relocate(Group)
      }

    }
  }

  readr::write_csv(
    x = out[[ "o_class_sel_ROC" ]]$roc_curve_best |> dplyr::bind_rows()
  , file =
      file.path(
        out_path
      , paste0(
          file_prefix
        , "__"
        , "o_class_sel_ROC"
        , ".csv"
        )
      )
  )

  out[[ "plot_o_class_sel_ROC" ]] <-
    patchwork::wrap_elements(
      full =
        cowplot::plot_grid(plotlist = out$o_class_sel_ROC$plot_roc, nrow = 1)
    ) +
    patchwork::plot_annotation(
    #labs(
      title     = plot_title
    , subtitle  = "Selected model, ROC Curves"
    #, caption   = paste0(
    #                ""
    #              )
    , theme = theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)
    )
    # +
    #theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)

  ggplot2::ggsave(
      filename =
        file.path(
          out_path
        , paste0(
            file_prefix
          , "__"
          , "plot_o_class_sel_ROC"
          , "."
          , plot_format
          )
        )
    , plot   =
        out[[ "plot_o_class_sel_ROC" ]]
    , width  = 5 * length(levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]]))
    , height = 6
    ## png, jpeg
    , dpi    = 300
    , bg     = "white"
    ## pdf
    , units  = "in"
    #, useDingbats = FALSE
    )

  for (i_level in seq_along(levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]]))) {
    ggplot2::ggsave(
        filename =
          file.path(
            out_path
          , paste0(
              file_prefix
            , "__"
            , "plot_o_class_sel_ROC"
            , "_"
            , i_level
            , "-"
            , levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]])[i_level]
            , "."
            , plot_format
            )
          )
      , plot   =
          out$o_class_sel_ROC$plot_roc[[ i_level ]] +
          patchwork::plot_annotation(
          #labs(
            title     = plot_title
          , subtitle  = "Selected model, ROC Curves"
          #, caption   = paste0(
          #                ""
          #              )
          , theme = theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)
          )
      , width  = 5
      , height = 6
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      , units  = "in"
      #, useDingbats = FALSE
      )
  } # i_level

  # VIMP and ROC for each target
  for (i_level in seq_along(levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]]))) {
    ## i_level = 1

    plot_temp <-
      patchwork::wrap_plots(
        out_vimp_sel_temp[[ i_level ]]
      , out$o_class_sel_ROC$plot_roc[[ i_level ]] +
        patchwork::plot_annotation(
        #labs(
          title     = plot_title
        , subtitle  = "Selected model, ROC Curves"
        #, caption   = paste0(
        #                ""
        #              )
        , theme = theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)
        )
      )

    ggplot2::ggsave(
        filename =
          file.path(
            out_path
          , paste0(
              file_prefix
            , "__"
            , "plot_o_class_sel_VIMP_ROC"
            , "_"
            , i_level
            , "-"
            , levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]])[i_level]
            , "."
            , plot_format
            )
          )
      , plot   =
          plot_temp
      , width  = (4 + 2 * 2) + 5
      , height = 6
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      , units  = "in"
      #, useDingbats = FALSE
      )
  } # i_level

# END ###############################################################################

  ### Confidence intervals and standard errors for VIMP (variable importance)
  this_text <-
    paste0("erikmisc::e_rfsrc_classification, "
      , round(lubridate::duration((proc.time() - time_start)["elapsed"], units="seconds"), 2) |> as.character()
      , ":  Subsample for VIMP with CIs and SEs (selected model)"
    )
  print(this_text)
  f_write_this_text(this_text)

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
    patchwork::wrap_elements(
      full =
        cowplot::as_grob(
          ~ randomForestSRC::plot.subsample(
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
    ) +
    labs(
      title     = plot_title
    , subtitle  = "Selected model, Subsampled VIMP Confidence Intervals"
    , caption   = paste0(
                    "Variable Importance is the prediction error attributable to excluding the variable."
                  )
    ) +
    theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)

  ggplot2::ggsave(
      filename =
        file.path(
          out_path
        , paste0(
            file_prefix
          , "__"
          , "plot_o_class_sel_subsample"
          , "."
          , plot_format
          )
        )
    , plot   =
        out[[ "plot_o_class_sel_subsample" ]]
    , width  = 8
    , height = 2 + 0.25 * length(rf_x_var_sel)
    ## png, jpeg
    , dpi    = 300
    , bg     = "white"
    ## pdf
    , units  = "in"
    #, useDingbats = FALSE
    )

  # cowplot::plot_grid(out[[ "plot_o_class_sel_subsample" ]])



  ### Marginal/Partial effects plots
  this_text <-
    paste0("erikmisc::e_rfsrc_classification, "
      , round(lubridate::duration((proc.time() - time_start)["elapsed"], units="seconds"), 2) |> as.character()
      , ":  Partial effects plots (selected model)"
    )
  print(this_text)
  f_write_this_text(this_text)

  # 11/2/2023 See the lapply() code below which overcomes lazy evaluation in the cowplot ggplot code,
  #   which was resulting in all plots in the list being the last plot
  ## out[[ "plot_o_class_sel_marginal_effects" ]] <- list()
  ## for (i_level in seq_along(levels(o_class_sel$class))) {
  ##   print(i_level)
  ##   print(levels(o_class_sel$class)[i_level])
  ##
  ##   # Partial effects plots
  ##   out[[ "plot_o_class_sel_marginal_effects" ]][[ levels(o_class_sel$class)[i_level] ]] <-
  ##     cowplot::as_grob(
  ##       ~randomForestSRC::plot.variable(
  ##         x               = o_class_sel
  ##       #, xvar.names
  ##       , target          = levels(o_class_sel$class)[i_level]   # classification: first event type
  ##       #, m.target        = NULL
  ##       #, time
  ##       #, surv.type       = c("mort", "rel.freq", "surv", "years.lost", "cif", "chf")
  ##       , class.type      = c("prob", "bayes")[1]
  ##       , partial         = TRUE # FALSE = Marginal plots, TRUE = Partial plots
  ##       , oob             = TRUE
  ##       , show.plots      = TRUE
  ##       , plots.per.page  = 4
  ##       , granule         = 5
  ##       , sorted          = FALSE #TRUE
  ##       #, nvar
  ##       , npts            = 25
  ##       , smooth.lines    = FALSE  # when partial = TRUE, use lowess smoothed lines (too smooth)
  ##       #, subset
  ##       , main            = paste0("Partial plot, target: ", levels(o_class_sel$class)[i_level])
  ##       )
  ##     )
  ## }
  # cowplot::plot_grid(out[[ "plot_o_class_sel_marginal_effects" ]][[ levels(o_class_sel$class)[1] ]])
  # cowplot::plot_grid(out[[ "plot_o_class_sel_marginal_effects" ]][[ 2 ]])

  out[[ "plot_o_class_sel_partial_effects" ]] <-
    lapply(
      seq_along(levels(o_class_sel$class))
    , function(i_level) {
        cowplot::as_grob(
          ~randomForestSRC::plot.variable(
            x               = o_class_sel
          #, xvar.names      = o_class_sel$importance |> tibble::as_tibble(rownames = "rf_x_var") |> dplyr::arrange(dplyr::desc(all)) |> dplyr::pull(rf_x_var)
          , target          = levels(o_class_sel$class)[i_level]   # classification: first event type
          #, m.target        = NULL
          #, time
          #, surv.type       = c("mort", "rel.freq", "surv", "years.lost", "cif", "chf")
          , class.type      = c("prob", "bayes")[1]
          , partial         = TRUE # FALSE = Marginal plots, TRUE = Partial plots
          , oob             = TRUE
          , show.plots      = TRUE
          , plots.per.page  = n_marginal_plot_across
          , granule         = 5
          , sorted          = TRUE #FALSE
          #, nvar
          , npts            = 25
          , smooth.lines    = FALSE  # when partial = TRUE, use lowess smoothed lines (too smooth)
          #, subset
          , main            = paste0("Partial plot, target: ", levels(o_class_sel$class)[i_level])
          )
        )
      }
    )

  for (i_level in seq_along(levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]]))) {
    out[[ "plot_o_class_sel_partial_effects" ]][[ i_level ]] <-
      patchwork::wrap_elements(
        full =
          #cowplot::as_grob(
            out[[ "plot_o_class_sel_partial_effects" ]][[ i_level ]] |>
                cowplot::plot_grid()
          #)
      ) +
      patchwork::plot_annotation(
      #labs(
        title     = plot_title
      , subtitle  = "Selected model, Partial plot, predicted marginal value (adjusted) for variable"
      , caption   = paste0(
                      "For continuous variables, red points are used to indicate partial values (adjusted, integrate out other variables) and"
                    #, "\n"
                    , "  dashed red lines indicate a smoothed error bar of +/- two standard errors. "
                    #, "\n"
                    , "Black dashed line are the partial values."
                    , "\n"
                    , "For discrete variables, partial values are indicated using boxplots with whiskers extending out approximately two standard errors from the mean."
                    , "\n"
                    , "Standard errors are meant only to be a guide and should be interpreted with caution."
                   )
      , theme = theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)
      )

    ggplot2::ggsave(
        filename =
          file.path(
            out_path
          , paste0(
              file_prefix
            , "__"
            , "plot_o_class_sel_partial_effects"
            , "_"
            , i_level
            , "-"
            , levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]])[i_level]
            , "."
            , plot_format
            )
          )
      , plot   =
          out[[ "plot_o_class_sel_partial_effects" ]][[ i_level ]]
      , width  = 3 * n_marginal_plot_across
      , height = 4 * ceiling(length(out[[ "rf_x_var_sel" ]]) / n_marginal_plot_across)
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      , units  = "in"
      #, useDingbats = FALSE
      )
  } # i_level

    ## for (i_level in seq_along(levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]]))) {
    ##   out[[ "plot_o_class_sel_partial_effects" ]][[ i_level ]] |>
    ##     cowplot::plot_grid() |>
    ##     print()
    ## }


  out[[ "plot_o_class_sel_marginal_effects" ]] <-
    lapply(
      seq_along(levels(o_class_sel$class))
    , function(i_level) {
        cowplot::as_grob(
          ~randomForestSRC::plot.variable(
            x               = o_class_sel
          #, xvar.names      = o_class_sel$importance |> tibble::as_tibble(rownames = "rf_x_var") |> dplyr::arrange(dplyr::desc(all)) |> dplyr::pull(rf_x_var)
          , target          = levels(o_class_sel$class)[i_level]   # classification: first event type
          #, m.target        = NULL
          #, time
          #, surv.type       = c("mort", "rel.freq", "surv", "years.lost", "cif", "chf")
          , class.type      = c("prob", "bayes")[1]
          , partial         = FALSE # FALSE = Marginal plots, TRUE = Partial plots
          , oob             = TRUE
          , show.plots      = TRUE
          , plots.per.page  = n_marginal_plot_across
          , granule         = 5
          , sorted          = TRUE #FALSE
          #, nvar
          , npts            = 25
          , smooth.lines    = FALSE  # when partial = TRUE, use lowess smoothed lines (too smooth)
          #, subset
          , main            = paste0("Marginal plot, target: ", levels(o_class_sel$class)[i_level])
          )
        )
      }
    )

  for (i_level in seq_along(levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]]))) {
    out[[ "plot_o_class_sel_marginal_effects" ]][[ i_level ]] <-
      patchwork::wrap_elements(
        full =
          #cowplot::as_grob(
            out[[ "plot_o_class_sel_marginal_effects" ]][[ i_level ]] |>
                cowplot::plot_grid()
          #)
      ) +
      patchwork::plot_annotation(
      #labs(
        title     = plot_title
      , subtitle  = "Selected model, Marginal plot, predicted marginal value (unadjusted) for variable"
      , caption   = paste0(
                      "For continuous variables, red points are used to indicate marginal values (unadjusted for other variables) and"
                    #, "\n"
                    , "  dashed red lines indicate a smoothed error bar of +/- two standard errors. "
                    #, "\n"
                    , "Black dashed line are the marginal values."
                    , "\n"
                    , "For discrete variables, marginal values are indicated using boxplots with whiskers extending out approximately two standard errors from the mean."
                    , "\n"
                    , "Standard errors are meant only to be a guide and should be interpreted with caution."
                   )
      , theme = theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)
      )

    ggplot2::ggsave(
        filename =
          file.path(
            out_path
          , paste0(
              file_prefix
            , "__"
            , "plot_o_class_sel_marginal_effects"
            , "_"
            , i_level
            , "-"
            , levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]])[i_level]
            , "."
            , plot_format
            )
          )
      , plot   =
          out[[ "plot_o_class_sel_marginal_effects" ]][[ i_level ]]
      , width  = 3 * n_marginal_plot_across
      , height = 4 * ceiling(length(out[[ "rf_x_var_sel" ]]) / n_marginal_plot_across)
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      , units  = "in"
      #, useDingbats = FALSE
      )
  } # i_level


  ## out[[ "plot_o_class_sel_marginal_effects" ]] <-
  ##   lapply(
  ##     seq_along(levels(o_class_sel$class))
  ##   , function(i_level) {
  ##       cowplot::as_grob(
  ##         ~randomForestSRC::plot.variable(
  ##           x               = o_class_sel
  ##         #, xvar.names      = o_class_sel$importance |> tibble::as_tibble(rownames = "rf_x_var") |> dplyr::arrange(dplyr::desc(all)) |> dplyr::pull(rf_x_var)
  ##         , target          = levels(o_class_sel$class)[i_level]   # classification: first event type
  ##         #, m.target        = NULL
  ##         #, time
  ##         #, surv.type       = c("mort", "rel.freq", "surv", "years.lost", "cif", "chf")
  ##         , class.type      = c("prob", "bayes")[1]
  ##         , partial         = FALSE # FALSE = Marginal plots, TRUE = Partial plots
  ##         , oob             = TRUE
  ##         , show.plots      = TRUE
  ##         , plots.per.page  = 4
  ##         , granule         = 5
  ##         , sorted          = TRUE #FALSE
  ##         #, nvar
  ##         , npts            = 25
  ##         , smooth.lines    = FALSE  # when partial = TRUE, use lowess smoothed lines (too smooth)
  ##         #, subset
  ##         , main            = paste0("Marginal plot, target: ", levels(o_class_sel$class)[i_level])
  ##         )
  ##       )
  ##     }
  ##   )

    ## for (i_level in seq_along(levels(dat_rf_class[[ out_e_rf[[ "rf_y_var" ]] ]]))) {
    ##   out[[ "plot_o_class_sel_marginal_effects" ]][[ i_level ]] |>
    ##     cowplot::plot_grid() |>
    ##     print()
    ## }



  ### ROC Curve
  this_text <-
    paste0("erikmisc::e_rfsrc_classification, "
      , round(lubridate::duration((proc.time() - time_start)["elapsed"], units="seconds"), 2) |> as.character()
      , ":  ROC Curve (selected model)"
    )
  print(this_text)
  f_write_this_text(this_text)

  # obtains the value of AUC (area under the ROC curve)
  o_class_sel_AUC <-
    randomForestSRC::get.auc(
      y    = dat_rf_data[[ rf_y_var ]]
    , prob = o_class_sel$predicted.oob
    )
  out[[ "o_class_sel_AUC" ]] <-
    o_class_sel_AUC

  this_text <-
    paste0("o_class_sel_AUC: ", o_class_sel_AUC)
  f_write_this_text(this_text)


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
  out[[ "o_class_sel_ROC" ]] <- list()
  for (n_object in names(out_roc_temp[[ 1 ]])) {
    ## n_object = names(out_roc_temp[[ 1 ]])[1]
    out[[ "o_class_sel_ROC" ]][[ n_object ]] <- list()

    for (n_target in names(out_roc_temp)) {
      ## n_target = names(out_roc_temp)[1]
      out[[ "o_class_sel_ROC" ]][[ n_object ]][[ n_target ]] <-
        out_roc_temp[[ n_target ]][[ n_object ]]

      if (n_object == "roc_curve_best") {
        out[[ "o_class_sel_ROC" ]][[ n_object ]][[ n_target ]] <-
          out[[ "o_class_sel_ROC" ]][[ n_object ]][[ n_target ]] |>
          dplyr::mutate(
            Group = n_target
          ) |>
          dplyr::relocate(Group)
      }
      if (n_object == "roc_curve") {
        out[[ "o_class_sel_ROC" ]][[ n_object ]][[ n_target ]] <-
          out[[ "o_class_sel_ROC" ]][[ n_object ]][[ n_target ]] |>
          dplyr::mutate(
            Group = n_target
          ) |>
          dplyr::relocate(Group)
      }

    }
  }

  readr::write_csv(
    x = out[[ "o_class_sel_ROC" ]]$roc_curve_best |> dplyr::bind_rows()
  , file =
      file.path(
        out_path
      , paste0(
          file_prefix
        , "__"
        , "o_class_sel_ROC"
        , ".csv"
        )
      )
  )


  out[[ "plot_o_class_sel_ROC" ]] <-
    patchwork::wrap_elements(
      full =
        cowplot::plot_grid(plotlist = out$o_class_sel_ROC$plot_roc, nrow = 1)
    ) +
    patchwork::plot_annotation(
    #labs(
      title     = plot_title
    , subtitle  = "Selected model, ROC Curves"
    #, caption   = paste0(
    #                ""
    #              )
    , theme = theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)
    )
    # +
    #theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)

  ggplot2::ggsave(
      filename =
        file.path(
          out_path
        , paste0(
            file_prefix
          , "__"
          , "plot_o_class_sel_ROC"
          , "."
          , plot_format
          )
        )
    , plot   =
        out[[ "plot_o_class_sel_ROC" ]]
    , width  = 5 * length(levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]]))
    , height = 6
    ## png, jpeg
    , dpi    = 300
    , bg     = "white"
    ## pdf
    , units  = "in"
    #, useDingbats = FALSE
    )

  for (i_level in seq_along(levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]]))) {
    ggplot2::ggsave(
        filename =
          file.path(
            out_path
          , paste0(
              file_prefix
            , "__"
            , "plot_o_class_sel_ROC"
            , "_"
            , i_level
            , "-"
            , levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]])[i_level]
            , "."
            , plot_format
            )
          )
      , plot   =
          out$o_class_sel_ROC$plot_roc[[ i_level ]] +
          patchwork::plot_annotation(
          #labs(
            title     = plot_title
          , subtitle  = "Selected model, ROC Curves"
          #, caption   = paste0(
          #                ""
          #              )
          , theme = theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)
          )
      , width  = 5
      , height = 6
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      , units  = "in"
      #, useDingbats = FALSE
      )
  } # i_level


  #out[[ "plot_o_class_sel_ROC" ]] <- p_list

  # p_arranged <-
  #   cowplot::plot_grid(
  #     plotlist = out[[ "plot_o_class_sel_ROC" ]]
  #   , nrow = 1
  #   #, ncol = 2
  #   )
  # p_arranged |> print()


  # Compile training summary plot (also after selection if no X var)
  ## out[[ "plot_rf_train_all_summary" ]] <-
  ##   cowplot::plot_grid(out$plot_o_class              ) +
  ##   cowplot::plot_grid(out$plot_o_class_subsample    ) +
  ##   cowplot::plot_grid(out$plot_o_class_sel          ) +
  ##   cowplot::plot_grid(out$plot_o_class_sel_subsample) +
  ##   cowplot::plot_grid(plotlist = out$plot_o_class_sel_ROC$plot_roc, nrow = 1) +
  ##   patchwork::plot_layout(design = plot_design) +
  ##   patchwork::plot_annotation(
  ##     title       = text_formula
  ##   , subtitle    = text_formula_sel
  ##   , caption     = paste0(
  ##                     "Full model AUC = "
  ##                   , round(out$o_class_AUC, 3)
  ##                   , ";  "
  ##                   , "Selected model AUC = "
  ##                   , round(out$o_class_sel_AUC, 3)
  ##                   )
  ##   , tag_levels  = "A"
  ##   ) +
  ##   theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)

    # ggsave(
    #     paste0("out/plot_rf_train_all_summary__", n_var_group, "_-_", n_dx_group, ".png")
    #   , plot   = out[[ "plot_rf_train_all_summary" ]]
    #   , width  = 18
    #   , height = 18
    #   ## png, jpeg
    #   , dpi    = 300
    #   , bg     = "white"
    #   ## pdf
    #   #, units  = "in"
    #   #, useDingbats = FALSE
    #   )


  out[[ "plot_rf_train_all_summary" ]] <-
    out[[ "plot_o_class_full" ]]               + labs(title = NULL) +
    out[[ "plot_o_class_full_subsample" ]]     + labs(title = NULL) +
    out[[ "plot_o_class_sel" ]]           + labs(title = NULL) +
    out[[ "plot_o_class_sel_subsample" ]] + labs(title = NULL) +
    out[[ "plot_o_class_sel_ROC" ]] +
    #cowplot::plot_grid(plotlist = out$plot_o_class_sel_ROC$plot_roc, nrow = 1) +
    patchwork::plot_layout(design = plot_design) +
    patchwork::plot_annotation(
      title       = plot_title
    , subtitle    =
                    paste0(
                      "Full model (AUC = ", round(out$o_class_full_AUC, 3), "): "
                    , text_formula
                    , "\n"
                    , "Selected model (AUC = ", round(out$o_class_sel_AUC, 3), "): "
                    , text_formula_sel
                    )
    #, caption     = paste0(
    #                  "Full model AUC = "
    #                , round(out$o_class_full_AUC, 3)
    #                )
    , theme = theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)
    , tag_levels  = "A"
    )

  ggplot2::ggsave(
      filename =
        file.path(
          out_path
        , paste0(
            file_prefix
          , "__"
          , "plot_rf_train_all_summary"
          , "."
          , plot_format
          )
        )
    , plot   =
        out[[ "plot_rf_train_all_summary" ]]
    , width  = 18
    , height = 18
    ## png, jpeg
    , dpi    = 300
    , bg     = "white"
    ## pdf
    , units  = "in"
    #, useDingbats = FALSE
    )



  this_text <-
    paste0("erikmisc::e_rfsrc_classification, "
      , round(lubridate::duration((proc.time() - time_start)["elapsed"], units="seconds"), 2) |> as.character()
      , ":  Complete"
    )
  print(this_text)
  f_write_this_text(this_text)

  return(out)

} # e_rfsrc_classification
