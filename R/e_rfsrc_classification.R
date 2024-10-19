#' Random Forests classification workflow using \code{randomForestSRC}
#'
#' @param dat_rf_class            data.frame with data
#' @param rf_y_var                y factor variable, if NULL then assumed to be the first column of \code{dat_rf_class}
#' @param rf_x_var                x variables, if NULL then assumed to be all except the first column of \code{dat_rf_class}
#' @param rf_id_var               ID variable, removed from dataset prior to analysis
#' @param sw_rfsrc_ntree          \code{ntree} argument for \code{randomForestSRC::rfsrc()}
#' @param sw_alpha_sel            \code{alpha} argument for \code{randomForestSRC::plot.subsample()} and \code{randomForestSRC::extract.bootsample()} for model selection
#' @param sw_select_full          run with model selection, or only fit full model
#' @param sw_na_action            missing values, omit or impute (only x var, never impute y var, always drop rows with missing y var)
#' @param sw_save_model           T/F to save model to .Rdata file
#' @param plot_title              title for plots
#' @param out_path                path to save output
#' @param file_prefix             file prefix for saved output
#' @param var_subgroup_analysis   variable(s) list (in \code{c(var1, var2)}) for subgroup analysis (group-specific ROC curves and confusion matrices) using ROC threshold from non-subgroup ROC curve, or \code{NULL} for none
#' @param plot_format             plot format supported by \code{ggplot2::ggsave()}
#' @param n_marginal_plot_across  for partial and marginal plots, number of plots per row (increase if not all plots are displayed)
#' @param sw_imbalanced_binary    T/F to use standard or imbalanced binary classification with \code{rfsrc::imbalanced()}.  It is recommended to increase ntree to \code{5 * sw_rfsrc_ntree}.
#' @param sw_threshold_to_use     T/F NOT YET USED XXX
#' @param sw_quick_full_only      T/F to only fit full model and return model object
#' @param sw_reduce_output        T/F exclude individual ROC and VIMP plots, and marginal plots
#' @param n_single_decision_tree_plots number of example decision trees to plot (recommend not too many)
#' @param k_partial_coplot_var    number of top variables by VIMP to create bivariate partial (conditioning) plots
#' @param n_boot_resamples        number of subsamples (or number of bootstraps) for VIMP CIs
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
#' @importFrom utils capture.output
#' @importFrom ggplotify as.ggplot
#' @importFrom graphics coplot
#' @export
#'
#' @examples
#' \dontrun{
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
#'   , X1 = rnorm(n = dplyr::n(), mean = 10)
#'   , X2 = rnorm(n = dplyr::n(), mean = 20)
#'   , X3 = rnorm(n = dplyr::n(), mean = 30)
#'   , X4 = rnorm(n = dplyr::n(), mean = 40)
#'   , X5 = rnorm(n = dplyr::n(), mean = 50)
#'   , X6 = rnorm(n = dplyr::n(), mean = 60)
#'   , X7 = rnorm(n = dplyr::n(), mean = 70)
#'   , X8 = rnorm(n = dplyr::n(), mean = 80)
#'   , X9 = rnorm(n = dplyr::n(), mean = 90)
#'   )
#'
#' out_e_rf <-
#'   e_rfsrc_classification(
#'     dat_rf_class           = dat_rf_class
#'   , rf_y_var               = "cyl"    # NULL
#'   , rf_x_var               = NULL # c("mpg", "disp", "hp", "drat", "wt", "qsec", "vs", "am") # NULL
#'   , rf_id_var              = "model"
#'   , sw_rfsrc_ntree         = 200
#'   , sw_alpha_sel           = 0.05
#'   , sw_select_full         = c("select", "full")[1]
#'   , sw_na_action           = c("na.omit", "na.impute")[1]
#'   , sw_save_model          = c(TRUE, FALSE)[1]
#'   , plot_title             = "Random Forest Title SEL"
#'   , out_path               = "./out_sel"
#'   , file_prefix            = "out_e_rf"
#'   , var_subgroup_analysis  = NULL
#'   , plot_format            = c("png", "pdf")[1]
#'   , n_marginal_plot_across = 4
#'   , sw_imbalanced_binary   = c(FALSE, TRUE)[1]
#'   , sw_threshold_to_use    = c(FALSE, TRUE)[1]
#'   , sw_quick_full_only     = c(FALSE, TRUE)[1]
#'   , sw_reduce_output       = c(TRUE, FALSE)[1]
#'   , n_single_decision_tree_plots = 0
#'   , k_partial_coplot_var   = 3
#'   , n_boot_resamples        = 100
#'   )
#'
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
#' # Full model: subsample double bootstrap VIMP CIs
#' out[[ "plot_o_class_full_vimp_CI" ]]
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
#' # Selected model: subsample double bootstrap VIMP CIs
#' out[[ "plot_o_class_sel_vimp_CI" ]]
#'
#'
#'
#'
#' ## With missing values and imputation
#' # create missing-at-random values
#' dat_rf_class_miss <- dat_rf_class
#' prop_missing <- 0.10
#' n_missing <-
#'   sample.int(
#'     n    = prod(dim(dat_rf_class_miss))
#'   , size = round( prop_missing * prod(dim(dat_rf_class_miss)))
#'   )
#' ind_missing <- expand.grid(1:dim(dat_rf_class_miss)[1], 1:dim(dat_rf_class_miss)[2])[n_missing, ]
#' for (i_row in seq_along(n_missing)) {
#'   dat_rf_class_miss[ind_missing[i_row, 1], ind_missing[i_row, 2] ] <- NA
#' }
#' dat_rf_class[[ "model" ]] = dat_rf_class[[ "model" ]]
#'
#' out_e_rf <-
#'   e_rfsrc_classification(
#'     dat_rf_class           = dat_rf_class_miss
#'   , rf_y_var               = "cyl"    # NULL
#'   , rf_x_var               = NULL # c("mpg", "disp", "hp", "drat", "wt", "qsec", "vs", "am") # NULL
#'   , rf_id_var              = "model"
#'   , sw_rfsrc_ntree         = 200
#'   , sw_alpha_sel           = 0.05
#'   , sw_select_full         = c("select", "full")[1]
#'   , sw_na_action            = c("na.omit", "na.impute")[2]
#'   , sw_save_model          = c(TRUE, FALSE)[1]
#'   , plot_title             = "Random Forest, imputing missing values"
#'   , out_path               = "./out_sel_miss"
#'   , file_prefix            = "out_e_rf_miss"
#'   , var_subgroup_analysis  = NULL
#'   , plot_format            = c("png", "pdf")[1]
#'   , n_marginal_plot_across = 4
#'   , sw_imbalanced_binary   = c(FALSE, TRUE)[1]
#'   , sw_threshold_to_use    = c(FALSE, TRUE)[1]
#'   , sw_quick_full_only     = c(FALSE, TRUE)[1]
#'   , sw_reduce_output        = c(TRUE, FALSE)[1]
#'   , n_single_decision_tree_plots = 0
#'   , k_partial_coplot_var   = 0
#'   , n_boot_resamples        = 100
#'   )
#'
#'
#' ## Imbalanced binary classification
#'
#' # Two mvnorm distributions with overlap
#' imbal_n     <- c(50, 1000)
#' imbal_mean  <- c(0, 0.75)
#' imbal_sigma <- c(1, 1)
#' imbal_dim   <- 10
#' dat_imbal <-
#'   dplyr::bind_rows(
#'     mvtnorm::rmvnorm(
#'       n     = imbal_n[1]
#'     , mean  = imbal_mean[1] |> rep(imbal_dim)
#'     , sigma = imbal_sigma[1] * diag(imbal_dim)
#'     ) |>
#'     tibble::as_tibble(.name_repair = "universal") |>
#'     dplyr::rename_with(~ stringr::str_c("V", 1:imbal_dim)) |>
#'     dplyr::mutate(Group = "A")
#'   ,
#'     mvtnorm::rmvnorm(
#'       n     = imbal_n[2]
#'     , mean  = imbal_mean[2] |> rep(imbal_dim)
#'     , sigma = imbal_sigma[2] * diag(imbal_dim)
#'     ) |>
#'     tibble::as_tibble(.name_repair = "universal") |>
#'     dplyr::rename_with(~ stringr::str_c("V", 1:imbal_dim)) |>
#'     dplyr::mutate(Group = "B")
#'   ) |>
#'   dplyr::relocate(
#'     Group
#'   ) |>
#'   dplyr::mutate(
#'     Group = Group |> factor()
#'   )
#'
#' out_e_rf <-
#'   e_rfsrc_classification(
#'     dat_rf_class           = dat_imbal
#'   , rf_y_var               = NULL
#'   , rf_x_var               = NULL
#'   , rf_id_var              = NULL
#'   , sw_rfsrc_ntree         = 2000
#'   , sw_alpha_sel           = 0.25
#'   , sw_select_full         = c("select", "full")[1]
#'   , sw_na_action            = c("na.omit", "na.impute")[1]
#'   , sw_save_model          = c(TRUE, FALSE)[1]
#'   , plot_title             = "Random Forest Imbalanced"
#'   , out_path               = "./out_imbal"
#'   , file_prefix            = "out_e_rf"
#'   , var_subgroup_analysis  = NULL
#'   , plot_format            = c("png", "pdf")[1]
#'   , n_marginal_plot_across = 4
#'   , sw_imbalanced_binary   = c(FALSE, TRUE)[2]
#'   , sw_threshold_to_use    = c(FALSE, TRUE)[1]
#'   , sw_quick_full_only     = c(FALSE, TRUE)[1]
#'   , sw_reduce_output        = c(TRUE, FALSE)[1]
#'   , n_single_decision_tree_plots = 0
#'   , k_partial_coplot_var   = 0
#'   , n_boot_resamples        = 100
#'   )
#'
#' }
e_rfsrc_classification <-
  function(
    dat_rf_class            = NULL
  , rf_y_var                = NULL
  , rf_x_var                = NULL
  , rf_id_var               = NULL
  , sw_rfsrc_ntree          = 500
  , sw_alpha_sel            = 0.05
  , sw_select_full          = c("select", "full")[1]
  , sw_na_action            = c("na.omit", "na.impute")[1]
  , sw_save_model           = c(TRUE, FALSE)[1]
  , plot_title              = "Random Forest"
  , out_path                = "out_sel"
  , file_prefix             = "out_e_rf"
  , var_subgroup_analysis   = NULL
  , plot_format             = c("png", "pdf")[1]
  , n_marginal_plot_across  = 6
  , sw_imbalanced_binary    = c(FALSE, TRUE)[1]
  , sw_threshold_to_use     = c(FALSE, TRUE)[1]
  , sw_quick_full_only      = c(FALSE, TRUE)[1]
  , sw_reduce_output        = c(TRUE, FALSE)[1]
  , n_single_decision_tree_plots = 0
  , k_partial_coplot_var    = 3
  , n_boot_resamples        = 100
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
  ##   , sw_alpha_sel    = 0.05
  ##   , sw_save_model   = c(TRUE, FALSE)[1]
  ##   , plot_title      = "Random Forest Title"
  ##   , out_path        = "C:/Users/erike/Desktop/temp"
  ##   , file_prefix     = "out_e_rf"
  ##   , plot_format     = c("png", "pdf")[1]
  ##   , n_marginal_plot_across = 6
  ##   )

  f_plot_VIMP_bs <-
    function(
      out_bs        = out[[ "o_class_full_subsample_extract_bootsample" ]]
    , sw_alpha_sel  = sw_alpha_sel
    , sw_model_name = c("Full", "Selected")[1]
    ) {

    dat_bs <-
      out_bs$var.sel.Z |>
      tibble::as_tibble(rownames = "Var") |>
      dplyr::bind_cols(
        tibble::tibble(se = out_bs$se)
      ) |>
      dplyr::mutate(
        mean_se_upper = mean + se
      , mean_se_lower = mean - se
      , signif = signif |> factor(levels = c(FALSE, TRUE), labels = c("FALSE", "TRUE"))
      )


    # Create a custom color scale
    myColors <- RColorBrewer::brewer.pal(4, "Set1")
    names(myColors) <- levels(dat_bs$signif)


    p <- ggplot(dat_bs, aes(x = mean, y = reorder(Var, mean), color = signif))
    p <- p + theme_bw()
    p <- p + geom_vline(aes(xintercept = 0), colour = "black", linetype = c("none", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")[2], linewidth = 0.3, alpha = 0.25)
    p <- p + geom_errorbarh(aes(xmin = mean_se_lower, xmax = mean_se_upper), height = 0.25, color = "gray50")
    p <- p + geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.5)
    p <- p + geom_point(shape = 18, size = 4)
    #p <- p + facet_grid(signif ~ ., scales = "free_y", drop = TRUE)
    p <- p + theme(legend.position = "right") # "none"
    p <- p + scale_color_manual(values = myColors, drop = FALSE)
    p <- p + guides(colour = guide_legend(reverse = TRUE))

    p <- p + labs(
                    title     = plot_title
                  , subtitle  = paste0(sw_model_name, " model, Variable Importance SE and CI using BS subsampling")
                  , x         = "VIMP"
                  , y         = "Variables"
                  , caption = paste0(  "Symbols: Diamond is esimated VIMP;  "
                                    , "Gray endpoints at 1 SE;  Colored bars at ", 100 * (1 - sw_alpha_sel), "% CI"
                                    )
                  , colour    = "VIMP\nSignificant"
                  #, shape     = "Class"
                  #, linetype  = "General Health"  #"Diagnosis"
                  #, fill      = "Diagnosis"
                  #, tag = "A"
                  )
    p <- p + theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)
    #print(p)

    return(list(dat_bs = dat_bs, p = p))

  } # f_plot_VIMP_bs


  # for large x, plot_o_class_full_partial_effects may fail
  options(ragg.max_dim=1e6)

  # create output directory, warning if already exists
  dir.create(out_path, showWarnings = FALSE, recursive = TRUE)

  # library(log4r)
  # Initialize logger
  log_obj <-
    e_log_write(
      sw_init     = TRUE
    , out_path    = out_path
    , file_prefix = file_prefix
    )

  if (n_single_decision_tree_plots > sw_rfsrc_ntree) {
    n_single_decision_tree_plots <-
      sw_rfsrc_ntree
  }

    # Try logging messages with different priorities.
    # At priority level INFO, a call to debug() won't print anything.
    #log4r::debug(log_out_to_file, "A Debugging Message")
    #log4r::info (log_out_to_file, "An Info Message")
    #log4r::warn (log_out_to_file, "A Warning Message")
    #log4r::error(log_out_to_file, "An Error Message")
    #log4r::fatal(log_out_to_file, "A Fatal Error Message")

  e_log_write(
    "erikmisc::e_rfsrc_classification Process BEGIN"
  , log_obj     = log_obj
  , i_level     = 2
  )
  e_log_write(
    paste0(
      "Input parameters: "
    , "\n"
    #, "\n    dat_rf_class            = ", dat_rf_class
    , "\n    rf_y_var                = ", ifelse(is.null(rf_y_var   ), "NULL", rf_y_var   )
    , "\n    rf_x_var                = ", ifelse(is.null(rf_x_var   ), "NULL", rf_x_var   )
    , "\n    rf_id_var               = ", ifelse(is.null(rf_id_var  ), "NULL", rf_id_var  )
    , "\n    sw_rfsrc_ntree          = ", sw_rfsrc_ntree
    , "\n    sw_alpha_sel            = ", sw_alpha_sel
    , "\n    sw_select_full          = ", sw_select_full
    , "\n    sw_na_action            = ", sw_na_action
    , "\n    sw_save_model           = ", sw_save_model
    , "\n    plot_title              = ", plot_title
    , "\n    out_path                = ", out_path
    , "\n    file_prefix             = ", file_prefix
    , "\n    var_subgroup_analysis   = ", var_subgroup_analysis
    , "\n    plot_format             = ", plot_format
    , "\n    n_marginal_plot_across  = ", n_marginal_plot_across
    , "\n    sw_imbalanced_binary    = ", sw_imbalanced_binary
    , "\n    sw_threshold_to_use     = ", sw_threshold_to_use
    , "\n    sw_quick_full_only      = ", sw_quick_full_only
    , "\n    sw_reduce_output        = ", sw_reduce_output
    , "\n    n_single_decision_tree_plots = ", n_single_decision_tree_plots
    , "\n    k_partial_coplot_var    = ", k_partial_coplot_var
    , "\n\n"
    )
  , log_obj     = log_obj
  , i_level     = 2
  )

  e_log_write(
    paste0("Create output directory: ", out_path)
  , log_obj     = log_obj
  , i_level     = 2
  )

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


  #library(parallel)
  n_cores <- max(1, parallel::detectCores() - 2)
  options(rf.cores = n_cores) # OpenMP Parallel Processing
  options(mc.cores = n_cores) # R-side Parallel Processing

  # list for output objects
  out <- list()

  # Data
  if (is.null(dat_rf_class)) {
    e_log_write(
      paste0("dat_rf_class is NULL")
    , log_obj     = log_obj
    , i_level     = 3
    )
  }
  dat_rf_data <-
    dat_rf_class |>
    as.data.frame()

  # ID label
  if (is.null(rf_id_var)) {
    rownames(dat_rf_data) <- 1:nrow(dat_rf_data)
  }
  if (!is.null(rf_id_var)) {
    if (!(length(unique(dat_rf_data[[ rf_id_var ]])) == nrow(dat_rf_data))) {
      e_log_write(
        paste0("ID label has duplicate values, replacing with sequential index numbers")
      , log_obj     = log_obj
      , i_level     = 2
      )
      rownames(dat_rf_data) <- 1:nrow(dat_rf_data)
      dat_rf_data <-
        dat_rf_data |>
        dplyr::select(
          -tidyselect::all_of(rf_id_var)
        )
    } else {
      rownames(dat_rf_data) <- dat_rf_data[[ rf_id_var ]]
      dat_rf_data <-
        dat_rf_data |>
        dplyr::select(
          -tidyselect::all_of(rf_id_var)
        )
    }
  }

  # Variables
  if (is.null(rf_y_var)) {
    rf_y_var <- names(dat_rf_data)[ 1]
  } else {
    if(!all(rf_y_var %in% names(dat_rf_data))) {
      e_log_write(
        paste0("rf_y_var var not in dat_rf_class")
      , log_obj     = log_obj
      , i_level     = 3
      )
    }
  }
  if (is.null(rf_x_var)) {
    rf_x_var <- names(dat_rf_data)[-1]
  } else {
    if(!all(rf_x_var %in% names(dat_rf_data))) {
      e_log_write(
        paste0("rf_x_var vars not in dat_rf_class")
      , log_obj     = log_obj
      , i_level     = 3
      )
    }
  }

  rf_x_var_full <-
    rf_x_var

  if(!sw_quick_full_only) {
    # plot missing
    plot_miss <-
      e_plot_missing(
        dat_plot            = dat_rf_data
      , var_group           = rf_y_var
      , sw_group_sort       = TRUE
      , var2_sort           = NULL
      , sw_title_data_name  = paste0("Training data: ", plot_title)
      , sw_text_pct_miss    = FALSE
      )
    ggplot2::ggsave(
        filename =
          file.path(
            out_path
          , paste0(
              file_prefix
            , "__"
            , "plot_missing_full"
            , "."
            , plot_format
            )
          )
      , plot   =
          plot_miss
      , width  = 12
      , height = 8
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      , units  = "in"
      #, useDingbats = FALSE
      , limitsize = FALSE
      )
  } # !sw_quick_full_only


  ## Missing values

  # check for all NA columns
  all_na_columns <-
    dat_rf_data |>
    purrr::keep(~all(is.na(.x))) |>
    names()

  if (length(all_na_columns)) {
    # check if y var
    if(any(all_na_columns %in% rf_y_var)) {
      e_log_write(
        paste0("erikmisc::e_rfsrc_classification, returning NULL: dat_rf_class y-variable is all NA:  ", rf_y_var)
      , log_obj     = log_obj
      , i_level     = 3
      )
      return(NULL)
    }

    e_log_write(
      paste0("erikmisc::e_rfsrc_classification, dat_rf_class removing column(s) that are all NA:  ", paste(all_na_columns, collapse = ", "))
    , log_obj     = log_obj
    , i_level     = 3
    )

    # remove column from data
    dat_rf_data <-
      dat_rf_data |>
      dplyr::select(
        -tidyselect::all_of(all_na_columns)
      )
    # remove variable from x var list
    rf_x_var_full <- rf_x_var_full[rf_x_var_full %notin% all_na_columns]
  }

  # Formula
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

  e_log_write(
    paste0("Full model: ", text_formula)
  , log_obj     = log_obj
  , i_level     = 2
  )


  ## Missing values
  out[[ "rf_data_input"  ]] <-
    dat_rf_data

  # Impute NAs, rf_x_var_full, only (not y variable)
  if (sw_na_action == c("na.omit", "na.impute")[2]) {
    e_log_write(
      paste0("Imputing missing x variables")
    , log_obj     = log_obj
    , i_level     = 2
    )
    dat_rf_data_impute <-
      randomForestSRC::impute.rfsrc(
        formula  = text_formula |> as.formula()
      , data     = dat_rf_data
      , ntree    = 500
      , nodesize = 1
      , nsplit   = 10
      , nimpute  = 10
      , fast     = c(FALSE, TRUE)[1]
      )

    # keep original y variable
    dat_rf_data_impute[[ rf_y_var ]] <-
      dat_rf_data[[ rf_y_var ]]

    # replace data with imputed data
    dat_rf_data <-
      dat_rf_data_impute


    # plot missing
    plot_miss <-
      e_plot_missing(
        dat_plot            = dat_rf_data
      , var_group           = rf_y_var
      , sw_group_sort       = TRUE
      , var2_sort           = NULL
      , sw_title_data_name  = paste0("Training data: ", plot_title, ", after imputation")
      , sw_text_pct_miss    = FALSE
      )
    ggplot2::ggsave(
        filename =
          file.path(
            out_path
          , paste0(
              file_prefix
            , "__"
            , "plot_missing_full_imputed"
            , "."
            , plot_format
            )
          )
      , plot   =
          plot_miss
      , width  = 12
      , height = 8
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      , units  = "in"
      #, useDingbats = FALSE
      , limitsize = FALSE
      )


  } else {
    e_log_write(
      paste0("Not imputing missing x variables")
    , log_obj     = log_obj
    , i_level     = 2
    )
  }
  # Drop NAs, y variable (and x if not impute)
  e_log_write(
    paste0("Removing rows with NAs")
  , log_obj     = log_obj
  , i_level     = 2
  )

  dat_rf_data <-
    dat_rf_data |>
    tidyr::drop_na()


  out[[ "rf_data_used"  ]] <-
    dat_rf_data

  # check for at least 2 y var levels
  if (length(unique(dat_rf_data[[ rf_y_var ]])) < 2) {
    e_log_write(
      paste0("erikmisc::e_rfsrc_classification, returning NULL: rf_y_var (", rf_y_var, ") needs at least two levels:  ", paste(unique(dat_rf_data[[ rf_y_var ]]), collapse = ", "))
    , log_obj     = log_obj
    , i_level     = 3
    )
    return(NULL)
  }
  # check for exactly 2 y var levels if binary
  if (sw_imbalanced_binary & !(length(unique(dat_rf_data[[ rf_y_var ]])) == 2)) {
    e_log_write(
      paste0("erikmisc::e_rfsrc_classification, returning NULL: rf_y_var (", rf_y_var, ") needs exactly two levels when sw_imbalanced_binary=TRUE:  ", paste(unique(dat_rf_data[[ rf_y_var ]]), collapse = ", "))
    , log_obj     = log_obj
    , i_level     = 3
    )
    return(NULL)
  }



  ### Grow forest
  e_log_write(
    "Full model, grow forest"
  , log_obj     = log_obj
  , i_level     = 2
  )

  if (sw_imbalanced_binary == c(FALSE, TRUE)[1]) {
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
  }
  if (sw_imbalanced_binary == c(FALSE, TRUE)[2]) {
    o_class_full <-
      randomForestSRC::imbalanced(
        formula     = rf_formula_full
      , data        = dat_rf_data
      , ntree       = sw_rfsrc_ntree
      , method      = c("rfq", "brf", "standard")[1]                            # imbalanced argument (added)
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
      #, samptype    = c("swor", "swr")[1]                                      # imbalanced argument (commented out)
      #, samp        = NULL
      #, membership  = FALSE
      #, sampsize    = if (samptype == "swor") function(x){x * .632} else function(x){x}
      , na.action   = c("na.omit", "na.impute")[2]
      #, nimpute     = 1
      #, ntime       = 150    # survival
      #, cause       = NULL   # competing risks
      , perf.type   = c("none", "misclass", "brier", "gmean")[4]  # classification  # imbalanced argument (gmean)
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

    ## (1) default threshold (2) directly optimized gmean threshold
    threshold_default <-
      randomForestSRC::get.imbalanced.performance(
        o_class_full
      )["threshold"]

    threshold_gmean   <-
      randomForestSRC::get.imbalanced.optimize(
        o_class_full
      , measure = c("gmean", "F1", "F1mod", "F1modgmean")[1]
      , plot.it = FALSE
      )["threshold"] |>
      as.numeric()

  }

  out[[ "o_class_full" ]] <-
    o_class_full

  e_log_write(
    paste0(
      "Full model, Model statistics and confusion matrix"
    , "\n\n"
    , o_class_full |>
      print() |>
      utils::capture.output() |>
      paste0(collapse = "\n")
    , "\n\n"
    )
  , log_obj     = log_obj
  , i_level     = 2
  )


  if(!sw_quick_full_only) {

    # save model (to be used for prediction)
    if(sw_save_model) {
      e_log_write(
        "Full model, Write model object to .RData file"
      , log_obj     = log_obj
      , i_level     = 2
      )

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

    if (!sw_reduce_output) {
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
        , limitsize = FALSE
        )
    } # sw_reduce_output

  } # !sw_quick_full_only


  # obtains the value of AUC (area under the ROC curve)
  o_class_full_AUC <-
    randomForestSRC::get.auc(
      y    = dat_rf_data[[ rf_y_var ]]
    , prob = o_class_full$predicted.oob
    )
  out[[ "o_class_full_AUC" ]] <-
    o_class_full_AUC

  e_log_write(
    paste0("Full model, o_class_full_AUC: ", o_class_full_AUC)
  , log_obj     = log_obj
  , i_level     = 2
  )


  ## ROC via erikmisc
  e_log_write(
    "Full model, plot ROC curves"
  , log_obj     = log_obj
  , i_level     = 2
  )

  if (sw_imbalanced_binary == c(FALSE, TRUE)[1]) {
    threshold_to_use <- NULL
    out_roc_temp <- list()
    for (n_target in levels(dat_rf_data[[ rf_y_var ]])) {
      out_roc <-
        e_plot_roc(
          labels_true       = ifelse(dat_rf_data[[ rf_y_var ]] == n_target, 1, 0)
        , pred_values_pos   = o_class_full$predicted.oob[, n_target]
        , label_neg_pos     = c(0, 1)
        , sw_plot           = !sw_quick_full_only #TRUE
        , cm_mode           = c("sens_spec", "prec_recall", "everything")[3]
        , threshold_to_use  = threshold_to_use
        )
      if(!sw_quick_full_only) {
        #p <- out$plot_roc
        out_roc$plot_roc <- out_roc$plot_roc + labs(title = paste0("ROC Curve, Target:  ", n_target))
        out_roc$plot_roc <- out_roc$plot_roc + coord_fixed(ratio = 1) # equal axes
      }

      out_roc_temp[[ n_target ]] <- out_roc
    } # n_target
  } # sw_imbalanced_binary FALSE
  if (sw_imbalanced_binary == c(FALSE, TRUE)[2]) {
    out_roc_temp <- list()
    for (n_target in levels(dat_rf_data[[ rf_y_var ]])) {
      if (n_target == levels(dat_rf_data[[ rf_y_var ]])[1]) {
        threshold_to_use <- threshold_gmean
        #threshold_to_use <- threshold_default
      } else {
        threshold_to_use <- 1 - threshold_gmean - 1e-10
        #threshold_to_use <- 1 - threshold_default - 1e-10
      }
      out_roc <-
        e_plot_roc(
          labels_true       = ifelse(dat_rf_data[[ rf_y_var ]] == n_target, 1, 0)
        , pred_values_pos   = o_class_full$predicted.oob[, n_target]
        , label_neg_pos     = c(0, 1)
        , sw_plot           = !sw_quick_full_only #TRUE
        , cm_mode           = c("sens_spec", "prec_recall", "everything")[3]
        , threshold_to_use  = threshold_to_use
        )
      if(!sw_quick_full_only) {
        #p <- out$plot_roc
        out_roc$plot_roc <- out_roc$plot_roc + labs(title = paste0("ROC Curve, Target:  ", n_target))
        out_roc$plot_roc <- out_roc$plot_roc + coord_fixed(ratio = 1) # equal axes
      }

      out_roc_temp[[ n_target ]] <- out_roc
    } # n_target
  } # sw_imbalanced_binary TRUE

  # hierarchy: reorder ROC objects by type (rather than target)
  out[[ "o_class_full_ROC" ]] <-
    out_roc_temp |>
    e_plot_roc_reorder_hierarchy()


  if (!all(var_subgroup_analysis %in% names(dat_rf_data))) {
    e_log_write(
      paste0(
        "Full model, plot ROC curves (subgroups: "
      , paste0(var_subgroup_analysis, collapse = ", ")
      , ") -- variables not in dataset"
      )
    , log_obj     = log_obj
    , i_level     = 3
    )
    var_subgroup_analysis <- NULL
  }

  # Subgroup analysis
  if (!is.null(var_subgroup_analysis)) {

    e_log_write(
      paste0(
        "Full model, plot ROC curves (subgroups: "
      , paste0(var_subgroup_analysis, collapse = ", ")
      , ")"
      )
    , log_obj     = log_obj
    , i_level     = 2
    )

    if(length(var_subgroup_analysis) == 1) {
      tab_list_subgroup_analysis_levels <-
        dat_rf_class |>
        tidyr::expand(
          tidyr::nesting(
            !!rlang::sym(var_subgroup_analysis[1])
          )
        )
    }
    if(length(var_subgroup_analysis) == 2) {
      tab_list_subgroup_analysis_levels <-
        dat_rf_class |>
        tidyr::expand(
          tidyr::nesting(
            !!rlang::sym(var_subgroup_analysis[1])
          , !!rlang::sym(var_subgroup_analysis[2])
          )
        )
    }
    if(length(var_subgroup_analysis) == 3) {
      tab_list_subgroup_analysis_levels <-
        dat_rf_class |>
        tidyr::expand(
          tidyr::nesting(
            !!rlang::sym(var_subgroup_analysis[1])
          , !!rlang::sym(var_subgroup_analysis[2])
          , !!rlang::sym(var_subgroup_analysis[3])
          )
        )
    }
    if(length(var_subgroup_analysis) > 3) {
      warning("erikmisc::e_rfsrc_classification: No more than 3 subgroup analysis variables implemented")
    }

    tab_list_subgroup_analysis_levels <-
      tab_list_subgroup_analysis_levels |>
      dplyr::mutate(
        dplyr::across(dplyr::where(is.factor), as.character)
      )


    out[[ "o_class_full_ROC_subgroup" ]] <- list()

    for (i_row in seq_len(nrow(tab_list_subgroup_analysis_levels))) {
      ## i_row = 1
      ind_subgroup <-
        tibble::tibble(
          all = rep(TRUE, nrow(dat_rf_data))
        )
      for (n_col in colnames(tab_list_subgroup_analysis_levels)) {
        ## n_col = colnames(tab_list_subgroup_analysis_levels)[1]
        ind_subgroup <-
          ind_subgroup |>
          dplyr::bind_cols(
            {{n_col}} :=
              dat_rf_data[[ n_col ]] ==
              dplyr::pull(tab_list_subgroup_analysis_levels[i_row, n_col])
          )

      } # for i_col

      # which indices for subgroup
      ind_subgroup <- which(apply(ind_subgroup, 1, all))

      out_roc_temp <- list()
      for (n_target in levels(dat_rf_data[[ rf_y_var ]])) {
        threshold_to_use <- out[[ "o_class_full_ROC" ]][[ "roc_curve_best" ]][[ n_target ]]$thresh
        out_roc <-
          e_plot_roc(
            labels_true       = ifelse(dat_rf_data[[ rf_y_var ]] == n_target, 1, 0)[ind_subgroup]
          , pred_values_pos   = o_class_full$predicted.oob[ind_subgroup, n_target]
          , label_neg_pos     = c(0, 1)
          , sw_plot           = !sw_quick_full_only #TRUE
          , cm_mode           = c("sens_spec", "prec_recall", "everything")[3]
          , sw_caption_desc   = FALSE
          , threshold_to_use  = threshold_to_use
          )
        label_subgroup <-
          paste0(
            colnames(tab_list_subgroup_analysis_levels[i_row,])
          , " = "
          , tab_list_subgroup_analysis_levels[i_row,] |> as.character()
          , collapse = ", "
          )
        if(!sw_quick_full_only) {
          #p <- out$plot_roc
          out_roc$plot_roc <- out_roc$plot_roc + labs(title = paste0("ROC Curve, Target:  ", n_target))

          out_roc$plot_roc <- out_roc$plot_roc + labs(subtitle = paste0("Subgroup:  ", label_subgroup))
          out_roc$plot_roc <- out_roc$plot_roc + coord_fixed(ratio = 1) # equal axes
        }

        out_roc_temp[[ n_target ]] <- out_roc
      } # n_target

      # hierarchy: reorder ROC objects by type (rather than target)
      out[[ "o_class_full_ROC_subgroup" ]][[ label_subgroup ]] <-
        out_roc_temp |>
        e_plot_roc_reorder_hierarchy()

    } # for i_row

  } # var_subgroup_analysis

  # sw_quick_full_only ends here
  if(sw_quick_full_only) {

    e_log_write(
      "erikmisc::e_rfsrc_classification Process END"
    , log_obj     = log_obj
    , i_level     = 2
    )

    return(out)
  } # sw_quick_full_only

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

  if (!sw_reduce_output) {
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
        , limitsize = FALSE
        )
    } # i_level
  } # sw_reduce_output


  # Subgroup analysis
  if (!is.null(var_subgroup_analysis)) {
    for (n_subgroup in names(out[[ "o_class_full_ROC_subgroup" ]])) {
      ## n_subgroup = names(out[[ "o_class_full_ROC_subgroup" ]])[1]
      ggplot2::ggsave(
          filename =
            file.path(
              out_path
            , paste0(
                file_prefix
              , "__"
              , "plot_o_class_full_ROC"
              , "__"
              , "subgroup"
              , "__"
              , n_subgroup
              , "."
              , plot_format
              )
            )
        , plot   =
            cowplot::plot_grid(
              plotlist = out[[ "o_class_full_ROC_subgroup" ]][[ n_subgroup ]]$plot_roc
            ,  nrow = 1
            ) +
            patchwork::plot_annotation(
            #labs(
              title     = plot_title
            , subtitle  = "Full model, ROC Curves, Subgroups"
            #, caption   = paste0(
            #                ""
            #              )
            , theme = theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)
            )
        , width  = 5 * length(levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]]))
        , height = 6
        ## png, jpeg
        , dpi    = 300
        , bg     = "white"
        ## pdf
        , units  = "in"
        #, useDingbats = FALSE
        , limitsize = FALSE
        )
    } # n_subgroup
  } # var_subgroup_analysis

  # Subgroup analysis
  if (!is.null(var_subgroup_analysis)) {
    for (n_subgroup in names(out[[ "o_class_full_ROC_subgroup" ]])) {
      if (!sw_reduce_output) {
        for (i_level in seq_along(levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]]))) {
          ggplot2::ggsave(
              filename =
                file.path(
                  out_path
                , paste0(
                    file_prefix
                  , "__"
                  , "plot_o_class_full_ROC"
                  , "__"
                  , "subgroup"
                  , "__"
                  , n_subgroup
                  , "_"
                  , i_level
                  , "-"
                  , levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]])[i_level]
                  , "."
                  , plot_format
                  )
                )
            , plot   =
                out[[ "o_class_full_ROC_subgroup" ]][[ n_subgroup ]]$plot_roc[[ i_level ]] +
                patchwork::plot_annotation(
                #labs(
                #  title     = plot_title
                #, subtitle  = "Full model, ROC Curves"
                #, caption   = paste0(
                #                ""
                #              )
                , theme = theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)
                )
            , width  = 4
            , height = 4.5
            ## png, jpeg
            , dpi    = 300
            , bg     = "white"
            ## pdf
            , units  = "in"
            #, useDingbats = FALSE
            , limitsize = FALSE
            )
        } # i_level
      } # sw_reduce_output

      ## n_subgroup = names(out[[ "o_class_full_ROC_subgroup" ]])[1]
      ggplot2::ggsave(
          filename =
            file.path(
              out_path
            , paste0(
                file_prefix
              , "__"
              , "plot_o_class_full_ROC"
              , "__"
              , "subgroup"
              , "__"
              , n_subgroup
              , "."
              , plot_format
              )
            )
        , plot   =
            cowplot::plot_grid(
              plotlist = out[[ "o_class_full_ROC_subgroup" ]][[ n_subgroup ]]$plot_roc
            ,  nrow = 1
            ) +
            patchwork::plot_annotation(
            #labs(
              title     = plot_title
            , subtitle  = "Full model, ROC Curves, Subgroups"
            #, caption   = paste0(
            #                ""
            #              )
            , theme = theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)
            )
        , width  = 5 * length(levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]]))
        , height = 6
        ## png, jpeg
        , dpi    = 300
        , bg     = "white"
        ## pdf
        , units  = "in"
        #, useDingbats = FALSE
        , limitsize = FALSE
        )
    } # n_subgroup
  } # var_subgroup_analysis



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

  if (!sw_reduce_output) {
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
      , limitsize = FALSE
      )
  } # sw_reduce_output

  if (!sw_reduce_output) {
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
        , limitsize = FALSE
        )

    } # i_level
  } # sw_reduce_output



  # VIMP and ROC for each target
  if (!sw_reduce_output) {
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
        , limitsize = FALSE
        )
    } # i_level
  } # sw_reduce_output



  ### Confidence intervals and standard errors for VIMP (variable importance)
  e_log_write(
    paste0("Full model, Subsample for VIMP and model selection")
  , log_obj     = log_obj
  , i_level     = 2
  )

  if (sw_imbalanced_binary == c(FALSE, TRUE)[1]) {
    this_subsample_importance = 1     # normal, use "anti"
  }
  if (sw_imbalanced_binary == c(FALSE, TRUE)[2]) {
    this_subsample_importance = 2     # imbalanced, use "permute"
  }
  o_class_full_subsample <-
    randomForestSRC::subsample(
      obj               = o_class_full
    , B                 = n_boot_resamples  # Number of subsamples (or number of bootstraps).
    , block.size        = 1             # 1 = VIMP is calculated for each tree. "ntree" =  VIMP is calculated for the entire forest, ensemble VIMP.
    , importance        = c("anti", "permute", "random")[this_subsample_importance]
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
    randomForestSRC::extract.subsample (o_class_full_subsample, alpha = sw_alpha_sel)

  # Use double bootstrap approach in place of subsampling? Much slower, but potentially more accurate.
  out[[ "o_class_full_subsample_extract_bootsample" ]] <-
    randomForestSRC::extract.bootsample(o_class_full_subsample, alpha = sw_alpha_sel)


  ### VIMP plot out to a selected alpha level
  out[[ "plot_o_class_full_subsample" ]] <-
    patchwork::wrap_elements(
      full =
        cowplot::as_grob(
          ~ randomForestSRC::plot.subsample(
            x           = o_class_full_subsample
          , alpha       = sw_alpha_sel
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
    , limitsize = FALSE
    )

  # cowplot::plot_grid(out[[ "plot_o_class_full_subsample" ]])

  readr::write_csv(
    x =
      out[[ "o_class_full_subsample_extract_bootsample" ]]$var.sel.Z |>
      tibble::as_tibble(rownames = "Var")
  , file =
      file.path(
        out_path
      , paste0(
          file_prefix
        , "__"
        , "o_class_full_VIMP_CI"
        , ".csv"
        )
      )
  )

  out[[ "plot_o_class_full_vimp_CI" ]] <-
    f_plot_VIMP_bs(
      out_bs        = out[[ "o_class_full_subsample_extract_bootsample" ]]
    , sw_alpha_sel  = sw_alpha_sel
    , sw_model_name = c("Full", "Selected")[1]
    )

  ggplot2::ggsave(
      filename =
        file.path(
          out_path
        , paste0(
            file_prefix
          , "__"
          , "plot_o_class_full_VIMP_CI"
          , "."
          , plot_format
          )
        )
    , plot   =
        out[[ "plot_o_class_full_vimp_CI" ]]$p
    , width  = 8
    , height = 2 + 0.15 * length(rf_x_var_full)
    ## png, jpeg
    , dpi    = 300
    , bg     = "white"
    ## pdf
    , units  = "in"
    #, useDingbats = FALSE
    , limitsize = FALSE
    )

  ## One decision tree
  e_log_write(
    paste0("Full model, plot ", n_single_decision_tree_plots, " decision tree(s)")
  , log_obj     = log_obj
  , i_level     = 2
  )

  if (n_single_decision_tree_plots > 0) {
    for (i_tree in 1:n_single_decision_tree_plots) {

      fn_tree_root <-
        file.path(
          out_path
        , paste0(
            file_prefix
          , "__"
          , "plot_o_class_full_one_tree_"
          , i_tree
          )
        )
      fn_tree_html <-
        paste0(
          fn_tree_root
        , "."
        , "html"
        )
      fn_tree_html_dir <-
        paste0(
          fn_tree_root
        , "_files"
        )
      fn_tree_plot <-
        paste0(
          fn_tree_root
        , "."
        , plot_format
        )


      # https://rdrr.io/cran/randomForestSRC/man/get.tree.rfsrc.html
      out[[ "plot_o_class_full_one_tree" ]] <-
        randomForestSRC::get.tree(
          object      = o_class_full
        , tree.id     = i_tree
        , class.type  = c("bayes", "rfq", "prob")[2]
        , ensemble    = FALSE
        , show.plots  = TRUE
        )

      # https://forum.posit.co/t/save-viewer-object-rendered-in-rstudio-as-image/32796/4
      htmlwidgets::saveWidget(
        out[[ "plot_o_class_full_one_tree" ]] |> plot()
      , file    = fn_tree_html

      )

      webshot::webshot(
        url     = fn_tree_html
      , file    = fn_tree_plot
      , vwidth  = 72 * (4 + 0.25 * length(rf_x_var_full))
      , vheight = 72 * (4 + 0.10 * length(rf_x_var_full))
      )

      unlink(fn_tree_html)
      unlink(fn_tree_html_dir, recursive = TRUE, force = TRUE)

    } # i_tree
  } # n_single_decision_tree_plots


  ## Select variables from randomForestSRC::extract.bootsample(o_class_full_subsample)
  e_log_write(
    "Full model, Model selection"
  , log_obj     = log_obj
  , i_level     = 2
  )

  rf_x_var_sel <-
    rownames(
      randomForestSRC::extract.bootsample(o_class_full_subsample, alpha = sw_alpha_sel)$var.sel.Z
    )[randomForestSRC::extract.bootsample(o_class_full_subsample, alpha = sw_alpha_sel)$var.sel.Z$signif]

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
    e_log_write(
      "Full model, Partial effects plots"
    , log_obj     = log_obj
    , i_level     = 2
    )

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
        , limitsize = FALSE
        )
    } # i_level

      ## for (i_level in seq_along(levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]]))) {
      ##   out[[ "plot_o_class_full_partial_effects" ]][[ i_level ]] |>
      ##     cowplot::plot_grid() |>
      ##     print()
      ## }


    if (!sw_reduce_output) {
      e_log_write(
        "Full model, Marginal effects plots"
      , log_obj     = log_obj
      , i_level     = 2
      )

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
          , limitsize = FALSE
          )
      } # i_level
    } # sw_reduce_output


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
      out[[ "plot_o_class_full_vimp_CI" ]]       + labs(title = NULL) +
      #out[[ "plot_o_class_full_subsample" ]]     + labs(title = NULL) +
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
                        "Full model (k var = ", length(out$rf_x_var_full), ", AUC = ", round(out$o_class_full_AUC, 3), "): "
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
      , limitsize = FALSE
      )

    e_log_write(
      "Full model, Complete -- no model selection"
    , log_obj     = log_obj
    , i_level     = 2
    )
    e_log_write(
      "erikmisc::e_rfsrc_classification Process END"
    , log_obj     = log_obj
    , i_level     = 2
    )

    return(out)
  } # full model only, stop here



  ##############################################################################
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

  e_log_write(
    paste0("Selected model: ", text_formula_sel)
  , log_obj     = log_obj
  , i_level     = 2
  )

  ### Grow forest
  e_log_write(
    "Selected model, Grow forest"
  , log_obj     = log_obj
  , i_level     = 2
  )

  if (sw_imbalanced_binary == c(FALSE, TRUE)[1]) {
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
  }
  if (sw_imbalanced_binary == c(FALSE, TRUE)[2]) {
    o_class_sel <-
      randomForestSRC::imbalanced(
        formula     = rf_formula_sel
      , data        = dat_rf_data
      , ntree       = sw_rfsrc_ntree
      , method      = c("rfq", "brf", "standard")[1]                            # imbalanced argument (added)
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
      #, samptype    = c("swor", "swr")[1]                                      # imbalanced argument (commented out)
      #, samp        = NULL
      #, membership  = FALSE
      #, sampsize    = if (samptype == "swor") function(x){x * .632} else function(x){x}
      , na.action   = c("na.omit", "na.impute")[2]
      #, nimpute     = 1
      #, ntime       = 150    # survival
      #, cause       = NULL   # competing risks
      , perf.type   = c("none", "misclass", "brier", "gmean")[4]  # classification  # imbalanced argument (gmean)
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

    ## (1) default threshold (2) directly optimized gmean threshold
    threshold_default <-
      randomForestSRC::get.imbalanced.performance(
        o_class_sel
      )["threshold"]

    threshold_gmean   <-
      randomForestSRC::get.imbalanced.optimize(
        o_class_sel
      , measure = "gmean"
      , plot.it = FALSE
      )["threshold"] |>
      as.numeric()

  }
  out[[ "o_class_sel" ]] <-
    o_class_sel


  e_log_write(
    paste0(
      "Selected model, Model statistics and confusion matrix"
    , "\n\n"
    , o_class_sel |>
      print() |>
      capture.output() |>
      paste0(collapse = "\n")
    , "\n\n"
    )
  , log_obj     = log_obj
  , i_level     = 2
  )

  # save model (to be used for prediction)
  if(sw_save_model) {
    e_log_write(
      "Selected model, Write model object to .RData file"
    , log_obj     = log_obj
    , i_level     = 2
    )

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

  if (!sw_reduce_output) {
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
      , limitsize = FALSE
      )
    # cowplot::plot_grid(out[[ "plot_o_class_sel" ]])
  } # sw_reduce_output


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

  if (!sw_reduce_output) {
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
      , limitsize = FALSE
      )



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
        , limitsize = FALSE
        )

    } # i_level
  } # sw_reduce_output




  ## ROC via erikmisc
  e_log_write(
    "Selected model, ROC curves"
  , log_obj     = log_obj
  , i_level     = 2
  )

  # obtains the value of AUC (area under the ROC curve)
  o_class_sel_AUC <-
    randomForestSRC::get.auc(
      y    = dat_rf_data[[ rf_y_var ]]
    , prob = o_class_sel$predicted.oob
    )
  out[[ "o_class_sel_AUC" ]] <-
    o_class_sel_AUC

  e_log_write(
    paste0("Selected model, o_class_sel_AUC: ", o_class_sel_AUC)
  , log_obj     = log_obj
  , i_level     = 2
  )


  ## ROC via erikmisc
  # out_roc_sel_temp <- list()
  # for (n_target in levels(dat_rf_data[[ rf_y_var ]])) {
  #   out_roc <-
  #     e_plot_roc(
  #       labels_true     = ifelse(dat_rf_data[[ rf_y_var ]] == n_target, 1, 0)
  #     , pred_values_pos = o_class_sel$predicted.oob[, n_target]
  #     , label_neg_pos   = c(0, 1)
  #     , sw_plot         = !sw_quick_full_only #TRUE
  #     , cm_mode         = c("sens_spec", "prec_recall", "everything")[3]
  #     )
  #   #p <- out$plot_roc
  #   out_roc$plot_roc <- out_roc$plot_roc + labs(title = paste0("ROC Curve, Target:  ", n_target))
  #   out_roc$plot_roc <- out_roc$plot_roc + coord_fixed(ratio = 1) # equal axes
  #
  #   out_roc_sel_temp[[ n_target ]] <- out_roc
  # } # n_target

  ##if (sw_imbalanced_binary == c(FALSE, TRUE)[1]) {
  ##  threshold_to_use <- NULL
  ##  out_roc_sel_temp <- list()
  ##  for (n_target in levels(dat_rf_data[[ rf_y_var ]])) {
  ##    out_roc <-
  ##      e_plot_roc(
  ##        labels_true       = ifelse(dat_rf_data[[ rf_y_var ]] == n_target, 1, 0)
  ##      , pred_values_pos   = o_class_sel$predicted.oob[, n_target]
  ##      , label_neg_pos     = c(0, 1)
  ##      , sw_plot           = !sw_quick_full_only #TRUE
  ##      , cm_mode           = c("sens_spec", "prec_recall", "everything")[3]
  ##      , threshold_to_use  = threshold_to_use
  ##      )
  ##    #p <- out$plot_roc
  ##    out_roc$plot_roc <- out_roc$plot_roc + labs(title = paste0("ROC Curve, Target:  ", n_target))
  ##    out_roc$plot_roc <- out_roc$plot_roc + coord_fixed(ratio = 1) # equal axes
  ##
  ##    out_roc_sel_temp[[ n_target ]] <- out_roc
  ##  } # n_target
  ##}
  ##if (sw_imbalanced_binary == c(FALSE, TRUE)[2]) {
  ##  out_roc_sel_temp <- list()
  ##  for (n_target in levels(dat_rf_data[[ rf_y_var ]])) {
  ##    if (n_target == levels(dat_rf_data[[ rf_y_var ]])[1]) {
  ##      threshold_to_use <- threshold_gmean
  ##    } else {
  ##      threshold_to_use <- 1 - threshold_gmean - 1e-10
  ##    }
  ##    out_roc <-
  ##      e_plot_roc(
  ##        labels_true       = ifelse(dat_rf_data[[ rf_y_var ]] == n_target, 1, 0)
  ##      , pred_values_pos   = o_class_sel$predicted.oob[, n_target]
  ##      , label_neg_pos     = c(0, 1)
  ##      , sw_plot           = !sw_quick_full_only #TRUE
  ##      , cm_mode           = c("sens_spec", "prec_recall", "everything")[3]
  ##      , threshold_to_use  = threshold_to_use
  ##      )
  ##    #p <- out$plot_roc
  ##    out_roc$plot_roc <- out_roc$plot_roc + labs(title = paste0("ROC Curve, Target:  ", n_target))
  ##    out_roc$plot_roc <- out_roc$plot_roc + coord_fixed(ratio = 1) # equal axes
  ##
  ##    out_roc_sel_temp[[ n_target ]] <- out_roc
  ##  } # n_target
  ##}
  ##
  ### hierarchy: reorder ROC objects by type (rather than target)
  ##out[[ "o_class_sel_ROC" ]] <- list()
  ##for (n_object in names(out_roc_sel_temp[[ 1 ]])) {
  ##  ## n_object = names(out_roc_sel_temp[[ 1 ]])[1]
  ##  out[[ "o_class_sel_ROC" ]][[ n_object ]] <- list()
  ##
  ##  for (n_target in names(out_roc_sel_temp)) {
  ##    ## n_target = names(out_roc_sel_temp)[1]
  ##    out[[ "o_class_sel_ROC" ]][[ n_object ]][[ n_target ]] <-
  ##      out_roc_sel_temp[[ n_target ]][[ n_object ]]
  ##
  ##    if (n_object == "roc_curve_best") {
  ##      out[[ "o_class_sel_ROC" ]][[ n_object ]][[ n_target ]] <-
  ##        out[[ "o_class_sel_ROC" ]][[ n_object ]][[ n_target ]] |>
  ##        dplyr::mutate(
  ##          Group = n_target
  ##        ) |>
  ##        dplyr::relocate(Group)
  ##    }
  ##    if (n_object == "roc_curve") {
  ##      out[[ "o_class_sel_ROC" ]][[ n_object ]][[ n_target ]] <-
  ##        out[[ "o_class_sel_ROC" ]][[ n_object ]][[ n_target ]] |>
  ##        dplyr::mutate(
  ##          Group = n_target
  ##        ) |>
  ##        dplyr::relocate(Group)
  ##    }
  ##
  ##  }
  ##}

  if (sw_imbalanced_binary == c(FALSE, TRUE)[1]) {
    threshold_to_use <- NULL
    out_roc_temp <- list()
    for (n_target in levels(dat_rf_data[[ rf_y_var ]])) {
      out_roc <-
        e_plot_roc(
          labels_true       = ifelse(dat_rf_data[[ rf_y_var ]] == n_target, 1, 0)
        , pred_values_pos   = o_class_sel$predicted.oob[, n_target]
        , label_neg_pos     = c(0, 1)
        , sw_plot           = !sw_quick_full_only #TRUE
        , cm_mode           = c("sens_spec", "prec_recall", "everything")[3]
        , threshold_to_use  = threshold_to_use
        )
      #p <- out$plot_roc
      out_roc$plot_roc <- out_roc$plot_roc + labs(title = paste0("ROC Curve, Target:  ", n_target))
      out_roc$plot_roc <- out_roc$plot_roc + coord_fixed(ratio = 1) # equal axes

      out_roc_temp[[ n_target ]] <- out_roc
    } # n_target
  } # sw_imbalanced_binary FALSE
  if (sw_imbalanced_binary == c(FALSE, TRUE)[2]) {
    out_roc_temp <- list()
    for (n_target in levels(dat_rf_data[[ rf_y_var ]])) {
      if (n_target == levels(dat_rf_data[[ rf_y_var ]])[1]) {
        threshold_to_use <- threshold_gmean
        #threshold_to_use <- threshold_default
      } else {
        threshold_to_use <- 1 - threshold_gmean - 1e-10
        #threshold_to_use <- 1 - threshold_default - 1e-10
      }
      out_roc <-
        e_plot_roc(
          labels_true       = ifelse(dat_rf_data[[ rf_y_var ]] == n_target, 1, 0)
        , pred_values_pos   = o_class_sel$predicted.oob[, n_target]
        , label_neg_pos     = c(0, 1)
        , sw_plot           = !sw_quick_full_only #TRUE
        , cm_mode           = c("sens_spec", "prec_recall", "everything")[3]
        , threshold_to_use  = threshold_to_use
        )
      #p <- out$plot_roc
      out_roc$plot_roc <- out_roc$plot_roc + labs(title = paste0("ROC Curve, Target:  ", n_target))
      out_roc$plot_roc <- out_roc$plot_roc + coord_fixed(ratio = 1) # equal axes

      out_roc_temp[[ n_target ]] <- out_roc
    } # n_target
  } # sw_imbalanced_binary TRUE

  # hierarchy: reorder ROC objects by type (rather than target)
  out[[ "o_class_sel_ROC" ]] <-
    out_roc_temp |>
    e_plot_roc_reorder_hierarchy()


  # Subgroup analysis
  if (!is.null(var_subgroup_analysis)) {

    e_log_write(
      paste0(
        "Selected model, plot ROC curves (subgroups: "
      , paste0(var_subgroup_analysis, collapse = ", ")
      , ")"
      )
    , log_obj     = log_obj
    , i_level     = 2
    )

    if(length(var_subgroup_analysis) == 1) {
      tab_list_subgroup_analysis_levels <-
        dat_rf_class |>
        tidyr::expand(
          tidyr::nesting(
            !!rlang::sym(var_subgroup_analysis[1])
          )
        )
    }
    if(length(var_subgroup_analysis) == 2) {
      tab_list_subgroup_analysis_levels <-
        dat_rf_class |>
        tidyr::expand(
          tidyr::nesting(
            !!rlang::sym(var_subgroup_analysis[1])
          , !!rlang::sym(var_subgroup_analysis[2])
          )
        )
    }
    if(length(var_subgroup_analysis) == 3) {
      tab_list_subgroup_analysis_levels <-
        dat_rf_class |>
        tidyr::expand(
          tidyr::nesting(
            !!rlang::sym(var_subgroup_analysis[1])
          , !!rlang::sym(var_subgroup_analysis[2])
          , !!rlang::sym(var_subgroup_analysis[3])
          )
        )
    }
    if(length(var_subgroup_analysis) > 3) {
      warning("erikmisc::e_rfsrc_classification: No more than 3 subgroup analysis variables implemented")
    }

    tab_list_subgroup_analysis_levels <-
      tab_list_subgroup_analysis_levels |>
      dplyr::mutate(
        dplyr::across(dplyr::where(is.factor), as.character)
      )


    out[[ "o_class_sel_ROC_subgroup" ]] <- list()

    for (i_row in seq_len(nrow(tab_list_subgroup_analysis_levels))) {
      ## i_row = 1
      ind_subgroup <-
        tibble::tibble(
          all = rep(TRUE, nrow(dat_rf_data))
        )
      for (n_col in colnames(tab_list_subgroup_analysis_levels)) {
        ## n_col = colnames(tab_list_subgroup_analysis_levels)[1]
        ind_subgroup <-
          ind_subgroup |>
          dplyr::bind_cols(
            {{n_col}} :=
              dat_rf_data[[ n_col ]] ==
              dplyr::pull(tab_list_subgroup_analysis_levels[i_row, n_col])
          )

      } # for i_col

      # which indices for subgroup
      ind_subgroup <- which(apply(ind_subgroup, 1, all))

      out_roc_temp <- list()
      for (n_target in levels(dat_rf_data[[ rf_y_var ]])) {
        threshold_to_use <- out[[ "o_class_sel_ROC" ]][[ "roc_curve_best" ]][[ n_target ]]$thresh
        out_roc <-
          e_plot_roc(
            labels_true       = ifelse(dat_rf_data[[ rf_y_var ]] == n_target, 1, 0)[ind_subgroup]
          , pred_values_pos   = o_class_sel$predicted.oob[ind_subgroup, n_target]
          , label_neg_pos     = c(0, 1)
          , sw_plot           = !sw_quick_full_only #TRUE
          , cm_mode           = c("sens_spec", "prec_recall", "everything")[3]
          , sw_caption_desc   = FALSE
          , threshold_to_use  = threshold_to_use
          )
        #p <- out$plot_roc
        out_roc$plot_roc <- out_roc$plot_roc + labs(title = paste0("ROC Curve, Target:  ", n_target))

        label_subgroup <-
          paste0(
            colnames(tab_list_subgroup_analysis_levels[i_row,])
          , " = "
          , tab_list_subgroup_analysis_levels[i_row,] |> as.character()
          , collapse = ", "
          )
        out_roc$plot_roc <- out_roc$plot_roc + labs(subtitle = paste0("Subgroup:  ", label_subgroup))
        out_roc$plot_roc <- out_roc$plot_roc + coord_fixed(ratio = 1) # equal axes

        out_roc_temp[[ n_target ]] <- out_roc
      } # n_target

      # hierarchy: reorder ROC objects by type (rather than target)
      out[[ "o_class_sel_ROC_subgroup" ]][[ label_subgroup ]] <-
        out_roc_temp |>
        e_plot_roc_reorder_hierarchy()

    } # for i_row

  } # var_subgroup_analysis

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

  if (!sw_reduce_output) {
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
        , limitsize = FALSE
        )
    } # i_level
  } # sw_reduce_output


  # Subgroup analysis
  if (!is.null(var_subgroup_analysis)) {
    for (n_subgroup in names(out[[ "o_class_sel_ROC_subgroup" ]])) {
      ## n_subgroup = names(out[[ "o_class_sel_ROC_subgroup" ]])[1]
      for (i_level in seq_along(levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]]))) {
        ggplot2::ggsave(
            filename =
              file.path(
                out_path
              , paste0(
                  file_prefix
                , "__"
                , "plot_o_class_sel_ROC"
                , "__"
                , "subgroup"
                , "__"
                , n_subgroup
                , "_"
                , i_level
                , "-"
                , levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]])[i_level]
                , "."
                , plot_format
                )
              )
          , plot   =
              out[[ "o_class_sel_ROC_subgroup" ]][[ n_subgroup ]]$plot_roc[[ i_level ]] +
              patchwork::plot_annotation(
              #labs(
              #  title     = plot_title
              #, subtitle  = "Selected model, ROC Curves"
              #, caption   = paste0(
              #                ""
              #              )
              , theme = theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)
              )
          , width  = 4
          , height = 4.5
          ## png, jpeg
          , dpi    = 300
          , bg     = "white"
          ## pdf
          , units  = "in"
          #, useDingbats = FALSE
          , limitsize = FALSE
          )
      } # i_level

      ggplot2::ggsave(
          filename =
            file.path(
              out_path
            , paste0(
                file_prefix
              , "__"
              , "plot_o_class_sel_ROC"
              , "__"
              , "subgroup"
              , "__"
              , n_subgroup
              , "."
              , plot_format
              )
            )
        , plot   =
            cowplot::plot_grid(
              plotlist = out[[ "o_class_sel_ROC_subgroup" ]][[ n_subgroup ]]$plot_roc
            ,  nrow = 1
            ) +
            patchwork::plot_annotation(
            #labs(
              title     = plot_title
            , subtitle  = "Selected model, ROC Curves, Subgroups"
            #, caption   = paste0(
            #                ""
            #              )
            , theme = theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)
            )
        , width  = 5 * length(levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]]))
        , height = 6
        ## png, jpeg
        , dpi    = 300
        , bg     = "white"
        ## pdf
        , units  = "in"
        #, useDingbats = FALSE
        , limitsize = FALSE
        )
    } # n_subgroup
  } # var_subgroup_analysis


  # VIMP and ROC for each target
  if (!sw_reduce_output) {
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
        , limitsize = FALSE
        )
    } # i_level
  } # sw_reduce_output


  ### Confidence intervals and standard errors for VIMP (variable importance)
  e_log_write(
    "Selected model, Subsample for VIMP with CIs and SEs"
  , log_obj     = log_obj
  , i_level     = 2
  )

  o_class_sel_subsample <-
    randomForestSRC::subsample(
      obj               = o_class_sel
    , B                 = n_boot_resamples  # Number of subsamples (or number of bootstraps).
    , block.size        = 1             # 1 = VIMP is calculated for each tree. "ntree" =  VIMP is calculated for the entire forest, ensemble VIMP.
    , importance        = c("anti", "permute", "random")[this_subsample_importance]
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
    randomForestSRC::extract.subsample(o_class_sel_subsample, alpha = sw_alpha_sel)

  # Use double bootstrap approach in place of subsampling? Much slower, but potentially more accurate.
  out[[ "o_class_sel_subsample_extract_bootsample" ]] <-
    randomForestSRC::extract.bootsample(o_class_sel_subsample, alpha = sw_alpha_sel)

  ### VIMP plot out to a selected alpha level
  out[[ "plot_o_class_sel_subsample" ]] <-
    patchwork::wrap_elements(
      full =
        cowplot::as_grob(
          ~ randomForestSRC::plot.subsample(
            x           = o_class_sel_subsample
          , alpha       = sw_alpha_sel
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

  if (!sw_reduce_output) {
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
      , limitsize = FALSE
      )
    # cowplot::plot_grid(out[[ "plot_o_class_sel_subsample" ]])
  } # sw_reduce_output

  readr::write_csv(
    x =
      out[[ "o_class_sel_subsample_extract_bootsample" ]]$var.sel.Z |>
      tibble::as_tibble(rownames = "Var")
  , file =
      file.path(
        out_path
      , paste0(
          file_prefix
        , "__"
        , "o_class_sel_VIMP_CI"
        , ".csv"
        )
      )
  )

  out[[ "plot_o_class_sel_vimp_CI" ]] <-
    f_plot_VIMP_bs(
      out_bs        = out[[ "o_class_sel_subsample_extract_bootsample" ]]
    , sw_alpha_sel  = sw_alpha_sel
    , sw_model_name = c("Full", "Selected")[2]
    )

  ggplot2::ggsave(
      filename =
        file.path(
          out_path
        , paste0(
            file_prefix
          , "__"
          , "plot_o_class_sel_VIMP_CI"
          , "."
          , plot_format
          )
        )
    , plot   =
        out[[ "plot_o_class_sel_vimp_CI" ]]$p
    , width  = 8
    , height = 2 + 0.15 * length(rf_x_var_sel)
    ## png, jpeg
    , dpi    = 300
    , bg     = "white"
    ## pdf
    , units  = "in"
    #, useDingbats = FALSE
    , limitsize = FALSE
    )


  ## One decision tree
  e_log_write(
    paste0("Selected model, plot ", n_single_decision_tree_plots, " decision tree(s)")
  , log_obj     = log_obj
  , i_level     = 2
  )

  if (n_single_decision_tree_plots > 0) {
    for (i_tree in 1:n_single_decision_tree_plots) {

      fn_tree_root <-
        file.path(
          out_path
        , paste0(
            file_prefix
          , "__"
          , "plot_o_class_sel_one_tree_"
          , i_tree
          )
        )
      fn_tree_html <-
        paste0(
          fn_tree_root
        , "."
        , "html"
        )
      fn_tree_html_dir <-
        paste0(
          fn_tree_root
        , "_files"
        )
      fn_tree_plot <-
        paste0(
          fn_tree_root
        , "."
        , plot_format
        )


      # https://rdrr.io/cran/randomForestSRC/man/get.tree.rfsrc.html
      out[[ "plot_o_class_sel_one_tree" ]] <-
        randomForestSRC::get.tree(
          object      = o_class_sel
        , tree.id     = i_tree
        , class.type  = c("bayes", "rfq", "prob")[2]
        , ensemble    = FALSE
        , show.plots  = TRUE
        )

      # https://forum.posit.co/t/save-viewer-object-rendered-in-rstudio-as-image/32796/4
      htmlwidgets::saveWidget(
        out[[ "plot_o_class_sel_one_tree" ]] |> plot()
      , file    = fn_tree_html

      )

      webshot::webshot(
        url     = fn_tree_html
      , file    = fn_tree_plot
      , vwidth  = 72 * (4 + 0.25 * length(rf_x_var_sel))
      , vheight = 72 * (4 + 0.10 * length(rf_x_var_sel))
      )

      unlink(fn_tree_html)
      unlink(fn_tree_html_dir, recursive = TRUE, force = TRUE)

    } # i_tree
  } # n_single_decision_tree_plots


  ### Marginal/Partial effects plots
  e_log_write(
    "Selected model, Partial effects plots"
  , log_obj     = log_obj
  , i_level     = 2
  )

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
      , limitsize = FALSE
      )
  } # i_level

    ## for (i_level in seq_along(levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]]))) {
    ##   out[[ "plot_o_class_sel_partial_effects" ]][[ i_level ]] |>
    ##     cowplot::plot_grid() |>
    ##     print()
    ## }

  if (!sw_reduce_output) {
    e_log_write(
      "Selected model, Marginal effects plots"
    , log_obj     = log_obj
    , i_level     = 2
    )

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
        , limitsize = FALSE
        )
    } # i_level
  } # sw_reduce_output


  ## bivariate partial plots for top k features by VIMP
  if ((k_partial_coplot_var >= 2) & length(out$rf_x_var_sel) >= 2) {

    e_log_write(
      "Selected model, Bivariate partial plots"
    , log_obj     = log_obj
    , i_level     = 2
    )

    if (k_partial_coplot_var > length(out$rf_x_var_sel)) {
      k_partial_coplot_var <- length(out$rf_x_var_sel)
    }

    x_var_sort_vimp <-
      out[[ "plot_o_class_sel_vimp_CI" ]]$dat_bs |>
      dplyr::arrange(dplyr::desc(mean)) |>
      dplyr::pull(Var)

    out[[ "plot_o_class_sel_partial_coplot" ]] <- list()
    i_plot <- 0

    for (i_var_1 in 1:(k_partial_coplot_var - 1)) {
      for (i_var_2 in (i_var_1 + 1):k_partial_coplot_var) {
        ## i_var_1 = 1
        ## i_var_2 = 2

        ## partial effect for x1|x2 then x2|x1

        ## specify x1 and x2 values of interest
        x1_sort <- sort(unique(o_class_sel$xvar[[ x_var_sort_vimp[i_var_1] ]]))
        x2_sort <- sort(unique(o_class_sel$xvar[[ x_var_sort_vimp[i_var_2] ]]))

        # x1|x2
        dat_partial_coplot <-
          do.call(
            rbind
          , lapply(
              x2_sort
            , function(x2) {
                o_partial <-
                  randomForestSRC::partial(
                    object          = o_class_sel
                  , partial.xvar    = x_var_sort_vimp[i_var_1]
                  , partial.xvar2   = x_var_sort_vimp[i_var_2]
                  , partial.values  = x1_sort
                  , partial.values2 = x2
                  )
                out <-
                  cbind(
                    x1_sort
                  , x2
                  , randomForestSRC::get.partial.plot.data(o_partial)$yhat
                  )
                return(out)
              }
            )
          ) |>
          data.frame()
        colnames(dat_partial_coplot) <- c("x1_sort", "x2_sort", "effectSize")

        ## coplot of partial effect of wind and temp
        coplot_1 <-
          ggplotify::as.ggplot(
            function() {
              graphics::coplot(
                formula = effectSize ~ x1_sort|x2_sort
              , data    = dat_partial_coplot
              , pch     = 16
              , overlap = 0
              , xlab    = c(x_var_sort_vimp[i_var_1], paste("Given :", x_var_sort_vimp[i_var_2]))
              )
            }
          ) +
          labs(title = paste0("Bivariate partial plot: (", i_var_1, ") ", x_var_sort_vimp[i_var_1], " given (", i_var_2, ") ", x_var_sort_vimp[i_var_2]))

        # x2|x1
        dat_partial_coplot <-
          do.call(
            rbind
          , lapply(
              x1_sort
            , function(x1) {
                o_partial <-
                  randomForestSRC::partial(
                    object          = o_class_sel
                  , partial.xvar    = x_var_sort_vimp[i_var_2]
                  , partial.xvar2   = x_var_sort_vimp[i_var_1]
                  , partial.values  = x2_sort
                  , partial.values2 = x1
                  )
                out <-
                  cbind(
                    x2_sort
                  , x1
                  , randomForestSRC::get.partial.plot.data(o_partial)$yhat
                  )
                return(out)
              }
            )
          ) |>
          data.frame()
        colnames(dat_partial_coplot) <- c("x2_sort", "x1_sort", "effectSize")

        ## coplot of partial effect of wind and temp
        coplot_2 <-
          ggplotify::as.ggplot(
            function() {
              graphics::coplot(
                formula = effectSize ~ x2_sort|x1_sort
              , data    = dat_partial_coplot
              , pch     = 16
              , overlap = 0
              , xlab    = c(x_var_sort_vimp[i_var_2], paste("Given :", x_var_sort_vimp[i_var_1]))
              )
            }
          ) +
          labs(title = paste0("Bivariate partial plot: (", i_var_2, ") ", x_var_sort_vimp[i_var_2], " given (", i_var_1, ") ", x_var_sort_vimp[i_var_1]))


        i_plot <- i_plot + 1
        out[[ "plot_o_class_sel_partial_coplot" ]][[ i_plot ]] <-
          coplot_1 + coplot_2 +
          patchwork::plot_layout(nrow = 1) +
          patchwork::plot_annotation(
            title       = plot_title
          , subtitle    = paste0("Selected model, Bivariate partial plot, VIMP variables ", i_var_1, " and ", i_var_2)
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
                , "plot_o_class_sel_partial_coplot"
                , "_"
                , i_var_1
                , "_"
                , i_var_2
                , "."
                , plot_format
                )
              )
          , plot   =
              out[[ "plot_o_class_sel_partial_coplot" ]][[ i_plot ]]
          , width  = 16
          , height = 8
          ## png, jpeg
          , dpi    = 300
          , bg     = "white"
          ## pdf
          , units  = "in"
          #, useDingbats = FALSE
          , limitsize = FALSE
          )

      } # i_var_2
    } # i_var_1

  } # if >=2

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


  #### duplicate to code above
  ## # XXX 1/8/2024 8:27PM
  ## ### ROC Curve
  ## e_log_write(
  ##   "Selected model, ROC Curve"
  ## , log_obj     = log_obj
  ## , i_level     = 2
  ## )
  ##
  ## # obtains the value of AUC (area under the ROC curve)
  ## o_class_sel_AUC <-
  ##   randomForestSRC::get.auc(
  ##     y    = dat_rf_data[[ rf_y_var ]]
  ##   , prob = o_class_sel$predicted.oob
  ##   )
  ## out[[ "o_class_sel_AUC" ]] <-
  ##   o_class_sel_AUC
  ##
  ## e_log_write(
  ##   paste0("Selected model, o_class_sel_AUC: ", o_class_sel_AUC)
  ## , log_obj     = log_obj
  ## , i_level     = 2
  ## )
  ##
  ##
  ## ## ROC via erikmisc
  ## out_roc_temp <- list()
  ## for (n_target in levels(dat_rf_data[[ rf_y_var ]])) {
  ##   out_roc <-
  ##     e_plot_roc(
  ##       labels_true     = ifelse(dat_rf_data[[ rf_y_var ]] == n_target, 1, 0)
  ##     , pred_values_pos = o_class_sel$predicted.oob[, n_target]
  ##     , label_neg_pos   = c(0, 1)
  ##     , sw_plot         = !sw_quick_full_only #TRUE
  ##     , cm_mode         = c("sens_spec", "prec_recall", "everything")[3]
  ##     )
  ##   #p <- out$plot_roc
  ##   out_roc$plot_roc <- out_roc$plot_roc + labs(title = paste0("ROC Curve, Target:  ", n_target))
  ##   out_roc$plot_roc <- out_roc$plot_roc + coord_fixed(ratio = 1) # equal axes
  ##
  ##   out_roc_temp[[ n_target ]] <- out_roc
  ## } # n_target
  ##
  ## # hierarchy: reorder ROC objects by type (rather than target)
  ## out[[ "o_class_sel_ROC" ]] <-
  ##   out_roc_temp |>
  ##   e_plot_roc_reorder_hierarchy()
  ##
  ## readr::write_csv(
  ##   x = out[[ "o_class_sel_ROC" ]]$roc_curve_best |> dplyr::bind_rows()
  ## , file =
  ##     file.path(
  ##       out_path
  ##     , paste0(
  ##         file_prefix
  ##       , "__"
  ##       , "o_class_sel_ROC"
  ##       , ".csv"
  ##       )
  ##     )
  ## )
  ##
  ##
  ## out[[ "plot_o_class_sel_ROC" ]] <-
  ##   patchwork::wrap_elements(
  ##     full =
  ##       cowplot::plot_grid(plotlist = out$o_class_sel_ROC$plot_roc, nrow = 1)
  ##   ) +
  ##   patchwork::plot_annotation(
  ##   #labs(
  ##     title     = plot_title
  ##   , subtitle  = "Selected model, ROC Curves"
  ##   #, caption   = paste0(
  ##   #                ""
  ##   #              )
  ##   , theme = theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)
  ##   )
  ##   # +
  ##   #theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)
  ##
  ## ggplot2::ggsave(
  ##     filename =
  ##       file.path(
  ##         out_path
  ##       , paste0(
  ##           file_prefix
  ##         , "__"
  ##         , "plot_o_class_sel_ROC"
  ##         , "."
  ##         , plot_format
  ##         )
  ##       )
  ##   , plot   =
  ##       out[[ "plot_o_class_sel_ROC" ]]
  ##   , width  = 5 * length(levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]]))
  ##   , height = 6
  ##   ## png, jpeg
  ##   , dpi    = 300
  ##   , bg     = "white"
  ##   ## pdf
  ##   , units  = "in"
  ##   #, useDingbats = FALSE
  ##   )
  ##
  ## for (i_level in seq_along(levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]]))) {
  ##   ggplot2::ggsave(
  ##       filename =
  ##         file.path(
  ##           out_path
  ##         , paste0(
  ##             file_prefix
  ##           , "__"
  ##           , "plot_o_class_sel_ROC"
  ##           , "_"
  ##           , i_level
  ##           , "-"
  ##           , levels(dat_rf_class[[ out[[ "rf_y_var" ]] ]])[i_level]
  ##           , "."
  ##           , plot_format
  ##           )
  ##         )
  ##     , plot   =
  ##         out$o_class_sel_ROC$plot_roc[[ i_level ]] +
  ##         patchwork::plot_annotation(
  ##         #labs(
  ##           title     = plot_title
  ##         , subtitle  = "Selected model, ROC Curves"
  ##         #, caption   = paste0(
  ##         #                ""
  ##         #              )
  ##         , theme = theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)
  ##         )
  ##     , width  = 5
  ##     , height = 6
  ##     ## png, jpeg
  ##     , dpi    = 300
  ##     , bg     = "white"
  ##     ## pdf
  ##     , units  = "in"
  ##     #, useDingbats = FALSE
  ##     )
  ## } # i_level


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
    out[[ "plot_o_class_full_vimp_CI" ]]       + labs(title = NULL) +
    #out[[ "plot_o_class_full_subsample" ]]     + labs(title = NULL) +
    out[[ "plot_o_class_sel" ]]           + labs(title = NULL) +
    out[[ "plot_o_class_sel_vimp_CI" ]]       + labs(title = NULL) +
    #out[[ "plot_o_class_sel_subsample" ]] + labs(title = NULL) +
    out[[ "plot_o_class_sel_ROC" ]] +
    #cowplot::plot_grid(plotlist = out$plot_o_class_sel_ROC$plot_roc, nrow = 1) +
    patchwork::plot_layout(design = plot_design) +
    patchwork::plot_annotation(
      title       = plot_title
    , subtitle    =
                    paste0(
                      "Full model (k var = ", length(out$rf_x_var_full), ", AUC = ", round(out$o_class_full_AUC, 3), "): "
                    , text_formula
                    , "\n"
                    , "Selected model (k var = ", length(out$rf_x_var_sel), ", AUC = ", round(out$o_class_sel_AUC, 3), "): "
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
    , limitsize = FALSE
    )

  e_log_write(
    "erikmisc::e_rfsrc_classification Process END"
  , log_obj     = log_obj
  , i_level     = 2
  )

  return(out)

} # e_rfsrc_classification
