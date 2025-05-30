% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/e_rfsrc_classification.R
\name{e_rfsrc_classification}
\alias{e_rfsrc_classification}
\title{Random Forests classification workflow using \code{randomForestSRC}}
\usage{
e_rfsrc_classification(
  dat_rf_class = NULL,
  rf_y_var = NULL,
  rf_x_var = NULL,
  rf_id_var = NULL,
  sw_rfsrc_ntree = 500,
  sw_alpha_sel = 0.05,
  sw_select_full = c("select", "full")[1],
  sw_na_action = c("na.omit", "na.impute")[1],
  sw_save_model = c(TRUE, FALSE)[1],
  plot_title = "Random Forest",
  out_path = "out_sel",
  file_prefix = "out_e_rf",
  var_subgroup_analysis = NULL,
  plot_format = c("png", "pdf")[1],
  n_marginal_plot_across = 6,
  sw_imbalanced_binary = c(FALSE, TRUE)[1],
  sw_threshold_to_use = c(FALSE, TRUE)[1],
  sw_quick_full_only = c(FALSE, TRUE)[1],
  sw_reduce_output = c(TRUE, FALSE)[1],
  n_single_decision_tree_plots = 0,
  k_partial_coplot_var = 3,
  n_boot_resamples = 100
)
}
\arguments{
\item{dat_rf_class}{data.frame with data}

\item{rf_y_var}{y factor variable, if NULL then assumed to be the first column of \code{dat_rf_class}}

\item{rf_x_var}{x variables, if NULL then assumed to be all except the first column of \code{dat_rf_class}}

\item{rf_id_var}{ID variable, removed from dataset prior to analysis}

\item{sw_rfsrc_ntree}{\code{ntree} argument for \code{randomForestSRC::rfsrc()}}

\item{sw_alpha_sel}{\code{alpha} argument for \code{randomForestSRC::plot.subsample()} and \code{randomForestSRC::extract.bootsample()} for model selection}

\item{sw_select_full}{run with model selection, or only fit full model}

\item{sw_na_action}{missing values, omit or impute (only x var, never impute y var, always drop rows with missing y var)}

\item{sw_save_model}{T/F to save model to .Rdata file}

\item{plot_title}{title for plots}

\item{out_path}{path to save output}

\item{file_prefix}{file prefix for saved output}

\item{var_subgroup_analysis}{variable(s) list (in \code{c(var1, var2)}) for subgroup analysis (group-specific ROC curves and confusion matrices) using ROC threshold from non-subgroup ROC curve, or \code{NULL} for none}

\item{plot_format}{plot format supported by \code{ggplot2::ggsave()}}

\item{n_marginal_plot_across}{for partial and marginal plots, number of plots per row (increase if not all plots are displayed)}

\item{sw_imbalanced_binary}{T/F to use standard or imbalanced binary classification with \code{rfsrc::imbalanced()}.  It is recommended to increase ntree to \code{5 * sw_rfsrc_ntree}.}

\item{sw_threshold_to_use}{T/F NOT YET USED XXX}

\item{sw_quick_full_only}{T/F to only fit full model and return model object}

\item{sw_reduce_output}{T/F exclude individual ROC and VIMP plots, and marginal plots}

\item{n_single_decision_tree_plots}{number of example decision trees to plot (recommend not too many)}

\item{k_partial_coplot_var}{number of top variables by VIMP to create bivariate partial (conditioning) plots}

\item{n_boot_resamples}{number of subsamples (or number of bootstraps) for VIMP CIs}
}
\value{
list with many RF objects, summaries, and plots
}
\description{
Random Forests classification workflow using \code{randomForestSRC}
}
\examples{
\dontrun{
dat_rf_class <-
  erikmisc::dat_mtcars_e |>
  #dplyr::select(
  #  -model
  #) |>
  #dplyr::select(
  #  cyl
  #, tidyselect::everything()
  #) |>
  dplyr::mutate(
  #  ID    = 1:dplyr::n() # ID number
    disp  =              # add missing values
      dplyr::case_when(
        disp |> dplyr::between(160, 170) ~ NA
      , TRUE ~ disp
      )
  , X1 = rnorm(n = dplyr::n(), mean = 10)
  , X2 = rnorm(n = dplyr::n(), mean = 20)
  , X3 = rnorm(n = dplyr::n(), mean = 30)
  , X4 = rnorm(n = dplyr::n(), mean = 40)
  , X5 = rnorm(n = dplyr::n(), mean = 50)
  , X6 = rnorm(n = dplyr::n(), mean = 60)
  , X7 = rnorm(n = dplyr::n(), mean = 70)
  , X8 = rnorm(n = dplyr::n(), mean = 80)
  , X9 = rnorm(n = dplyr::n(), mean = 90)
  )

out_e_rf <-
  e_rfsrc_classification(
    dat_rf_class           = dat_rf_class
  , rf_y_var               = "cyl"    # NULL
  , rf_x_var               = NULL # c("mpg", "disp", "hp", "drat", "wt", "qsec", "vs", "am") # NULL
  , rf_id_var              = "model"
  , sw_rfsrc_ntree         = 200
  , sw_alpha_sel           = 0.05
  , sw_select_full         = c("select", "full")[1]
  , sw_na_action           = c("na.omit", "na.impute")[1]
  , sw_save_model          = c(TRUE, FALSE)[1]
  , plot_title             = "Random Forest Title SEL"
  , out_path               = "./out_sel"
  , file_prefix            = "out_e_rf"
  , var_subgroup_analysis  = NULL
  , plot_format            = c("png", "pdf")[1]
  , n_marginal_plot_across = 4
  , sw_imbalanced_binary   = c(FALSE, TRUE)[1]
  , sw_threshold_to_use    = c(FALSE, TRUE)[1]
  , sw_quick_full_only     = c(FALSE, TRUE)[1]
  , sw_reduce_output       = c(TRUE, FALSE)[1]
  , n_single_decision_tree_plots = 0
  , k_partial_coplot_var   = 3
  , n_boot_resamples        = 100
  )


## Overall summaries

# Summary of Full and reduced model fits with ROC curves
out_e_rf[[ "plot_rf_train_all_summary" ]]

# Selected model: ROC object from e_plot_roc()
out_e_rf[[ "plot_o_class_sel_ROC" ]]$plot_roc |>
  cowplot::plot_grid(plotlist = _, nrow = 1)

# Selected model: Marginal/Partial effects plots
  # for each level of the response variable
for (i_level in seq_along(levels(dat_rf_class[[ out_e_rf[[ "rf_y_var" ]] ]]))) {
  out_e_rf[[ "plot_o_class_sel_marginal_effects" ]][[ i_level ]] |>
    cowplot::plot_grid() |>
    print()
}


## Full model summaries
# y response variable
out_e_rf[[ "rf_y_var" ]]
# Full model: x predictor variables
out_e_rf[[ "rf_x_var_full" ]]
# Full model: formula
out_e_rf[[ "rf_formula_full" ]]
# Full model: rfsrc classification object
out_e_rf[[ "o_class_full" ]]
# Full model: convergence
out_e_rf[[ "plot_o_class_full" ]] |>
  cowplot::plot_grid()
# Full model: variable importance (VIMP) table
out_e_rf[[ "o_class_full_importance" ]]
# Full model: variable importance (VIMP) plot
out_e_rf[[ "plot_o_class_full_importance" ]]
# Full model: ROC AUC (area under the curve)
out_e_rf[[ "o_class_full_AUC" ]]
# Full model: subsample iterates for VIMP and model selection
out_e_rf[[ "o_class_full_subsample" ]]
# Full model: subsample VIMP model selection
out_e_rf[[ "o_class_full_subsample_extract_subsample" ]]
# Full model: subsample double bootstrap VIMP model selection
out_e_rf[[ "o_class_full_subsample_extract_bootsample" ]]
# Full model: variable importance (VIMP) plot boxplots
out_e_rf[[ "plot_o_class_full_subsample" ]] |>
  cowplot::plot_grid()
# Full model: subsample double bootstrap VIMP CIs
out[[ "plot_o_class_full_vimp_CI" ]]

## Selected model summaries
# Selected model: x predictor variables
out_e_rf[[ "rf_x_var_sel" ]]
# Selected model: formula
out_e_rf[[ "rf_formula_sel" ]]
# Selected model: rfsrc classification object
out_e_rf[[ "o_class_sel" ]]
# Selected model: convergence
out_e_rf[[ "plot_o_class_sel" ]] |>
  cowplot::plot_grid()
# Selected model: variable importance (VIMP) table
out_e_rf[[ "o_class_sel_importance" ]]
# Selected model: variable importance (VIMP) plot
out_e_rf[[ "plot_o_class_sel_importance" ]]
# Selected model: ROC AUC (area under the curve)
out_e_rf[[ "o_class_sel_AUC" ]]
# Selected model: subsample iterates for VIMP and model selection
out_e_rf[[ "o_class_sel_subsample" ]]
# Selected model: subsample VIMP model selection
out_e_rf[[ "o_class_sel_subsample_extract_subsample" ]]
# Selected model: subsample double bootstrap VIMP model selection
out_e_rf[[ "o_class_sel_subsample_extract_bootsample" ]]
# Selected model: variable importance (VIMP) plot boxplots
out_e_rf[[ "plot_o_class_sel_subsample" ]] |>
  cowplot::plot_grid()
# Selected model: subsample double bootstrap VIMP CIs
out[[ "plot_o_class_sel_vimp_CI" ]]




## With missing values and imputation
# create missing-at-random values
dat_rf_class_miss <- dat_rf_class
prop_missing <- 0.10
n_missing <-
  sample.int(
    n    = prod(dim(dat_rf_class_miss))
  , size = round( prop_missing * prod(dim(dat_rf_class_miss)))
  )
ind_missing <- expand.grid(1:dim(dat_rf_class_miss)[1], 1:dim(dat_rf_class_miss)[2])[n_missing, ]
for (i_row in seq_along(n_missing)) {
  dat_rf_class_miss[ind_missing[i_row, 1], ind_missing[i_row, 2] ] <- NA
}
dat_rf_class[[ "model" ]] = dat_rf_class[[ "model" ]]

out_e_rf <-
  e_rfsrc_classification(
    dat_rf_class           = dat_rf_class_miss
  , rf_y_var               = "cyl"    # NULL
  , rf_x_var               = NULL # c("mpg", "disp", "hp", "drat", "wt", "qsec", "vs", "am") # NULL
  , rf_id_var              = "model"
  , sw_rfsrc_ntree         = 200
  , sw_alpha_sel           = 0.05
  , sw_select_full         = c("select", "full")[1]
  , sw_na_action            = c("na.omit", "na.impute")[2]
  , sw_save_model          = c(TRUE, FALSE)[1]
  , plot_title             = "Random Forest, imputing missing values"
  , out_path               = "./out_sel_miss"
  , file_prefix            = "out_e_rf_miss"
  , var_subgroup_analysis  = NULL
  , plot_format            = c("png", "pdf")[1]
  , n_marginal_plot_across = 4
  , sw_imbalanced_binary   = c(FALSE, TRUE)[1]
  , sw_threshold_to_use    = c(FALSE, TRUE)[1]
  , sw_quick_full_only     = c(FALSE, TRUE)[1]
  , sw_reduce_output        = c(TRUE, FALSE)[1]
  , n_single_decision_tree_plots = 0
  , k_partial_coplot_var   = 0
  , n_boot_resamples        = 100
  )


## Imbalanced binary classification

# Two mvnorm distributions with overlap
imbal_n     <- c(50, 1000)
imbal_mean  <- c(0, 0.75)
imbal_sigma <- c(1, 1)
imbal_dim   <- 10
dat_imbal <-
  dplyr::bind_rows(
    mvtnorm::rmvnorm(
      n     = imbal_n[1]
    , mean  = imbal_mean[1] |> rep(imbal_dim)
    , sigma = imbal_sigma[1] * diag(imbal_dim)
    ) |>
    tibble::as_tibble(.name_repair = "universal") |>
    dplyr::rename_with(~ stringr::str_c("V", 1:imbal_dim)) |>
    dplyr::mutate(Group = "A")
  ,
    mvtnorm::rmvnorm(
      n     = imbal_n[2]
    , mean  = imbal_mean[2] |> rep(imbal_dim)
    , sigma = imbal_sigma[2] * diag(imbal_dim)
    ) |>
    tibble::as_tibble(.name_repair = "universal") |>
    dplyr::rename_with(~ stringr::str_c("V", 1:imbal_dim)) |>
    dplyr::mutate(Group = "B")
  ) |>
  dplyr::relocate(
    Group
  ) |>
  dplyr::mutate(
    Group = Group |> factor()
  )

out_e_rf <-
  e_rfsrc_classification(
    dat_rf_class           = dat_imbal
  , rf_y_var               = NULL
  , rf_x_var               = NULL
  , rf_id_var              = NULL
  , sw_rfsrc_ntree         = 2000
  , sw_alpha_sel           = 0.25
  , sw_select_full         = c("select", "full")[1]
  , sw_na_action            = c("na.omit", "na.impute")[1]
  , sw_save_model          = c(TRUE, FALSE)[1]
  , plot_title             = "Random Forest Imbalanced"
  , out_path               = "./out_imbal"
  , file_prefix            = "out_e_rf"
  , var_subgroup_analysis  = NULL
  , plot_format            = c("png", "pdf")[1]
  , n_marginal_plot_across = 4
  , sw_imbalanced_binary   = c(FALSE, TRUE)[2]
  , sw_threshold_to_use    = c(FALSE, TRUE)[1]
  , sw_quick_full_only     = c(FALSE, TRUE)[1]
  , sw_reduce_output        = c(TRUE, FALSE)[1]
  , n_single_decision_tree_plots = 0
  , k_partial_coplot_var   = 0
  , n_boot_resamples        = 100
  )

}
}
