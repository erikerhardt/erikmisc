## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(erikmisc)
library(dplyr)

## -----------------------------------------------------------------------------
# Two variables always in the model with a second-order model of two others
e_model_all_subsets_formula(
    var_formula = NULL
  , x_var_names = list(c("a", "b"), c("c", "d"))
  , y_var_name  = "z"
  , max_scope   = list("always", "SO")
  , sw_return   = c("formula", "table")[2]
  )

## -----------------------------------------------------------------------------
dat_mtcars_e %>% head(3)

# table of the candidate models
form_table <-
  e_model_all_subsets_formula(
      var_formula = NULL
    , x_var_names = list(c("cyl", "carb"), c("disp", "hp"), c("wt", "vs", "am", "gear"))
    , y_var_name  = "mpg"
    , max_scope   = list("always", "TWI", "FO")
    , sw_return   = c("formula", "table")[2]
    )
print(form_table)

# generate candidate model formulas
form_list <-
  e_model_all_subsets_formula(
      var_formula = NULL
    , x_var_names = list(c("cyl", "carb"), c("disp", "hp"), c("wt", "vs", "am", "gear"))
    , y_var_name  = "mpg"
    , max_scope   = list("always", "TWI", "FO")
    , sw_return   = c("formula", "table")[1]
    )

# fit models and calculate model selection criteria
lm_fit_list <- list()
lm_fit_list_criteria <- list()

for (i_form in seq_len(length(form_list))) {
  # model fit
  lm_fit_list[[ i_form ]] <-
    stats::lm(
      formula = form_list[[ i_form ]]
    , data    = dat_mtcars_e
    )

  # model selection criteria
  lm_fit_list_criteria[[ i_form ]] <-
    e_lm_model_criteria(
      lm_fit    = lm_fit_list[[ i_form ]]
    , dat_fit   = dat_mtcars_e
    , model_id  = i_form
    )
}

# sort the models by a criterion
lm_fit_criteria <-
  lm_fit_list_criteria %>%
  dplyr::bind_rows() %>%
  dplyr::arrange(
    # arrange models by this model criterion
    bic               # ascending by bic (best on top)
    # dplyr::desc(r2)   # descending by r2 (best on top)
  )

# print the head and tail of fit criteria table
lm_fit_criteria %>% head()
lm_fit_criteria %>% tail(3)

# best model ID
lm_fit_criteria$model_id[1]

# summarize "best" model
lm_fit_list[[ lm_fit_criteria$model_id[1] ]] %>%
  car::Anova(type = 3)
lm_fit_list[[ lm_fit_criteria$model_id[1] ]] %>%
  summary()

## -----------------------------------------------------------------------------
set.seed(76543)

dat_ergoStool_e <-
  dat_ergoStool_e %>%
  # add a few noise variables just to use for model selection example
  dplyr::mutate(
    x1 = stats::rnorm(n = dplyr::n())
  , x2 = stats::rnorm(n = dplyr::n())
  , x3 = stats::rnorm(n = dplyr::n())
  )
dat_ergoStool_e %>% head(8)

# table of the candidate models
form_table <-
  e_model_all_subsets_formula(
      var_formula = NULL
    , x_var_names = list(c("Type", "(1 | Subject)"), c("x1", "x2", "x3"))
    , y_var_name  = "Effort"
    , max_scope   = list("always", "SO")
    , sw_return   = c("formula", "table")[2]
    )
print(form_table)

# generate candidate model formulas
form_list <-
  e_model_all_subsets_formula(
      var_formula = NULL
    , x_var_names = list(c("Type", "(1 | Subject)"), c("x1", "x2", "x3"))
    , y_var_name  = "Effort"
    , max_scope   = list("always", "SO")
    , sw_return   = c("formula", "table")[1]
    )

# fit models and calculate model selection criteria
lmer_fit_list <- list()
lmer_fit_list_criteria <- list()

for (i_form in seq_len(length(form_list))) {
  # model fit
  lmer_fit_list[[ i_form ]] <-
    lme4::lmer(
      formula = form_list[[ i_form ]]
    , REML    = TRUE
    , data    = dat_ergoStool_e
    )

  # model selection criteria
  lmer_fit_list_criteria[[ i_form ]] <-
    e_lm_model_criteria(
      lm_fit    = lmer_fit_list[[ i_form ]]
    , dat_fit   = dat_ergoStool_e
    , model_id  = i_form
    )
}

# sort the models by a criterion
lmer_fit_criteria <-
  lmer_fit_list_criteria %>%
  dplyr::bind_rows() %>%
  dplyr::arrange(
    # arrange models by this model criterion
    caic              # ascending by caic (best on top)
    # dplyr::desc(r2)   # descending by r2 (best on top)
  )

# print the head and tail of fit criteria table
lmer_fit_criteria %>% head()
lmer_fit_criteria %>% tail(3)

# best model ID
lmer_fit_criteria$model_id[1]

# summarize "best" model
lmer_fit_list[[ lmer_fit_criteria$model_id[1] ]] %>%
  car::Anova(type = 3)
lmer_fit_list[[ lmer_fit_criteria$model_id[1] ]] %>%
  summary()

## ---- eval=FALSE--------------------------------------------------------------
#  ## Set up parallel backend
#    ## From: https://www.blasbenito.com/post/02_parallelizing_loops_with_r/
#    library(doParallel)
#    # use n-1 cores
#    n_cores <- parallel::detectCores() - 1
#    # create the cluster
#    my_cluster <-
#      parallel::makeCluster(
#        spec = n_cores
#      , type = "PSOCK"
#      )
#    # check cluster definition (optional)
#    print(my_cluster)
#    # register it to be used by %dopar%
#    doParallel::registerDoParallel(cl = my_cluster)
#    # check if it is registered (optional)
#    foreach::getDoParRegistered()
#    # how many workers are available? (optional)
#    foreach::getDoParWorkers()
#  
#  
#  # table of the candidate models
#  form_table <-
#    e_model_all_subsets_formula(
#        var_formula = NULL
#      , x_var_names = list(c("Type", "(1 | Subject)"), c("x1", "x2", "x3"))
#      , y_var_name  = "Effort"
#      , max_scope   = list("always", "SO")
#      , sw_return   = c("formula", "table")[2]
#      )
#  print(form_table)
#  
#  # generate candidate model formulas
#  form_list <-
#    e_model_all_subsets_formula(
#        var_formula = NULL
#      , x_var_names = list(c("Type", "(1 | Subject)"), c("x1", "x2", "x3"))
#      , y_var_name  = "Effort"
#      , max_scope   = list("always", "SO")
#      , sw_return   = c("formula", "table")[1]
#      )
#  
#  # fit models and calculate model selection criteria
#  lmer_fit_list <-
#    foreach::foreach(
#      i_form = seq_len(length(form_list))
#    , .combine = "c"
#    ) %dopar% {
#      # model fit
#      lme4::lmer(
#        formula = form_list[[ i_form ]]
#      , REML    = TRUE
#      , data    = dat_ergoStool_e
#      )
#    }
#  
#  lmer_fit_list_criteria <-
#    foreach::foreach(
#      i_form = seq_len(length(form_list))
#    #, .combine = "c"
#    ) %dopar% {
#      # model selection criteria
#      e_lm_model_criteria(
#        lm_fit    = lmer_fit_list[[ i_form ]]
#      , dat_fit   = dat_ergoStool_e
#      , model_id  = i_form
#      )
#    }
#  
#  
#  
#  # sort the models by a criterion
#  lmer_fit_criteria <-
#    lmer_fit_list_criteria %>%
#    dplyr::bind_rows() %>%
#    dplyr::arrange(
#      # arrange models by this model criterion
#      caic              # ascending by caic (best on top)
#      # dplyr::desc(r2)   # descending by r2 (best on top)
#    )
#  
#  # print the head and tail of fit criteria table
#  lmer_fit_criteria %>% head()
#  lmer_fit_criteria %>% tail(3)
#  
#  # best model ID
#  lmer_fit_criteria$model_id[1]
#  
#  # summarize "best" model
#  lmer_fit_list[[ lmer_fit_criteria$model_id[1] ]] %>%
#    car::Anova(type = 3)
#  lmer_fit_list[[ lmer_fit_criteria$model_id[1] ]] %>%
#    summary()
#  
#  
#  ## stop cluster
#  parallel::stopCluster(cl = my_cluster)

