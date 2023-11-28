## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
knitr::opts_chunk$set(fig.width = 6, fig.height = 6)

library(erikmisc)

## -----------------------------------------------------------------------------
data(dat_mtcars_e)
dat_mtcars_e <-
  dat_mtcars_e |>
  dplyr::mutate(
    mpg_log2 = mpg |> log2()
  ) |>
  dplyr::slice(
    -c(29, 31)
  ) |>
  dplyr::select(
    model, mpg, mpg_log2
  , tidyselect::everything()
  )

# create some missing values in variable we won't use in the regression model
var_missing <- c("qsec", "carb", "am")
prop_missing = 0.25
n_missing = sample.int(n = nrow(dat_mtcars_e) * length(var_missing), size = round( prop_missing * nrow(dat_mtcars_e) * length(var_missing)))
ind_missing = expand.grid(1:dim(dat_mtcars_e)[1], 1:length(var_missing))[n_missing, ]
for (i_row in seq_len(nrow(ind_missing))) {
  dat_mtcars_e[ind_missing[i_row,1], var_missing[ind_missing[i_row,2]] ] <- NA
}

str(dat_mtcars_e)
#summary(dat_mtcars_e)

## -----------------------------------------------------------------------------
e_plot_missing(
    dat_plot       = dat_mtcars_e
  , var_group      = "cyl"
  , sw_group_sort  = TRUE
  , var2_sort      = "disp"
  )

## -----------------------------------------------------------------------------
 # Set model with all two-way interactions
#form_model <-
#  mpg ~ (cyl + disp + hp + wt + vs + am + gear)^2
#form_model <-
#  mpg ~ (cyl + disp + hp + wt + am)^2
form_model <-
  mpg_log2 ~ (cyl + disp + hp + wt)^2

# fit model
lm_fit <-
  lm(
    formula = form_model
  , data    = dat_mtcars_e
  )

## -----------------------------------------------------------------------------
car::Anova(lm_fit, type = 3)

## -----------------------------------------------------------------------------
summary(lm_fit)

## ---- fig.width = 8, fig.height = 3-------------------------------------------
e_plot_lm_diagostics(
    fit = lm_fit
  , sw_plot_set = "simple"
  )

## -----------------------------------------------------------------------------
tab_lm_best <-
  e_lm_bestsubset(
    form  = form_model
  , dat   = dat_mtcars_e
  , nbest = 4
  )

  ### consider these options if you temporarily need a wider output in your Rmd output
  op <- options(); # saving old options
  options(width = 200) # setting command window output text width wider
tab_lm_best |> print(n = Inf, width = Inf)
  options(op); # reset (all) initial options

## -----------------------------------------------------------------------------
tab_lm_best[9, ] |> print(width = Inf)

form_model_red <-
  mpg_log2 ~ cyl + hp + wt + cyl:wt + hp:wt

# fit reduced model
lm_fit_red <-
  lm(
    formula = form_model_red
  , data    = dat_mtcars_e
  )
car::Anova(lm_fit_red, type = 3)
summary(lm_fit_red)

## ---- fig.width = 8, fig.height = 3-------------------------------------------
e_plot_lm_diagostics(
    fit = lm_fit_red
  , sw_plot_set = "simpleAV"
  )

## ---- fig.width = 10, fig.height = 8------------------------------------------
lm_fit_contrasts <-
  e_plot_model_contrasts(
    fit                 = lm_fit_red
  , dat_cont            = dat_mtcars_e
  , adjust_method       = c("none", "tukey", "scheffe", "sidak", "bonferroni", "dunnettx", "mvt")[2]
  , sw_print            = FALSE
  , sw_TWI_plots_keep   = c("singles", "both", "all")[2]
  )

## -----------------------------------------------------------------------------
lm_fit_contrasts |> names()

## -----------------------------------------------------------------------------
# to print tables
lm_fit_contrasts$tables

## -----------------------------------------------------------------------------
# to print caption text
#lm_fit_contrasts$text

## ---- fig.width = 8, fig.height = 4-------------------------------------------
# These are the two plots
lm_fit_contrasts$plots$`cyl:wt`$both
lm_fit_contrasts$plots$`hp:wt`$both

