## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
knitr::opts_chunk$set(fig.height = 6, fig.width = 6)

library(erikmisc)

## -----------------------------------------------------------------------------
str(dat_mtcars_e)

## -----------------------------------------------------------------------------
 # Set specific model with some interactions
form_model <-
  mpg ~ cyl + disp + hp + wt + vs + am + cyl:vs + disp:hp + hp:vs

fit <-
  lm(
    formula = form_model
  , data    = dat_mtcars_e
  )

car::Anova(
    fit
  , type = 3
  , singular.ok = TRUE
  )

## -----------------------------------------------------------------------------
summary(fit)

## -----------------------------------------------------------------------------
fit_contrasts <-
  e_plot_model_contrasts(
    fit                = fit
  , dat_cont           = dat_mtcars_e
  , sw_print           = FALSE
  )

## -----------------------------------------------------------------------------
fit_contrasts |> names()

## -----------------------------------------------------------------------------
fit_contrasts$tables  # to print tables

## -----------------------------------------------------------------------------
fit_contrasts$text    # to print caption text

## -----------------------------------------------------------------------------
fit_contrasts$plots   # to print plots

## ---- fig.height = 7, fig.width = 6-------------------------------------------
fit_contrasts <-
  e_plot_model_contrasts(
    fit                = fit
  , dat_cont           = dat_mtcars_e
  , choose_contrasts   = "cyl:vs"
  , sw_print           = FALSE
  , sw_table_in_plot   = FALSE
  , sw_TWI_plots_keep  = c("singles", "both", "all")[2]
  , sw_TWI_both_orientation = c("wide", "tall")[2]
  )

fit_contrasts$plots   # to print plots
fit_contrasts$tables  # to print tables
fit_contrasts$text    # to print caption text

