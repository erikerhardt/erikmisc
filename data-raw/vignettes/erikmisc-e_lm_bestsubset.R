## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(erikmisc)

## -----------------------------------------------------------------------------
tab_best <-
  e_lm_bestsubset(
    form  = formula(mpg ~ cyl + disp + hp + gear)
  , dat   = datasets::mtcars
  , nbest = 3
  )

  ### consider these options if you temporarily need a wider output in your Rmd output
  # op <- options(); # saving old options
  # options(width=100) # setting command window output text width wider
tab_best |> print(n = Inf, width = Inf)
  # options(op); # reset (all) initial options

## -----------------------------------------------------------------------------
# print into RStudio viewer or a pretty table in Rmd output
# include "always_allow_html: yes" in Rmd yaml header
tab_best |> e_table_print(sw_kable_format  = c("simple", "kbl", "html", "latex", "doc")[1])

