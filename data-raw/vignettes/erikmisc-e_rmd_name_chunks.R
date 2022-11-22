## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(erikmisc)

## -----------------------------------------------------------------------------
e_rmd_name_chunks(
    fn_in             = "test_in_Rmd.txt"
  , fn_out            = "test_out_Rmd.txt"
  , prefix_chunk_name = "chunk-"
  )

