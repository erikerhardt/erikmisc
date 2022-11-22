## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(erikmisc)

## -----------------------------------------------------------------------------
# with data for key matching imputation
dat_data <-
  tibble::tribble(
    ~a, ~b, ~c, ~x, ~d1, ~d2
  ,  1, 11, NA, NA,   1,  20
  ,  1, NA, 10, NA,   2,  21
  ,  2, NA, NA, NA,   3,  22
  , NA, 22, 20, NA,   4,  23
  ,  2, NA, 20, NA,   5,  24
  ,  3, 33, NA, NA,   6,  25
  ,  4, NA, 40, NA,   7,  26
  ,  5, NA, NA, NA,   8,  27
  ,  6, NA, 60, NA,   9,  28
  ,  6, NA, 60, NA,  10,  29
  , NA, 77, NA, NA,  11,  30
  , NA, 88, 80, NA,  12,  31
  , NA, 88, NA, NA,  13,  32
  , NA, NA, NA, NA,  14,  33
  )

# pre-determined best key variables completion
dat_keys_pre <-
  tibble::tribble(
    ~a, ~b, ~c, ~x
  ,  1, 11, 10, NA
  ,  2, 22, 20, NA
  ,  3, 33, NA, NA
  ,  4, NA, 40, NA
  ,  5, NA, NA, NA
  ,  6, NA, 60, NA
  , NA, 77, NA, NA
  , NA, 88, 80, NA
  , NA, NA, NA, NA
  )

## -----------------------------------------------------------------------------
dat_data %>% print(n=Inf)

## -----------------------------------------------------------------------------
dat_keys <-
  dat_data %>%
  dplyr::select(
    a, b, c, x
  ) %>%
  e_coalesce_column_set()
dat_keys %>% print(n=Inf)

## -----------------------------------------------------------------------------
dat_keys_pre %>% print(n=Inf)

## -----------------------------------------------------------------------------
dat_keys      <- dat_keys     %>% dplyr::arrange(a, b, c, x)
dat_keys_pre  <- dat_keys_pre %>% dplyr::arrange(a, b, c, x)

dplyr::all_equal(dat_keys_pre, dat_keys)

## -----------------------------------------------------------------------------
dat_data_updated <-
  e_complete_multiple_keys(
    dat_data
  , dat_keys = NULL
  , col_keys = c("a", "b", "c", "x")
  )

dat_data_updated %>% print(n=Inf)

