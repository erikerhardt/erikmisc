## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(erikmisc)

## -----------------------------------------------------------------------------
if (.Platform$OS.type == "windows") {
  system(
      command = "tree ..\\data-raw\\dat_subdir /f"
    , intern  = TRUE
    )
}

## -----------------------------------------------------------------------------
e_read_data_subdir_into_lists(
    fn_path   = "../data-raw/dat_subdir"
  , fn_detect = NULL
  , sw_fn_or_dat  = c("fn", "dat")[1]
  , sw_exclude_empty_dir = c(TRUE, FALSE)[2]
  )

## -----------------------------------------------------------------------------
# selected file names
e_read_data_subdir_into_lists(
    fn_path   = "../data-raw/dat_subdir"
  , fn_detect = c("csv$", "xls$", "xlsx$")
  , sw_fn_or_dat  = c("fn", "dat")[1]
  , sw_exclude_empty_dir = c(TRUE, FALSE)[1]
  )

## -----------------------------------------------------------------------------
# selected data
dat_sheet <-
  e_read_data_subdir_into_lists(
    fn_path                 = "../data-raw/dat_subdir"
  , fn_detect               = c("csv$", "xls$", "xlsx$")
  , sw_fn_or_dat            = c("fn", "dat")[2]
  , sw_exclude_empty_dir    = c(TRUE, FALSE)[1]
  , sw_dat_add_col_path_fn  = c(TRUE, FALSE)[1]
  , sw_dat_print_fn_read    = c(TRUE, FALSE)[1]
  )

dat_sheet %>% print()

## -----------------------------------------------------------------------------
dplyr::bind_rows(dat_sheet$dir_b)

