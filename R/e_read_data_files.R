#' Read a list of csv, xls, or xlsx data into a list of tibbles
#'
#' Called from \code{e_read_data_subdir_into_lists()}.
#'
#' @param read_fn_path           Directory path
#' @param read_fn_names          Character list of file names
#' @param sw_dat_add_col_path_fn T/F for data, add two columns specifying the directory (\code{DIR__}) and filename (\code{FILE__})
#' @param sw_dat_print_fn_read   T/F print file names and dimensions as the files are read
#'
#' @return dat_sheet             A list of tibbles
#' @import dplyr
#' @importFrom stringr str_sub
#' @importFrom readr read_csv
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate
#' @export
#'
#' @examples
#' \dontrun{
#' # See e_read_data_subdir_into_lists()
#' }
e_read_data_files <-
  function(
    read_fn_path            = "."
  , read_fn_names           = NULL
  , sw_dat_add_col_path_fn  = c(TRUE, FALSE)[1]
  , sw_dat_print_fn_read    = c(TRUE, FALSE)[1]
  ) {

  ## read_fn_path <- "D:/Dropbox/StatAcumen/consult/Rpackages/erikmisc/data-raw/dat_subdir/dir_a/dir_aa/dir_aaa"
  ## read_fn_names <- c("dat_aa1.csv", "dat_aa2.csv")
  ## sw_dat_add_col_path_fn = c(TRUE, FALSE)[1]

  dat_sheet <- list()

  for (i_fn in seq_along(read_fn_names)) {
    ## i_fn = 1

    fn_full_this <- file.path(read_fn_path, read_fn_names[i_fn])

    fn_ext <-
      read_fn_names[i_fn] %>%
        # find the last period . and take the sub string after that
      stringr::str_sub(start = stringi::stri_locate_last_fixed(read_fn_names[i_fn], ".")[, 1] + 1)

      ### OLD
      # stringr::str_split_fixed(pattern = fixed("."), n = 2) %>%
      # tibble::as_tibble() %>%
      # dplyr::pull(
      #   V2
      # )

    # not data, skip
    if (fn_ext %notin% c("csv", "xls", "xlsx")) {
      print(paste0("SKIPPING non-csv, -xls, or -xlsx: ", fn_full_this))
      next
    }

    # csv file
    if (fn_ext == "csv") {

      dat_sheet[[ read_fn_names[i_fn] ]] <-
        readr::read_csv(
          file = fn_full_this
        , show_col_types = FALSE
        )

      # remove column attributes
      attr(dat_sheet[[ read_fn_names[i_fn] ]], 'spec') <- NULL

    }

    # xls file
    if (fn_ext %in% c("xls", "xlsx")) {
      ## worksheet names
      #n_sheets <-
      #  readxl::excel_sheets(fn_full_this)

      #for (i_sheet in seq_along(n_sheets)) {
      i_sheet = 1
      dat_sheet[[ read_fn_names[i_fn] ]] <-
        readxl::read_xlsx(
          path  = fn_full_this
        , sheet = i_sheet
        )
    }

    # print file name and dim
    if (sw_dat_print_fn_read) {
      print(paste0(fn_full_this))
      dat_sheet[[ read_fn_names[i_fn] ]] %>% dim() %>% print()
      #warnings() %>% print()
    }

    # add dir and filename columns
    if (sw_dat_add_col_path_fn) {
      # process each data file
      dat_sheet[[ read_fn_names[i_fn] ]] <-
        dat_sheet[[ read_fn_names[i_fn] ]] %>%
          dplyr::mutate(
            DIR__   = read_fn_path
          , FILE__  = read_fn_names[i_fn]
          )
          # %>%
          # dplyr::select(
          #   DIR__
          # , FILE__
          # , everything()
          # )
    }

  } # i_fn

  return(dat_sheet)

} # e_read_data_files
