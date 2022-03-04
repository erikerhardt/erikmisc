#' Read a list of csv, xls, or xlsx data into a list of tibbles
#'
#' Called from \code{e_read_data_subdir_into_lists()}.
#'
#' @param read_fn_path           Directory path
#' @param read_fn_names          Character list of file names
#' @param sw_dat_add_col_path_fn T/F for data, add two columns specifying the directory (\code{DIR__}) and filename (\code{FILE__})
#' @param sw_dat_print_fn_read   T/F print file names and dimensions as the files are read
#' @param excel_sheets           "all" for all sheets, or a list of numbers "\code{c(1, 2)}"; applies to all excel sheets.
#' @param sw_clean_names         T/F to clean column names using \code{janitor::clean_names}
#'
#' @return dat_sheet             A list of tibbles
#' @import dplyr
#' @importFrom stringr str_sub
#' @importFrom readr read_csv
#' @importFrom readxl read_xlsx
#' @importFrom readxl excel_sheets
#' @importFrom dplyr mutate
#' @importFrom janitor clean_names
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
  , excel_sheets            = "all"
  , sw_clean_names          = c(TRUE, FALSE)[2]
  ) {

  ## read_fn_path <- "D:/Dropbox/StatAcumen/consult/Rpackages/erikmisc/data-raw/dat_subdir/dir_a/dir_aa"
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
      warning(paste0("erikmisc::e_read_data_files() SKIPPING non-csv, -xls, or -xlsx: ", fn_full_this))
      next
    }

    # csv file
    if (fn_ext == "csv") {

      ind_sheets <- 1 # to match Excel file

      dat_sheet[[ read_fn_names[i_fn] ]] <-
        readr::read_csv(
          file = fn_full_this
        , show_col_types = FALSE
        )

      if (sw_clean_names) {
        dat_sheet[[ read_fn_names[i_fn] ]] <-
          dat_sheet[[ read_fn_names[i_fn] ]] %>%
          janitor::clean_names()
      }

      # remove column attributes
      attr(dat_sheet[[ read_fn_names[i_fn] ]], 'spec') <- NULL

    }

    # xls file
    if (fn_ext %in% c("xls", "xlsx")) {

      ## worksheet names
      n_sheets <-
        readxl::excel_sheets(fn_full_this)

      if (excel_sheets == "all") {
        ind_sheets <- na.omit(seq_along(n_sheets))
      } else {
        ind_sheets <- na.omit(seq_along(n_sheets)[excel_sheets])
      }

      if (length(ind_sheets) == 0) {
        warning(paste0("erikmisc::e_read_data_files() no matching sheets in Excel file"))
        dat_sheet[[ read_fn_names[i_fn] ]] <- list()
      } # = 0

      if (length(ind_sheets) == 1) {
        for (i_sheet in ind_sheets) {
          dat_sheet[[ read_fn_names[i_fn] ]] <-
            readxl::read_xlsx(
              path  = fn_full_this
            , sheet = i_sheet
            )

          if (sw_clean_names) {
            dat_sheet[[ read_fn_names[i_fn] ]] <-
              dat_sheet[[ read_fn_names[i_fn] ]] %>%
              janitor::clean_names()
          }

        }
      } # = 1

      if (length(ind_sheets) > 1) {
        dat_sheet[[ read_fn_names[i_fn] ]] <- list()

        if(length(ind_sheets) > 0) {
          for (i_sheet in ind_sheets) {
            dat_sheet[[ read_fn_names[i_fn] ]][[ n_sheets[i_sheet] ]] <-
              readxl::read_xlsx(
                path  = fn_full_this
              , sheet = i_sheet
              )

            if (sw_clean_names) {
              dat_sheet[[ read_fn_names[i_fn] ]][[ n_sheets[i_sheet] ]] <-
                dat_sheet[[ read_fn_names[i_fn] ]][[ n_sheets[i_sheet] ]] %>%
                janitor::clean_names()
            }

          }
        }

      } # > 1

    }

    # print file name and dim
    if (sw_dat_print_fn_read) {

      if (length(ind_sheets) == 0) {
      } # = 0

      if (length(ind_sheets) == 1) {
        print(paste0(fn_full_this))
        dat_sheet[[ read_fn_names[i_fn] ]] %>% dim() %>% print()
      } # = 1

      if (length(ind_sheets) > 1) {
        print(paste0(fn_full_this))
        for (i_sheet in ind_sheets) {
          dat_sheet[[ read_fn_names[i_fn] ]][[ n_sheets[i_sheet] ]] %>% dim() %>% print()
        }
      } # > 1

      #warnings() %>% print()
    }

    # add dir and filename columns
    if (sw_dat_add_col_path_fn) {
      if (length(ind_sheets) == 0) {
      } # = 0

      if (length(ind_sheets) == 1) {
        # process each data file
        dat_sheet[[ read_fn_names[i_fn] ]] <-
          dat_sheet[[ read_fn_names[i_fn] ]] %>%
            dplyr::mutate(
              DIR__   = read_fn_path
            , FILE__  = read_fn_names[i_fn]
            )
      } # = 1

      if (length(ind_sheets) > 1) {
        for (i_sheet in ind_sheets) {
          # process each data file
          dat_sheet[[ read_fn_names[i_fn] ]][[ n_sheets[i_sheet] ]] <-
            dat_sheet[[ read_fn_names[i_fn] ]][[ n_sheets[i_sheet] ]] %>%
              dplyr::mutate(
                DIR__   = read_fn_path
              , FILE__  = read_fn_names[i_fn]
              , SHEET__ = n_sheets[i_sheet]
              )
        }
      } # > 1

    }

  } # i_fn

  return(dat_sheet)

} # e_read_data_files
