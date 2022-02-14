#' Read data from subdirectories into a structured list
#'
#' Recurse through subdirectories returning either filenames or tibbles (data.frames) by reading csv, xls, or xlsx with \code{e_read_data_files()}.
#'
#' @param fn_path                Starting directory path
#' @param fn_detect              NULL for all.  File specification, used by \code{stringr::str_detect()}, usually specifying file extensions.
#' @param sw_fn_or_dat           Return filenames or tibbles (data.frames)
#' @param sw_exclude_empty_dir   T/F exclude empty directories
#' @param sw_dat_add_col_path_fn T/F for data, add two columns specifying the directory (\code{DIR__}) and filename (\code{FILE__})
#' @param sw_dat_print_fn_read   T/F print file names and dimensions as the files are read
#'
#' @return fn_names              Either a structured list of filenames or of tibbles
#' @import dplyr
#' @importFrom stringr str_detect
#' @export
#'
#' @examples
#' \dontrun{
#' # # all file names
#' # e_read_data_subdir_into_lists(
#' #     fn_path   = "./data-raw/dat_subdir"
#' #   , fn_detect = NULL
#' #   , sw_fn_or_dat  = c("fn", "dat")[1]
#' #   , sw_exclude_empty_dir = c(TRUE, FALSE)[1]
#' #   )
#' # # selected file names
#' # e_read_data_subdir_into_lists(
#' #     fn_path   = "./data-raw/dat_subdir"
#' #   , fn_detect = c("csv$", "xls$", "xlsx$")
#' #   , sw_fn_or_dat  = c("fn", "dat")[1]
#' #   , sw_exclude_empty_dir = c(TRUE, FALSE)[1]
#' #   )
#' # # selected data
#' # e_read_data_subdir_into_lists(
#' #     fn_path                 = "./data-raw/dat_subdir"
#' #   , fn_detect               = c("csv$", "xls$", "xlsx$")
#' #   , sw_fn_or_dat            = c("fn", "dat")[2]
#' #   , sw_exclude_empty_dir    = c(TRUE, FALSE)[1]
#' #   , sw_dat_add_col_path_fn  = c(TRUE, FALSE)[1]
#' #   , sw_dat_print_fn_read    = c(TRUE, FALSE)[1]
#' #   )
#' }
e_read_data_subdir_into_lists <-
  function(
    fn_path                = "."
  , fn_detect              = c("csv$", "xls$", "xlsx$")
  , sw_fn_or_dat           = c("fn", "dat")[1]
  , sw_exclude_empty_dir   = c(TRUE, FALSE)[1]
  , sw_dat_add_col_path_fn = c(TRUE, FALSE)[1]
  , sw_dat_print_fn_read   = c(TRUE, FALSE)[2]
  ) {

  ## fn_path   = "D:/Dropbox/StatAcumen/consult/Rpackages/erikmisc/data-raw/dat_subdir"  #/dir_a/dir_aa/dir_aaa"
  ## fn_detect = NULL #c("csv$", "xls$", "xlsx$")
  ## sw_fn_or_dat  = c("fn", "dat")[1]
  ## sw_exclude_empty_dir = c(TRUE, FALSE)[1]


  # All files and directories
  fn_names <-
    list.files(
      path    = fn_path
    , no..    = TRUE
    )

  # Only directory names
  dir_names <-
    list.dirs(
      path       = fn_path
    , full.names = FALSE
    , recursive  = FALSE
    )

  # If there are directories
  if(length(dir_names)) {

    # recursion
    fn_subdir <-
      lapply(
        file.path(fn_path, dir_names)
      , e_read_data_subdir_into_lists
          # include function arguments for recursion, otherwise takes function defaults
      , fn_detect              = fn_detect
      , sw_fn_or_dat           = sw_fn_or_dat
      , sw_exclude_empty_dir   = sw_exclude_empty_dir
      , sw_dat_add_col_path_fn = sw_dat_add_col_path_fn
      , sw_dat_print_fn_read   = sw_dat_print_fn_read
      )
    # Set names for the new list
    names(fn_subdir) <-
      dir_names

    if (sw_exclude_empty_dir) {
      ind_empty_dir <-
        which(
          unlist(
            lapply(
              fn_subdir
            , is.null
            )
          )
        )

      if(length(ind_empty_dir)) {
        for(i_dir in ind_empty_dir) {
          fn_subdir[i_dir] <- NULL
        }
      }
    }


    # Determine files found, excluding directory names
    fn_to_return <-
      fn_names[!fn_names %in% dir_names]

    # keep those matching fn_detect specification
    if (!is.null(fn_detect)){
      ind_dat <-
        stringr::str_detect(
          string  = fn_to_return
        , pattern = paste0(fn_detect, collapse = "|")
        )
      fn_to_return <-
        fn_to_return[ind_dat]
    }


    if (sw_fn_or_dat == "fn") {
      # Combine appropriate results for current list
      if(length(fn_to_return)) {
        fn_names <-
          c(
            # list() makes this a list of fn's instead of separate lists for each fn
            list(fn_to_return)
          , fn_subdir
          )
      } else {
        fn_names <-
          fn_subdir
      }
    }

    # read data
    if (sw_fn_or_dat == "dat") {
      if(length(fn_to_return)) {
        dat_to_return <-
          e_read_data_files(
            read_fn_path            = fn_path
          , read_fn_names           = fn_to_return
          , sw_dat_add_col_path_fn  = sw_dat_add_col_path_fn
          , sw_dat_print_fn_read    = sw_dat_print_fn_read
          )

        fn_names <-
          c(
            dat_to_return
          , fn_subdir
          )

      } else {
        fn_names <-
          fn_subdir
      }
    }

  } else {
    if (sw_fn_or_dat == "dat") {
      if(length(fn_names)) {
        dat_to_return <-
          e_read_data_files(
            read_fn_path            = fn_path
          , read_fn_names           = fn_names
          , sw_dat_add_col_path_fn  = sw_dat_add_col_path_fn
          , sw_dat_print_fn_read    = sw_dat_print_fn_read
          )

        fn_names <-
          c(
            dat_to_return
          )
      }
    }
  } # if length(dir_names)

  if(length(fn_names)) {
    return(fn_names)
  } else {
    return(NULL)
  }

} # e_read_data_subdir_into_lists

