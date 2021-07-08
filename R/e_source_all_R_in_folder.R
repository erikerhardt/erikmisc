#' Source all *.R files in a folder
#'
#' @param fn_R_location directory (folder) location
#' @param fn_list_name  file name pattern
#'
#' @return NULL, invisibly
#' @export
e_source_all_R_in_folder <-
  function(
    fn_R_location = "./R"
  , fn_list_name  = "*.R"
  ) {

  # list of files
  fn_R_to_source <-
    list.files(
      path    = fn_R_location
    , pattern = fn_list_name
    )

  if (length(fn_R_to_source) > 0) {
    # source the files
    sapply(
      file.path(
        fn_R_location
      , fn_R_to_source
      )
    , source
    )
  } else {
    message("  e_source_all_R_in_folder:  No files to source")
  }

  invisible(NULL)
}
