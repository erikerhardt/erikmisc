#' Replace Unicode with ASCII
#'
#' Read a text file and replace unicode characters \code{[^\\x00-\\x7F]} with equivalent ASCII or specified replacement.
#' Review table \code{?dat_unicode_tables_sel_byhand_e} for the ASCII replacements.
#' Not all unicode has a natural ASCII replacement, so those replacements can be specified with \code{unicode_generic_replacement}.
#'
#' @param fn_in                       text filename in.
#' @param fn_out                      text filename out.  If NULL, then this is assigned \code{fn_in}.
#' @param unicode_generic_replacement a string to replace non-matched unicode characters
#'
#' @return NULL, invisibly
#' @import dplyr
#' @import stringr
#' @importFrom stringi stri_unescape_unicode
#' @export
#'
#' @examples
#' \dontrun{
#' # e_text_unicode_to_ascii(
#' #     fn_in  = "./data-raw/text_in_unicode.csv"
#' #   , fn_out = "./data-raw/text_out_unicode.csv"
#' #   , unicode_generic_replacement = "XxXunicodeXxX"
#' #   )
#' }
e_text_unicode_to_ascii <-
  function(
    fn_in  = NULL
  , fn_out = NULL
  , unicode_generic_replacement = "XunicodeX"
  ) {

  if(is.null(fn_in)) {
    stop(paste0("Input text filename not specified: ", fn_in))
  }

  if(is.null(fn_out)) {
    paste0("Output text filename not specified, assigning input filename: ", fn_in)
    fn_out <- fn_in
  }

  if(!file.exists(fn_in)) {
    stop(paste0("File does not exist: ", fn_in))
  }


  # read text_in file
  text_in <- readLines(fn_in)

  # copy to text_out, the updated text will be replaced later
  text_out <- text_in

  ind_unicode <-
    text_out %>%
    stringr::str_detect(pattern = "[^\\x00-\\x7F]") %>%
    which()

  if (length(ind_unicode) == 0) {
    message("No unicode detected, not returning output file.")
    invisible(NULL)
  } else {
    message("Unicode detected on ", length(ind_unicode), " lines of the Input text file; row numbers:")
    print(ind_unicode)
  }

  # set unicode_generic_replacement in unicode table
  dat_unicode_tables_sel_byhand_e$ASCII[dat_unicode_tables_sel_byhand_e$ASCII == "XunicodeX"] <-
    unicode_generic_replacement


  for (i_row in ind_unicode) {
    ## i_row = ind_unicode[1]

    ind_uni_detected <-
      stringr::str_detect(
        text_out[i_row]
      , pattern = dat_unicode_tables_sel_byhand_e$CodeEscaped %>% stringi::stri_unescape_unicode()
      ) %>% which()

    for (i_uni in ind_uni_detected) {

      text_out[i_row] <-
        stringr::str_replace_all(
          text_out[i_row]
        , dat_unicode_tables_sel_byhand_e$CodeEscaped[i_uni] %>% stringi::stri_unescape_unicode()
        , dat_unicode_tables_sel_byhand_e$ASCII[i_uni]
        )

    }
  }

  # check for remaining unicode
  ind_unicode <-
    text_out %>%
    stringr::str_detect(pattern = "[^\\x00-\\x7F]") %>%
    which()

  if (length(ind_unicode) == 0) {
    message("All unicode replaced.")
    invisible(NULL)
  } else {
    message("Unicode remains on ", length(ind_unicode), " lines of the Output text file.  Replacing with generic \"", unicode_generic_replacement, "\" string.")
    print(ind_unicode)
  }

  for (i_row in ind_unicode) {
    ## i_row = ind_unicode[1]

    ind_uni_detected <-
      stringr::str_detect(
        text_out[i_row]
      , pattern = "[^\\x00-\\x7F]"
      ) %>% which()

    for (i_uni in ind_uni_detected) {

      text_out[i_row] <-
        stringr::str_replace_all(
          text_out[i_row]
        , "[^\\x00-\\x7F]"
        , unicode_generic_replacement
        )

    }
  }

  # check for remaining unicode
  ind_unicode <-
    text_out %>%
    stringr::str_detect(pattern = "[^\\x00-\\x7F]") %>%
    which()

  if (length(ind_unicode) == 0) {
    message("All unicode replaced.")
    invisible(NULL)
  } else {
    message("Unicode remains on ", length(ind_unicode), " lines of the Output text file.")
    print(ind_unicode)
  }

  # write fn_out file
  fileConn <- file(fn_out)
  writeLines(text_out, fileConn)
  close(fileConn)

  invisible(NULL)
}
