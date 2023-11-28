#' Replace Unicode with ASCII
#'
#' Read a text file or character list and replace unicode characters \code{[^\\x00-\\x7F]} with equivalent ASCII or specified replacement.
#' Review table \code{?dat_unicode_tables_sel_byhand_e} for the ASCII replacements.
#' Not all unicode has a natural ASCII replacement, so those replacements can be specified with \code{unicode_generic_replacement}.
#'
#' Either use \code{fn_in} and \code{fn_out} for files, or use \code{text_in} for a character list, but not both.
#'
#' @param fn_in                       text filename in.
#' @param fn_out                      text filename out.  If NULL, then this is assigned \code{fn_in}.
#' @param text_in                     character list in.
#' @param unicode_generic_replacement a string to replace non-matched unicode characters.
#' @param sw_print_line_text          \code{FALSE} to print only the line/index number, \code{TRUE} to print the line/index number and text on the line.
#'
#' @return NULL, invisibly
#' @import dplyr
#' @import stringr
#' @importFrom stringi stri_unescape_unicode
#' @export
#'
#' @examples
#' \dontrun{
#' # # file with lots of unicode to replace
#' # e_text_unicode_to_ascii(
#' #     fn_in    = "./data-raw/unicode/text_in_unicode.csv"
#' #   , fn_out   = "./data-raw/unicode/text_out_unicode.csv"
#' #   , text_in  = NULL
#' #   , unicode_generic_replacement = "XxXunicodeXxX"
#' #   , sw_print_line_text          = FALSE
#' #   )
#' #
#' # # list with lots of unicode to replace
#' # text_no_unicode <-
#' #  e_text_unicode_to_ascii(
#' #     fn_in    = NULL
#' #   , fn_out   = NULL
#' #   , text_in  = erikmisc::dat_unicode_tables_sel_byhand_e$Glyph
#' #   , unicode_generic_replacement = "XxXunicodeXxX"
#' #   , sw_print_line_text          = FALSE
#' #   )
#' #
#' # # no unicode in text_in list
#' # text_no_unicode <-
#' #  e_text_unicode_to_ascii(
#' #     fn_in    = NULL
#' #   , fn_out   = NULL
#' #   , text_in  = c("a", "b", "c")
#' #   , unicode_generic_replacement = "XxXunicodeXxX"
#' #   , sw_print_line_text          = FALSE
#' #   )
#' #
#' # # remove all unicode from all variables
#' # dat_all <-
#' #   dat_all |>
#' #   dplyr::mutate(
#' #     dplyr::across(
#' #       .cols = tidyselect::everything()
#' #     , .fns  = ~ e_text_unicode_to_ascii(text_in = .x)
#' #     )
#' #   )
#' }
e_text_unicode_to_ascii <-
  function(
    fn_in   = NULL
  , fn_out  = NULL
  , text_in = NULL
  , unicode_generic_replacement = "XunicodeX"
  , sw_print_line_text          = FALSE
  ) {

  if (!xor(!is.null(fn_in), !is.null(text_in))) {
    stop(paste0("erikmisc::e_text_unicode_to_ascii() Either fn_in or text_in needs to be specified, but not both."))
  }

  if (!is.null(fn_in)) {
    sw_file_text <- "file"
  }
  if (!is.null(text_in)) {
    sw_file_text <- "text"
  }

  # file
  if (sw_file_text == "file") {
    if(is.null(fn_in)) {
      stop(paste0("erikmisc::e_text_unicode_to_ascii() Input text filename not specified: ", fn_in))
    }

    if(is.null(fn_out)) {
      paste0("Output text filename not specified, assigning input filename: ", fn_in)
      fn_out <- fn_in
    }

    if(!file.exists(fn_in)) {
      stop(paste0("erikmisc::e_text_unicode_to_ascii() File does not exist: ", fn_in))
    }

    # read text_in file
    text_in <- readLines(fn_in)
  }

  # copy to text_out, the updated text will be replaced later
  text_out <- text_in

  ind_unicode <-
    text_out |>
    stringr::str_detect(pattern = "[^\\x00-\\x7F]") |>
    which()

  if (length(ind_unicode) == 0) {
    if (sw_file_text == "file") {
      message("No unicode detected, not returning output file.")
      invisible(NULL)
    }
    if (sw_file_text == "text") {
      #message("No unicode detected, returning original character list.")
      return(text_out)
    }
  } else {
    if (sw_file_text == "file") {
      message("Unicode detected on ", length(ind_unicode), " lines of the Input text file.")
    }
    if (sw_file_text == "text") {
      message("Unicode detected in ", length(ind_unicode), " elements of the Input text list.")
    }
    if(sw_print_line_text) {
      message("Line numbers and line text:")
      print(paste0("L", ind_unicode, ":  ", text_out[ind_unicode]))

    } else {
      message("Line numbers:")
      print(ind_unicode)
    }
  }

  # set unicode_generic_replacement in unicode table
  dat_unicode_tables_sel_byhand_e$ASCII[dat_unicode_tables_sel_byhand_e$ASCII == "XunicodeX"] <-
    unicode_generic_replacement


  for (i_row in ind_unicode) {
    ## i_row = ind_unicode[1]

    ind_uni_detected <-
      stringr::str_detect(
        text_out[i_row]
      , pattern = dat_unicode_tables_sel_byhand_e$CodeEscaped |> stringi::stri_unescape_unicode()
      ) |> which()

    for (i_uni in ind_uni_detected) {

      text_out[i_row] <-
        stringr::str_replace_all(
          text_out[i_row]
        , dat_unicode_tables_sel_byhand_e$CodeEscaped[i_uni] |> stringi::stri_unescape_unicode()
        , dat_unicode_tables_sel_byhand_e$ASCII[i_uni]
        )

    }
  }

  # check for remaining unicode
  ind_unicode <-
    text_out |>
    stringr::str_detect(pattern = "[^\\x00-\\x7F]") |>
    which()

  if (length(ind_unicode) == 0) {
    message("All unicode replaced.")
    invisible(NULL)
  } else {
    if (sw_file_text == "file") {
      message("Unicode remains on ", length(ind_unicode), " lines of the Output text file.  Replacing with generic \"", unicode_generic_replacement, "\" string.")
      message("Line numbers:")
    }
    if (sw_file_text == "text") {
      message("Unicode remains in ", length(ind_unicode), " elements of the Output text list.  Replacing with generic \"", unicode_generic_replacement, "\" string.")
      message("Indices:")
    }
    print(ind_unicode)
  }

  for (i_row in ind_unicode) {
    ## i_row = ind_unicode[1]

    ind_uni_detected <-
      stringr::str_detect(
        text_out[i_row]
      , pattern = "[^\\x00-\\x7F]"
      ) |> which()

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
    text_out |>
    stringr::str_detect(pattern = "[^\\x00-\\x7F]") |>
    which()

  if (length(ind_unicode) == 0) {
    message("All unicode replaced.")
    invisible(NULL)
  } else {
    if (sw_file_text == "file") {
      message("Unicode remains on ", length(ind_unicode), " lines of the Output text file.")
    }
    if (sw_file_text == "text") {
      message("Unicode remains in ", length(ind_unicode), " elements of the Output text list.")
    }
    print(ind_unicode)
  }

  if (sw_file_text == "file") {
    # write fn_out file
    fileConn <- file(fn_out)
    writeLines(text_out, fileConn)
    close(fileConn)

    invisible(NULL)
  }
  if (sw_file_text == "text") {
    return(text_out)
  }

}
