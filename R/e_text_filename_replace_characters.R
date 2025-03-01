#' Replace special characters in a filename
#'
#' @param fn          list of filenames
#' @param ch_replace  replacement character for special characters
#' @param ch_pattern  special characters (regex) to replace
#'
#' @return fn_new     list of filenames with special characters replaced by \code{ch_replace}
#' @importFrom stringr str_replace_all
#' @export
#'
#' @examples
#'
#' fn <- c("dat.txt", "dat[]<>()|:;'&#?* .txt")
#' fn |> e_text_filename_replace_characters()
#'
e_text_filename_replace_characters <-
  function(
    fn
  , ch_replace = "_"
  , ch_pattern = "[\\[\\]<>()|:;'&#?*\\ ]"
  ) {
  fn_new <-
    stringr::str_replace_all(
      string      = fn
    , pattern     = ch_pattern
    , replacement = ch_replace
    )
  return(fn_new)
} # e_text_filename_replace_characters
