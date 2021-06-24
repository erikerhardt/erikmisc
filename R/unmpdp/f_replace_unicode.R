#' Replace unicode in a file
#'
#' @param dat_to_fix is the file that needs unicode replaced
#' @param unicode_ascii is a data.frame with columns \code{"Unicode" "ASCII"   "comment"}.  The "Unicode" string will be replaced with "ASCII", and "comment" is ignored.
#'
#' @return file with unicode replaced
#' @import stringi
#' @export
f_replace_unicode <- function(dat_to_fix = NULL, unicode_ascii = REDCap_unicode) {

  # Read Unicode replacement file, needs to be escaped for replace
  # unicode_ascii <- read.csv(fn_unicode, skip = 2)
  #library(stringi)
  #stri_escape_unicode("ï»¿")
  unicode_ascii$UnicodeEscaped <- stringi::stri_escape_unicode(unicode_ascii$Unicode)

  # change unicode to escaped
  dat_to_fix <- stringi::stri_escape_unicode(dat_to_fix)
  for (i.row in 1:nrow(unicode_ascii)) {
    # replaced escaped unicode
    dat_to_fix <- stringi::stri_replace_all_fixed(dat_to_fix, unicode_ascii$UnicodeEscaped[i.row], unicode_ascii$ASCII[i.row])
  }

  # remove extra \
  dat_to_fix <- stringi::stri_replace_all_fixed(dat_to_fix, "\\", "")

  return(dat_to_fix)
}
