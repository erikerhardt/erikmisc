# https://kbroman.org/pkg_primer/pages/data.html
# Including datasets

## dat_mtcars_e

#' datasets::mtcars, factored and labeled
#'
#' Using \code{datasets::mtcars}, then applying labeled factors and labeling variables with \code{labelled::var_label} based on \code{?datasets::mtcars}
#'
#' @docType data
#' @usage data(dat_mtcars_e)
#' @keywords datasets
#' @source \code{?datasets::mtcars}
#' @format tibble
#' @examples
#' data(dat_mtcars_e)
#' str(dat_mtcars_e)
"dat_mtcars_e"


## dat_unicode_tables_sel_byhand_e

#' Unicode characters from most of the tables from Wikipedia's "List of Unicode characters"
#'
#' The table includes Code, Glyph (symbol), Description of the character, CharacterSet group, whether it is_unicode (always TRUE), the CodeEscaped as stringi needs it, and the hand-selected ASCII replacement.
#'
#' @docType data
#' @usage data(dat_unicode_tables_sel_byhand_e)
#' @keywords datasets
#' @source \code{https://en.wikipedia.org/wiki/List_of_Unicode_characters} from 4/25/2021
#' @format tibble
#' @examples
#' data(dat_unicode_tables_sel_byhand_e)
#' str(dat_unicode_tables_sel_byhand_e)
"dat_unicode_tables_sel_byhand_e"



#' @importFrom tibble tibble
NULL
