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
#'
#' # Generate missing values
#' dat_mtcars_miss_e <- dat_mtcars_e
#' prop_missing <- 0.10
#' n_missing <-
#'   sample.int(
#'     n    = prod(dim(dat_mtcars_miss_e))
#'   , size = round( prop_missing * prod(dim(dat_mtcars_miss_e)))
#'   )
#' ind_missing <- expand.grid(1:dim(dat_mtcars_miss_e)[1], 1:dim(dat_mtcars_miss_e)[2])[n_missing, ]
#' for (i_row in seq_along(n_missing)) {
#'   dat_mtcars_miss_e[ind_missing[i_row, 1], ind_missing[i_row, 2] ] <- NA
#' }
#' dat_mtcars_miss_e |> print()
"dat_mtcars_e"


## dat_ergoStool_e

#' nlme::ergoStool, labeled
#'
#' Using \code{nlme::ergoStool}, then applying labeling variables with \code{labelled::var_label} based on \code{?nlme::ergoStool}
#'
#' @docType data
#' @usage data(dat_ergoStool_e)
#' @keywords datasets
#' @source \code{?nlme::ergoStool}
#' @format tibble
#' @examples
#' data(dat_ergoStool_e)
#' str(dat_ergoStool_e)
"dat_ergoStool_e"


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
