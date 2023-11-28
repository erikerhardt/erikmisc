#' Combine multiple header rows into a column name for a text data frame
#'
#' Some old text files have header rows for column labels that span multiple rows.
#' In this case, we want to preserve those names but combine the labels into a
#' single column name.
#'
#' * When reading data from text, keep values as "text"
#'    * \code{utils::read.table(..., , stringsAsFactors = FALSE)}
#' * When reading data from Excel, keep values as "text" and do not fix duplicate names
#'    * \code{readxl::read_xlsx(..., col_types = "text", .name_repair = "minimal" )}
#'
#' @param dat_this                 data.frame with all text columns
#' @param skip                     number of rows to skip that are not part of header rows
#' @param row_header_span          number of rows that comprise the header column names
#' @param row_header_span_collapse character to separate each row of the header
#'                                 into the single column name
#'
#' @return dat_this                data.frame with updated columns names
#' @import dplyr
#' @import stringr
#' @importFrom tidyr replace_na
#' @export
#'
#' @examples
#' # data should be text
#' dat_this <-
#'   read.csv(
#'     text = "
#' X,X,Z
#' a1,b1,c1
#' a2,b2,
#' a3,,
#' 1,2,3
#' "
#'   , stringsAsFactors = FALSE
#'   )
#' dat_this |> print()
#'
#' # return dataset as it is
#' e_read_df_header_span_rows(
#'     dat_this        = dat_this
#'   , skip            = 0
#'   , row_header_span = 1
#'   )
#' # no header row (first row is data), adverse affect when two values are the same
#' #   and utils::read.table adds suffix of ".1", etc., to value
#' e_read_df_header_span_rows(
#'     dat_this        = dat_this
#'   , skip            = 0
#'   , row_header_span = 0
#'   )
#' # skip first row
#' e_read_df_header_span_rows(
#'     dat_this        = dat_this
#'   , skip            = 1
#'   , row_header_span = 1
#'   )
#' # skip first row, combine first three rows into a column header, collapse with underscore
#' e_read_df_header_span_rows(
#'     dat_this        = dat_this
#'   , skip            = 1
#'   , row_header_span = 3
#'   , row_header_span_collapse = "_"
#'   )
#' # First row had multiple of same value, so ".1", ..., were appended;
#' #   so first remove ".1", then join header rows together
#' e_read_df_header_span_rows(
#'     dat_this        = dat_this
#'   , skip            = 0
#'   , row_header_span = 4
#'   , row_header_span_collapse = "_"
#'   )
#' # First row is data, so header is row 1 and add new column names
#' e_read_df_header_span_rows(
#'     dat_this        = dat_this
#'   , skip            = 0
#'   , row_header_span = 0
#'   )
#' # Skip 3 and rirst row is data, so add new column names
#' e_read_df_header_span_rows(
#'     dat_this        = dat_this
#'   , skip            = 3
#'   , row_header_span = 0
#'   )
#'
e_read_df_header_span_rows <-
  function(
    dat_this        = NULL
  , skip            = 0
  , row_header_span = 1
  , row_header_span_collapse = "_"
  ) {

  if (skip == 0 & row_header_span == 1) {
    message("erikmisc::e_read_df_header_span_rows, returning data as is")
    return(dat_this)
  }

  # first, restore false header row as first row of data
  dat_this <-
    colnames(dat_this) |>
    rbind(
      dat_this
    )

  # If first row had multiple of same value, ".1", ..., are appended
  #   (from utils::read.table, etc.);
  #   So, remove ".1" suffixes when part of column headers.
  if (skip == 0 & row_header_span > 0) {
    dat_this[1, ] <-
      dat_this[1, ] |>
      stringr::str_replace(
        pattern     = "\\.[:digit:]$"
      , replacement = ""
      )
  }

  # row_header_span = 0 means the header should be data
  # if header should be first row of data, then put generic headers and return
  if (row_header_span == 0) {
    colnames(dat_this) <- paste0("V", 1:ncol(dat_this))
    if (skip > 0) {
      # remove skip rows
      dat_this <- dat_this[-c(1:skip), ]
    }
    if (skip == 0) {
      message("erikmisc::e_read_df_header_span_rows, possible adverse issues if data on first row has same values (if utils::read.table added \".1\" suffix)")
    }
    return(dat_this)
  }

  # grab header rows, combine if necessary, then create a string to use later
  header_rows <-
    dat_this[1:row_header_span + skip, ]

  for (i_col in 1:ncol(header_rows)) {
    ## i_col = 1
    #header_rows[1, i_col]
    #paste(header_rows[, i_col], sep = " ")

    colnames(header_rows)[i_col] <-
      header_rows[, i_col] |>
      unlist() |>
      as.character() |>
      tidyr::replace_na("") |>
      stringr::str_subset(pattern = ".") |>
      paste(collapse = row_header_span_collapse) |>
      trimws()
  }

  col_names <- colnames(header_rows)
  # replace empty variable names with "V"
  col_names <- ifelse(col_names == "", "V", col_names)
  # make unique
  col_names <- col_names |> make.unique(sep = "..")

  colnames(dat_this) <- col_names

  # skip and header_rows

  if (row_header_span + skip > 0) {
    dat_this <- dat_this[-c(1:(row_header_span + skip)), ]
  }

  return(dat_this)
} # e_read_df_header_span_rows

