#' Create a Data Frame from All Combinations of Data Frames
#'
#' \code{expand.grid} for data.frames.  Create a data frame from all combinations of the supplied data frames.
#'
#' @param ... a list of data.frames
#'
#' @return A data frame containing one block of rows for each combination of
#'         the supplied data frames. The last data frames vary fastest. The columns retain
#'         their original names except for identical names in different data frames, in
#'         which case the suffix ".x", ".y", etc., are added in the returned data frame but may not be unique.
#'
#' @export
#'
#' @examples
#' df1 <- data.frame(A = 1:3, B = 11:13)
#' df2 <- data.frame(C = 51:52, D = c("Y", "N"))
#' df3 <- data.frame(E = c("+", "-"))
#' e_expand_grid_df(df1, df2, df3)
e_expand_grid_df <-
  function(
    ...
  ) {
    # https://stackoverflow.com/questions/11693599/alternative-to-expand-grid-for-data-frames
    Reduce(
      function(
        ...
      ) {
        merge(
          ...
        , by = NULL
        )
      }
      , list(
        ...
      )
    )
  }
