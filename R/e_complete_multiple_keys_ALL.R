# OLD: key_variables_completion.Rmd at https://statacumen.com/2021/03/28/r-key-variables-completion/



#' Combine rows with different data values
#'
#'
#' For matching \code{group_by()} keys, keeps first non-NA value for each column.
#'
#' @param dat data.frame of keys (or other columns) to combine
#'
#' @return coalesced data.frame
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' dat %>%
#'   dplyr::group_by(A) %>%
#'   dplyr::summarise_all( e_coalesce_by_column )
#' }
e_coalesce_by_column <-
  function(
    dat
  ) {
  # for matching group_by() keys, keeps first non-NA value for each column

  # combine rows in data frame containing NA to make complete row
  # https://stackoverflow.com/questions/45515218/combine-rows-in-data-frame-containing-na-to-make-complete-row

  ## use:
  ## dat %>%
  ##   group_by(A) %>%
  ##   summarise_all(e_coalesce_by_column)


  ## TESTING
  # dat <-
  #   # A = 1, easy
  #   # A = 2, easy
  #   # A = 3, 3rd row has a dup and NA
  #   # A = 4, 3rd row has conflucing info
  #   tibble::tribble(
  #    ~A, ~B, ~C, ~D, ~E
  #   , 1, NA,  3, NA,  5
  #   , 1,  2, NA,  2, NA
  #   , 2, NA, NA,  3, NA
  #   , 2,  4,  5, NA,  4
  #   , 3, NA, NA,  3, NA
  #   , 3,  4,  5, NA,  4
  #   , 3,  4, NA, NA,  4
  #   , 4, NA, NA,  3, NA
  #   , 4,  4,  5, NA,  4
  #   , 4, 99, 99, 99, NA
  #   )

  dplyr::coalesce(!!! as.list(dat)) %>%
  return()
}



#' Coalesce column set
#'
#' For a set of key variables that are unique for participants,
#'   identify sets of rows with unique key variables in common
#'   and collapse them together to have a single "most complete" row
#'   of key values by removing the NAs.
#' Each key variable must have a unique value for each participant.
#'
#' @param dat data.frame keys to combine
#'
#' @return data.frame with most complete key rows
#' @import dplyr
#' @export
e_coalesce_column_set <-
  function(
    dat
  ) {

  names_var <-
    names(dat)

  for (i_key in seq_along(names_var)) {
    ## i_key = 1

    # rows for current column without NAs
    dat_rows_process <-
      dat[!is.na(dat[[i_key]]), ]

    # bind to end to process later
    dat_rows_skip_NA <-
      dat[is.na(dat[[i_key]]), ]

    # coalesce for this key column
    if (nrow(dat_rows_process) > 0) {
      dat_rows_process <-
        dat_rows_process %>%
        dplyr::group_by_at(
          i_key
        ) %>%
        dplyr::summarise_all(
          e_coalesce_by_column
        ) %>%
        dplyr::ungroup()
    }

    # combine, assure original column order for iteration
    dat <-
      dplyr::bind_rows(
        dat_rows_process
      , dat_rows_skip_NA
      ) %>%
      dplyr::select(
        one_of(names_var)
      )
  }

  return(dat)
}


#' Match and replace less complete rows with most complete row
#'
#' @param dat_data          data.frame of data for which to update the keys
#' @param dat_most_complete data.frame with most complete keys
#' @param col_keys          names of columns to be used as keys
#'
#' @return data.frame updated with data keys
#' @import dplyr
e_replace_keys_less_with_most_complete <-
  function(
    dat_data
  , dat_most_complete
  , col_keys = c("a", "b", "c")
  ) {

  # combination to match key variables, from most to least
  dat_combination_var <-
    expand.grid(
      replicate(
        length(col_keys)
      , 0:1
      , simplify = FALSE)
    )
  dat_combination_var$s <-
    rowSums(dat_combination_var)
  dat_combination_var <-
    dat_combination_var %>%
    dplyr::arrange(
      -s
    ) %>%
    dplyr::select(
      -s
    )
  colnames(dat_combination_var) <- col_keys

  # dat_combination_var

  # list to fill with matches, then bind at end
  dat_data_out <- list()
  dat_data_in  <- list()

  for (i_combination in 1:nrow(dat_combination_var)) {
    ## i_combination = 1
    ## i_combination = 2

    # keep track of which rows to include and exclude for this combination
    ind_all      <- matrix(NA, nrow = nrow(dat_data), ncol = length(col_keys))

    keys_include <- (dat_combination_var[i_combination, ] == 1)

    for (i_col_keys in seq_along(col_keys)) {
      ## i_col_keys = 1

      if (keys_include[i_col_keys]) {
        # values for these keys
        ind_all[, i_col_keys] <- !is.na(dat_data[[ col_keys[i_col_keys] ]])
      }
      if (!keys_include[i_col_keys]) {
        # NAs for these col_keys
        ind_all[, i_col_keys] <- is.na(dat_data[[ col_keys[i_col_keys] ]])
      }

    }

    # identify rows for this combination
    ind_combination <- rowSums(ind_all) == length(col_keys)

    dat_data_in[[i_combination]] <-
      dat_data[ind_combination,]

    if(sum(keys_include) == 0) {
      dat_data_out[[i_combination]] <-
        dat_data_in[[i_combination]]

    } else {

      dat_data_out[[i_combination]] <-
        dplyr::right_join(
          dat_most_complete
        , dat_data_in[[i_combination]]
        , by = col_keys[which(keys_include)]
        , suffix = c("", ".EMPTY")
        , keep = FALSE
        ) %>%
        dplyr::select(
          -ends_with(".EMPTY")
        )
    }

  }

  # combine all results
  dat_data_updated <-
    bind_rows(
      dat_data_out
    )

  return(dat_data_updated)
}


#' Complete multiple key ID variables
#'
#' When data are combined from multiple sources and multiple identifier keys (ID)
#' are used, sometimes in combinations, then we may want to provide the
#' most complete set of the IDs for each observation.
#' By definition, each key value is unique within its column to identify an observational unit.
#'
#' @param dat_data data frame with a set of variables that are keys
#' @param dat_keys optional data frame with only key variables, more complete than set in dat_data
#' @param col_keys key columns names
#'
#' @return dat_data with updated key columns
#' @import dplyr
#' @export
#'
#' @examples
#'
#' dat_data <-
#'   tibble::tribble(
#'     ~a, ~b, ~c, ~x, ~d1, ~d2
#'   ,  1, 11, NA, NA,   1,  20
#'   ,  1, NA, 10, NA,   2,  21
#'   ,  2, NA, NA, NA,   3,  22
#'   , NA, 22, 20, NA,   4,  23
#'   ,  2, NA, 20, NA,   5,  24
#'   ,  3, 33, NA, NA,   6,  25
#'   ,  4, NA, 40, NA,   7,  26
#'   ,  5, NA, NA, NA,   8,  27
#'   ,  6, NA, 60, NA,   9,  28
#'   ,  6, NA, 60, NA,  10,  29
#'   , NA, 77, NA, NA,  11,  30
#'   , NA, 88, 80, NA,  12,  31
#'   , NA, 88, NA, NA,  13,  32
#'   , NA, NA, NA, NA,  14,  33
#'   )
#'
#' dat_data_updated <-
#'   e_complete_multiple_keys(
#'     dat_data
#'   , dat_keys = NULL
#'   , col_keys = c("a", "b", "c", "x")
#'   )
#'
#' dat_data_updated %>% print(n=Inf)
#'
e_complete_multiple_keys <-
  function(
    dat_data
  , dat_keys = NULL
  , col_keys = c("a", "b", "c")
  ) {

  # create ID to restore original row order at end
  dat_data <-
    dat_data %>%
    dplyr::mutate(
      .ID. = 1:n()
    )

  # create complete keys
  if(is.null(dat_keys)) {
    dat_keys <-
      e_coalesce_column_set(
        dat_data %>%
        dplyr::select(
          one_of(col_keys)
        )
      )
  }

  # update data with complete keys
  dat_data_updated <-
    e_replace_keys_less_with_most_complete(
      dat_data          = dat_data
    , dat_most_complete = dat_keys
    , col_keys          = col_keys
    ) %>%
    # restore original row order
    dplyr::arrange(
      .ID.
    ) %>%
    dplyr::select(
      -.ID.
    )

  return(dat_data_updated)

}

