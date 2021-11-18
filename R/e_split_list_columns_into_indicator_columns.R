#' Split a (set of) item-listing columns into indicator columns
#'
#' Commonly used for comorbidities or prescription lists within a single (or multiple) column(s).
#' Takes a column where items are separated by puncutation (,.;/|) and creates separate columns with indicators.
#' Can treat counts of items >1 as 1 to simplify summary tables.
#'
#' @param dat_this                entire data.frame or tibble, will return with additional indicator columns
#' @param var_names_items         column names with lists of items
#' @param item_delimiters         delimiter(s) that separate items within a single column
#' @param code_other_below_freq   replace item name with \code{label_other} if total frequency for an item is less than this value
#' @param label_other             label for the "other" category
#' @param indicator_col_prefix    prefix for the new indicator columns
#' @param sw_replace_GT1_with_1   if a person's item count is more than one in the indicator column, should we replace with a "1" to interpret as "at least 1"
#' @param sw_print_unique         print list of items before and after replacing with "other"
#'
#' @return dat_this with additional indicator columns
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_cols
#' @importFrom dplyr desc
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom stringr str_flatten
#' @importFrom stringr str_length
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom stringr str_trim
#' @importFrom tibble as_tibble
#' @importFrom tidyr uncount
#' @importFrom tidyselect all_of
#' @export
#'
#' @examples
#' dat_ex <-
#'   dplyr::tibble(
#'     col1 = c(NA, "", "a", "A, B  ,C", "b", "D. c ;    d", "x  ;Y", "ab/0|J", 2, "other", 1)
#'   , col2 = LETTERS[1:length(col1)]
#'   ) %>%
#'   dplyr::mutate(
#'     ID = 1:dplyr::n()
#'   ) %>%
#'   dplyr::select(
#'     ID
#'   , everything()
#'   )
#' dat_ex %>% print(n = Inf)
#'
#' dat_ex_out <-
#'   e_split_list_columns_into_indicator_columns(
#'     dat_this              = dat_ex
#'   , var_names_items       = c("col1", "col2")
#'   , item_delimiters       = ",.;/|"
#'   , code_other_below_freq = 2
#'   , label_other           = "other"
#'   , indicator_col_prefix  = "item_"
#'   , sw_replace_GT1_with_1 = FALSE
#'   , sw_print_unique       = TRUE
#'   )
#' dat_ex_out %>% print(n = Inf)
e_split_list_columns_into_indicator_columns <-
  function(
    dat_this
  , var_names_items       = NULL
  , item_delimiters       = ",.;/|"
  , code_other_below_freq = 5
  , label_other           = "other"
  , indicator_col_prefix  = "item_"
  , sw_replace_GT1_with_1 = FALSE
  , sw_print_unique       = TRUE
  ) {

  if(is.null(var_names_items)) {
    warning("erikmisc::e_split_list_columns_into_indicator_columns:  No column name(s) for items provided, exiting")
    return(dat_this)
  }

  # create temporary copies of columns to work on
  var_names_items_internal <-
    paste0("X___items_", seq_along(var_names_items))

  for(i_var in seq_along(var_names_items)) {
    ## i_var = 1
    dat_this[[ var_names_items_internal[i_var] ]] <-
        dat_this[[ var_names_items[i_var] ]] %>%
        # make all items lowercase,
        tolower() %>%
        # trim external whitespace
        stringr::str_trim(side = "both") %>%
        # replace selected punctuation with ","
        stringr::str_replace_all(paste0("[[", item_delimiters, "]]"), ",") %>%
        # remove internal white space,
        stringr::str_replace_all("\\s+,\\s+", ",") %>%
        stringr::str_replace_all(",\\s+", ",") %>%
        stringr::str_replace_all("\\s+,", ",")
  }

  # combine item columns into a single list
  dat_this_items <-
    dat_this %>%
    dplyr::select(var_names_items_internal) %>%
    unlist() %>%
    as.character() %>%
    tolower() %>%
    na.omit()
  # remove empty strings
  dat_this_items <-
    dat_this_items[stringr::str_length(dat_this_items) > 0]
  # split by punctuation
  dat_this_items <-
    dat_this_items %>%
    stringr::str_split(
      pattern = ","
    ) %>%
    unlist()

  tab_items_unique <-
    dat_this_items %>%
    table() %>%
    tibble::as_tibble() %>%
    dplyr::rename(
      "items" = "."
    , "Freq"  = "n"
    ) %>%
    dplyr::arrange(
      dplyr::desc(Freq), items
    )
  if (sw_print_unique) {
    print(paste0("Unique items: ", nrow(tab_items_unique)))
    tab_items_unique %>% print(n = Inf)
    print(paste0("Coding items with frequencies less than ", code_other_below_freq, " to '", label_other, "'"))
  }

  # items to code as "other"
  items_other <-
    tab_items_unique %>%
    dplyr::filter(Freq < code_other_below_freq) %>%
    dplyr::select(1) %>%
    unlist() %>%
    as.character()

  if(label_other %in% tab_items_unique[["items"]]) {
    message("erikmisc::e_split_list_columns_into_indicator_columns: Other category `", label_other, "` already appears in data.")
  }


  for(n_var in var_names_items_internal) {
    ## n_var = var_names_items_internal[1]

    this_col_split <-
      dat_this[[n_var]] %>%
      stringr::str_split(
        pattern = ","
      )

    for(i_row in 1:length(this_col_split)) {
      ## i_row = 8

      for(n_cond in items_other) {
        ## n_cond = items_other[1]
        this_col_split[[i_row]] <-
          this_col_split[[i_row]] %>%
          stringr::str_replace_all(
            paste0("^", n_cond, "$")
          , label_other
          )
      }

    }

    dat_this[[n_var]] <-
      this_col_split %>%
      sapply(
        stringr::str_flatten
      , collapse = ","
      )

  }

  # make table after low frequencies are coded as "other"
  tab_items_unique_other <-
    tab_items_unique

  for(n_cond in items_other) {
    ## n_cond = items_other[1]
    tab_items_unique_other[["items"]] <-
      tab_items_unique_other[["items"]] %>%
      str_replace_all(
        #stringr::fixed(n_cond)
        paste0("^", n_cond, "$")
      , label_other
      )
  }

  tab_items_unique_other <-
    tab_items_unique_other %>%
    tidyr::uncount(
      weights = Freq
    ) %>%
    dplyr::pull(items) %>%
    table() %>%
    tibble::as_tibble() %>%
    dplyr::rename(
      "items" = "."
    , "Freq"  = "n"
    ) %>%
    dplyr::arrange(
      dplyr::desc(Freq), items
    )
  if (sw_print_unique) {
    print(paste0("Unique items: ", nrow(tab_items_unique_other), " with other category `", label_other, "`"))
    tab_items_unique_other %>% print(n = Inf)
  }

  # create seperate columns for each item
  new_var_names_items <-
    tab_items_unique_other %>%
    dplyr::pull(items) %>%
    as.character()


  # put all items in a table for counting
  tab_items <- list()

  for(i_var in seq_along(var_names_items_internal)) {
    ## i_var = 1

    tab_items[[i_var]] <-
      dat_this %>%
      dplyr::pull(
        tidyselect::all_of(var_names_items_internal[i_var])
      ) %>%
      stringr::str_split(pattern = ",", simplify = TRUE)
  }

  tab_items <-
    tab_items %>%
    #dplyr::bind_cols() %>%
    do.call(cbind, .) %>%
    as.matrix()

  for(n_new_var in new_var_names_items) {
    ## n_new_var = new_var_names_items[1]
    dat_this[, paste0(indicator_col_prefix, n_new_var)] <-
      (tab_items == n_new_var) %>% rowSums(na.rm = TRUE)
  }

  if(sw_replace_GT1_with_1) {
    # Let "1" mean "at least 1"
    ind_GE1 <- (dat_this[,paste0(indicator_col_prefix, new_var_names_items)] > 1)
    dat_this[,paste0(indicator_col_prefix, new_var_names_items)][ind_GE1] <- 1
  }

  # remove temporary columns
  dat_this <-
    dat_this %>%
    dplyr::select(
      -tidyselect::all_of(var_names_items_internal)
    )

  return(dat_this)
} # e_split_list_columns_into_indicator_columns

