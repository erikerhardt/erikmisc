#' Table 1 Moonbook significant digit version
#' function to run each variable individually, determining significant digits
#' then join them into a single file at the end
#'    Uses moonBook::mytable(method = 2) to force analysis as continuous non-normal (does not assume Normal) (
#'      Numeric summaries are Median and IQR.
#'    CatMethod = 2, Perform chisq.test with continuity correction
#'
#' @param dat_tab1            Dataset for table.
#' @param list_var_col_table  Column variable name.
#' @param list_var_col_names  Column variable label.
#' @param list_var_row_table  Variable names to loop over.
#' @param list_var_row_names  Variable labels to label table.
#' @param fn_root             Filename for output table.
#' @param label_width         Width to wrap column and row variable labels, does not affect category labels.
#' @param sw_verbose          Print status to console during execution?
#'
#' @return
#' @importFrom moonBook mytable mycsv
#' @export
#'
#' @examples
f_table1_moonbook_sig <-
  function(
    dat_tab1            = dat_sub
  , list_var_col_table  = NULL
  , list_var_col_names  = NULL
  , list_var_row_table  = NULL
  , list_var_row_names  = NULL
  , fn_root             = "tab1_vars"
  , label_width         = 40
  , sw_verbose          = FALSE
  ) {

  if (is.null(list_var_row_names)) {
    list_var_row_names <- list_var_row_table
  }

  #ind_list_csv <- NULL

  tab1_list <- NULL
  for (i_var in seq_along(list_var_row_table)) {

    if(!is.null(list_var_col_table)) {
      form <- formula(paste0(paste0(list_var_col_table, collapse = " + "), " ~ ", list_var_row_table[i_var]))
    } else {
      form <- formula(paste0(" ~ ", list_var_row_table[i_var]))
    }
    if (sw_verbose) {
      print(form)
    }

    # if not a factor, then calculate significant digits to use
    if (is.factor(dat_tab1[,list_var_row_table[i_var]][[1]])) {
      temp_digits = 1
    } else {

      # determine digits to use, when < 1.0

      # start with minimum positive absolute value, then log10 + 2 to give 3 sig digits
      abs_val <- abs(dat_tab1[,list_var_row_table[i_var]])
      abs_val <- abs_val[!is.na(abs_val)]
      abs_val <- abs_val[abs_val > 0]
      min_abs_val <- min(abs_val)

      if (min_abs_val >= 1.0) {
        temp_digits <- 1
      } else {
        temp_digits <- abs(floor(log10( min_abs_val ))) + 2
      }
    }

    # Produce a demographics table
    #library(moonBook)
    tryCatch(
      tab1_list[[i_var]] <-
        moonBook::mytable(
          form
        , data = dat_tab1
        , digits = temp_digits
        , method = 2 # forces analysis as continuous non-normal (does not assume Normal)
        #, show.all = TRUE #  all statistical values have to be shown in table
        , maxCatLevel = 100
        , show.total = TRUE
        )
    , finally = next
    )

    #ind_list_csv <- c(ind_list_csv, i_var)

    #tab1_list[[i_var]]$res

    # Better name for column
    if (i_var == 1) {
      # first row, only, for long column name
      if (!is.null(list_var_col_names) & !(list_var_col_names == "NULL") & !(list_var_col_names == "character(0)")) {
        colnames(tab1_list[[i_var]]$res)[1] <- stringr::str_wrap(list_var_col_names, width = label_width)
      }
    }

    # Better name for variable
    if (!is.na(list_var_row_names[i_var])) {
      if (!is.null(list_var_row_names[i_var]) & !(list_var_row_names[i_var] == "NULL") & !(list_var_row_names[i_var] == "character(0)")) {
        tab1_list[[i_var]]$res[1,1] <- stringr::str_wrap(list_var_row_names[i_var], width = label_width)
      }
    }

    #tab1_list[[i_var]]$res

    # didn't work inside this loop 4/20/2020
    #moonBook::mycsv(tab1_list[[i_var]], file = paste0(fn_root, "_", i_var, ".csv"))
  }
  for (i_var in seq_along(list_var_row_table)) {
    if(!is.null(list_var_col_table)) {
      moonBook::mycsv(tab1_list[[i_var]], file = paste0(fn_root, "_", i_var, ".csv"))
    } else {
      xxx     = list()
      xxx$res = tab1_list[[i_var]]
      attr(xxx, "class") <- "mytable"
      xxx$ show.all = TRUE
      moonBook::mycsv(xxx, file = paste0(fn_root, "_", i_var, ".csv"))
    }
  }

  tab1_out <- NULL
  #for (i_var in ind_list_csv) {  #seq_along(list_var_row_table)) {
  for (i_var in seq_along(list_var_row_table)) {
    fn_in <- paste0(fn_root, "_", i_var, ".csv")
    if (sw_verbose) {
      print(fn_in)
    }
    temp_in <- readLines(fn_in)
    if (i_var > 1) {
      temp_in <- temp_in[-(1:2)]
    }
    # remove trailing spaces from variable names and factor levels
    for (i_row in 1:length(temp_in)) {
      # when multiple spaces followed by a \", remove those spaces
      temp_in[i_row] <- str_replace_all(temp_in[i_row], "\\s+\\\"", "\\\"")
    }
    if (sw_verbose) {
      print(temp_in)
    }
    tab1_out <- c(tab1_out, temp_in)
    file.remove(fn_in)
  }
  writeLines(tab1_out, paste0(fn_root, "_", "all", ".csv"))

  invisible(NULL)
}
