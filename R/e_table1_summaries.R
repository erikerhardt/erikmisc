#' Table 1 demographics summaries
#'
#' Moonbook significant digit version
#' function to run each variable individually, determining significant digits
#' then join them into a single file at the end
#'
#'    Uses \code{moonBook::mytable}; default is CatMethod = 2: Numeric summaries are Median and IQR with kruskal.test, Categorical summaries are proportions with chisq.test with continuity correction.
#'
#' @param dat_tab1            Dataset for table.
#' @param list_var_col_names  Column variable name.
#' @param list_var_col_labels Column variable label.
#' @param list_var_row_names  Variable names to loop over.
#' @param list_var_row_labels Variable labels to label table.
#' @param fn_root             Filename for temporary tables.
#' @param fn_all              Filename for output table, defaults to \code{paste0(fn_root, "_", "all", ".csv")}.
#' @param label_width         Width to wrap column and row variable labels, does not affect category labels.
#' @param sw_verbose          Print status to console during execution?
#' @param moonbook_max_ylev   An integer indicating the maximum number of levels of grouping variable ('y'). If a colummn have unique values less than max.ylev it is treated as a categorical variable. Default value is 2. (\code{?moonBook::mytable_sub})
#' @param moonbook_digits     An integer indicating the number of decimal places (round) or significant digits to be used. Default value is 3. (\code{?moonBook::mytable_sub})
#' @param moonbook_method     An integer indicating methods for continuous variables. Possible values in methods are: 1 = forces analysis as normal-distributed; 2 = forces analysis as continuous non-normal; 3 = performs a Shapiro-Wilk test to decide between normal or non-normal. (\code{?moonBook::mytable_sub})
#'
#' @return NULL, invisibly
#' @importFrom moonBook mytable mycsv
#' @importFrom stringr str_wrap
#' @importFrom stringr str_replace_all
#' @importFrom stats formula
#' @export
#'
#' @examples
#' \dontrun{
#' ## For Rmd code chunk attractive html table, code chunk option ```{r, results='asis'}
#'
#' # specify tows to summarize and columns to summarize by
#' list_var_col_names <- c("cyl", "am")
#' list_var_row_names <- c("mpg", "cyl", "disp", "hp", "drat", "wt"
#'                       , "qsec", "vs", "am", "gear", "carb")
#'
#' # assigning to new dataset since you may want to filter or subset in some way,
#' #   for example, only baseline measurements in a longitudinal analysis
#' dat_tab1 <-
#'   dat_mtcars_e |>
#'   #dplyr::filter(EVENT_ID == "BL") |>
#'   dplyr::select(all_of(c(list_var_col_names, list_var_row_names)))
#'
#'
#' # label variables
#' list_var_col_labels <- labelled::var_label(dat_tab1[, list_var_col_names]) |> unlist()
#' list_var_row_labels <- labelled::var_label(dat_tab1[, list_var_row_names]) |> unlist()
#'
#' # This loop creates a table summarized by each variable,
#' #   but can also specify multiple columns to summarize conditional on multiple columns.
#' for (i_col in seq_along(list_var_col_names)) {
#'
#'   # filename root for each separate variable (will write, then read to compile into one)
#'   fn_root <- paste0("tab1_vars_", list_var_col_names[i_col], "_summary")
#'
#'   # run function (additional options are available)
#'   e_table1_summaries(
#'       dat_tab1           = dat_tab1
#'     , list_var_col_names = list_var_col_names[i_col]
#'     , list_var_col_labels = list_var_col_labels[i_col]
#'     , list_var_row_names = list_var_row_names
#'     , list_var_row_labels = list_var_row_labels
#'     , fn_root            = fn_root
#'     , sw_verbose         = TRUE
#'     )
#'
#'   # read final table
#'   tab1_temp <- utils::read.csv(paste0(fn_root, "_", "all", ".csv"))
#'   # display in Rmd file
#'   knitr::kable(tab1_temp)
#' }
#'
#' ## For totals, helpful when missing data because has sample size for each row.
#' # filename root for each separate variable (will write, then read to compile into one)
#' fn_root <- paste0("tab1_vars_", "total", "_summary")
#'
#' # run function (additional options are available)
#' e_table1_summaries(
#'     dat_tab1           = dat_tab1
#'   , list_var_col_names = NULL
#'   , list_var_col_labels = "total"
#'   , list_var_row_names = list_var_row_names
#'   , list_var_row_labels = list_var_row_labels
#'   , fn_root            = fn_root
#'   , sw_verbose         = TRUE
#'   )
#'
#' # read final table
#' tab1_temp <- utils::read.csv(paste0(fn_root, "_", "all", ".csv"))
#' # display in Rmd file
#' knitr::kable(tab1_temp)
#'
#' }
e_table1_summaries <-
  function(
    dat_tab1            = dat_sub
  , list_var_col_names  = NULL
  , list_var_col_labels  = NULL
  , list_var_row_names  = NULL
  , list_var_row_labels  = NULL
  , fn_root             = "tab1_vars"
  , fn_all              = NULL
  , label_width         = 40
  , sw_verbose          = FALSE
  , moonbook_max_ylev   = 2
  , moonbook_digits     = c(factor = 1, numeric = 3)
  , moonbook_method     = c(1, 2, 3)[2]
  ) {

  if (is.null(list_var_row_labels)) {
    list_var_row_labels <- list_var_row_names
  }

  #ind_list_csv <- NULL

  tab1_list <- NULL
  for (i_var in seq_along(list_var_row_names)) {
    ## i_var = 5

    if(!is.null(list_var_col_names)) {
      form <- stats::formula(paste0(paste0(list_var_col_names, collapse = " + "), " ~ ", list_var_row_names[i_var]))
    } else {
      form <- stats::formula(paste0(" ~ ", list_var_row_names[i_var]))
    }
    if (sw_verbose) {
      print(form)
    }



    # if not a factor, then calculate significant digits to use
    if (is.factor(dat_tab1[,list_var_row_names[i_var]][[1]])) {
      temp_digits = moonbook_digits[1] |> as.numeric()
    } else {

      # determine digits to use, when < 1.0

      # start with minimum positive absolute value, then log10 + 2 to give 3 sig digits
      abs_val <- abs(dat_tab1[,list_var_row_names[i_var]])
      abs_val <- abs_val[!is.na(abs_val)]
      abs_val <- abs_val[abs_val > 0]
      min_abs_val <- min(abs_val)

      if (min_abs_val >= 1.0) {
        temp_digits <- moonbook_digits[2] |> as.numeric()
      } else {
        temp_digits <- abs(floor(log10( min_abs_val ))) + (as.numeric(moonbook_digits[2]) - 1)
      }
    }

    # Produce a demographics table
    #library(moonBook)
    tryCatch(
      tab1_list[[i_var]] <-
        moonBook::mytable(
          x      = form
        , data   = dat_tab1
        , digits = temp_digits
        , method = moonbook_method # forces analysis as continuous non-normal (does not assume Normal)
        #, show.all = TRUE #  all statistical values have to be shown in table
        , max.ylev    = moonbook_max_ylev
        , maxCatLevel = 100
        , show.total  = TRUE
        )
    , finally = next
    )

    #ind_list_csv <- c(ind_list_csv, i_var)

    #tab1_list[[i_var]]$res

    if(!is.null(list_var_col_names)) {
      # Better name for column
      if (i_var == 1) {
        # first row, only, for long column name
        if (!is.null(list_var_col_labels) & !(list_var_col_labels == "NULL") & !(list_var_col_labels == "character(0)")) {
          colnames(tab1_list[[i_var]]$res)[1] <- stringr::str_wrap(list_var_col_labels, width = label_width)
        }
      }
      # Better name for variable
      if (!is.na(list_var_row_labels[i_var])) {
        if (!is.null(list_var_row_labels[i_var]) & !(list_var_row_labels[i_var] == "NULL") & !(list_var_row_labels[i_var] == "character(0)")) {
          tab1_list[[i_var]]$res[1,1] <- stringr::str_wrap(list_var_row_labels[i_var], width = label_width)
        }
      }

    } else {
      ## 3/22/2022 (moonbook updated?)
      ## For "Total", the data structure is different, can only label a row

      # # Better name for column
      # if (i_var == 1) {
      #   # first row, only, for long column name
      #   if (!is.null(list_var_col_labels) & !(list_var_col_labels == "NULL") & !(list_var_col_labels == "character(0)")) {
      #     colnames(tab1_list[[i_var]]$res)[1] <- stringr::str_wrap(list_var_col_labels, width = label_width)
      #   }
      # }
      # Better name for variable
      if (!is.na(list_var_row_labels[i_var])) {
        if (!is.null(list_var_row_labels[i_var]) & !(list_var_row_labels[i_var] == "NULL") & !(list_var_row_labels[i_var] == "character(0)")) {
          tab1_list[[i_var]]$name <- stringr::str_wrap(list_var_row_labels[i_var], width = label_width)
        }
      }
    }

    #tab1_list[[i_var]]$res

    # didn't work inside this loop 4/20/2020
    # modifications must go in the loop below!

    #moonBook::mycsv(tab1_list[[i_var]], file = paste0(fn_root, "_", i_var, ".csv"))
  } # i_var



  # save each variable in a separate file to merge together below
  for (i_var in seq_along(list_var_row_names)) {
    if(!is.null(list_var_col_names)) {

      # skip if no table created
      if(is.null(tab1_list[[i_var]])) {
        next
      }

      # Label small p-values
      if (tab1_list[[i_var]]$res$p[1] == "0.000") {
        tab1_list[[i_var]]$res$p[1] <- "< 0.0005"
      }

      moonBook::mycsv(tab1_list[[i_var]], file = paste0(fn_root, "_", i_var, ".csv"))
    } else {
      xxx     = list()
      xxx$res = tab1_list[[i_var]]
      attr(xxx, "class") <- "mytable"
      xxx$ show.all = TRUE
      moonBook::mycsv(xxx, file = paste0(fn_root, "_", i_var, ".csv"))
    }
  } # i_var


  # read each variable file and merge together, then delete original files
  tab1_out <- NULL
  #for (i_var in ind_list_csv) {  #seq_along(list_var_row_names)) {
  for (i_var in seq_along(list_var_row_names)) {
    fn_in <- paste0(fn_root, "_", i_var, ".csv")
    if (sw_verbose) {
      print(fn_in)
    }

    # skip if no table created
    if(is.null(tab1_list[[i_var]])) {
      next
    }

    temp_in <- readLines(fn_in)
    if (i_var > 1) {
      temp_in <- temp_in[-(1:2)]
    }
    # remove trailing spaces from variable names and factor levels
    for (i_row in 1:length(temp_in)) {
      # when multiple spaces followed by a \", remove those spaces
      temp_in[i_row] <- stringr::str_replace_all(temp_in[i_row], "\\s+\\\"", "\\\"")
    }
    if (sw_verbose) {
      print(temp_in)
    }
    tab1_out <- c(tab1_out, temp_in)

    # If couldn't remove file (because too fast for operating system),
    #   then sleep and try again
    check_message <-
      e_message_capture(
        file.remove(fn_in)
      )(1)
    if (stringr::str_detect(string = check_message$logs[[1]]$message, pattern = stringr::fixed("Permission denied")) ) {
      message("erikmisc::e_table1_summaries Remove file, permission denied. Sleeping 0.1 s and trying again.")
      Sys.sleep(0.1)
      file.remove(fn_in)
    }
  } # i_var

  # save one file with all of the variables
  if (is.null(fn_all)) {
    fn_all <- paste0(fn_root, "_", "all", ".csv")
  }
  writeLines(tab1_out, fn_all)

  invisible(NULL)
}
