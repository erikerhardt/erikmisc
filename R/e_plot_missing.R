#' Plots missing data in a data.frame, possibly grouped by one variable and sorted by a second.
#'
#' @param dat           data.frame or tibble
#' @param var_group     variable name to group by (colors data)
#' @param sw_group_sort TRUE/FALSE to sort by grouped variable
#' @param var2_sort     second variable name to sort by if data is grouped
#'
#' @return              ggplot grob plot object
#' @import dplyr
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @export
#'
#' @examples
#' dat = datasets::mtcars
#' prop_missing = 0.10
#' n_missing = sample.int(n = prod(dim(dat)), size = round( prop_missing * prod(dim(dat))))
#' ind_missing = expand.grid(1:dim(dat)[1], 1:dim(dat)[2])[n_missing, ]
#' for (i_row in seq_along(n_missing)) {
#'   dat[ind_missing[i_row,1], ind_missing[i_row,2] ] <- NA
#' }
#'
#' e_plot_missing(
#'     dat            = dat
#'   , var_group      = "cyl"
#'   , sw_group_sort  = TRUE
#'   , var2_sort      = "disp"
#'   )
e_plot_missing <-
  function(
    dat
  , var_group     = NULL
  , sw_group_sort = FALSE
  , var2_sort     = NULL
  ) {
  ## DEBUG
  ## 6/17/2021
  # #dat = datasets::mtcars %>% as_tibble(rownames = "Model")
  # dat = datasets::mtcars
  # n_missing = sample.int(n = prod(dim(dat)), size = round( 0.10 * prod(dim(dat))))
  # ind_missing = expand.grid(1:dim(dat)[1], 1:dim(dat)[2])[n_missing, ]
  # for (i_row in seq_along(n_missing)) {
  #   dat[ind_missing[i_row,1], ind_missing[i_row,2] ] <- NA
  # }
  # var_group = "cyl"
  # #var_group = "cylx"
  # sw_group_sort = TRUE
  # var2_sort = "hp"

  # If there are rownames, then create a separate variable
  if (any(is.na(as.numeric(rownames(dat))))) {
    # if there are any non-numeric rownames, then process this
    sw_rowname_column <- TRUE
  } else {
    if (all(as.numeric(rownames(dat)) == 1:nrow(dat))) {
      # if all number in order, then they don't have rownames
      sw_rowname_column <- FALSE
    } else {
      # otherwise, some row names might be numbers in non-sequential order
      sw_rowname_column <- TRUE
    }
  }
  if (sw_rowname_column) {
    dat <-
      dat %>%
      as_tibble(rownames = "ROWNAME")
  }

  names_col <- colnames(dat)


  dat <-
    dat %>%
    mutate(
      ID_MISSING___ = 1:n()
    ) %>%
    select(
      ID_MISSING___
    , everything()
    )

  # label the groups
  sw_group <- FALSE
  if (!is.null(var_group)) {
    sw_group <- TRUE
    if (!any(names_col %in% var_group)) {
      warning("erikmisc::plot_missing() var_group name not in dataset, plotting without group.")
      sw_group <- FALSE
    }
    n_levels <- factor(dat[[var_group]]) %>% levels() %>% length()
    if ((n_levels > nrow(dat)/2) | (n_levels > 20) ) {
      warning("erikmisc::plot_missing() var_group variable has too many levels (>20 or > nrow/2), plotting without group.")
      sw_group <- FALSE
    }
  }
  if (sw_group) {
    # turn it into a factor variable (it might already be, that's ok)
    dat[["GROUP___"]] <- factor(dat[[var_group]])
  } else {
    dat[["GROUP___"]] <- factor(1)
  }

  # sort by group
  if (!is.null(var2_sort)) {
    sw_var2_sort <- TRUE
  } else {
    sw_var2_sort <- FALSE
  }

  #### XXX
  #### SORT BY COMPLETE PROPORTION
  ##   dat_NA_temp <-
  ##     dat_all %>%
  ##     select(matches(match = "^Q[0-9]", perl = TRUE)) %>%
  ##     is.na()
  ##
  ##   dat_all$CompleteProp <-
  ##     (rowMeans(!dat_NA_temp))
  ##
  ##   rm(dat_NA_temp)


  if (sw_group_sort) {
    # secondary sort
    if (!is.null(var2_sort) & !any(names_col %in% var2_sort)) {
      warning("erikmisc::plot_missing() var2_sort name not in dataset, not sorting within group.")
      var2_sort <- NULL
    }
    if (!is.null(var2_sort)) {
      dat <-
        dat %>%
        arrange(
          GROUP___, !!as.name(var2_sort)
        )
    } else {
      dat <-
        dat %>%
        arrange(
          GROUP___
        )
    }
    # update ID number for sorted data
    dat <-
      dat %>%
      mutate(
        ID_MISSING___ = 1:n()
      )
  }

  # Find the NAs
  dat2 <-
    dat %>%
    is.na()

  n_missing <- sum(dat2)

  # create a column indicating which rows all have data (no missing)
  NO_MISSING <-
    !(rowSums(!dat2) == ncol(dat2))

  dat2 <-
    cbind(dat2, NO_MISSING) %>%
    as_tibble()

  # convert to numeric
  #cols_logical <- sapply(dat2, is.logical)
  #dat2[ ,cols_logical] <- lapply(dat2[ ,cols_logical], as.numeric)

  dat2 <-
    dat2 %>%
    as_tibble() %>%
    mutate(
      ID_MISSING___ = dat$ID_MISSING___
    , GROUP___      = dat$GROUP___
    ) %>%
    tidyr::pivot_longer(
      cols = one_of(c(names_col, "NO_MISSING"))
    ) %>%
    mutate(
    #  value = value %>% factor(levels = c(1, 0), labels = c("Missing", "Present"))
      value = value %>% factor(levels = c(TRUE, FALSE), labels = c("Missing", "Present"))
    , name  = name %>% factor(levels = c(names_col, "NO_MISSING"))
    )

  #### OLD
  ## dat2 <-
  ##   cbind(dat2, NO_MISSING) %>%
  ##   reshape2::melt()

  if (sw_group) {
    p <- ggplot2::ggplot(data = dat2, aes(x = name, y = ID_MISSING___, fill = GROUP___, alpha = value))
  } else {
    p <- ggplot2::ggplot(data = dat2, aes(x = name, y = ID_MISSING___, alpha = value))
  }
  p <- p + ggplot2::geom_raster()  # , alpha = 0.6
  #p <- p + ggplot2::geom_raster(aes(fill = value))  # , alpha = 0.6
  #p <- p + ggplot2::scale_fill_grey(name = "", labels = c("Present", "Missing"))
  p <- p + ggplot2::theme_minimal()
  p <- p + ggplot2::theme(axis.text.x  = element_text(angle=90, vjust=0, hjust=0))
  p <- p + ggplot2::labs(x = "Variables in Dataset", y = "Rows / observations")
  #ggplot2::scale_y_continuous(expand = c(0,0)) +
  #p <- p + ggplot2::scale_alpha_discrete(limits = c(0, 1), labels = c("Missing", "Present"))

  if (n_missing == 0) {
    p <- p + ggplot2::scale_alpha_discrete(limits = c(0, 1), labels = c("Missing", "Present"))
  }

  breaks_seq_by = e_plot_calc_break_interval(values = 1:nrow(dat))

  #p <- p + ggplot2::scale_y_reverse(expand = c(0,0), breaks = c(1, seq(0, 10000, by=20)))
  p <- p + ggplot2::scale_y_reverse(expand = c(0,0), breaks = c(1, seq(0, nrow(dat), by = breaks_seq_by)))

  p <- p + labs(
              title = "Missing values"
            , alpha = "Value is"
            , caption =
                paste0(
                  "Missing values: "
                , n_missing
                , " / "
                , prod(dim(dat))
                , ",  "
                , 100 * round(n_missing / prod(dim(dat)), 3)
                , " %"
                )
            )

  if (sw_group) {
    p <- p + labs(
              fill = var_group
            )
    text_subtitle <- paste0("Grouped by ", var_group)

    if(sw_var2_sort) {
      text_subtitle <- paste0(text_subtitle, ", sorted by ", var2_sort)
    }

    p <- p + labs(
              subtitle = text_subtitle
            )
  }

  #print(p)


  return(p)
} # e_plot_missing



#' Interval for plot breaks, determine a "nice" length of seq(by = Interval)
#'
#' @param values            numeric values for an axis
#' @param num_intervals     rough desired number of intervals
#' @param val_leading_digit leading digits to consider (in ascending order, end with 10)
#'
#' @return interval for breaks
#' @export
#'
#' @examples
#' e_plot_calc_break_interval(1:250)
#' e_plot_calc_break_interval(1:1000)
#' e_plot_calc_break_interval(-1:1)
e_plot_calc_break_interval <-
  function(
    values
  , num_intervals     = 10
  , val_leading_digit = c(1, 2, 5, 10)   # always end with 10 to assure a number is returned
  ) {

  # https://stackoverflow.com/questions/237220/tickmark-algorithm-for-a-graph-axis

  values_range  <- values %>% range() %>% diff()
  num_digits    <- values_range %>% log10() %>% floor()
  order_mag     <- 10 ^ num_digits

  for (i_val in val_leading_digit) {
    if (values_range / (order_mag / i_val) >= num_intervals) {
      break_interval <- order_mag / i_val
      return( break_interval )
    }
  }

  # this won't run if last val_leading_digit is 10
  break_interval <- order_mag / 10
  return( break_interval )

} # e_plot_calc_break_interval

