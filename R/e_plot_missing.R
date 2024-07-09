#' Plots missing data in a data.frame, possibly grouped by one variable and sorted by a second.
#'
#' @param dat_plot            data.frame or tibble
#' @param var_group           variable name to group by (colors data)
#' @param sw_group_sort       TRUE/FALSE to sort by grouped variable
#' @param var2_sort           second variable name to sort by if data is grouped
#' @param sw_title_data_name  TRUE/FALSE to include data object name in title or text string of title to use
#' @param sw_text_pct_miss    TRUE/FALSE to include text values of percent missing on bar plot
#'
#' @return                    ggplot grob plot object
#' @import dplyr
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @importFrom cowplot plot_grid
#' @importFrom tidyselect any_of
#' @export
#'
#' @examples
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
#'
#' e_plot_missing(
#'     dat_plot       = dat_mtcars_miss_e
#'   , var_group      = "cyl"
#'   , sw_group_sort  = TRUE
#'   , var2_sort      = "disp"
#'   )
#'
#' e_plot_missing(
#'     dat_plot       = dat_mtcars_miss_e
#'   , var_group      = "cyl"
#'   , sw_group_sort  = TRUE
#'   , var2_sort      = "disp"
#'   , sw_title_data_name  = "mtcars with random missing values"
#'   )
e_plot_missing <-
  function(
    dat_plot
  , var_group           = NULL
  , sw_group_sort       = FALSE
  , var2_sort           = NULL
  , sw_title_data_name  = TRUE
  , sw_text_pct_miss    = FALSE
  ) {
  ## DEBUG
  ## 6/17/2021
  # #dat_plot = dat_mtcars_e |> as_tibble(rownames = "Model")
  # dat_plot = dat_mtcars_e
  # n_missing = sample.int(n = prod(dim(dat_plot)), size = round( 0.10 * prod(dim(dat_plot))))
  # ind_missing = expand.grid(1:dim(dat_plot)[1], 1:dim(dat_plot)[2])[n_missing, ]
  # for (i_row in seq_along(n_missing)) {
  #   dat_plot[ind_missing[i_row,1], ind_missing[i_row,2] ] <- NA
  # }
  # var_group = "cyl"
  # #var_group = "cylx"
  # sw_group_sort = TRUE
  # var2_sort = "hp"

  dims <-
    c(
      rows = dim(dat_plot)[1]
    , cols = dim(dat_plot)[2]
    , vals = prod(dim(dat_plot))
    )

  # extract name of data.frame passed to function
  name_dat <- deparse(substitute(dat_plot))
  #print(name_dat)


  # If there are rownames, then create a separate variable
  if (any(is.na(as.numeric(rownames(dat_plot))))) {
    # if there are any non-numeric rownames, then process this
    sw_rowname_column <- TRUE
  } else {
    if (all(as.numeric(rownames(dat_plot)) == 1:nrow(dat_plot))) {
      # if all number in order, then they don't have rownames
      sw_rowname_column <- FALSE
    } else {
      # otherwise, some row names might be numbers in non-sequential order
      sw_rowname_column <- TRUE
    }
  }
  if (sw_rowname_column) {
    dat_plot <-
      dat_plot |>
      as_tibble(rownames = "ROWNAME")
  }

  names_col <- colnames(dat_plot)


  dat_plot <-
    dat_plot |>
    mutate(
      ID_MISSING___ = 1:n()
    ) |>
    dplyr::select(
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
    n_levels <- factor(dat_plot[[var_group]]) |> levels() |> length()
    if ((n_levels > nrow(dat_plot)/2) | (n_levels > 20) ) {
      warning("erikmisc::plot_missing() var_group variable has too many levels (>20 or > nrow/2), plotting without group.")
      sw_group <- FALSE
    }
  }
  if (sw_group) {
    # turn it into a factor variable (it might already be, that's ok)
    dat_plot[["GROUP___"]] <- factor(dat_plot[[var_group]])
  } else {
    dat_plot[["GROUP___"]] <- factor(1)
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
  ##     dat_all |>
  ##     dplyr::select(matches(match = "^Q[0-9]", perl = TRUE)) |>
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
      dat_plot <-
        dat_plot |>
        arrange(
          GROUP___, !!as.name(var2_sort)
        )
    } else {
      dat_plot <-
        dat_plot |>
        arrange(
          GROUP___
        )
    }
    # update ID number for sorted data
    dat_plot <-
      dat_plot |>
      mutate(
        ID_MISSING___ = 1:n()
      )
  }

  # Find the NAs
  dat_plot2 <-
    dat_plot |>
    is.na()

  n_missing <- sum(dat_plot2)

  # create a column indicating which rows all have data (no missing)
  NO_MISSING <-
    !(rowSums(!dat_plot2) == ncol(dat_plot2))

  dat_plot2 <-
    cbind(dat_plot2, NO_MISSING) |>
    as_tibble()

  # convert to numeric
  #cols_logical <- sapply(dat_plot2, is.logical)
  #dat_plot2[ ,cols_logical] <- lapply(dat_plot2[ ,cols_logical], as.numeric)

  dat_plot2 <-
    dat_plot2 |>
    as_tibble() |>
    mutate(
      ID_MISSING___ = dat_plot$ID_MISSING___
    , GROUP___      = dat_plot$GROUP___
    ) |>
    tidyr::pivot_longer(
      cols = tidyselect::any_of(c(names_col, "NO_MISSING"))
    ) |>
    mutate(
    #  value = value |> factor(levels = c(1, 0), labels = c("Missing", "Present"))
      value = value |> factor(levels = c(TRUE, FALSE), labels = c("Missing", "Present"))
    , name  = name |> factor(levels = c(names_col, "NO_MISSING"))
    )

  dat_barplot_missing <-
    dat_plot2 |>
    dplyr::group_by(
      name
    ) |>
    dplyr::summarize(
      prop_missing = sum(value == "Missing") / dplyr::n()
    ) |>
    dplyr::ungroup()

  #### OLD
  ## dat_plot2 <-
  ##   cbind(dat_plot2, NO_MISSING) |>
  ##   reshape2::melt()

  # bar plot of missing
  p1 <- ggplot2::ggplot(data = dat_barplot_missing, aes(x = name, y = prop_missing))
  p1 <- p1 + ggplot2::theme_bw()
  p1 <- p1 + ggplot2::theme(axis.title.x = element_blank(), axis.text.x = element_blank()) #, axis.ticks.x = element_blank())
  p1 <- p1 + ggplot2::geom_col(fill = "gray60")
  if (sw_text_pct_miss) {
    p1 <- p1 + ggplot2::geom_text(aes(label = paste0(100 * round(prop_missing, 2), "%"), y = 1), colour = "black", nudge_y = -0.2, hjust = 0.5) # size = 4,
  }
  p1 <- p1 + ggplot2::labs(y = "Missing %")
  p1 <- p1 + ggplot2::scale_y_continuous(labels = scales::label_percent(), breaks = seq(0, 1, by = 0.5), minor_breaks = seq(0, 1, by = 0.1), limits = c(0, 1))
  if (is.logical(sw_title_data_name)) {
    if (sw_title_data_name) {
      p1 <- p1 + labs(
                   title = paste0("Missing values: ", name_dat)
                 )
    } else {
      p1 <- p1 + labs(
                   title = "Missing values"
                 )
    }
  } else {
    p1 <- p1 + labs(
                 title = paste0("Missing values: ", sw_title_data_name)
               )
  }
  if (sw_group) {
    # p1 <- p1 + labs(
    #           fill = var_group
    #         )
    text_subtitle <- paste0("Grouped by ", var_group)

    if(sw_var2_sort) {
      text_subtitle <- paste0(text_subtitle, ", sorted by ", var2_sort)
    }

    p1 <- p1 + labs(
              subtitle = text_subtitle
            )
  }


  # indicate missing for each value
  if (sw_group) {
    p2 <- ggplot2::ggplot(data = dat_plot2, aes(x = name, y = ID_MISSING___, fill = GROUP___, alpha = value))
  } else {
    p2 <- ggplot2::ggplot(data = dat_plot2, aes(x = name, y = ID_MISSING___, alpha = value))
  }
  p2 <- p2 + ggplot2::geom_raster()  # , alpha = 0.6
  #p2 <- p2 + ggplot2::geom_raster(aes(fill = value))  # , alpha = 0.6
  #p2 <- p2 + ggplot2::scale_fill_grey(name = "", labels = c("Present", "Missing"))
  p2 <- p2 + ggplot2::theme_minimal()
  #p2 <- p2 + ggplot2::theme_bw()
  p2 <- p2 + ggplot2::theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_line())
  p2 <- p2 + ggplot2::theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust=0.0))
  #p2 <- p2 + ggplot2::theme(axis.text.x  = element_text(angle=-90, vjust=0, hjust=0))
  p2 <- p2 + ggplot2::labs(x = "Variables in Dataset", y = "Rows / observations")
  #ggplot2::scale_y_continuous(expand = c(0,0)) +
  #p2 <- p2 + ggplot2::scale_alpha_discrete(limits = c(0, 1), labels = c("Missing", "Present"))

  if (n_missing == 0) {
    p2 <- p2 + ggplot2::scale_alpha_discrete(limits = c(0, 1), labels = c("Missing", "Present"))
  }

  breaks_seq_by = e_plot_calc_break_interval(values = 1:nrow(dat_plot))

  #p2 <- p2 + ggplot2::scale_y_reverse(expand = c(0,0), breaks = c(1, seq(0, 10000, by=20)))
  p2 <- p2 + ggplot2::scale_y_reverse(expand = c(0,0), breaks = c(1, seq(0, nrow(dat_plot), by = breaks_seq_by)))

  p2 <- p2 + theme(legend.position = "bottom") # "none"
  p2 <- p2 + theme(plot.caption = element_text(hjust = 0))

  p2 <- p2 + labs(
            #  title = "Missing values"
              alpha = "Value is"
            , caption =
                paste0(
                  "Dimensions: "
                , dims["rows"], " rows"
                , " * "
                , dims["cols"], " columns"
                , " = "
                , dims["vals"], " values"
                , "\n"
                , "Missing values: "
                , n_missing
                , " / "
                , dims["vals"]  # prod(dim(dat_plot))
                , ",  "
                , 100 * round(n_missing / dims["vals"], 3)  # prod(dim(dat_plot))
                , " %;"
                , "    "
                , "Complete observations: "
                , sum(!NO_MISSING)
                , " / "
                , length(NO_MISSING)
                , ",  "
                , 100 * round(sum(!NO_MISSING) / length(NO_MISSING), 3)
                , " %"
                )
            )
  if (sw_group) {
    p2  <- p2  + labs(
              fill = var_group
            )
    # text_subtitle <- paste0("Grouped by ", var_group)
    #
    # if(sw_var2_sort) {
    #   text_subtitle <- paste0(text_subtitle, ", sorted by ", var2_sort)
    # }
    #
    # p2  <- p2  + labs(
    #           subtitle = text_subtitle
    #         )
  }

  p_arranged <-
    cowplot::plot_grid(
      p1
    , p2
    , ncol = 1
    , rel_heights = c(0.2, 0.8)
    , align = "v"
    )

  #print(p_arranged)


  return(p_arranged)
} # e_plot_missing
