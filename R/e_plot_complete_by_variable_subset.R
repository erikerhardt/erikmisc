#' For missing data, determine which sets of variables result in the most number of complete observations
#'
#' @param dat             data data.frame or tibble
#' @param var_list        list of variables, \code{NULL} for all
#' @param var_resp        \code{NULL} or one variable name to always be included (filters to keep only observations with this variable)
#' @param sw_n_complete   NULL for all, or an integer for the subsets with largest \code{n_complete}
#' @param sw_sel_subset   NULL for none, or one integer for the subset to highlight as the one selected for analysis
#' @param sw_plot_print   T/F, print plot in addition to returning the plot object?
#'
#' @return out      a list with the plot and a tibble with the \code{n_complete}, \code{n_var}, \code{var_names_print}, and a list of variable names in \code{var_names}
#' @importFrom stringr str_replace_all
#' @importFrom finalfit missing_pattern
#' @importFrom stats complete.cases
#' @importFrom patchwork plot_annotation
#' @import dplyr
#' @import tidyselect
#' @import tibble
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
#' # Plot missing data
#' dat_mtcars_miss_e |> e_plot_missing()
#'
#' out <- dat_mtcars_miss_e |> e_plot_complete_by_variable_subset()
#' # Print table
#' out$table |> print(n = Inf, width = Inf)
#' # Print variable names from first row
#' out$table$var_names[1] |> unlist()
#'
#' # Print top 5 subsets, select Subset 2
#' out <-
#'   dat_mtcars_miss_e |>
#'   e_plot_complete_by_variable_subset(
#'       sw_n_complete = 5
#'     , sw_sel_subset = 2
#'     , sw_plot_print = FALSE
#'     )
#' out
#'
#' # Print plot with modified title
#' out$plot +
#'   patchwork::plot_annotation(title = "New title for Missing data patterns plot", subtitle = NULL)
e_plot_complete_by_variable_subset <-
  function(
    dat
  , var_list      = NULL
  , var_resp      = NULL
  , sw_n_complete = NULL
  , sw_sel_subset = NULL
  , sw_plot_print = TRUE
  ) {
  ## dat = dat_mtcars_miss_e
  ## var_list  = NULL
  ## var_resp = "mpg"
  ## sw_n_complete  = NULL

  if (is.null(var_list)) {
    var_list <- dat |> names()
  }

  # move response variable to first position
  if (!is.null(var_resp)) {
    var_list <- c(var_list[which(var_list == var_resp)], var_list[-which(var_list == var_resp)])
  }

  var_list_blank <-
    var_list |>
    stringr::str_replace_all(
      pattern = "."
    , replacement = " "
    )

  dat_miss <-
    dat |>
    dplyr::select(
      tidyselect::all_of(var_list)
    )

  # restrict to where response is not missing
  if (!is.null(var_resp)) {
    dat_miss <-
      dat_miss |>
      dplyr::filter(
        !is.na(!!sym(var_resp))
      )
  }

  dat_miss_pattern <-
    dat_miss |>
    finalfit::missing_pattern(
      plot = FALSE
    )

  dim_dat_miss_pattern <-
    dat_miss_pattern |>
    dim()

  # reorder columns to match data
  dat_miss_pattern <-
    dat_miss_pattern[, var_list]

  # remove summary last row and last column
  #   When no missing values, do not drop the "extra" dimension!
  dat_miss_pattern <-
    dat_miss_pattern[-dim_dat_miss_pattern[1], -dim_dat_miss_pattern[2], drop = FALSE]

  dim_dat_miss_pattern <-
    dat_miss_pattern |>
    dim()

  dat_var_complete <-
    tibble::tibble(
      n_complete      = rep(NA, dim_dat_miss_pattern[1]) |> as.numeric()
    , n_var           = rep(NA, dim_dat_miss_pattern[1]) |> as.numeric()
    , var_names_print = rep(NA, dim_dat_miss_pattern[1]) |> as.character()
    , var_names       = rep(list(NA |> as.character()), dim_dat_miss_pattern[1])
    )

  for (i_row in seq_len(dim_dat_miss_pattern[1])) {
    ## i_row = 7

    var_names_this <-
      names(dat_miss)[dat_miss_pattern[i_row,] == 1]

    dat_var_complete$n_complete[i_row] <-
      dat_miss |>
      dplyr::select(
        tidyselect::all_of(var_names_this)
      ) |>
      stats::complete.cases() |>
      sum()
    dat_var_complete$n_var[i_row] <-
      var_names_this |>
      length()
    dat_var_complete$var_names[i_row] <-
      list(var_names_this)


    ind_var_1 <- which(dat_miss_pattern[i_row,] == 1)
    ind_var_0 <- which(dat_miss_pattern[i_row,] == 0)
    names(ind_var_0) <- var_list_blank[var_list %in% names(ind_var_0)]

    dat_var_complete$var_names_print[i_row] <-
      tibble::tibble(
        var =
          c(
            ind_var_1
          , ind_var_0
          ) |>
          names()
      , order =
          c(
            ind_var_1
          , ind_var_0
          )
      ) |>
      dplyr::arrange(
        order
      ) |>
      dplyr::pull(
        var
      ) |>
      paste(
        collapse = " "
      )
  } # i_row

  dat_var_complete <-
    dat_var_complete |>
    dplyr::arrange(
      dplyr::desc(n_complete)
    , dplyr::desc(n_var)
    ) |>
    dplyr::mutate(
      Subset__  = 1:dplyr::n()
    ) |>
    dplyr::relocate(
      Subset__
    )

  if (!is.null(sw_sel_subset)) {
    dat_var_complete <-
      dat_var_complete |>
      dplyr::mutate(
        Selected =
          dplyr::case_when(
            Subset__ == sw_sel_subset ~ TRUE
          , .default = FALSE
          )
      )
  }

  dat_var_complete_plot <-
    dat_var_complete |>
    dplyr::mutate(
      Subset__  = 1:dplyr::n()
    , Include__ = 1
    ) |>
    dplyr::select(
      -var_names_print
    , -tidyselect::any_of("Selected")
    ) |>
    tidyr::unnest(
      cols = "var_names"
    ) |>
    dplyr::mutate(
      var_names = var_names |> factor(levels = var_list)
    , Include__ = Include__ |> factor(levels = c(0, 1))
    , Subset__  = Subset__  |> factor(ordered = TRUE)
    ) |>
    dplyr::arrange(
      Subset__
    , var_names
    )

  # restrict to top n Subset__
  if (is.numeric(sw_n_complete)) {
    dat_var_complete_plot <-
      dat_var_complete_plot |>
      dplyr::filter(
        Subset__ <= sw_n_complete
      )
  } # if

  p1 <- ggplot(data = dat_var_complete_plot, aes(x = var_names, y = Subset__, fill = Include__))
  p1 <- p1 + theme_bw()
  p1 <- p1 + geom_tile(color = "grey50", fill = "gray20", na.rm = TRUE)
  if (!is.null(sw_sel_subset)) {
    p1 <- p1 + annotate("rect"
                , xmin = 0.5
                , xmax = length(unique(dat_var_complete_plot$var_names)) + 0.5
                , ymin = (max(as.numeric(as.character(dat_var_complete_plot$Subset__))) - sw_sel_subset + 1) - 0.5
                , ymax = (max(as.numeric(as.character(dat_var_complete_plot$Subset__))) - sw_sel_subset + 1) + 0.5
                , color = "red"
                , fill = NA
                , linewidth = 1.5
                , linetype = 1
                )
  }
  p1 <- p1 + scale_y_discrete(limits = rev)
  p1 <- p1 + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) # rotate labels
  p1 <- p1 + theme(plot.title.position = "plot") # move title to far left
  p1 <- p1 + labs(
                  # title = "Missing data patterns sorted by number of complete observations"
                  , x = "Variable names"
                  , y = "Subset"
                  )
  if (is.numeric(sw_n_complete)) {
    p1 <- p1 + labs(
                    subtitle = paste0("Top ", sw_n_complete, " subsets with largest n_complete")
                    )
  } # if

  p2 <- ggplot(data = dat_var_complete_plot |> dplyr::select(Subset__, n_complete) |> dplyr::distinct(), aes(x = Subset__, y = n_complete))
  p2 <- p2 + theme_bw()
  p2 <- p2 + geom_bar(stat = "identity", fill = "gray20")
  if (!is.null(sw_sel_subset)) {
    p2 <- p2 + annotate("rect"
                , xmin = (max(as.numeric(as.character(dat_var_complete_plot$Subset__))) - sw_sel_subset + 1) - 0.5
                , xmax = (max(as.numeric(as.character(dat_var_complete_plot$Subset__))) - sw_sel_subset + 1) + 0.5
                , ymin = 0
                , ymax = dat_var_complete_plot$n_complete[dat_var_complete_plot$Subset__ == sw_sel_subset][1]
                , color = "red"
                , fill = NA
                , linewidth = 1.5
                , linetype = 1
                )
  }
  p2 <- p2 + geom_text(aes(label = n_complete), y = 0, hjust = -.25, color = "gray80")
  p2 <- p2 + scale_x_discrete(limits = rev)
  #p2 <- p2 + scale_y_continuous(expand = expansion(mult = c(0.15, 0), add = 0))
  p2 <- p2 + coord_flip()
  p2 <- p2 + labs(
                    y = "n Complete observations"
                  , x = NULL #"Subset"
                  )

  p3 <- ggplot(data = dat_var_complete_plot |> dplyr::select(Subset__, n_var) |> dplyr::distinct(), aes(x = Subset__, y = n_var))
  p3 <- p3 + theme_bw()
  p3 <- p3 + geom_bar(stat = "identity", fill = "gray20")
  if (!is.null(sw_sel_subset)) {
    p3 <- p3 + annotate("rect"
                , xmin = (max(as.numeric(as.character(dat_var_complete_plot$Subset__))) - sw_sel_subset + 1) - 0.5
                , xmax = (max(as.numeric(as.character(dat_var_complete_plot$Subset__))) - sw_sel_subset + 1) + 0.5
                , ymin = 0
                , ymax = dat_var_complete_plot$n_var[dat_var_complete_plot$Subset__ == sw_sel_subset][1]
                , color = "red"
                , fill = NA
                , linewidth = 1.5
                , linetype = 1
                )
  }
  p3 <- p3 + geom_text(aes(label = n_var), y = 0, hjust = -.25, color = "gray80")
  p3 <- p3 + scale_x_discrete(limits = rev, position = "top")
  #p3 <- p3 + scale_y_continuous(expand = expansion(mult = c(0.15, 0), add = 0))
  p3 <- p3 + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
  p3 <- p3 + coord_flip()
  p3 <- p3 + labs(
                    y = "n Var"
                  , x = NULL #"Subset"
                  )

  p_arranged <-
    patchwork::wrap_plots(
      list(p1, p3, p2)
    , ncol        = NULL
    , nrow        = 1
    , byrow       = c(TRUE, FALSE)[1]
    , widths      = c(2, 0.25, 1)
    , heights     = NULL
    , guides      = c("collect", "keep", "auto")[1]
    , tag_level   = c("keep", "new")[1]
    , design      = NULL
    , axes        = NULL
    , axis_titles = c("keep", "collect", "collect_x", "collect_y")[4]
    )

  p_arranged <-
    p_arranged +
    patchwork::plot_annotation(
      title       = "Missing data patterns sorted by number of complete observations"
    , theme = theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1, Caption align left
    )

  if (!is.null(sw_sel_subset)) {
    p_arranged <-
      p_arranged +
      patchwork::plot_annotation(
        subtitle    = paste0("Selected Subset = ", sw_sel_subset)
      )
  }

  if (sw_plot_print) {
    print(p_arranged)
  }


  out <- list()
  out[[ "table" ]] <- dat_var_complete
  out[[ "plot"  ]] <- p_arranged
  #out[[ "plots" ]] <- list()
  #out[[ "plots" ]][[ "p_arranged" ]] <- p_arranged
  #out[[ "plots" ]][[ "components" ]] <-
  #  list(p1 = p1, p2 = p2, p3 = p3)

  return(out)
} # e_plot_complete_by_variable_subset
