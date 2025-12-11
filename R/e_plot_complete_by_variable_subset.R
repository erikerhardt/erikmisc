#' For missing data, determine which sets of variables result in the most number of complete observations
#'
#' @param dat       data data.frame or tibble
#' @param var_list  list of variables, \code{NULL} for all
#' @param var_resp  \code{NULL} or one variable name to always be included (filters to keep only observations with this variable)
#' @param sw_plot_complete  \code{TRUE} for all, or an integer for the subsets with largest \code{n_complete}
#'
#' @return out      a tibble with the \code{n_complete}, \code{n_var}, \code{var_names_print}, and a list of variable names in \code{var_names}
#' @importFrom stringr str_replace_all
#' @importFrom finalfit missing_pattern
#' @importFrom stats complete.cases
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
#' out |> print(n = Inf, width = Inf)
#' # Print variable names from first row
#' out$var_names[1] |> unlist()
#'
#' # Print top 5 subsets
#' dat_mtcars_miss_e |> e_plot_complete_by_variable_subset(sw_plot_complete = 5)
e_plot_complete_by_variable_subset <-
  function(
    dat
  , var_list          = NULL
  , var_resp          = NULL
  , sw_plot_complete  = TRUE
  ) {
  ## dat = dat_mtcars_miss_e
  ## var_list  = NULL
  ## var_resp = "mpg"
  ## sw_plot_complete  = TRUE

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
    )

  if (sw_plot_complete) {
    dat_var_complete_plot <-
      dat_var_complete |>
      dplyr::mutate(
        Subset__  = 1:dplyr::n()
      , Include__ = 1
      ) |>
      dplyr::select(
        -var_names_print
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
    if (is.numeric(sw_plot_complete)) {
      dat_var_complete_plot <-
        dat_var_complete_plot |>
        dplyr::filter(
          Subset__ <= sw_plot_complete
        )
    } # if

    p1 <- ggplot(data = dat_var_complete_plot, aes(x = var_names, y = Subset__, fill = Include__))
    p1 <- p1 + theme_bw()
    p1 <- p1 + geom_tile(color = "grey50", fill = "gray20", na.rm = TRUE)
    p1 <- p1 + scale_y_discrete(limits = rev)
    p1 <- p1 + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) # rotate labels

    p1 <- p1 + labs(
                    title = "Missing data patterns sorted by number of complete observations"
                    , x = "Variable names"
                    , y = "Subset"
                    )
    if (is.numeric(sw_plot_complete)) {
      p1 <- p1 + labs(
                      subtitle = paste0("Top ", sw_plot_complete, " subsets with largest n_complete")
                      )
    } # if


    p2 <- ggplot(data = dat_var_complete_plot |> dplyr::select(Subset__, n_complete) |> dplyr::distinct(), aes(x = Subset__, y = n_complete))
    p2 <- p2 + theme_bw()
    p2 <- p2 + geom_bar(stat = "identity", fill = "gray20")
    p2 <- p2 + geom_text(aes(label = n_complete), y = 0, hjust = -.25, color = "gray80")
    p2 <- p2 + scale_x_discrete(limits = rev)
    #p2 <- p2 + scale_y_continuous(expand = expansion(mult = c(0.15, 0), add = 0))
    p2 <- p2 + coord_flip()
    p2 <- p2 + labs(
                      y = "n Complete observations"
                    , x = "Subset"
                    )

    p_arranged <-
      patchwork::wrap_plots(
        list(p1, p2)
      , ncol        = NULL
      , nrow        = 1
      , byrow       = c(TRUE, FALSE)[1]
      , widths      = c(2, 1)
      , heights     = NULL
      , guides      = c("collect", "keep", "auto")[1]
      , tag_level   = c("keep", "new")[1]
      , design      = NULL
      , axes        = NULL
      , axis_titles = c("keep", "collect", "collect_x", "collect_y")[4]
      )

    print(p_arranged)

  } # sw_plot_complete

  return(dat_var_complete)
} # e_plot_complete_by_variable_subset
