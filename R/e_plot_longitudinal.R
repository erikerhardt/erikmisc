#' Longitudinal interaction plots
#'
#' @param dat_plot          data.frame for plotting
#' @param var_x_time        x variable, usually a "time" variable
#' @param var_y_resp        y response variable
#' @param var_ID            ID variable for each repeated measures subject
#' @param var_group         If there are groups, this is the group variable.
#' @param label_title       plot title, defaults to y-variable
#' @param label_subtitle    plot subtitle
#' @param label_x_time      x-axis label
#' @param label_y_resp      y-axis label
#' @param label_group       group label
#' @param x_scale_breaks    breaks for x-axis
#' @param y_scale_breaks    breaks for y-axis
#' @param sw_which_plots    which plots to include, a subset of \code{c("line", "hist")}
#' @param hist_scale_breaks breaks for histogram x-axis
#' @param sw_group_reverse  TRUE/FALSE to reverse the order of groups
#' @param hist_binwidth     histogram binwidth
#' @param hist_align        align "center" or "boundary"?  If numeric non-negative responses, usually "boundary" is preferred.
#' @param line_type_grand   Grand mean line type
#' @param line_type_group   Group mean line type
#'
#' @return plot in ggplot grob format
#' @importFrom labelled var_label
#' @importFrom cowplot plot_grid
#' @importFrom tidyselect any_of
#' @import ggplot2
#' @import dplyr
#' @export
#'
#' @examples
#' # numeric
#' e_plot_longitudinal(
#'     dat_plot         = lme4::sleepstudy
#'   , var_x_time       = "Days"
#'   , var_y_resp       = "Reaction"
#'   , var_ID           = "Subject"
#'   , var_group        = NULL
#'   , label_title      = NULL
#'   , label_subtitle   = "Longitudinal plot"
#'   , label_x_time     = "Number of days of sleep deprivation"
#'   , label_y_resp     = "Average reaction time (ms)"
#'   #, label_group      = "Program"
#'   , x_scale_breaks   = seq(0, 9, by = 2)
#'   , y_scale_breaks   = seq(0, 1000, by = 50)
#'   , sw_which_plots  = c("line", "hist")
#'   , hist_scale_breaks = NULL
#'   #, sw_group_reverse = TRUE
#'   , hist_binwidth    = 25
#'   , hist_align       = c("center", "boundary")[2]
#'   , line_type_grand  = c("blank","solid","dashed","dotted","dotdash","longdash","twodash")[6]
#'   , line_type_group  = c("blank","solid","dashed","dotted","dotdash","longdash","twodash")[4]
#'   )
#'
#' # numeric, only line plot
#' e_plot_longitudinal(
#'     dat_plot         = lme4::sleepstudy
#'   , var_x_time       = "Days"
#'   , var_y_resp       = "Reaction"
#'   , var_ID           = "Subject"
#'   , var_group        = NULL
#'   , label_title      = NULL
#'   , label_subtitle   = "Longitudinal plot"
#'   , label_x_time     = "Number of days of sleep deprivation"
#'   , label_y_resp     = "Average reaction time (ms)"
#'   #, label_group      = "Program"
#'   , x_scale_breaks   = seq(0, 9, by = 2)
#'   , y_scale_breaks   = seq(0, 1000, by = 50)
#'   , sw_which_plots  = c("line", "hist")[1]
#'   , hist_scale_breaks = NULL
#'   #, sw_group_reverse = TRUE
#'   , hist_binwidth    = 25
#'   , hist_align       = c("center", "boundary")[2]
#'   , line_type_grand  = c("blank","solid","dashed","dotted","dotdash","longdash","twodash")[6]
#'   , line_type_group  = c("blank","solid","dashed","dotted","dotdash","longdash","twodash")[4]
#'   )
#'
#' # categorical
#' e_plot_longitudinal(
#'     dat_plot         = lme4::sleepstudy |>
#'                          dplyr::mutate(Reaction_cat =
#'                                          cut(Reaction, breaks = seq(200, 400, by = 50))
#'                          )
#'   , var_x_time       = "Days"
#'   , var_y_resp       = "Reaction_cat"
#'   , var_ID           = "Subject"
#'   , var_group        = NULL
#'   , label_title      = NULL
#'   , label_subtitle   = "Longitudinal plot"
#'   , label_x_time     = "Number of days of sleep deprivation"
#'   , label_y_resp     = "Average reaction time (ms)"
#'   #, label_group      = "Program"
#'   , x_scale_breaks   = seq(0, 9, by = 2)
#'   , y_scale_breaks   = NULL
#'   , sw_which_plots  = c("line", "hist")
#'   , hist_scale_breaks = NULL
#'   #, sw_group_reverse = TRUE
#'   , hist_binwidth    = NULL
#'   , hist_align       = c("center", "boundary")[2]
#'   , line_type_grand  = c("blank","solid","dashed","dotted","dotdash","longdash","twodash")[6]
#'   , line_type_group  = c("blank","solid","dashed","dotted","dotdash","longdash","twodash")[4]
#'   )
e_plot_longitudinal <-
  function(
    dat_plot         = dat
  , var_x_time       = "x"
  , var_y_resp       = "y"
  , var_ID           = "ID"
  , var_group        = NULL
  , label_title      = NULL
  , label_subtitle   = "Longitudinal plot"
  , label_x_time     = NULL
  , label_y_resp     = NULL
  #, label_ID         = NULL
  , label_group      = NULL
  , x_scale_breaks   = NULL
  , y_scale_breaks   = NULL
  , sw_which_plots  = c("line", "hist")
  , hist_scale_breaks = NULL
  , sw_group_reverse = FALSE
  , hist_binwidth    = NULL
  , hist_align       = c("center", "boundary")[1]
  , line_type_grand  = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")[6]
  , line_type_group  = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")[4]
  ) {
  ## ## DEBUG
  ## library(ggplot2)
  ##
  ## dat_plot         = lme4::sleepstudy
  ## var_x_time       = "Days"
  ## var_y_resp       = "Reaction"
  ## var_ID           = "Subject"
  ## var_group        = NULL
  ## label_title      = "My title"
  ## label_subtitle   = "Longitudinal plot"
  ## label_x_time     = "Number of days of sleep deprivation"
  ## label_y_resp     = "Average reaction time (ms)"
  ##  label_group      = "Program"
  ## x_scale_breaks   = seq(0, 9, by = 2)
  ## y_scale_breaks   = seq(0, 1000, by = 100)
  ## sw_which_plots  = TRUE
  ## hist_scale_breaks = NULL
  ##  sw_group_reverse = TRUE
  ## hist_binwidth    = 25
  ## hist_align       = c("center", "boundary")[2]

  #### Categorical y-variable
  ## dat_plot$Reaction_cat <- cut(dat_plot$Reaction, breaks = seq(200, 400, by = 50))
  ## var_y_resp       = "Reaction_cat"


  # subset and rename variables
  if (!is.null(var_group)) {
    dat_plot <-
      dat_plot |>
      dplyr::select(
        tidyselect::any_of(
          c(
            var_ID
          , var_group
          , var_x_time
          , var_y_resp
          )
        )
      ) |>
      dplyr::rename(
        var_ID     = !!var_ID
      , var_group  = !!var_group
      , var_x_time = !!var_x_time
      , var_y_resp = !!var_y_resp
      ) |>
      dplyr::as_tibble()
  } else {
    dat_plot <-
      dat_plot |>
      dplyr::select(
        tidyselect::any_of(
          c(
            var_ID
          #, var_group
          , var_x_time
          , var_y_resp
          )
        )
      ) |>
      dplyr::rename(
        var_ID     = !!var_ID
      #, var_group  = !!var_group
      , var_x_time = !!var_x_time
      , var_y_resp = !!var_y_resp
      ) |>
      dplyr::as_tibble()
  }

  # label data
  # if data are already labelled, then use those labels
  # otherwise, this will override those labels
  if(!is.null(label_x_time)) {
    labelled::var_label(dat_plot[["var_x_time"]]) <- label_x_time
  }
  if(!is.null(label_y_resp)) {
    labelled::var_label(dat_plot[["var_y_resp"]]) <- label_y_resp
  }
  if(!is.null(label_group )) {
    labelled::var_label(dat_plot[["var_group" ]]) <- label_group
  }
  # if still no labels, then assign variable name
  if(is.null(labelled::var_label(dat_plot[["var_x_time"]]))) {
    labelled::var_label(dat_plot[["var_x_time"]]) <- var_x_time
  }
  if(is.null(labelled::var_label(dat_plot[["var_y_resp"]]))) {
    labelled::var_label(dat_plot[["var_y_resp"]]) <- var_y_resp
  }
  if(is.null(labelled::var_label(dat_plot[["var_group" ]]))) {
    labelled::var_label(dat_plot[["var_group" ]]) <- var_group
  }


  # if still no labels, then assign variable name
  if(is.null(labelled::var_label(dat_plot[["var_x_time"]]))) {
    labelled::var_label(dat_plot[["var_x_time"]]) <- var_x_time
  }
  if(is.null(labelled::var_label(dat_plot[["var_y_resp"]]))) {
    labelled::var_label(dat_plot[["var_y_resp"]]) <- var_y_resp
  }
  if(is.null(labelled::var_label(dat_plot[["var_group" ]]))) {
    labelled::var_label(dat_plot[["var_group" ]]) <- var_group
  }

  # title
  if(is.null(label_title)) {
    label_title <- labelled::var_label(dat_plot[["var_y_resp"]]) |> as.character()
  }
  #if(is.null(label_subtitle)) {
  #  label_subtitle <- "Longitudinal plot"
  #}

  # numeric or categorical?
  if (is.numeric(dat_plot$var_y_resp)) {
    sw_categorical <- FALSE
  } else {
    sw_categorical <- TRUE
  }


  if (!sw_categorical) {
    # grand mean over time
    annotate_y_mean <-
      dat_plot |>
      dplyr::pull(var_y_resp) |>
      mean(na.rm = TRUE)

    # mean for each group over time
    if (!is.null(var_group)) {
      annotate_y_group_means <-
        dat_plot |>
        dplyr::group_by(
          var_group
        ) |>
        dplyr::summarise(
          var_y_resp = mean(var_y_resp, na.rm = TRUE)
        , .groups = "drop"
        ) |>
        dplyr::ungroup()
    }

    # mean for each group at each time
    if (!is.null(var_group)) {
      annotate_y_time_group_means <-
        dat_plot |>
        dplyr::group_by(
          var_x_time
        , var_group
        ) |>
        dplyr::summarise(
          var_y_resp = mean(var_y_resp, na.rm = TRUE)
        , .groups = "drop"
        ) |>
        dplyr::ungroup()
    } else {
      annotate_y_time_group_means <-
        dat_plot |>
        dplyr::group_by(
          var_x_time
        #, var_group
        ) |>
        dplyr::summarise(
          var_y_resp = mean(var_y_resp, na.rm = TRUE)
        , .groups = "drop"
        ) |>
        dplyr::ungroup()
    }
  } # if !sw_categorical

  # Plot the data using ggplot

  if ("line" %in% sw_which_plots) {

    #library(ggplot2)
    #p1 <- ggplot(dat_plot, aes(x = redcap_event_name.factor, y = dkq_score_pat, colour = surv_prog.factor))
    if (!is.null(var_group)) {
      p1 <- ggplot(dat_plot, aes(x = var_x_time, y = var_y_resp, colour = var_group))
    } else {
      p1 <- ggplot(dat_plot, aes(x = var_x_time, y = var_y_resp))
    }
    p1 <- p1 + theme_bw()


    if (!sw_categorical) {
      # plot a reference line for the global mean (assuming no groups)
      #p1 <- p1 + geom_hline(aes(yintercept = 0), colour = "black", linetype = "solid", size = 0.2, alpha = 0.3)
      #p1 <- p1 + geom_hline(aes(yintercept =  0), alpha = 0.5)
      #p1 <- p1 + geom_hline(aes(yintercept = 23), alpha = 0.5)
      p1 <- p1 + geom_hline(aes(yintercept = annotate_y_mean), colour = "black", linetype = line_type_grand, size = 0.3, alpha = 0.5)

      if (!is.null(var_group)) {
        p1 <- p1 + geom_hline(data = annotate_y_group_means, aes(yintercept = var_y_resp, colour = var_group), linetype = line_type_group, size = 0.3, alpha = 1)
      }
    } # if !sw_categorical


    if (!sw_categorical) {
      # colored line for each patient
      if (!is.null(var_group)) {
        p1 <- p1 + geom_line(aes(group = var_ID, colour = var_group), alpha = 1/4)
      } else {
        p1 <- p1 + geom_line(aes(group = var_ID), alpha = 1/4)
      }
    } # if !sw_categorical
    if (sw_categorical) {
      # colored line for each patient
      if (!is.null(var_group)) {
        p1 <- p1 + geom_line(aes(group = var_ID, colour = var_group), alpha = 1/10, size = 3)
      } else {
        p1 <- p1 + geom_line(aes(group = var_ID), alpha = 1/10, size = 3)
      }
    } # if sw_categorical

    # boxplot, size=.75 to stand out behind CI
    #p1 <- p1 + geom_boxplot(size = 0.25, alpha = 0.5)
    # points for observed data
    #p1 <- p1 + geom_point(aes(colour = var_group), alpha = 0.5)


    if (!sw_categorical) {
      #p1 <- p1 + geom_smooth(aes(group = 1), method = mgcv::gam, size = 1, colour = "blue", se = FALSE)
      #p1 <- p1 + geom_smooth(aes(group = var_group, colour = var_group), size = 1, se = FALSE)
      if (!is.null(var_group)) {
        p1 <- p1 + geom_line(data = annotate_y_time_group_means, aes(group = var_group), size = 1.25)
      } else {
        p1 <- p1 + geom_line(data = annotate_y_time_group_means, group = 1, size = 1.25)
      }
    } # if !sw_categorical


    if (!sw_categorical) {
      # diamond at mean for each group
      p1 <- p1 + stat_summary(fun = mean, geom = "point", shape = 18, size = 4, alpha = 1)
      # confidence limits based on normal distribution
      #p1 <- p1 + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.2, alpha = 0.8)
      #p1 <- p1 + facet_grid(surv_prog ~ pci_part_id_ps, drop = TRUE)
      if(!is.null(y_scale_breaks)) {
        p1 <- p1 + scale_y_continuous(breaks = y_scale_breaks)
      }
    } # if !sw_categorical

    if(!is.null(x_scale_breaks)) {
      p1 <- p1 + scale_x_continuous(breaks = x_scale_breaks)
    }
    p1 <- p1 + labs(
                    title     = label_title
                  , subtitle  = label_subtitle
                  #, x         = labelled::var_label(dat_pdp[["redcap_event_name.factor"]]) |> as.character()
                  , x         = labelled::var_label(dat_plot[["var_x_time"]]) |> as.character()
                  , y         = labelled::var_label(dat_plot[["var_y_resp"]]) |> as.character()
                  #, caption = paste0(  "Power at a sample size = ", n_total, ":"
                  #                  , "\nObserved: ", dat_power_curve_long |> dplyr::filter(Sample_Size == n_total, Effect_Size == "Observed"    ) |> pull(Power) |> round(3)
                  #                  , ";  Cohen Large: ", dat_power_curve_long |> dplyr::filter(Sample_Size == n_total, Effect_Size == "Cohen Large" ) |> pull(Power) |> round(3)
                  #                  , ";  Cohen Medium: ", dat_power_curve_long |> dplyr::filter(Sample_Size == n_total, Effect_Size == "Cohen Medium") |> pull(Power) |> round(3)
                  #                  , ";  Cohen Small: ", dat_power_curve_long |> dplyr::filter(Sample_Size == n_total, Effect_Size == "Cohen Small" ) |> pull(Power) |> round(3)
                  #                   )
                  )

    if (!is.null(var_group)) {
      p1 <- p1 + labs(
                    colour    = labelled::var_label(dat_plot[["var_group" ]])
                  , shape     = labelled::var_label(dat_plot[["var_group" ]])
                  #, linetype  = "General Health"  #"Diagnosis"
                  #, fill      = "Diagnosis"
                  )
    }

    #p1 <- p1 + theme(legend.position = "bottom") # "none"
    #p1 <- p1 + theme(axis.text.x = element_text(angle = 15, vjust = 1, hjust = 1))
    if (!is.null(var_group)) {
      if(sw_group_reverse) {
        p1 <- p1 + guides(colour = guide_legend(reverse = TRUE))
      }
    }
    #print(p1)
  } # "line"

  if ("hist" %in% sw_which_plots) {
    # marginal histograms
    #library(ggplot2)
    if (!is.null(var_group)) {
      p2 <- ggplot(dat_plot, aes(x = var_y_resp, fill = var_group))
    } else {
      p2 <- ggplot(dat_plot, aes(x = var_y_resp))
    }
    p2 <- p2 + theme_bw()

    if (!sw_categorical) {
      # grand mean line
      p2 <- p2 + geom_vline(aes(xintercept = annotate_y_mean), colour = "black", linetype = line_type_grand, size = 0.5, alpha = 0.5)
    } # if !sw_categorical

    if (!sw_categorical) {
      # histogram
      if (hist_align == c("center", "boundary")[1]) {
        p2 <- p2 + geom_histogram(binwidth = hist_binwidth, alpha = 1, center = 0)
      }
      if (hist_align == c("center", "boundary")[2]) {
        p2 <- p2 + geom_histogram(binwidth = hist_binwidth, alpha = 1, boundary = 0)
      }

      # group mean lines
      if (!is.null(var_group)) {
        p2 <- p2 + geom_vline(data = annotate_y_time_group_means, aes(xintercept = var_y_resp), colour = "black", linetype = line_type_group, size = 0.5, alpha = 1)
      } else {
        p2 <- p2 + geom_vline(data = annotate_y_time_group_means, aes(xintercept = var_y_resp), colour = "black", linetype = line_type_group, size = 0.5, alpha = 1)
      }
      #p2 <- p2 + geom_line(data = annotate_y_time_group_means, size = 1.5)
    } # if !sw_categorical

    if (sw_categorical) {
      # bar plot
      p2 <- p2 + geom_bar(width = 0.8, alpha = 1)
    } # if sw_categorical


    # facets
    if (!is.null(var_group)) {
      if(!sw_group_reverse) {
        p2 <- p2 + facet_grid(var_group ~ var_x_time, drop = TRUE)
      } else {
        p2 <- p2 + facet_grid(reorder(var_group, desc(var_group)) ~ var_x_time, drop = TRUE)
      }
    } else {
      p2 <- p2 + facet_grid(. ~ var_x_time, drop = TRUE)
    }

    if (!sw_categorical) {
      if(!is.null(hist_scale_breaks)) {
        p2 <- p2 + scale_x_continuous(breaks = hist_scale_breaks)
      } else {
        ## https://gist.github.com/tomhopper/9076152
        # two most extreme breaks
        x_two_breaks <- ggplot_build(p2)$layout$panel_params[[1]]$x$breaks
        x_two_breaks <- range(x_two_breaks, na.rm = TRUE)
        p2 <- p2 + scale_x_continuous(breaks = x_two_breaks)
      }
    } # if !sw_categorical


    p2 <- p2 + labs(
                    title     = NULL
                  , subtitle  = NULL
                  , x         = labelled::var_label(dat_plot[["var_y_resp"]]) |> as.character()
                  )

    if (!is.null(var_group)) {
      p2 <- p2 + theme(legend.position = "none") # "none"
    }
    #print(p2)
  } # "hist"


  if (all(c("line", "hist") %in% sw_which_plots)) {

    p_arranged <-
      cowplot::plot_grid(
        plotlist = list(p1, p2)
      , nrow = NULL
      , ncol = 1
      , rel_heights = c(2, 1)
      )

    return(p_arranged)
  }

  if (sw_which_plots == "line") {
    return(p1)
  }
  if (sw_which_plots == "hist") {
    return(p2)
  }

} # e_plot_longitudinal

