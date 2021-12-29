#' ANOVA-style dot-plot comparing means
#'
#' Some functions won't work without running \code{library(ggpubr)}.
#'
#' @param dat_plot            Data.frame
#' @param var_response        Variable name of response (y) (as string, "var_name")
#' @param var_groups          Variable name of groups (x) (as string, "var_name")
#' @param label_response      Label for y-axis
#' @param label_groups        Label for x-axis
#' @param plot_title          Plot title
#' @param plot_subtitle       Plot subtitle
#' @param comparisons         NULL=no comparisons, "all"=all pairwise comparisons, or a list of comparisons to make, such as \code{list(c("A", "B"), c("A", "C"))}.
#' @param cm_method           Compare method: "wilcox.test" for nonparametric, "t.test" for parametric
#' @param cm_p.adjust.method  Compare method: p-value adjust method "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
#' @param cm_dot              Compare method: red center estimate "mean", "median"
#' @param cm_fun              Compare method: red error bar range "mean", "mean_se", "mean_sd", "mean_ci", "mean_range", "median", "median_iqr", "median_hilow", "median_q1q3", "median_mad", "median_range"
#' @param cm_error.plot       Compare method: red error bar style "pointrange", "linerange", "crossbar", "errorbar", "upper_errorbar", "lower_errorbar", "upper_pointrange", "lower_pointrange", "upper_linerange", "lower_linerange"
#' @param sw_add              Add a second plot to the violin plot: "none", "dotplot", "jitter", "boxplot", "point"
#' @param sw_caption_desc     T/F for caption describing red summary center and spread
#' @param symnum.args         List of cutpoints and symbols for significance
#'
#' @return p_pub              \code{p_pub$result_compare_means} has pairwise comparison table.  \code{p_pub} is a ggplot plot grob.
#' @import dplyr
#' @import ggpubr
#' @import ggplot2
#' @export
#'
#' @examples
#' # medians
#' e_plot_compare_medians(
#'     dat_plot            = dat_mtcars_e
#'   , var_response        = "mpg"
#'   , var_groups          = "cyl"
#'   , label_response      = labelled::var_label(dat_mtcars_e$mpg)
#'   , label_groups        = labelled::var_label(dat_mtcars_e$cyl)
#'   , plot_title          = "MPG by cylinders"
#'   , plot_subtitle       = "Comparison of medians"
#'   , comparisons         = "all"
#'   , cm_method           = "wilcox.test"
#'   , cm_p.adjust.method  = "holm"
#'   , cm_dot              = "median"
#'   , cm_fun              = "median_iqr"
#'   , cm_error.plot       = "errorbar"
#'   , sw_add              = "dotplot"
#'   , sw_caption_desc     = TRUE
#'   )
#'
#' # means
#' e_plot_compare_medians(
#'     dat_plot            = dat_mtcars_e
#'   , var_response        = "mpg"
#'   , var_groups          = "cyl"
#'   , label_response      = labelled::var_label(dat_mtcars_e$mpg)
#'   , label_groups        = labelled::var_label(dat_mtcars_e$cyl)
#'   , plot_title          = "MPG by cylinders"
#'   , plot_subtitle       = "Comparison of means"
#'   , comparisons         = "all"
#'   , cm_method           = "t.test"
#'   , cm_p.adjust.method  = "holm"
#'   , cm_dot              = "mean"
#'   , cm_fun              = "mean_ci"
#'   , cm_error.plot       = "errorbar"
#'   , sw_add              = "boxplot"
#'   , sw_caption_desc     = FALSE
#'   )
#'
e_plot_compare_medians <-
  function(
    dat_plot
  , var_response        = NULL
  , var_groups          = NULL
  , label_response      = NULL
  , label_groups        = NULL
  , plot_title          = NULL
  , plot_subtitle       = NULL
  , comparisons         = c("all", list(), NULL)[1]
  , cm_method           = c("wilcox.test", "t.test")[1]
  , cm_p.adjust.method  = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")[1]
  , cm_dot              = c("mean", "median")[2]
  , cm_fun              = c("mean", "mean_se", "mean_sd", "mean_ci", "mean_range", "median", "median_iqr", "median_hilow", "median_q1q3", "median_mad", "median_range")[7]
  , cm_error.plot       = c("pointrange", "linerange", "crossbar", "errorbar", "upper_errorbar", "lower_errorbar", "upper_pointrange", "lower_pointrange", "upper_linerange", "lower_linerange")[4]
  , sw_add              = c("none", "dotplot", "jitter", "boxplot", "point")[2]
  , sw_caption_desc     = c(TRUE, FALSE)[1]
  , symnum.args         = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.10, 1), symbols = c("****", "***", "**", "*", "-", "ns"))
  ) {

  if (is.null(label_response)) {
    label_response = var_response
  }
  if (is.null(label_groups)) {
    label_groups = var_groups
  }

  # select columns
  dat_plot <-
    dat_plot %>%
    dplyr::select(
      dplyr::all_of(var_response)
    , dplyr::all_of(var_groups)
    )
  # rename for convenience
  colnames(dat_plot)[1] <- "response"
  colnames(dat_plot)[2] <- "groups"
  # assure finite and not NA
  dat_plot <-
    dat_plot %>%
    dplyr::filter(
      is.finite(response)
    , !is.na(response)
    , !is.na(groups)
    )

  # Perform pairwise comparisons
  result_compare_means <-
    ggpubr::compare_means(
      formula         = response ~ groups
    , data            = dat_plot
    , method          = cm_method
    , p.adjust.method = cm_p.adjust.method
    )
  #print(result_compare_means)

  if (comparisons == "all") {
    # better ordering for plot of all comparisons
    list_group <- as.character(sort(unique(dat_plot$groups)))
    my_comparisons <- list()
    i_list <- 0
    for (i_gap in 1:(length(list_group) - 1)) {
      for (i_ind in 1:(length(list_group) - i_gap)) {
        i_list <- i_list + 1
        my_comparisons[[i_list]] <- c(list_group[i_ind], list_group[i_ind + i_gap])
      }
    }
  }
  if(class(comparisons) == "list") {
    # Visualize: Specify the comparisons you want
    # my_comparisons <-
    #   list(
    #     c("MX", "SIVD")
    #   , c("AD", "SIVD")
    #   , c("AD", "MX")
    #   , c("SIVD", "LA")
    #   , c("MX", "LA")
    #   , c("AD", "LA")
    #   , c("SIVD", "Control")
    #   , c("MX", "Control")
    #   , c("AD", "Control")
    #   , c("LA", "Control")
    #   )
    my_comparisons <- comparisons
  }


  # we use the following convention for symbols indicating statistical significance:
  #   ns  : p >  0.05
  #   -   : p <= 0.10
  #   *   : p <= 0.05
  #   **  : p <= 0.01
  #   *** : p <= 0.001
  #   ****: p <= 0.0001
  #
  # symnum.args <-
  #   list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.10, 1)
  #      , symbols = c("****", "***", "**", "*", "-", "ns")
  #   )

  #library(ggpubr)
  p_pub <- ggpubr::ggviolin(
                  data        = dat_plot
                , x           = "groups"
                , y           = "response"
                #, notch      = TRUE
                , size        = 0.8
                , width       = 1
                , color       = "gray80"
                , fill        = "gray90"
                , add         = sw_add
                , add.params  = list(color = "black")
                , ggtheme     = ggplot2::theme_bw()
                )
  p_pub <- p_pub + ggplot2::geom_hline(yintercept = mean(dat_plot$response, na.rm = TRUE), linetype = 2, color = "gray50")
  p_pub <- ggpubr::add_summary(p_pub, cm_dot, size = 0.25, color = "red", shape = 3)
  #p_pub <- ggpubr::add_summary(p_pub, "mean_ci")
  p_pub <- ggpubr::add_summary(p_pub, cm_fun, size = 0.75, width = 0.15, color = "red", error.plot = cm_error.plot)
  p_pub <- p_pub + ggpubr::stat_compare_means(
                      comparisons = my_comparisons   #, label = "p.signif") # Add pairwise comparisons p-value
                    , symnum.args = symnum.args
                    , hide.ns     = TRUE             # hide "ns" for non-sig comparisons
                    )

  # Add global p-value
  if (cm_method == "t.test") {
    cm_method_omnibus = "anova"
  } else {
    cm_method_omnibus = "kruskal.test"
  }
  p_pub <- p_pub + ggpubr::stat_compare_means(
                      method  = cm_method_omnibus
                    , label.y = min(dat_plot$response, na.rm = TRUE)    # - 0.1 * diff(range(dat_plot$response, na.rm = TRUE))
                    , label.x = min(as.numeric(dat_plot$groups))        # + diff(range(unique(as.numeric(dat_plot$groups)))) * 0.2
                    , vjust   = 2
                    , hjust   = +0.2
                    , size    = 3  # text size in plot
                    )

  if (sw_caption_desc) {
    text_caption <-
      paste0(
        "Red is ", cm_dot, ", ", cm_fun, " ", cm_error.plot
      )
  } else {
    text_caption <- NULL
  }

  p_pub <- ggpubr::ggpar(
                  p_pub
                , main    = plot_title
                , submain = plot_subtitle
                , ylab    = label_response
                , xlab    = label_groups
                , caption = text_caption
                )
  #print(p_pub)

  p_pub$result_compare_means <-
    result_compare_means

  return(p_pub)
}
