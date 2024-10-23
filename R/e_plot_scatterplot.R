#' Plot scatterplot of a numeric y-variable against a numeric or categorical x-variable
#'
#' Can add up to one color factor or numeric variable and two facets factor variables, and choose to display correlation.
#'
#' @param dat_plot                  data to plot
#' @param var_x                     a numeric or categorical variable name
#' @param var_y                     a numeric variable name
#' @param var_color                 factor or numeric color variable
#' @param var_facet                 factor varibles (1 or 2) to facet by, (row facets, then column facets),
#' @param text_title                text for title of plot to override default
#' @param sw_print                  T/F whether to print table and display plot
#' @param smooth_all                Smooth method for all the points together.
#' @param sw_smooth_all_se          T/F, SEs for smooth all the points together?
#' @param smooth_by_var_color       Smooth method by the color variable.
#' @param sw_smooth_by_var_color_se T/F, SEs for smooth by the color variable?
#' @param sw_corr_in_subtitle       T/F, calculate correlation and include in subtitle of plot?
#'
#' @return          a ggplot object
#' @importFrom stats cor.test
#' @import ggplot2
#' @export
#'
#' @examples
#' e_plot_scatterplot(
#'     dat_plot  = dat_mtcars_e
#'   , var_x     = "disp"           # numeric x
#'   , var_y     = "mpg"
#'   )
#' e_plot_scatterplot(
#'     dat_plot  = dat_mtcars_e
#'   , var_x     = "am"             # factor x
#'   , var_y     = "mpg"
#'   )
#' e_plot_scatterplot(
#'     dat_plot  = dat_mtcars_e
#'   , var_x     = "disp"
#'   , var_y     = "mpg"
#'   , var_color = "cyl"            # factor color
#'   )
#' e_plot_scatterplot(
#'     dat_plot  = dat_mtcars_e
#'   , var_x     = "disp"
#'   , var_y     = "mpg"
#'   , var_color = "cyl"            # factor color
#'   , smooth_all = c("none", "loess", "lm", "glm", "gam")[2]  # with all smooth
#'   )
#' e_plot_scatterplot(
#'     dat_plot  = dat_mtcars_e
#'   , var_x     = "disp"
#'   , var_y     = "mpg"
#'   , var_color = "cyl"            # factor color
#'   , smooth_by_var_color = c("none", "loess", "lm", "glm", "gam")[3]  # with group smooth
#'   )
#' e_plot_scatterplot(
#'     dat_plot  = dat_mtcars_e
#'   , var_x     = "disp"
#'   , var_y     = "mpg"
#'   , var_color = "wt"             # numeric color
#'   )
#' e_plot_scatterplot(
#'     dat_plot  = dat_mtcars_e
#'   , var_x     = "disp"
#'   , var_y     = "mpg"
#'   , var_facet = c("vs")          # one facet
#'   )
#' e_plot_scatterplot(
#'     dat_plot  = dat_mtcars_e
#'   , var_x     = "disp"
#'   , var_y     = "mpg"
#'   , var_color = "cyl"
#'   , var_facet = c("vs", "am")    # two facets
#'   )
#' e_plot_scatterplot(
#'     dat_plot  = dat_mtcars_e
#'   , var_x     = "am"             # factor x
#'   , var_y     = "mpg"
#'   , var_color = "wt"             # numeric color
#'   , var_facet = c("vs")
#'   )
#' e_plot_scatterplot(
#'     dat_plot  = dat_mtcars_e
#'   , var_x     = "am"             # factor x
#'   , var_y     = "mpg"
#'   , var_color = "vs"             # factor color
#'   )
e_plot_scatterplot <-
  function(
    dat_plot                  = NULL
  , var_x                     = NULL
  , var_y                     = NULL
  , var_color                 = NULL
  , var_facet                 = NULL
  , text_title                = NULL
  , sw_print                  = FALSE
  , smooth_all                = c("none", "loess", "lm", "glm", "gam")[1]
  , sw_smooth_all_se          = c(TRUE, FALSE)[1]
  , smooth_by_var_color       = c("none", "loess", "lm", "glm", "gam")[1]
  , sw_smooth_by_var_color_se = c(TRUE, FALSE)[1]
  , sw_corr_in_subtitle       = c(TRUE, FALSE)[1]
  ) {

  if (is.null(text_title)) {
    text_title <- paste0(var_y, " vs ", var_x)
  }

  if (sw_corr_in_subtitle &
      (inherits(dat_plot[[ var_x ]], "numeric") | inherits(dat_plot[[ var_x ]], "integer")) &
      !(var_x == var_y) &
      !all(is.na(dat_plot[[ var_x ]]))
      ) {
    if (var_y == var_x) {
      text_subtitle <- NULL
    } else {

      formula_cor <-
        paste0(
          " ~ "
        , var_y
        , " + "
        , var_x
        ) |>
        as.formula()
      cor_p <-
        stats::cor.test(
          formula     = formula_cor
        , data        = dat_plot
        , alternative = "two.sided"
        , method      = c("pearson", "kendall", "spearman")[1]
        )
      cor_s <-
        suppressWarnings(
          stats::cor.test(
            formula     = formula_cor
          , data        = dat_plot
          , alternative = "two.sided"
          , method      = c("pearson", "kendall", "spearman")[3]
          )
        )

      text_subtitle <-
        paste0(
        #  "Correlation:  "
          "Pearson r = "
        , cor_p$estimate |> signif(3)
        , ", p-val = "
        , cor_p$p.value  |> signif(3)
        #, " "
        #, cor_p$p.value  |> e_pval_stars()
        #, ";  "
        , "\n"
        , "Spearman r = "
        , cor_s$estimate |> signif(3)
        , ", p-val = "
        , cor_s$p.value  |> signif(3)
        #, " "
        #, cor_p$p.value  |> e_pval_stars()
        )
    }

  } else {
    text_subtitle <- NULL
  }

  position_dodge_val <- position_dodge(0)

  if (is.null(var_color)) {
    p <- ggplot(dat_plot, aes_string(x = var_x, y = var_y))
  } else {
    if (inherits(dat_plot[[ var_color ]], "numeric") | inherits(dat_plot[[ var_color ]], "integer")) {
      p <- ggplot(dat_plot, aes_string(x = var_x, y = var_y, color = var_color))
    } else {
      p <- ggplot(dat_plot, aes_string(x = var_x, y = var_y, color = var_color, shape = var_color))
      position_dodge_val <- position_dodge(0.6) # move them .05 to the left and right
    }

    if (is.null(text_title)) {
      text_title <- paste0(text_title, " by ", var_color)
    }
  }
  p <- p + theme_bw()

  if (inherits(dat_plot[[ var_x ]], "numeric") | inherits(dat_plot[[ var_x ]], "integer")) {
    p <- p + geom_point()
    p <- p + stat_ellipse(aes(color = NULL, shape = NULL), type = "t", segments = 101, color = "black", linetype = 2, alpha = 0.5)  # , level = 0.80
  } else {
    # plot a reference line for the global mean (assuming no groups)
    p <- p + geom_hline(yintercept = mean(dat_plot[[ var_y ]]),
                        color = "black", linetype = "dashed", size = 0.3, alpha = 0.5)
    # boxplot, size=.75 to stand out behind CI
    p <- p + geom_violin(width = 0.2, alpha = 0.25, position = position_dodge_val)
    p <- p + geom_boxplot(width = 0.2, alpha = 0.25, position = position_dodge_val)
    # points for observed data
    p <- p + geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 1) #, position = position_dodge_val)
    # diamond at mean for each group
    p <- p + stat_summary(fun = mean, geom = "point", shape = 18, size = 5,
                          alpha = 0.8, position = position_dodge_val) # color = "red",
    # confidence limits based on normal distribution
    p <- p + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",
                          width = 0.2, alpha = 0.8, position = position_dodge_val) # color = "red",
  }


  if (!is.null(var_facet)) {
    if (length(var_facet) == 1) {
      p <- p + facet_grid(as.formula(paste0(". ~ ", var_facet[1])), scales = "free_y", drop = TRUE)

      if (is.null(text_title)) {
        text_title <- paste0(text_title, ", by ", var_facet[1])
      }
    }
    if (length(var_facet) == 2) {
      p <- p + facet_grid(as.formula(paste0(var_facet[2], " ~ ", var_facet[1])), scales = "free_y", drop = TRUE)

      if (is.null(text_title)) {
        text_title <- paste0(text_title, ", by ", var_facet[1], " and ", var_facet[2])
      }
    }
  }

  if (smooth_all %in% c("loess", "lm", "glm", "gam")) {
    p <- p + stat_smooth(method = smooth_all, aes(group = 1), color = "black", alpha = 1/6, se = sw_smooth_all_se)
  }
  if (smooth_by_var_color %in% c("loess", "lm", "glm", "gam")) {
    p <- p + stat_smooth(method = smooth_by_var_color, aes_string(fill = var_color), alpha = 1/8, se = sw_smooth_by_var_color_se)
  }

  p <- p + labs(
                  title     = text_title
                , subtitle  = text_subtitle
                )
  if( !is.null(labelled::var_label(dat_plot[[ var_x ]])) ) {
    p <- p + labs(
                    x         = labelled::var_label(dat_plot[[ var_x ]])
                  )
  }
  if( !is.null(labelled::var_label(dat_plot[[ var_y ]])) ) {
    p <- p + labs(
                    y         = labelled::var_label(dat_plot[[ var_y ]])
                  )
  }

  if (sw_corr_in_subtitle) {
    p <- p + theme(plot.subtitle = element_text(hjust = 1))
  }
  #p <- p + theme(legend.position = "bottom") # "none"

  if (sw_print) {
    print(p)
  }


  if (sw_corr_in_subtitle &
      (inherits(dat_plot[[ var_x ]], "numeric") | inherits(dat_plot[[ var_x ]], "integer")) &
      !(var_x == var_y) &
      !all(is.na(dat_plot[[ var_x ]]))
      ) {
    p$e_correlation       <- list()
    p$e_correlation$cor_p <- cor_p
    p$e_correlation$cor_s <- cor_s
  }

  return(p)
} # e_plot_scatterplot

