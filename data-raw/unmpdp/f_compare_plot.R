#' ANOVA-style dot-plot comparing means
#'
#' @param dat_sub data subset
#' @param var_plot variables to plot against \code{surv_prog}
#' @param var_name plot title
#' @param comparisons NULL=no comparisons, "all"=all pairwise comparisons, or a list of comparisons to make, such as \code{list(c("MX", "SIVD"), c("AD", "SIVD"))}.
#'
#' @return
#' @import ggpubr
#' @import ggplot2
#' @export
#'
#' @examples
f_compare_plot <- function(dat_sub, var_plot, var_name = NULL, comparisons = c(NULL, "all", list())) {

  if(is.null(var_name)) {
    var_name <- var_plot
  }

  dat_plot <-
    dat_sub %>%
    select(surv_prog, var_plot)
  colnames(dat_plot)[2] <- "var"

  print(paste0(var_plot, " =================================================="))
  #library(ggpubr)
  # Perform pairwise comparisons
  dat_analy <- subset(dat_plot, is.finite(var) & !is.na(surv_prog))
  cm_result <- ggpubr::compare_means(var ~ surv_prog, data = dat_analy)
  print(cm_result)

  if (comparisons == "all") {
    # better ordering for plot of all comparisons
    list_group <- as.character(sort(unique(dat_sub$surv_prog)))
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
    #my_comparisons <- list(c("SIVD", "AD"), c("AD", "Control"), c("SIVD", "Control"))
    #my_comparisons <- list(c("SIVD", "Control"))
    # my_comparisons <- list(
    #                         c("MX", "SIVD")
    #                       , c("AD", "SIVD")
    #                       , c("AD", "MX")
    #                       , c("SIVD", "LA")
    #                       , c("MX", "LA")
    #                       , c("AD", "LA")
    #                       , c("SIVD", "Control")
    #                       , c("MX", "Control")
    #                       , c("AD", "Control")
    #                       , c("LA", "Control")
    #                       )
    my_comparisons <- comparisons
  }


  # we use the following convention for symbols indicating statistical significance:
  #   ns  : p >  0.05
  #   *   : p <= 0.05
  #   **  : p <= 0.01
  #   *** : p <= 0.001
  #   ****: p <= 0.0001

  symnum.args <-
    list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1)
       , symbols = c("****", "***", "**", "*", "ns")
    )

  library(ggpubr)
  p.pubr2 <- ggpubr::ggviolin(dat_analy
                , x = "surv_prog"
                , y = "var"
                #, notch = TRUE
                , width = 0.2
                , color = "gray80"
                , fill = "gray90"
                , add = c("dotplot")
                , add.params = list(color = "black")
                , ggtheme = theme_bw()
                )
  p.pubr2 <- p.pubr2 + geom_hline(yintercept = mean(dat_plot$var, na.rm = TRUE), linetype = 2, color = "gray50")
  p.pubr2 <- ggpubr::add_summary(p.pubr2, fun = "mean", size = 0.5, color = "red")
  p.pubr2 <- ggpubr::add_summary(p.pubr2, fun = "mean_ci", size = 0.75, color = "red", error.plot = "errorbar")
  p.pubr2 <- p.pubr2 + ggpubr::stat_compare_means(comparisons = my_comparisons   #, label = "p.signif") # Add pairwise comparisons p-value
                                                , symnum.args = symnum.args
                                                , hide.ns     = TRUE             # hide "ns" for non-sig comparisons
                                                )
  p.pubr2 <- p.pubr2 + ggpubr::stat_compare_means(label.y = min(dat_analy$var, na.rm = TRUE) - 0.2 * diff(range(dat_analy$var, na.rm = TRUE))
                                                , label.x = min(as.numeric(dat_sub$surv_prog)) + diff(range(unique(as.numeric(dat_sub$surv_prog)))) * 0.2
                                                , size = 3  # text size in plot
                                                )     # Add global p-value
  p.pubr2 <- ggpubr::ggpar(p.pubr2
                , main = var_name
                #, submain = "by Diagnosis with mean and 95% CI"
                , xlab = "Diagnosis"
                , ylab = var_plot
                )
  #print(p.pubr2)

  return(p.pubr2)
}
