#' A function to calculate ROC curve, calculate area under the curve (AUC), calculate the optimal threshold, plot the curve, and classify the peaks.
#'
#' @param dat
#' @param var_class
#' @param val.feat
#' @param sw.plot
#' @param method.name
#'
#' @return
#' @importFrom ROCR prediction
#' @importFrom ROCR performance
#' @import ggplot2
#' @export
#'
#' @examples
f_roc_gg <-
  function(
    dat
  , var_class
  , val.feat
  , sw.plot = TRUE
  , method.name = ""
  ) {
  ## Plotting a pROC object with ggplot2
  ## https://gist.github.com/copsacgist/6d8f4eb096e4f18a0894ca1ce27af834

  # need only 2 levels for ROCR functions
  if ((dat[[var_class]] |> levels() |> length()) > 2) {
    out <-
      list(
        opt_t      = NULL
      ## debugging order permutation issue
      #, dat        = dat
      , val.feat   = NULL
      , good_class = NULL
      , p_roc      = ggplot() + theme_void() + geom_text(aes(0,0,label="ROC N/A for >2 groups")) + xlab(NULL)
      , roc_dat    = NULL
      )
    return(out)
  }

  #library(ROCR)
  pred <- ROCR::prediction(predictions = as.numeric(val.feat[,1]), labels = dat[[var_class]])
  perf <- ROCR::performance(pred, measure = "tpr", x.measure = "fpr")

  # determine the best threshold as having the highest overall classification rate
  # Find t that minimizes error
  roc.curve <- data.frame(Spec = 1 - unlist(perf@x.values)
                        , Sens = unlist(perf@y.values)
                        , thresh = unlist(perf@alpha.values))
  roc.curve$dist <- sqrt((1 - roc.curve$Sens)^2 + (1 - roc.curve$Spec)^2)
  roc.curve <- subset(roc.curve, is.finite(roc.curve$thresh))
  opt_t <- subset(roc.curve, roc.curve$dist == min(roc.curve$dist))
  opt_t

  perf.auc <- unlist(ROCR::performance(pred, measure = "auc")@y.values)
  opt_t$AUC <- perf.auc

  roc.curve.best <-
    roc.curve |>
    filter(
      dist == min(dist)
    ) |>
    mutate(
      AUC = opt_t$AUC
    )


  # define peak using optimal threshold
  good_class <- ifelse(as.numeric(val.feat[,1]) >= opt_t$thresh, 1, 0)

  # assess confusion matrix accuracy
  cm.feat <-
    f_cm(
      classification  = good_class
    , reference       = as.numeric(dat[[var_class]] == colnames(val.feat)[1])
    , sw.echo         = FALSE
    )

  opt_t$BalAccuracy <- cm.feat$byClass["Balanced Accuracy"]

  # plot results
  if (sw.plot) {

    interval <- 0.2
    breaks   <- seq(0, 1, interval)

    library(ggplot2)
    p <- ggplot(roc.curve, aes(x = Spec, y = Sens))
    p <- p + theme_bw()
    #p <- p + theme(axis.ticks = element_line(color = "grey80"))
    p <- p + geom_segment(aes(x = 0, y = 1, xend = 1, yend = 0), alpha = 0.25, linetype = 3)
    p <- p + geom_step(size = 1) # aes(colour = Method, linetype = Method),
    p <- p + scale_x_reverse   (name = "Specificity", limits = c(1,0), breaks = breaks, expand = c(0.001, 0.001))
    p <- p + scale_y_continuous(name = "Sensitivity", limits = c(0,1), breaks = breaks, expand = c(0.001, 0.001))
    # manual scale labels
    #p <- p + scale_colour_discrete(name = "Method AUC",
    #                               breaks = c("RF", "LR", "EFA", "SIVDS"),
    #                               labels = roc.auc.labels)
    #p <- p + scale_linetype_discrete(name = "Method AUC",
    #                               breaks = c("RF", "LR", "EFA", "SIVDS"),
    #                               labels = roc.auc.labels)

      # optim values
      p <- p + geom_point(aes(x = roc.curve.best$Spec, y = roc.curve.best$Sens), shape = 21, size = 3, alpha = 1)
    p <- p + coord_equal()
    #p <- p + annotate("text", x = interval/2, y = interval/2, vjust = 0, label = paste("AUC =",sprintf("%.3f",roc$auc)))
    p <- p +
      annotate(
        "text"
      , x = 0.05
      , y = 0.05
      , hjust = 1
      , vjust = 0
      , label =
          paste0(
                  "Area Under Curve (AUC) = ", sprintf("%.3f", opt_t$AUC)
          , "\n", "Balanced Accuracy = ", sprintf("%.3f", cm.feat$byClass["Balanced Accuracy"])
          , "\n", "Sensitivity = "      , sprintf("%.3f", cm.feat$byClass["Sensitivity"])
          , "\n", "Specificity = "      , sprintf("%.3f", cm.feat$byClass["Specificity"])
          , "\n", "Pos Pred Value = "   , sprintf("%.3f", cm.feat$byClass["Pos Pred Value"])
          , "\n", "Neg Pred Value = "   , sprintf("%.3f", cm.feat$byClass["Neg Pred Value"])
          )
      )

    #p <- p + theme(legend.position = c(0.8, 0.2))
    #p <- p + labs(title = "ROC Curves by classification method")
    #p <- p + theme(plot.title = element_text(hjust = 0.5))
    #print(p)

  }

  out <-
    list(
      opt_t      = opt_t
    ## debugging order permutation issue
    #, dat        = dat
    , val.feat   = as.numeric(val.feat)
    , good_class = good_class
    , p_roc      = p
    , roc_dat    = list(roc.curve = roc.curve, roc.curve.best = roc.curve.best, cm.feat = cm.feat)
    )

  return(out)
}
