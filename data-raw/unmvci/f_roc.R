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
#' @export
#'
#' @examples
f_roc <-
  function(
    dat
  , var_class
  , val.feat
  , sw.plot = TRUE
  , method.name = ""
  ) {

  #library(ROCR)
  pred <- ROCR::prediction(predictions = as.numeric(val.feat), labels = dat[[var_class]])
  perf <- ROCR::performance(pred, measure = "tpr", x.measure = "fpr")

  # determine the best threshold as having the highest overall classification rate
  # Find t that minimizes error
  roc.curve <- data.frame(Spec = 1 - unlist(perf@x.values)
                        , Sens = unlist(perf@y.values)
                        , thresh = unlist(perf@alpha.values))
  roc.curve$dist <- sqrt((1 - roc.curve$Sens)^2 + (1 - roc.curve$Spec)^2)
  opt_t <- subset(roc.curve, roc.curve$dist == min(roc.curve$dist))
  opt_t

  perf.auc <- unlist(ROCR::performance(pred, measure = "auc")@y.values)
  opt_t$AUC <- perf.auc

  # define peak using optimal threshold
  good_class <- ifelse(as.numeric(val.feat) >= opt_t$thresh, 1, 0)

  # assess confusion matrix accuracy
  cm.feat <-
    f_cm(
      classification  = good_class
    , reference       = dat[[var_class]]
    , sw.echo         = FALSE
    )

  opt_t$BalAccuracy <- cm.feat$byClass["Balanced Accuracy"]

  # plot results
  if (sw.plot) {
    # color the ROC curve by threshold
    ROCR::plot(perf
        , colorize.palette = rev(rainbow(256, start=0, end=4/6))
        , colorize = TRUE
        , main = method.name
                      # paste0(method.name
                      # , ":  AUC = ", signif(perf.auc, 3)
                      # , ", Optim Sens = ", signif(opt_t$Sens, 3)
                      # , ", Spec = ", signif(opt_t$Spec, 3)
                      # , ", Acc = ", signif(opt_t$BalAccuracy, 3)
                      # )
        )
    abline(0,1, col = "gray80") # reference line
    # optimal threshold
    points(x = 1 - opt_t$Spec, y = opt_t$Sens, cex = 2)
    abline(v = 1 - opt_t$Spec, col = "gray80")
    abline(h = opt_t$Sens, col = "gray80")
    text(0.6, 0.5, pos = 4, cex = 1.2, labels = paste0(signif(perf.auc, 3), " AUC"))
    text(0.6, 0.4, pos = 4, cex = 1.2, labels = paste0(signif(opt_t$BalAccuracy, 3), " BalAccuracy"))
    text(0.6, 0.3, pos = 4, cex = 1.2, labels = paste0(signif(opt_t$Sens, 3), " Sensitivity"))
    text(0.6, 0.2, pos = 4, cex = 1.2, labels = paste0(signif(opt_t$Spec, 3), " Specificity"))
    text(0.6, 0.1, pos = 4, cex = 1.2, labels = paste0(signif(opt_t$thresh, 3), " Threshold"))
  }

  out <-
    list(
      opt_t      = opt_t
    ## debugging order permutation issue
    #, dat        = dat
    , val.feat   = as.numeric(val.feat)
    , good_class = good_class
    ,
    )

  return(out)
}
