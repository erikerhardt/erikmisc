#' A function to calculate the ROC curve, determine the optimal threshold, plot the curve, and provide classification statistics
#'
#' @param actual_labels   true labels of binary observations
#' @param pred_values     either predicted labels or a value (such as a probability) associated with the success label
#' @param sw_plot         T/F to return a ROC curve ggplot object
#' @param cm_mode         \code{mode} from \code{caret::confusionMatrix}
#'
#' @return                a list including
#' \itemize{
#'   \item roc_curve_best   - one-row tibble of classification statistics for best Sensitivity and Specificity (closest to upper-left corner of ROC curve)
#'   \item pred_positive    - pred_values, returned as numeric 1 or 0
#'   \item confusion_matrix - confusion matrix statistics
#'   \item p_roc            - ROC curve ggplot object
#'   \item roc_curve        - ROC curve data
#' }
#' @importFrom ROCR prediction
#' @importFrom ROCR performance
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom caret confusionMatrix
#' @import ggplot2
#' @import dplyr
#' @export
#'
#' @examples
#' # Categorical prediction-value example (from ?caret::confusionMatrix)
#' ex_lvs    <- c("normal", "abnormal")
#' ex_truth  <- factor(rep(ex_lvs, times = c(86, 258)), levels = rev(ex_lvs))
#' ex_pred   <- factor(c(rep(ex_lvs, times = c(54,  32))
#'                     , rep(ex_lvs, times = c(27, 231)))
#'                   , levels = rev(ex_lvs))
#' out <-
#'   e_plot_roc(
#'     actual_labels = ex_truth
#'   , pred_values   = ex_pred
#'   , sw_plot       = TRUE
#'   )
#' out$roc_curve_best %>% print(width = Inf)
#'
#'
#' # Numeric prediction-value example
#' out <-
#'   e_plot_roc(
#'     actual_labels = sample(c("a", "b"), size = 50, replace = TRUE)
#'   , pred_values   = runif(n = 50)
#'   , sw_plot       = TRUE
#'   )
#' out$roc_curve_best %>% print(width = Inf)
#' out$p_roc
#'
e_plot_roc <-
  function(
    actual_labels = NULL
  , pred_values   = NULL
  , sw_plot       = TRUE
  , cm_mode       = c("sens_spec", "prec_recall", "everything")[1]
  ) {

  # need only 2 levels for ROCR functions
  if ((actual_labels %>% unique() %>% length()) > 2) {
    out <-
      list(
        roc_curve_best    = NULL
      , pred_positive     = NULL
      , confusion_matrix  = NULL
      , p_roc             = ggplot() + theme_void() + geom_text(aes(0,0,label="ROC N/A for >2 groups")) + xlab(NULL)
      , roc_curve         = NULL
      )
    return(out)
  }

  #library(ROCR)
  rocr_pred <- ROCR::prediction (predictions = pred_values %>% as.numeric(), labels = actual_labels)
  #rocr_perf <- ROCR::performance(rocr_pred, measure = "tpr", x.measure = "fpr")
  rocr_perf <- ROCR::performance(rocr_pred, measure="sens", x.measure="spec")

  # determine the best threshold as having the highest overall classification rate
  # Find t that minimizes error
  # roc_curve <-
  #   tibble::tibble(
  #     Spec    = 1 - unlist(rocr_perf@x.values)
  #   , Sens    = unlist(rocr_perf@y.values)
  #   , thresh  = unlist(rocr_perf@alpha.values)
  #   ) %>%
  #   dplyr::mutate(
  #     dist = sqrt((1 - Sens)^2 + (1 - Spec)^2)
  #   ) %>%
  #   dplyr::filter(
  #     is.finite(thresh)
  #   )
  roc_curve <-
    tibble::tibble(
      Spec    = unlist(rocr_perf@x.values)
    , Sens    = unlist(rocr_perf@y.values)
    , thresh  = unlist(rocr_perf@alpha.values)
    ) %>%
    dplyr::mutate(
      dist = sqrt((1 - Sens)^2 + (1 - Spec)^2)
    ) %>%
    dplyr::filter(
      is.finite(thresh)
    )

  roc_curve_best <-
    roc_curve %>%
    filter(
      dist == min(dist)
    ) %>%
    mutate(
      AUC = unlist(ROCR::performance(rocr_pred, measure = "auc")@y.values)
    )


  # define peak using optimal threshold
  pred_positive <- ifelse(as.numeric(pred_values) >= roc_curve_best$thresh, 1, 0)

  # assess confusion matrix accuracy
  confusion_matrix <-
    caret::confusionMatrix(
      data      = factor(pred_positive , levels = c(1, 0))
    , reference = factor(as.numeric(actual_labels == unique(sort(actual_labels))[2]), levels = c(1, 0))
    , mode      = cm_mode
    )
  # if Sens = 1-Spec and Spec = 1-Sens, then use index [1] instead of [2]
  if (abs(confusion_matrix$byClass["Sensitivity"] + roc_curve_best$Spec - 1) < 1e-4) {
    confusion_matrix <-
      caret::confusionMatrix(
        data      = factor(pred_positive , levels = c(1, 0))
      , reference = factor(as.numeric(actual_labels == unique(sort(actual_labels))[1]), levels = c(1, 0))
      , mode      = cm_mode
      )
  }

  roc_curve_best <-
    dplyr::bind_cols(
      roc_curve_best
    , confusion_matrix$byClass %>% t() %>% tibble::as_tibble()
    )

  # roc_curve_best

  # plot results
  if (sw_plot) {

    interval <- 0.2
    breaks   <- seq(0, 1, interval)

    library(ggplot2)
    p <- ggplot(roc_curve, aes(x = Spec, y = Sens))
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
      p <- p + geom_point(aes(x = roc_curve_best$Spec, y = roc_curve_best$Sens), shape = 21, size = 3, alpha = 1)
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
                  "Area Under Curve (AUC) = ", sprintf("%.3f", roc_curve_best$AUC)
          , "\n", "Balanced Accuracy = ", sprintf("%.3f", confusion_matrix$byClass["Balanced Accuracy"])
          , "\n", "Sensitivity = "      , sprintf("%.3f", confusion_matrix$byClass["Sensitivity"      ])
          , "\n", "Specificity = "      , sprintf("%.3f", confusion_matrix$byClass["Specificity"      ])
          , "\n", "Pos Pred Value = "   , sprintf("%.3f", confusion_matrix$byClass["Pos Pred Value"   ])
          , "\n", "Neg Pred Value = "   , sprintf("%.3f", confusion_matrix$byClass["Neg Pred Value"   ])
          )
      )

    #p <- p + theme(legend.position = c(0.8, 0.2))
    #p <- p + labs(title = "ROC Curves by classification method")
    #p <- p + theme(plot.title = element_text(hjust = 0.5))
    #print(p)

  } else {
    p <- NULL
  }

  out <-
    list(
      roc_curve_best    = roc_curve_best
    , pred_positive     = pred_positive
    , confusion_matrix  = confusion_matrix
    , p_roc             = p
    , roc_curve         = roc_curve
    #, actual_labels   = actual_labels
    #, pred_values     = pred_values
    )

  return(out)
} # e_plot_roc

