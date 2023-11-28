#' A function to calculate the ROC curve, determine the optimal threshold, plot the curve, and provide classification statistics
#'
#' @param labels_true       true labels of binary observations, should be the same (and not a proxy) as what was used to build the prediction model
#' @param pred_values_pos   either predicted labels or a value (such as a probability) associated with the success label
#' @param label_neg_pos     labels in order c("negative", "positive")
#' @param sw_plot           T/F to return a ROC curve ggplot object
#' @param cm_mode           \code{mode} from \code{caret::confusionMatrix}
#' @param sw_caption_desc   T/F to include verbose caption descriptions of classification statistics
#'
#' @return                a list including
#' \itemize{
#'   \item roc_curve_best   - one-row tibble of classification statistics for best Sensitivity and Specificity (closest to upper-left corner of ROC curve)
#'   \item pred_positive    - pred_values_pos, returned as numeric 1 or 0
#'   \item confusion_matrix - confusion matrix statistics
#'   \item plot_roc         - ROC curve ggplot object
#'   \item roc_curve        - ROC curve data
#' }
#' @importFrom ROCR prediction
#' @importFrom ROCR performance
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom caret confusionMatrix
#' @import dplyr
#' @import ggplot2
#' @export
#'
#' @examples
#' ## Categorical prediction-value example (from ?caret::confusionMatrix)
#' ex_lvs    <- c("normal", "abnormal")
#' ex_truth  <- factor(rep(ex_lvs, times = c(86, 258)), levels = rev(ex_lvs))
#' ex_pred   <- factor(c(rep(ex_lvs, times = c(54,  32))
#'                     , rep(ex_lvs, times = c(27, 231)))
#'                   , levels = ex_lvs)
#' out <-
#'   e_plot_roc(
#'     labels_true     = ex_truth
#'   , pred_values_pos = ex_pred
#'   , label_neg_pos   = ex_lvs
#'   , sw_plot         = TRUE
#'   , sw_caption_desc = c(TRUE, FALSE)[2]
#'   )
#' out$roc_curve_best |> print(width = Inf)
#' out$plot_roc
#' out$confusion_matrix
#'
#'
#' ## Numeric prediction-value example
#' out <-
#'   e_plot_roc(
#'     labels_true     = sample(c("a", "b"), size = 50, replace = TRUE)
#'   , pred_values_pos = runif(n = 50)
#'   , label_neg_pos   = c("a", "b")
#'   , sw_plot         = TRUE
#'   , sw_caption_desc = c(TRUE, FALSE)[1]
#'   )
#' out$roc_curve_best |> print(width = Inf)
#' out$plot_roc
#' out$confusion_matrix
#'
#'
#' ## Logistic regression
#' data(dat_mtcars_e)
#'
#' dat_mtcars_e <-
#'   dat_mtcars_e |>
#'   dplyr::mutate(
#'     vs_V = ifelse(vs == "V-shaped", 1, 0) # 0-1 binary for logistic regression
#'   )
#'
#' # Predict engine type `vs` ("V-shaped" vs "straight") from other features.
#' fit_glm_vs <-
#'   glm(
#'     cbind(vs_V, 1 - vs_V) ~ disp + wt + carb
#'   , family = binomial
#'   , data = dat_mtcars_e
#'   )
#' cat("Test residual deviance for lack-of-fit (if > 0.10, little-to-no lack-of-fit)\n")
#' dev_p_val <- 1 - pchisq(fit_glm_vs$deviance, fit_glm_vs$df.residual)
#' dev_p_val |> print()
#' car::Anova(fit_glm_vs, type = 3)
#' #summary(fit_glm_vs)
#'
#' glm_roc <-
#'   e_plot_roc(
#'     labels_true     = dat_mtcars_e$vs_V
#'   , pred_values_pos = fit_glm_vs$fitted.values
#'   , label_neg_pos   = c(0, 1)
#'   , sw_plot         = TRUE
#'   , cm_mode         = c("sens_spec", "prec_recall", "everything")[3]
#'   , sw_caption_desc = c(TRUE, FALSE)[1]
#'   )
#' glm_roc$roc_curve_best |> print(width = Inf)
#' glm_roc$plot_roc
#' glm_roc$confusion_matrix
#'
#'
#' \dontrun{
#'
#'    ## REWRITE SO THIS WORKS IN GENERAL, developed for e_rfsrc_classification()
#'
#'   # hierarchy: reorder ROC objects by type (rather than target)
#'   out[[ "plot_o_class_sel_ROC" ]] <- list()
#'   for (n_object in names(out_roc_temp[[ 1 ]])) {
#'     ## n_object = names(out_roc_temp[[ 1 ]])[1]
#'     out[[ "plot_o_class_sel_ROC" ]][[ n_object ]] <- list()
#'
#'     for (n_target in names(out_roc_temp)) {
#'       ## n_target = names(out_roc_temp)[1]
#'       out[[ "plot_o_class_sel_ROC" ]][[ n_object ]][[ n_target ]] <-
#'         out_roc_temp[[ n_target ]][[ n_object ]]
#'
#'       if (n_object == "roc_curve_best") {
#'         out[[ "plot_o_class_sel_ROC" ]][[ n_object ]][[ n_target ]] <-
#'           out[[ "plot_o_class_sel_ROC" ]][[ n_object ]][[ n_target ]] |>
#'           dplyr::mutate(
#'             Group = n_target
#'           ) |>
#'           dplyr::relocate(Group)
#'       }
#'       if (n_object == "roc_curve") {
#'         out[[ "plot_o_class_sel_ROC" ]][[ n_object ]][[ n_target ]] <-
#'           out[[ "plot_o_class_sel_ROC" ]][[ n_object ]][[ n_target ]] |>
#'           dplyr::mutate(
#'             Group = n_target
#'           ) |>
#'           dplyr::relocate(Group)
#'       }
#'
#'     }
#'   }
#'
#'
#' }
#'
e_plot_roc <-
  function(
    labels_true     = NULL
  , pred_values_pos = NULL
  , label_neg_pos   = NULL
  , sw_plot         = TRUE
  , cm_mode         = c("sens_spec", "prec_recall", "everything")[1]
  , sw_caption_desc = c(TRUE, FALSE)[1]
  ) {

  # need only 2 levels for ROCR functions
  if ((labels_true   |> unique() |> length()) > 2) {
    warning("e_plot_roc: Only two classes for ROC at this time")
    out <-
      list(
        roc_curve_best    = NULL
      , pred_positive     = NULL
      , confusion_matrix  = NULL
      , plot_roc          = ggplot() + theme_void() + geom_text(aes(0,0,label="ROC N/A for >2 groups")) + xlab(NULL)
      , roc_curve         = NULL
      )
    return(out)
  }

  # format data
  labels_true     <- labels_true     |> as.character()
  pred_values_pos <- pred_values_pos |> as.numeric()
  label_neg_pos   <- label_neg_pos   |> as.character()

  #library(ROCR)
  ## Most would assume the first category be the "positive", but not ROCR...
  ## Note: "Ideally, labels should be supplied as ordered factor(s),
  ##        the lower level corresponding to the negative class,
  ##        the upper level to the positive class."
  ##  Thus, below I swap Sens and Spec for plots and summaries.
  rocr_pred <-
    ROCR::prediction(
      predictions    = pred_values_pos
    , labels         = labels_true
    , label.ordering = label_neg_pos  # c("negative", "positive")
    )
  #rocr_perf <- ROCR::performance(rocr_pred, measure = "tpr", x.measure = "fpr")
  rocr_perf <-
    ROCR::performance(
      prediction.obj  = rocr_pred
    , measure         = "sens"
    , x.measure       = "spec"
    #, measure         = "spec"      # x,y swapped, see ROCR note above
    #, x.measure       = "sens"      # x,y swapped, see ROCR note above
    )
  #plot(rocr_perf)

  # ROC curve details in a tibble, calculate distance from top-lelt corner
  roc_curve <-
    tibble::tibble(
      Sens    = unlist(rocr_perf@y.values)
    , Spec    = unlist(rocr_perf@x.values)
    , thresh  = unlist(rocr_perf@alpha.values)
    ) |>
    dplyr::mutate(
      dist = sqrt((1 - Sens)^2 + (1 - Spec)^2)
    ) |>
    dplyr::arrange(
      Sens
    , desc(Spec)
    )
    #|>
    #dplyr::filter(
    #  is.finite(thresh)
    #)

  # determine best threshold as closest to top-left corner: highest overall classification rate
  roc_curve_best <-
    roc_curve |>
    dplyr::filter(
      dist == min(dist)
    ) |>
    dplyr::mutate(
      AUC = unlist(ROCR::performance(rocr_pred, measure = "auc")@y.values)
    ) |>
    dplyr::slice(1)  # when > 1 have same min dist

  # define positive classification using optimal threshold
  pred_positive <- ifelse(as.numeric(pred_values_pos) >= roc_curve_best$thresh, 1, 0)

  # assess confusion matrix accuracy
  confusion_matrix <-
    caret::confusionMatrix(
      data      = pred_positive |> factor(levels = c(0, 1), labels = label_neg_pos)
    , reference = labels_true   |> as.character() |> factor(levels = label_neg_pos)
    , positive  = label_neg_pos[2]
    , mode      = cm_mode
    )
  # # if Sens = 1-Spec and Spec = 1-Sens, then use index [1] instead of [2]
  # if (abs(confusion_matrix$byClass["Sensitivity"] + roc_curve_best$Spec - 1) < 1e-4) {
  #   message("e_plot_roc: swapping success index")
  #   confusion_matrix <-
  #     caret::confusionMatrix(
  #       data      = factor(pred_positive, levels = c(0, 1), labels = as.character(unique(sort(labels_true  , decreasing = TRUE))))
  #     , reference = labels_true   |> factor()
  #     , mode      = cm_mode
  #     )
  # }

  # add classification statistics
  roc_curve_best <-
    dplyr::bind_cols(
      roc_curve_best
    , confusion_matrix$byClass |> t() |> tibble::as_tibble()
    )
  # roc_curve_best |> print()

  # plot results
  if (sw_plot) {

    interval <- 0.2
    breaks   <- seq(0, 1, interval)

    #library(ggplot2)
    p <- ggplot(roc_curve, aes(x = Spec, y = Sens))
    p <- p + theme_bw()
    p <- p + geom_segment(aes(x = 0, y = 1, xend = 1, yend = 0), alpha = 0.25, linetype = 3)
    p <- p + geom_step(size = 1) # aes(colour = Method, linetype = Method),
    p <- p + scale_x_reverse   (name = "Specificity", limits = c(1,0), breaks = breaks, expand = c(0.001, 0.001))
    p <- p + scale_y_continuous(name = "Sensitivity", limits = c(0,1), breaks = breaks, expand = c(0.001, 0.001))

    # optim values
    p <- p + geom_point(aes(x = roc_curve_best$Spec, y = roc_curve_best$Sens), shape = 21, size = 3, alpha = 1)

    # optim values
    p <- p + annotate(
               geom     = "segment"
             , x        = roc_curve_best$Spec
             , xend     = roc_curve_best$Spec
             , y        = roc_curve_best$Sens
             , yend     = 0
             #, colour   = "gray25"
             , size     = 0.5
             , alpha    = 1/2
             , linetype = 2
             #, arrow = arrow(angle = 20, length = unit(0.15, "inches"), ends = "last", type = "open")
             )
    p <- p + annotate(
               geom     = "segment"
             , x        = roc_curve_best$Spec
             , xend     = 1
             , y        = roc_curve_best$Sens
             , yend     = roc_curve_best$Sens
             #, colour   = "gray25"
             , size     = 0.5
             , alpha    = 1/2
             , linetype = 2
             #, arrow = arrow(angle = 20, length = unit(0.15, "inches"), ends = "last", type = "open")
             )


    p <- p + coord_equal()

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
          , "\n", "Pos Threshold >= "   , sprintf("%.3f", roc_curve_best$thresh)
          )
      )

    if(sw_caption_desc) {
      p <- p +
        labs(
          caption =
            paste0(
              "Classification labels:  \"", label_neg_pos[2], "\" is positive, \"", label_neg_pos[1], "\" is negative."
            , "\n"
            , "Balanced Accuracy:  average of sensitivity and specificity, average probability of correctly classifying all targets."
            , "\n"
            , "Sensitivity:  true positive rate, probability correctly classifying \"", label_neg_pos[2], "\"."
            , "\n"
            , "Specificity:  true negative rate, probability correctly classifying \"", label_neg_pos[1], "\"."
            , "\n"
            , "Pos Pred Value:  positive predictive value (precision), probability that classification of \"", label_neg_pos[2], "\" is correct."
            , "\n"
            , "Neg Pred Value:  negative predictive value, probability that classification of \"", label_neg_pos[1], "\" is correct."
            , "\n"
            , "Pos Threshold >=:  value of predictive outcome metric that partitions classification of \"", label_neg_pos[2], "\" from \"", label_neg_pos[1], "\"."
            )
        )
    } else {
      p <- p +
        labs(
          caption =
            paste0(
              "Classification labels:  \"", label_neg_pos[2], "\" is positive, \"", label_neg_pos[1], "\" is negative."
            )
        )
    }
    p <- p + theme(plot.caption = element_text(hjust = 0, size = 6), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)

  } else {
    p <- NULL
  }

  out <-
    list(
      roc_curve_best    = roc_curve_best
    , pred_positive     = pred_positive
    , confusion_matrix  = confusion_matrix
    , plot_roc          = p
    , roc_curve         = roc_curve
    )

  return(out)
} # e_plot_roc

