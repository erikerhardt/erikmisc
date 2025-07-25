#' A function to calculate the ROC curve, determine the optimal threshold, plot the curve, and provide classification statistics
#'
#' @param labels_true       true labels of binary observations, should be the same (and not a proxy) as what was used to build the prediction model
#' @param pred_values_pos   either predicted labels or a value (such as a probability) associated with the success label
#' @param label_neg_pos     labels in order c("negative", "positive")
#' @param sw_plot           T/F to return a ROC curve ggplot object
#' @param cm_mode           \code{mode} from \code{caret::confusionMatrix}
#' @param threshold_to_use  A threshold value to use for plot and "best" ROC, \code{NULL} to determine.
#' @param sw_confusion_matrix T/F include confusion matrix table inset in plot
#' @param pos_conf_mat      \code{c(x, y)} inset position
#' @param sw_caption_desc   T/F define statistics in caption
#' @param sw_class_labels   T/F classification labels indicated in caption
#' @param sw_thresh_bounds  T/F print threshold bounds at ends of ROC Curve
#' @param sw_val_AUC        T/F report statistic
#' @param sw_val_BA         T/F report statistic
#' @param sw_val_Sens       T/F report statistic
#' @param sw_val_Spec       T/F report statistic
#' @param sw_val_PPV        T/F report statistic
#' @param sw_val_NPV        T/F report statistic
#' @param sw_val_Thresh     T/F report statistic
#' @param label_AUC         label for statistic
#' @param label_BA          label for statistic
#' @param label_Sens        label for statistic
#' @param label_Spec        label for statistic
#' @param label_PPV         label for statistic
#' @param label_NPV         label for statistic
#' @param label_Thresh      label for statistic
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
#' @importFrom ggpp annotate
#' @import dplyr
#' @import ggplot2
#' @import ggpmisc
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
#'     labels_true         = dat_mtcars_e$vs_V
#'   , pred_values_pos     = fit_glm_vs$fitted.values
#'   , label_neg_pos       = c(0, 1)
#'   , sw_plot             = TRUE
#'   , cm_mode             = c("sens_spec", "prec_recall", "everything")[3]
#'   , threshold_to_use    = NULL
#'   )
#' glm_roc$roc_curve_best |> print(width = Inf)
#' glm_roc$plot_roc
#' glm_roc$confusion_matrix
#'
#' # specified threshold
#' glm_roc <-
#'   e_plot_roc(
#'     labels_true         = dat_mtcars_e$vs_V
#'   , pred_values_pos     = fit_glm_vs$fitted.values
#'   , label_neg_pos       = c(0, 1)
#'   , sw_plot             = TRUE
#'   , cm_mode             = c("sens_spec", "prec_recall", "everything")[3]
#'   , threshold_to_use    = 0.001
#'   )
#' glm_roc$roc_curve_best |> print(width = Inf)
#' glm_roc$plot_roc
#' glm_roc$confusion_matrix
#'
#'
#' # Illustrate labels and caption definitions
#' glm_roc <-
#'   e_plot_roc(
#'     labels_true         = dat_mtcars_e$vs_V
#'   , pred_values_pos     = fit_glm_vs$fitted.values
#'   , label_neg_pos       = c(0, 1)
#'   , sw_plot             = TRUE
#'   , cm_mode             = c("sens_spec", "prec_recall", "everything")[3]
#'   , sw_confusion_matrix = TRUE
#'   , pos_conf_mat        = c(0.9, 0)
#'   , sw_caption_desc     = TRUE
#'   , sw_class_labels     = TRUE
#'   , sw_thresh_bounds    = TRUE
#'   , sw_val_AUC          = TRUE
#'   , sw_val_BA           = TRUE
#'   , sw_val_Sens         = FALSE
#'   , sw_val_Spec         = FALSE
#'   , sw_val_PPV          = TRUE
#'   , sw_val_NPV          = TRUE
#'   , sw_val_Thresh       = TRUE
#'   , label_AUC           = c("AUC"   , "Area Under Curve"  )[1]
#'   , label_BA            = c("BA"    , "Balanced Accuracy" )[1]
#'   , label_Sens          = c("Sens"  , "Sensitivity"       )[1]
#'   , label_Spec          = c("Spec"  , "Specificity"       )[1]
#'   , label_PPV           = c("PPV"   , "Pos Pred Value"    )[2]
#'   , label_NPV           = c("NPV"   , "Neg Pred Value"    )[2]
#'   , label_Thresh        = c("Thresh", "Pos Threshold"     )[1]
#'   )
#' glm_roc$roc_curve_best |> print(width = Inf)
#' glm_roc$plot_roc
#' glm_roc$confusion_matrix
#'
#'
#' \dontrun{
#'
#' ## Categorical prediction-value example (from ?caret::confusionMatrix)
#' ex_lvs    <- c("normal", "abnormal")
#' ex_truth  <- factor(rep(ex_lvs, times = c(86, 258)), levels = rev(ex_lvs))
#' ex_pred   <- factor(c(rep(ex_lvs, times = c(54,  32))
#'                     , rep(ex_lvs, times = c(27, 231)))
#'                   , levels = ex_lvs)
#' # List of ROC curves for each target
#' out_roc_temp <- list()
#' for (n_target in levels(ex_truth)) {  # replace "unique()" by "levels()" if categorical
#'   out_roc <-
#'     e_plot_roc(
#'       labels_true     = ex_truth |> relevel(ref = n_target)
#'     , pred_values_pos = ex_pred
#'     , label_neg_pos   = ex_lvs
#'     , sw_plot         = TRUE
#'     , sw_caption_desc = c(TRUE, FALSE)[2]
#'     )
#'
#'   out_roc$plot_roc <- out_roc$plot_roc + labs(title = paste0("ROC Curve, Target:  ", n_target))
#'   out_roc$plot_roc <- out_roc$plot_roc + coord_fixed(ratio = 1) # equal axes
#'
#'   out_roc_temp[[ n_target ]] <- out_roc
#' } # n_target
#'
#' # to display list tree hierarchy
#' out_roc_temp |> e_print_list_tree_hierarchy()
#'
#' # Reorder ROC objects by type (rather than target)
#' out_roc_reordered <-
#'   out_roc_temp |>
#'   e_plot_roc_reorder_hierarchy()
#'
#' out_roc_reordered |> e_print_list_tree_hierarchy()
#'
#' }
#'
e_plot_roc <-
  function(
    labels_true         = NULL
  , pred_values_pos     = NULL
  , label_neg_pos       = NULL
  , sw_plot             = TRUE
  , cm_mode             = c("sens_spec", "prec_recall", "everything")[1]
  , threshold_to_use    = NULL
  , sw_confusion_matrix = TRUE
  , pos_conf_mat        = c(0, 0.75)
  , sw_caption_desc     = TRUE
  , sw_class_labels     = TRUE
  , sw_thresh_bounds    = TRUE
  , sw_val_AUC          = TRUE
  , sw_val_BA           = TRUE
  , sw_val_Sens         = TRUE
  , sw_val_Spec         = TRUE
  , sw_val_PPV          = TRUE
  , sw_val_NPV          = TRUE
  , sw_val_Thresh       = TRUE
  , label_AUC           = c("AUC"   , "Area Under Curve"  )[1]
  , label_BA            = c("BA"    , "Balanced Accuracy" )[1]
  , label_Sens          = c("Sens"  , "Sensitivity"       )[1]
  , label_Spec          = c("Spec"  , "Specificity"       )[1]
  , label_PPV           = c("PPV"   , "Pos Pred Value"    )[1]
  , label_NPV           = c("NPV"   , "Neg Pred Value"    )[1]
  , label_Thresh        = c("Thresh", "Pos Threshold"     )[1]
  ) {

  # need only 2 levels for ROCR functions
  if ((labels_true   |> unique() |> length()) > 2) {
    warning("erikmisc::e_plot_roc: Only two classes for ROC at this time")
    out <-
      list(
        roc_curve_best    = NULL
      , pred_positive     = NULL
      , confusion_matrix  = NULL
      , plot_roc          = ggplot() + theme_void() + geom_text(aes(0,0,label="ROC N/A for > 2 groups")) + xlab(NULL)
      , roc_curve         = NULL
      )
    return(out)
  }

  if ((labels_true   |> unique() |> length()) < 2) {
    warning("erikmisc::e_plot_roc: Has only one class, requires two classes for ROC at this time")
    out <-
      list(
        roc_curve_best    = NULL
      , pred_positive     = NULL
      , confusion_matrix  = NULL
      , plot_roc          = ggplot() + theme_void() + geom_text(aes(0,0,label="ROC N/A for < 2 groups")) + xlab(NULL)
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
  if (is.null(threshold_to_use)) {
    roc_curve_best <-
      roc_curve |>
      dplyr::filter(
        dist == min(dist)
      ) |>
      dplyr::mutate(
        AUC = unlist(ROCR::performance(rocr_pred, measure = "auc")@y.values)
      ) |>
      dplyr::slice(1)  # when > 1 have same min dist
  } else {
    roc_curve_best <-
      roc_curve |>
      dplyr::mutate(
        row_id        = 1:dplyr::n()
      , dist_thresh   = thresh - threshold_to_use
      ) |>
      dplyr::filter(
        dist_thresh > 0
      ) |>
      dplyr::filter(
        dist_thresh == min(dist_thresh)
      ) |>
      dplyr::select(
        -row_id
      , -dist_thresh
      ) |>
      dplyr::mutate(
        AUC = unlist(ROCR::performance(rocr_pred, measure = "auc")@y.values)
      ) |>
      dplyr::slice(1)  # when > 1 have same min dist
  } # if threshold_to_use

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

  if(sw_confusion_matrix) {

    tab_confusion <-
      dplyr::full_join(
        confusion_matrix$table |>
        tibble::as_tibble()
      , confusion_matrix$table |>
        prop.table(margin = 2) |>
        round(3) |>
        tibble::as_tibble() |>
        dplyr::rename(prop = n)
      , by = dplyr::join_by(Prediction, Reference)
      ) |>
      dplyr::mutate(
        percent = sprintf("%5.1f", prop * 100)
      , n_percent = paste0(n, " (", percent, "%)")
      ) |>
      tidyr::pivot_wider(
        id_cols       = "Reference"
      , names_from    = "Prediction"
      , values_from   = "n_percent"
      #, names_prefix  = "Pred "
      ) |>
      dplyr::rename(
        `Target \\ Class` = Reference
      )

  }

  # add classification statistics
  roc_curve_best <-
    dplyr::bind_cols(
      roc_curve_best
    , confusion_matrix$byClass |> t() |> tibble::as_tibble()
    ) |>
    dplyr::mutate(
      `Geom Mean` = sqrt(Sensitivity * Specificity)
    )
  # roc_curve_best |> print()

  label_annotate <- list()
  if (sw_val_AUC   ) {label_annotate[[ "AUC"    ]] <- paste0(label_AUC   ,  " = ", sprintf("%.3f", roc_curve_best$AUC)                           ) }
  if (sw_val_BA    ) {label_annotate[[ "BA"     ]] <- paste0(label_BA    ,  " = ", sprintf("%.3f", confusion_matrix$byClass["Balanced Accuracy"])) }
  if (sw_val_Sens  ) {label_annotate[[ "Sens"   ]] <- paste0(label_Sens  ,  " = ", sprintf("%.3f", confusion_matrix$byClass["Sensitivity"      ])) }
  if (sw_val_Spec  ) {label_annotate[[ "Spec"   ]] <- paste0(label_Spec  ,  " = ", sprintf("%.3f", confusion_matrix$byClass["Specificity"      ])) }
  if (sw_val_PPV   ) {label_annotate[[ "PPV"    ]] <- paste0(label_PPV   ,  " = ", sprintf("%.3f", confusion_matrix$byClass["Pos Pred Value"   ])) }
  if (sw_val_NPV   ) {label_annotate[[ "NPV"    ]] <- paste0(label_NPV   ,  " = ", sprintf("%.3f", confusion_matrix$byClass["Neg Pred Value"   ])) }
  if (sw_val_Thresh) {label_annotate[[ "Thresh" ]] <- paste0(label_Thresh, " >= ", sprintf("%.3f", roc_curve_best$thresh)                        ) }
  label_annotate <-
    label_annotate |>
    unlist() |>
    paste(collapse = "\n")

  label_caption <- list()
  if (sw_caption_desc) {
    if (sw_class_labels) {label_caption[[ "Class"  ]] <- paste0("Classification labels:  \"", label_neg_pos[2], "\" is positive, \"", label_neg_pos[1], "\" is negative.") }
    if (sw_val_AUC     ) {label_caption[[ "AUC"    ]] <- paste0(label_AUC   ,    ": ", "overall performance, average value of sensitivity for all possible values of specificity") }
    if (sw_val_BA      ) {label_caption[[ "BA"     ]] <- paste0(label_BA    ,    ": ", "average of sensitivity and specificity, average probability of correctly classifying all targets."                                ) }
    if (sw_val_Sens    ) {label_caption[[ "Sens"   ]] <- paste0(label_Sens  ,    ": ", "true positive rate, probability correctly classifying \"", label_neg_pos[2], "\"."                                                ) }
    if (sw_val_Spec    ) {label_caption[[ "Spec"   ]] <- paste0(label_Spec  ,    ": ", "true negative rate, probability correctly classifying \"", label_neg_pos[1], "\"."                                                ) }
    if (sw_val_PPV     ) {label_caption[[ "PPV"    ]] <- paste0(label_PPV   ,    ": ", "positive predictive value (precision), probability that classification of \"", label_neg_pos[2], "\" is correct."                 ) }
    if (sw_val_NPV     ) {label_caption[[ "NPV"    ]] <- paste0(label_NPV   ,    ": ", "negative predictive value, probability that classification of \"", label_neg_pos[1], "\" is correct."                             ) }
    if (sw_val_Thresh  ) {label_caption[[ "Thresh" ]] <- paste0(label_Thresh, " >=: ", "value of predictive outcome metric that partitions classification of \"", label_neg_pos[2], "\" from \"", label_neg_pos[1], "\"." ) }
  } # sw_caption_desc
  label_caption <-
    label_caption |>
    unlist() |>
    paste(collapse = "\n")

  if (!is.null(threshold_to_use)) {
    label_caption <-
      paste0(
        label_caption
      , "\nThreshold provided, used closest greater than: "
      , threshold_to_use |> signif(3)
      )
  }


  # plot results
  if (sw_plot) {

    interval <- 0.2
    breaks   <- seq(0, 1, interval)

    #library(ggplot2)
    p <- ggplot(roc_curve, aes(x = Spec, y = Sens))
    p <- p + theme_bw()
    p <- p + geom_segment(aes(x = 0, y = 1, xend = 1, yend = 0), alpha = 0.25, linetype = 3)
    p <- p + geom_step(linewidth = 1) # aes(colour = Method, linetype = Method),
    p <- p + scale_x_reverse   (name = "Specificity", limits = c(1,0), breaks = breaks, expand = c(0.001, 0.001))
    p <- p + scale_y_continuous(name = "Sensitivity", limits = c(0,1), breaks = breaks, expand = c(0.001, 0.001))

    # optim values
    p <- p + geom_point(aes(x = roc_curve_best$Spec, y = roc_curve_best$Sens), shape = 21, size = 3, alpha = 1)

    # optim values
    p <- p + annotate(
               geom       = "segment"
             , x          = roc_curve_best$Spec
             , xend       = roc_curve_best$Spec
             , y          = roc_curve_best$Sens
             , yend       = 0
             #, colour     = "gray25"
             , linewidth  = 0.5
             , alpha      = 1/2
             , linetype   = 2
             #, arrow     = arrow(angle = 20, length = unit(0.15, "inches"), ends = "last", type = "open")
             )
    p <- p + annotate(
               geom       = "segment"
             , x          = roc_curve_best$Spec
             , xend       = 1
             , y          = roc_curve_best$Sens
             , yend       = roc_curve_best$Sens
             #, colour     = "gray25"
             , linewidth  = 0.5
             , alpha      = 1/2
             , linetype   = 2
             #, arrow     = arrow(angle = 20, length = unit(0.15, "inches"), ends = "last", type = "open")
             )

    p <- p + coord_equal()

    p <- p +
      annotate(
        "text"
      , x = 0.05
      , y = 0.05
      , hjust = 1
      , vjust = 0
      , label = label_annotate
      )

    if (sw_thresh_bounds) {
      roc_curve_min_max <-
        roc_curve |>
        #dplyr::filter(
        #  !is.infinite(thresh)
        #) |>
        dplyr::mutate(
          thresh = ifelse(thresh ==  Inf, max(thresh[is.finite(thresh)]), thresh)
        , thresh = ifelse(thresh == -Inf, min(thresh[is.finite(thresh)]), thresh)
        ) |>
        dplyr::slice(
          1
        , dplyr::n()
        )

      p <- p + geom_point(aes(x = roc_curve_min_max$Spec[1], y = roc_curve_min_max$Sens[1]), shape = 19, size = 2, alpha = 1)
      p <- p +
        annotate(
          "text"
        , x = roc_curve_min_max$Spec[1]
        , y = roc_curve_min_max$Sens[1]
        , hjust = -0.5
        , vjust = -1.5
        , label = paste0("th=", roc_curve_min_max$thresh[1] |> round(2))
        )
      p <- p + geom_point(aes(x = roc_curve_min_max$Spec[2], y = roc_curve_min_max$Sens[2]), shape = 19, size = 2, alpha = 1)
      p <- p +
        annotate(
          "text"
        , x = roc_curve_min_max$Spec[2]
        , y = roc_curve_min_max$Sens[2]
        , hjust = 1.5
        , vjust = 1.5
        , label = paste0("th=", roc_curve_min_max$thresh[2] |> round(2))
        )
    } # sw_thresh_bounds

    if (sw_caption_desc) {
      p <- p +
        labs(
          caption = label_caption
        )
      p <- p + theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") # Default is hjust=1, Caption align left (*.position all the way left)
    } # sw_caption_desc

    if(sw_confusion_matrix) {
      # https://cran.r-project.org/web/packages/ggpmisc/vignettes/model-based-annotations.html
      #library(ggpmisc)  # for geom="table"
      #p <- p + annotate(geom = "table", x = 0.9, y = 0, label = list(as.data.frame(tab_confusion)))
      p <- p + ggpp::annotate(
                  geom = "table"
                , x = pos_conf_mat[1]
                , y = pos_conf_mat[2]
                , label = list(as.data.frame(tab_confusion))
                )

    }

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

