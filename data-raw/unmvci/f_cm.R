#' Calculate classification confusion matrix.
#'
#' @param classification
#' @param reference
#' @param sw.echo
#' @param method.name
#'
#' @return
#' @importFrom caret confusionMatrix
#' @export
#'
#' @examples
f_cm <- function(classification, reference, sw.echo = TRUE) {
  #library(caret)
  cm.temp <-
    caret::confusionMatrix(
      data      = factor(classification, levels = c(1, 0))
    , reference = factor(reference     , levels = c(1, 0))
    )
  if (sw.echo) {
    print(cm.temp$test)
    print(cm.temp$table)
    print(formatC(signif(cm.temp$byClass[c("Sensitivity", "Specificity", "Balanced Accuracy")], 3)))
  }
  return(cm.temp)
}
