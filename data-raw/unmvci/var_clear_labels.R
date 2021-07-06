#' Clear labels from variables in data frames
#'
#' @param dat is one of the project dataset data frames
#'
#' @return
#' @export
#'
#' @examples
#' var_clear_labels(dat_pdp)
var_clear_labels <- function(dat) {
  if(is.list(dat)) {
    for(i in 1 : length(dat)) class(dat[[i]]) <- setdiff(class(dat[[i]]), 'labelled')
    for(i in 1 : length(dat)) attr(dat[[i]],"label") <- NULL
  }
  else {
    class(dat) <- setdiff(class(dat), "labelled")
    attr(dat, "label") <- NULL
  }
  return(dat)
}

## ref: https://stackoverflow.com/questions/2394902/remove-variable-labels-attached-with-foreign-hmisc-spss-import-functions
