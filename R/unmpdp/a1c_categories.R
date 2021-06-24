#' CDC classification for A1c category labels
#'
#' @param a1c
#'
#' @return a1c_cat, categorical levels of A1c
#' @export
#'
#' @examples
a1c_categories <- function(a1c) {

  # https://www.cdc.gov/diabetes/managing/managing-blood-sugar/a1c.html
  # A1c
  # Below 5.7%     Normal
  # 5.7% to 6.4%   Prediabetes
  # 6.5% or above  Diabetes

  a1c_cat <- rep(NA, length(a1c))
  a1c_cat[                (a1c < 5.7 )] <- "Neither"
  a1c_cat[(a1c >= 5.7 ) & (a1c < 6.4 )] <- "Pre-diabetes"
  a1c_cat[(a1c >= 6.4 )               ] <- "Diabetes"

  a1c_cat <- factor(a1c_cat
                  #, levels = 1:4
                  , levels = c( "Neither"
                              , "Pre-diabetes"
                              , "Diabetes"
                              )
                  #, ordered = TRUE
                  )

  return(a1c_cat)
}
