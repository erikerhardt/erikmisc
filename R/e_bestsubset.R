#' Best subset selection returns results sorted by BIC
#'
#' @param form  lm formula for full model
#' @param dat   data to use
#' @param nbest number of models to return for each model size, default is 5
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' i_best <-
#'   e_bestsubset(
#'     form = formula(mpg ~ cyl + disp + hp + gear)
#'   , dat  = datasets::mtcars
#'   )
#'
#'   op <- options(); # saving old options
#'   options(width=100) # setting command window output text width wider
#' i_best %>% print(n = Inf, width = Inf)
#'   options(op); # reset (all) initial options
#' }
e_bestsubset <-
  function(
    form
  , dat
  , nbest = 5
  ) {

  # all output in the bs "best subset" object
  bs <-
    leaps::regsubsets(
      form
    , data    = dat
    , nvmax   = 30
    , nbest   = nbest
    , method  = "exhaustive"
    )

  # selected output in named columns
  bs2 <-
    cbind(
      summary(bs)$which   # columns indicating which model terms are included
    , SIZE  = (rowSums(summary(bs)$which) - 1)  # number of terms in model
    , rss   = summary(bs)$rss                   # residual sum of squares
    , r2    = summary(bs)$rsq                   # R^2
    , adjr2 = summary(bs)$adjr2                 # Adj-R^2
    , cp    = summary(bs)$cp                    # Cp
    , bic   = summary(bs)$bic                   # BIC
    ) %>%
    tibble::as_tibble() %>%
    # sort models ascending by BIC (best model at top)
    dplyr::arrange(
      bic
    )

  # return sorted table
  return(bs2)
}
