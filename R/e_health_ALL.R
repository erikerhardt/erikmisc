#' A1c category labels using CDC classification or user-defined categories
#'
#' See CDC \code{https://www.cdc.gov/diabetes/managing/managing-blood-sugar/a1c.html}
#'
#' @param a1c         A1c values
#' @param a1c_breaks  Breaks defining labeled ranges, intervals are closed on the left
#' @param a1c_labels  Labels for values between break values including lower bound
#'
#' @return a1c_cat, categorical levels of A1c
#' @export
#'
#' @examples
#' a1c      <- c(NA, seq(0,15, by =1), NA)
#' a1c_cat  <- e_categories_a1c(a1c)
#' data.frame(a1c, a1c_cat)
e_categories_a1c <-
  function(
    a1c
  , a1c_breaks    = c(-Inf, 5.7, 6.4, Inf)
  , a1c_labels    = c("Neither", "Pre-diabetes", "Diabetes")
  ) {

  # https://www.cdc.gov/diabetes/managing/managing-blood-sugar/a1c.html
  # A1c            Category
  # Below 5.7%     Normal
  # 5.7% to 6.4%   Prediabetes
  # 6.5% or above  Diabetes

  a1c_cat <-
    cut(
      a1c
    , breaks = a1c_breaks
    , labels = a1c_labels
    , right  = FALSE
    )

  ## tidyverse method
  ### @param a1c_max       A max A1c for a separate label at least this value
  ### @param a1c_max_label Label for at least \code{a1c_max} value
  ###, a1c_max       = 14.1
  ###, a1c_max_label = "Diabetes"
  # a1c_cat <-
  #   dplyr::case_when(
  #     is.na(a1c)                      ~ NA %>% as.character()
  #   ,                 (a1c < 5.7 )    ~ "Neither"
  #   , (a1c >= 5.7 ) & (a1c < 6.4 )    ~ "Pre-diabetes"
  #   , (a1c >= 6.4 ) & (a1c < a1c_max) ~ "Diabetes"
  #   , (a1c >= a1c_max )               ~ a1c_max_label
  #   ) %>%
  #   factor(
  #     levels =
  #       c(
  #         "Neither"
  #       , "Pre-diabetes"
  #       , "Diabetes"
  #       , a1c_max_label
  #       ) %>% unique()
  #   )

  return(a1c_cat)
}
