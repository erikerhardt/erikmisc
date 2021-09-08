#' Print an lm() anova table to html
#'
#' Open and copy html and paste into Excel to format tables for manuscript in MS Word.
#'
#' @param fit           result of lm()
#' @param n_digits      number of digits to use in table
#' @param filename_html filename.html to save html
#'
#' @return html table to print within Rmd report
#' @importFrom lmerTest as_lmerModLmerTest
#' @importFrom sjPlot tab_df
#' @importFrom sjPlot css_theme
#' @importFrom stats anova
#' @import dplyr
#' @export
#'
#' @examples
#' fit <-
#'   lm(
#'     mpg ~ cyl + disp + hp + drat + wt + qsec
#'   , data = dat_mtcars_e
#'   )
#' e_lm_print_html_anova(
#'     fit           = fit
#'   , n_digits      = 3
#'   , filename_html = NULL # NULL will not save html
#'   )
e_lm_print_html_anova <-
  function(
    fit           = fit
  , n_digits      = 3
  , filename_html = "table_anova.html"
  ) {
  #library(sjPlot)
  #library(sjmisc)
  #library(sjlabelled)

  if (class(fit) == "lm") {
    fit_anova <-
      fit %>%
      stats::anova()
  }
  if (class(fit) == "lmerMod") {
    fit_anova <-
      fit %>%
      lmerTest::as_lmerModLmerTest() %>%
      stats::anova()
  }

  tab <-
    fit_anova %>%
    as_tibble(rownames = "Variables") %>%
    mutate(sig = `Pr(>F)` %>% e_pval_stars())
                        #  %>%
                        # rename(
                        #   `Sum Sq`  = Sum.Sq
                        # , `Mean Sq` = Mean.Sq
                        # , `NumDF`   = NumDF
                        # , `DenDF`   = DenDF
                        # , `F value` = F.value
                        # , `Pr(>F)`  = Pr..F.
                        # )

  # print table to folder (this is the only way to print the clean html file)
  # the print() wrapper is needed to execute the file writing within the function.
  if (!is.null(filename_html)) {
    print(
      sjPlot::tab_df(
        x             = tab
      , title         = attr(fit, "heading")  #"ANOVA"
      #, footnote      = attr(fit, "heading")
      , digits        = n_digits
      , minus.sign    = "&#8722;"  # https://en.wikipedia.org/wiki/Plus_and_minus_signs
      , CSS           = sjPlot::css_theme(c("regression", "cells", "right_aligned")[1])
      , file          = filename_html
      )
    )
  }

  # save to object to print within report
  tab_html <-
    sjPlot::tab_df(
      x             = tab
    , title         = attr(fit, "heading")  #"ANOVA"
    #, footnote      = attr(fit, "heading")
    , digits        = n_digits
    , minus.sign    = "&#8722;"  # https://en.wikipedia.org/wiki/Plus_and_minus_signs
    , CSS           = sjPlot::css_theme(c("regression", "cells", "right_aligned")[1])
    #, file          = filename_html
    )

  return(tab_html)
}


#' Print an lm() summary table to html
#'
#' Open and copy html and paste into Excel to format tables for manuscript in MS Word.
#'
#' @param fit           result of lm()
#' @param filename_html filename.html to save html
#' @param n_digits_p    number of digits to use for p-values in table
#'
#' @return html table to print within Rmd report
#' @importFrom lmerTest as_lmerModLmerTest
#' @importFrom sjPlot tab_model
#' @importFrom sjPlot css_theme
#' @import dplyr
#' @export
#'
#' @examples
#' fit <-
#'   lm(
#'     mpg ~ cyl + disp + hp + drat + wt + qsec
#'   , data = dat_mtcars_e
#'   )
#' e_lm_print_html_summary(
#'     fit           = fit
#'   , n_digits_p    = 4
#'   , filename_html = NULL # NULL will not save html
#'   )
e_lm_print_html_summary <-
  function(
    fit           = fit
  , n_digits_p    = 4
  , filename_html = "table_model.html"
  ) {
  #library(sjPlot)
  #library(sjmisc)
  #library(sjlabelled)

  if (class(fit) == "lm") {
    fit <-
      fit
  }
  if (class(fit) == "lmerMod") {
    fit <-
      fit %>%
      lmerTest::as_lmerModLmerTest()
  }

  # print table to folder (this is the only way to print the clean html file)
  # the print() wrapper is needed to execute the file writing within the function.
  if (!is.null(filename_html)) {
    print(
      sjPlot::tab_model(
        fit
      , string.ci     = "CI (95%)"
      , string.p      = "p-value"
      , show.se       = TRUE
      , show.reflvl   = TRUE
      , prefix.labels = c("none", "varname", "label")[3]
      , p.style       = c("numeric", "stars", "numeric_stars", "scientific", "scientific_stars")[1]
      , digits.p      = n_digits_p
      , minus.sign    = "&#8722;"  # https://en.wikipedia.org/wiki/Plus_and_minus_signs
      , CSS           = sjPlot::css_theme(c("regression", "cells", "right_aligned")[1])
      , file          = filename_html
      )
    )
  }

  # save to object to print within report
  tab_html <-
    sjPlot::tab_model(
      fit
    , string.ci     = "CI (95%)"
    , string.p      = "p-value"
    , show.se       = TRUE
    , show.reflvl   = TRUE
    , prefix.labels = c("none", "varname", "label")[3]
    , p.style       = c("numeric", "stars", "numeric_stars", "scientific", "scientific_stars")[1]
    , digits.p      = n_digits_p
    , minus.sign    = "&#8722;"  # https://en.wikipedia.org/wiki/Plus_and_minus_signs
    , CSS           = sjPlot::css_theme(c("regression", "cells", "right_aligned")[1])
    #, file          = filename_html
    )

  return(tab_html)
}
