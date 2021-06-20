#' Print a data.frame table for latex and scale to fit page
#'
#' @param dat
#' @param sw_scale
#' @param sw_kable_format
#'
#' @return
#' @export
#'
#' @examples
e_kable_latex <-
  function(
    dat
  , sw_scale = FALSE
  , sw_kable_format = c("html", "latex")[1]
  ) {
  # printing pretty tables
  # requires
  # latex:
  #   - \usepackage{booktabs}
  #   - \usepackage{colortbl}
  # R:
  #   library(knitr)
  #   library(kableExtra)
  # http://haozhu233.github.io/kableExtra/awesome_table_in_pdf.pdf # tables
  # Example with packages: https://stackoverflow.com/questions/59994486/r-markdown-trouble-with-rowcolor-in-kable-styling

  if (sw_kable_format == "latex") {
    if (sw_scale) {
      dat %>%
      knitr::kable(format = sw_kable_format, booktabs = TRUE, linesep = "") %>%
      kableExtra::kable_styling(full_width = FALSE, position = "center", latex_options = c("striped", "scale_down"))
    } else {
      dat %>%
      knitr::kable(format = sw_kable_format, booktabs = TRUE, linesep = "") %>%
      kableExtra::kable_styling(full_width = FALSE, position = "center", latex_options = c("striped"))  # , "scale_down"
    }
  }
  if (sw_kable_format == "html") {
    if (sw_scale) {
      dat %>%
      knitr::kable(format = sw_kable_format, booktabs = TRUE, linesep = "") %>%
      kableExtra::kable_styling(full_width = FALSE, bootstrap_options = "striped", position = "center", font_size = sw_scale)
    } else {
      dat %>%
      knitr::kable(format = sw_kable_format, booktabs = TRUE, linesep = "") %>%
      kableExtra::kable_styling(full_width = FALSE, bootstrap_options = "striped", position = "center")
    }
  }
} # e_kable_latex
