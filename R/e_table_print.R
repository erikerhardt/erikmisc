#' Print a data.frame table for latex and scale to fit page
#'
#' If using in Rmd file and a latex documnet, then requires two header includes:
#'
#' \preformatted{
#' EXAMPLE OF .Rmd YAML header
#'
#' ---
#' title: "Title"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output:
#'   pdf_document:
#'     df_print: kable
#'     latex_engine: lualatex
#'     toc: true
#'     number_sections: true
#'     toc_depth: 2
#'     keep_tex: true
#' header-includes:
#' - \usepackage{booktabs}
#' - \usepackage{colortbl}
#' always_allow_html: yes
#' fig_caption: TRUE
#' ---
#' }
#'
#' @param dat             data.frame or tibble to print
#' @param sw_scale        if "latex" T/F to scale to fit page, if "html" then a point size to pass to \code{kableExtra::kable_styling} argument \code{font_size}
#' @param sw_kable_format "html" or "latex" format
#'
#' @return
#' @export
#'
#' @examples
#' datasets::mtcars %>% head() %>% e_table_print()
#' \dontrun{
#' datasets::mtcars %>% head() %>% e_table_print(sw_scale = 6)
#' datasets::mtcars %>% head() %>% e_table_print(sw_scale = TRUE, sw_kable_format = "latex") # outputs into LaTeX document
#' }
e_table_print <-
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
} # e_table_print
