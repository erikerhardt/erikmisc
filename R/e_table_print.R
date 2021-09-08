#' Print a data.frame table for latex and scale to fit page
#'
#' If using in Rmd file and a latex documnet, then requires two header includes:
#'
#' \preformatted{
#' EXAMPLE OF .Rmd YAML header for pdf via LaTeX
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
#' @param sw_kable_format "simple", "html", "latex", or "doc"
#' @param sw_latex_options passed to \code{kableExtra::kable_styling(latex_options = sw_latex_options)}, updated if \code{sw_scale=TRUE} to include \code{"scale_down"}
#' @param ...             other arguments passed to \code{knitr::kable()}
#'
#' @return \code{invisible(NULL)}
#' @importFrom knitr kable
#' @importFrom kableExtra kbl
#' @importFrom kableExtra kable_paper
#' @importFrom kableExtra kable_styling
#' @importFrom flextable regulartable
#' @export
#'
#' @examples
#' datasets::mtcars %>% head() %>% e_table_print()
#' \dontrun{
#' # html rescaled size
#' datasets::mtcars %>%
#'   head() %>%
#'   e_table_print(sw_scale = 6)
#' # outputs into LaTeX document, scaling is automatic to fit page width
#' # works best before a \clearpage
#' # use chunk option: results = 'asis', see other options at https://yihui.org/knitr/options/
#' datasets::mtcars %>%
#'   head() %>%
#'   e_table_print(sw_scale = TRUE, sw_kable_format = "latex")
#' }
e_table_print <-
  function(
    dat
  , sw_scale = FALSE
  , sw_kable_format  = c("simple", "kbl", "html", "latex", "doc")[2]
  , sw_latex_options = c("basic", "striped", "hold_position", "HOLD_position", "scale_down", "repeat_header")[c(2, 3)]
  , ...
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

  if (sw_kable_format == "simple") {
    dat %>%
    knitr::kable(format = sw_kable_format, ...) %>%
    print()
  }
  if (sw_kable_format == "kbl") {
    if (sw_scale) {
      dat %>%
      kableExtra::kbl() %>%
      kableExtra::kable_paper(lightable_options = "hover", full_width = FALSE, bootstrap_options = "striped", position = "center", font_size = sw_scale) %>%
      #kableExtra::kable_styling(font_size = sw_scale) %>%
      print()
    } else {
      dat %>%
      kableExtra::kbl() %>%
      kableExtra::kable_paper(lightable_options = "hover", full_width = FALSE, bootstrap_options = "striped", position = "center") %>%
      #kableExtra::kable_styling() %>%
      print()
    }
  }
  if (sw_kable_format == "html") {
    if (sw_scale) {
      dat %>%
      knitr::kable(format = sw_kable_format, booktabs = TRUE, linesep = "", ...) %>%
      kableExtra::kable_styling(full_width = FALSE, bootstrap_options = "striped", position = "center", font_size = sw_scale) %>%
      print()
    } else {
      dat %>%
      knitr::kable(format = sw_kable_format, booktabs = TRUE, linesep = "", ...) %>%
      kableExtra::kable_styling(full_width = FALSE, bootstrap_options = "striped", position = "center") %>%
      print()
    }
  }
  if (sw_kable_format == "latex") {
    if (sw_scale) {
      sw_latex_options = c(sw_latex_options, "scale_down") %>% unique()
      dat %>%
      knitr::kable(format = sw_kable_format, booktabs = TRUE, linesep = "", ...) %>%
      kableExtra::kable_styling(full_width = FALSE, position = "center", latex_options = sw_latex_options) %>%
      print()
    } else {
      dat %>%
      knitr::kable(format = sw_kable_format, booktabs = TRUE, linesep = "", ...) %>%
      kableExtra::kable_styling(full_width = FALSE, position = "center", latex_options = sw_latex_options) %>%   # , "scale_down"
      print()
    }
  }
  if (sw_kable_format == "doc") {
    if (sw_scale) {
      dat %>%
      flextable::regulartable() %>%
      print()
    } else {
      dat %>%
      flextable::regulartable() %>%
      print()
    }
  }

  invisible(NULL)
} # e_table_print
