#' The erikmisc logo, using ASCII or Unicode characters
#'
#' Use [crayon::strip_style()] to get rid of the colors.
#'
#' @param sw_unicode Whether to use Unicode symbols. Default is `TRUE`
#'   on UTF-8 platforms.
#'
#' @importFrom purrr map2
#' @importFrom crayon make_style
#' @importFrom crayon blue
#' @export
#' @examples
#' erikmisc_logo()
erikmisc_logo <-
  function
  (
    sw_unicode = l10n_info()$`UTF-8`
  ) {
# https://www.colorschemer.com/ascii-art-generator/
# Standard, Smush
#            _ _              _
#   ___ _ __(_) | ___ __ ___ (_)___  ___
#  / _ \ '__| | |/ / '_ ` _ \| / __|/ __|
# |  __/ |  | |   <| | | | | | \__ \ (__
#  \___|_|  |_|_|\_\_| |_| |_|_|___/\___|
#
# Fitted
#              _  _                _
#   ___  _ __ (_)| | __ _ __ ___  (_) ___   ___
#  / _ \| '__|| || |/ /| '_ ` _ \ | |/ __| / __|
# |  __/| |   | ||   < | | | | | || |\__ \| (__
#  \___||_|   |_||_|\_\|_| |_| |_||_||___/ \___|
#
# Full
#                _   _                  _
#   ___   _ __  (_) | | __  _ __ ___   (_)  ___    ___
#  / _ \ | '__| | | | |/ / | '_ ` _ \  | | / __|  / __|
# |  __/ | |    | | |   <  | | | | | | | | \__ \ | (__
#  \___| |_|    |_| |_|\_\ |_| |_| |_| |_| |___/  \___|

  # original
  # logo <-
  #   c(
  #     ""
  #   , "   0    1   _ _  2      9    _  3  4     "
  #   , "   ___ _ __(_) | ___ __ ___ (_)___  ___  "
  #   , "  / _ \\ '__| | |/ / '_ ` _ \\| / __|/ __| "
  #   , " |  __/ |  | |   <| | | | | | \\__ \\ (__  "
  #   , "  \\___|_|  |_|_|\\_\\_| |_| |_|_|___/\\___| "
  #   , "     5       6           7      8     9  "
  #   , ""
  #   )
  logo <-
    c(
      ""
    , " 3        8 _ _   7          _    9  2   "
    , "   ___ _ __(_) | ___ __ ___ (_)___  ___  "
    , "  / _ \\ '__| | |/ / '_ ` _ \\| / __|/ __| "
    , " |  __/ |  | |   <| | | | | | \\__ \\ (__  "
    , "  \\___|_|  |_|_|\\_\\_| |_| |_|_|___/\\___| "
    , "    4 1       9           5   0       6  "
    , ""
    )

  hexa <- c("*", ".", "o", "*", ".", "*", ".", "o", ".", "*")

  if (sw_unicode) {
    hexa <- c("*" = "\u2b22", "o" = "\u2b21", "." = ".")[hexa]
  }

  cols <-
    c("red", "yellow", "green", "magenta", "cyan", "yellow",
      "green", "white", "magenta", "cyan")

  col_hexa <-
    purrr::map2(hexa, cols, ~ crayon::make_style(.y)(.x))

  for (i in 0:9) {
    pat <- paste0("\\b", i, "\\b")
    logo <- sub(pat, col_hexa[[i + 1]], logo)
  }

  #structure(crayon::blue(logo), class = "tidyverse_logo")
  structure(crayon::blue(logo), class = "erikmisc_logo")

}


#' Print for erikmisc_logo
#'
#' @param x the \code{erikmisc_logo()}
#' @param ... additional parameters passed to \code{cat()}
#'
#' @return the logo, invisibly
#' @export
print.erikmisc_logo <-
  function(
    x
  , ...
  ) {
  cat(x, ..., sep = "\n")
  invisible(x)
}
