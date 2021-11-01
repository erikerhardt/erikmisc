## Startup functions ------------------------------------

# Based on zzz.R from the tidyverse package, https://github.com/tidyverse/tidyverse/blob/main/R/zzz.R
#' .onAttach start message
#'
#' @param ...             arguments
#' @return invisible(NULL)
.onAttach <- function(...) {
  start_message <- c( "erikmisc, solving common complex data analysis workflows\n"
                      , "  by Dr. Erik Barry Erhardt <erik@StatAcumen.com>")
  packageStartupMessage(start_message)
  #print(erikmisc_logo())

  needed <- core[!is_attached(core)]
  if (length(needed) == 0)
    return()

  crayon::num_colors(TRUE)
  erikmisc_attach()

  if (!"package:conflicted" %in% search()) {
    x <- erikmisc_conflicts()
    msg(erikmisc_conflict_message(x), startup = TRUE)
  }

  invisible(NULL)
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}

#' .onLoad getOption package settings
#'
#' @param libname defunct
#' @param pkgname defunct
#'
#' @return invisible(NULL)
#'
#' @examples
#' getOption("erikmisc.name")
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.erikmisc <- list(
    #erikmisc.path = "~/R-dev",
    erikmisc.install.args  = "",
    erikmisc.name          = "Erik Barry Erhardt",
    erikmisc.desc.author   = "Erik Erhardt <erik@StatAcumen.com> [aut, cre]",
    erikmisc.desc.license  = "GPL (>= 2)",
    erikmisc.desc.suggests = NULL,
    erikmisc.desc          = list()
  )
  toset <- !(names(op.erikmisc) %in% names(op))
  if(any(toset)) options(op.erikmisc[toset])

  invisible(NULL)
}
