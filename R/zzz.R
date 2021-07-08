## Startup functions ------------------------------------

#' .onAttach start message
#'
#' @param libname defunct
#' @param pkgname defunct
#'
#' @return invisible()
.onAttach <- function(libname, pkgname) {
  start_message <- c( "erikmisc, solving common complex data analysis workflows\n"
                      , "  by Dr. Erik Barry Erhardt <erik@StatAcumen.com>")
  packageStartupMessage(start_message)
  invisible()
}


#' .onLoad getOption package settings
#'
#' @param libname defunct
#' @param pkgname defunct
#'
#' @return
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

  invisible()
}