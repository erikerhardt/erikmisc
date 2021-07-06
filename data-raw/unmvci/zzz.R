## Startup functions ------------------------------------

#' .onAttach start message
#'
#' @param libname defunct
#' @param pkgname defunct
#'
#' @return invisible()
.onAttach <- function(libname, pkgname) {
  start_message <- c( "unmvci, UNM UNM Vascular Cognitive Impairment (VCI) Project\n"
                      , "  by Dr. Erik Barry Erhardt <erik@StatAcumen.com>\n"
                      , "  PI: Dr. Gary Rosenberg\n")
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
#' getOption("unmvci.name")
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.unmvci <- list(
    #unmvci.path = "~/R-dev",
    unmvci.install.args  = "",
    unmvci.name          = "Erik Barry Erhardt",
    unmvci.desc.author   = "Erik Erhardt <erik@StatAcumen.com> [aut, cre]",
    unmvci.desc.license  = "GPL (>= 2)",
    unmvci.desc.suggests = NULL,
    unmvci.desc          = list()
  )
  toset <- !(names(op.unmvci) %in% names(op))
  if(any(toset)) options(op.unmvci[toset])

  invisible()
}
