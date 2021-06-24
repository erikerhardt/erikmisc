## Startup functions ------------------------------------

#' .onAttach start message
#'
#' @param libname defunct
#' @param pkgname defunct
#'
#' @return invisible()
.onAttach <- function(libname, pkgname) {
  start_message <- c( "unmpdp, UNM PCORI Diabetes Project\n"
                      , "  by Dr. Erik Barry Erhardt <erik@StatAcumen.com>\n"
                      , "  PI: Janet M Page-Reeves")
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
#' getOption("unmpdp.name")
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.unmpdp <- list(
    #unmpdp.path = "~/R-dev",
    unmpdp.install.args  = "",
    unmpdp.name          = "Erik Barry Erhardt",
    unmpdp.desc.author   = "Erik Erhardt <erik@StatAcumen.com> [aut, cre]",
    unmpdp.desc.license  = "GPL (>= 2)",
    unmpdp.desc.suggests = NULL,
    unmpdp.desc          = list()
  )
  toset <- !(names(op.unmpdp) %in% names(op))
  if(any(toset)) options(op.unmpdp[toset])

  invisible()
}
