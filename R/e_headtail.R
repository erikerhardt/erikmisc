#' Print head and tail of a data.frame
#'
#' @param x           data.frame or vector
#' @param n           number of top rows and bottom rows to print, it prints 2n rows
#' @param which_col   selected columns, if desired
#' @param ...         passed to head() and tail() functions
#'
#' @return dat_ht     the head and tail of the data.frame
#' @export
#'
#' @examples
#' e_headtail(datasets::mtcars)
e_headtail <-
  function(
    x
  , n             = 3L
  , which_col     = NULL
  , ...
  ) {
  # https://rdrr.io/cran/FSA/src/R/FSAUtils.R

    ## Not using this -- doesn't matter for non-rownames datasets, and overwrites rownames when it is used
    # @param sw_addrownum  also print row numbers?
    # sw_addrownum  = TRUE

  if (length(n)!=1L) {
    stop("'n' must be a single number.")
  }

  ## tables
  if (is.matrix(x) | is.data.frame(x)) {

    ## Remove tbl_df class if it exists
    if ("tbl_df" %in% class(x)) {
      x <- as.data.frame(x)
    }

    ## Process data.frame
    N <- nrow(x)

    if (n >= N) {
      tmp <- x
    } else {
      dat_h <- utils::head(x, n, ...)

      # if (sw_addrownum) {
      #   if (is.null(rownames(x))) {
      #     rownames(dat_h) <- paste0("[", seq_len(n), ",]")
      #   }
      # } else {
      #   rownames(dat_h) <- NULL
      # }

      dat_t <- utils::tail(x, n, ...)
      dat_ht <- rbind(dat_h, dat_t)
    }
    if (!is.null(which_col)) {
      dat_ht <- dat_ht[, which_col]
    }
  }

  # vector
  if (is.vector(x)) {

    ## Process data.frame
    N <- length(x)

    if (n >= N) {
      tmp <- x
    } else {
      dat_h <- utils::head(x, n, ...)

      # if (sw_addrownum) {
      #   if (is.null(rownames(x))) {
      #     rownames(dat_h) <- paste0("[", seq_len(n), ",]")
      #   }
      # } else {
      #   rownames(dat_h) <- NULL
      # }

      dat_t <- utils::tail(x, n, ...)
      dat_ht <- c(dat_h, dat_t)
    }
  }

  return(dat_ht)
} # e_headtail
