#' Convert all factor variables to their numeric factor levels
#'
#' Before saving to csv, default is the factor labels.  This replaces those with the numeric factor levels.
#'
#' @param dat data.frame or tibble
#'
#' @return dat with factor variables converted to numeric
#' @export
#'
#' @examples
#' dat <- datasets::mtcars %>% mutate(cyl = cyl %>% factor(), vs = vs %>% factor())
#' str(dat)
#' dat2 <- dat %>% e_data_convert_factor_to_numeric()
#' str(dat2)
#' # readr::write_csv(x = dat , file = "dat.csv"    )
#' # readr::write_csv(x = dat2, file = "dat_num.csv")
e_data_convert_factor_to_numeric <-
  function(
    dat
  ) {
  ## dat = dat_pdp
  for (i_var in 1:ncol(dat)) {
    if("factor" %in% class(dat[,i_var])) {
      dat[,i_var] <- as.numeric(dat[,i_var])
    }
  }
  return(dat)
}
