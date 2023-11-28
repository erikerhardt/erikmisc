#' CAHPS categorical to numeric response
#'
#' @param cahps_cat categorical variable from CAHPS
#' @param cahps_scale     scale type.  "neg1pos1" goes from minimum (negative) and maximum (positive) values for the scale
#' @param cahps_direction is low value category associated with negative or positive values
#'
#' @return cahps_val, numeric value of CAHPS
#' @export
#'
#' @examples
cahps_cat_to_val <-
  function(
    cahps_cat
  , cahps_scale     = c("neg1pos1", "other")[1]
  , cahps_direction = c("low_neg", "low_pos")[1]
  ) {
  # Citation for use
  # https://www.ahrq.gov/sites/default/files/wysiwyg/cahps/surveys-guidance/helpful-resources/analysis/2020-instructions-for-analyzing-data.pdf
  #
  # Requirements for Recoding Survey Response Options
  #
  # Tables 5.1 - 5.3 for numeric coding of categories.
  #
  # __Table 5.1 Yes/No Variables__
  # ```
  #   Typical Response Value      Recoded Numeric
  #   on CAHPS Surveys            Response value      Label/description           We use THIS
  #   2                           0                   No                          -1.0
  #   1                           1                   Yes                         +1.0
  #   Any other value             . (Missing)         Not analyzed                NA
  # ```
  #
  # __Table 5.2 Three Response Variables__
  # ```
  #   Typical Response Value      Recoded Numeric
  #   on CAHPS Surveys            Response value      Label/description           We use THIS
  #   1                           3                   Yes, definitely             +1.0
  #   2                           2                   Yes, somewhat                0.0
  #   3                           1                   No                          -1.0
  #   Any other value             . (Missing)         Not analyzed                NA
  # ```
  #
  # __Table 5.3 Four-Point Frequency Scale Variables__
  # ```
  #   Typical Response Value      Recoded Numeric
  #   on CAHPS Surveys            Response value      Label/description           We use THIS
  #   4                           1                   Definitely no               -1.0
  #   3                           2                   Somewhat no/Probably no     -0.33
  #   2                           3                   Somewhat yes/Probably yes   +0.33
  #   1                           4                   Definitely yes              +1.0
  #   Any other value             . (Missing)         Not analyzed                NA
  # ```

  ## cahps_cat = dat_pdp$cahpscc_pat_02
  ## cahps_cat = dat_pdp$cahpscc_pat_06

  num_levels <-
    cahps_cat |>
    levels() |>
    length()

  cahps_num <-
    cahps_cat |>
    as.numeric()

  if (cahps_scale == "neg1pos1") {
    val_unique <-
      cahps_num |>
      unique() |>
      na.omit() |>
      sort()

    #if (cahps_direction == "low_neg") {
    cahps_scale_val <-
      seq(-1, +1, length.out = num_levels)
    #}

    if (cahps_direction == "low_neg") {
      cahps_scale_val <-
        cahps_scale_val * +1
    }
    if (cahps_direction == "low_pos") {
      cahps_scale_val <-
        cahps_scale_val * -1
    }

  } else {
    return(NULL)
  }

  cahps_val <- rep(NA, length = length(cahps_num))

  for (i_val in seq_along(val_unique)) {
    ## i_val = 1
    ind <- which(cahps_num == val_unique[i_val])
    cahps_val[ind] <- cahps_scale_val[i_val]
  }

  # data.frame(cahps_val, cahps_num)  # check

  return(cahps_val)
}
