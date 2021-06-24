#' Federal Poverty Level ratio
#'
#' @param income_annual
#' @param n_persons
#' @param poverty_year
#'
#' @return poverty_ratio, the ratio of a person's income to the Federal Poverty Level based on the number of people in the household
#' @export
#'
#' @examples
poverty_ratio_FederalPovertyLevel <-
  function(
    income_annual = 0
  , n_persons     = 1
  , poverty_year  = 2016
  ) {
  ## income_annual = seq(10000, 100000, length = 15)
  ## n_persons     = 1:15

    # Annual Update of the HHS Poverty Guidelines
    # A Notice by the Health and Human Services Department on 01/25/2016
    # Document Citation: 81 FR 4036
    # Page: 4036-4037 (2 pages)
    # Document Number: 2016-01450
    # https://www.federalregister.gov/d/2016-01450
    #
    # 2016 Poverty Guidelines for the 48 Contiguous States and the District of Columbia
    #
    # Persons in family/household Poverty guideline
    # 1 11,880
    # 2 16,020
    # 3 20,160
    # 4 24,300
    # 5 28,440
    # 6 32,580
    # 7 36,730
    # 8 40,890
    # For families/households with more than 8 persons, add $4,160 for each additional person.

  fed_pov_level <-
    case_when(
      n_persons ==  1 ~ 11880
    , n_persons ==  2 ~ 16020
    , n_persons ==  3 ~ 20160
    , n_persons ==  4 ~ 24300
    , n_persons ==  5 ~ 28440
    , n_persons ==  6 ~ 32580
    , n_persons ==  7 ~ 36730
    , n_persons ==  8 ~ 40890
    , n_persons >   8 ~ 40890 + (n_persons - 8) * 4160
    )

  poverty_ratio <- income_annual / fed_pov_level

  return(poverty_ratio)
}
