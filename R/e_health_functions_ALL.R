## All health related functions

#' A1c concentration to CDC classification category labels
#'
#' https://www.cdc.gov/diabetes/managing/managing-blood-sugar/a1c.html
#'
#' @param a1c           a1c concentration values
#' @param sw_fine_cat   T/F Finer classification for A1c category labels. False = 3 categories with cut points: 5.7 and 6.4. True = 5 categories with cut points: 5.7, 6.4, 9, and 12.
#' @return a1c_cat      categorical levels of A1c
#' @export
#'
#' @examples
#' 0:16 |> e_calc_health_a1c_categories()
#' 0:16 |> e_calc_health_a1c_categories(sw_fine_cat = c(FALSE, TRUE)[2])
e_health_calc_a1c_categories <-
  function(
    a1c
  , sw_fine_cat = c(FALSE, TRUE)[1]
  ) {

  # https://www.cdc.gov/diabetes/managing/managing-blood-sugar/a1c.html
  # A1c
  # Below 5.7%     Normal
  # 5.7% to 6.4%   Prediabetes
  # 6.5% or above  Diabetes

  if(sw_fine_cat == c(FALSE, TRUE)[1]) {
    a1c_cat <- rep(NA, length(a1c))
    a1c_cat[                (a1c < 5.7 )] <- "Neither"
    a1c_cat[(a1c >= 5.7 ) & (a1c < 6.4 )] <- "Pre-diabetes"
    a1c_cat[(a1c >= 6.4 )               ] <- "Diabetes"

    a1c_cat <- factor(a1c_cat
                    #, levels = 1:4
                    , levels = c( "Neither"
                                , "Pre-diabetes"
                                , "Diabetes"
                                )
                    #, ordered = TRUE
                    )
  } # FALSE

  # Finer classification for A1c category labels
  if(sw_fine_cat == c(FALSE, TRUE)[2]) {
    a1c_cat <- rep(NA, length(a1c))
    a1c_cat[                (a1c <  5.7)] <- "0   -  5.7"
    a1c_cat[(a1c >=  5.7) & (a1c <  6.4)] <- "5.7 -  6.4"
    a1c_cat[(a1c >=  6.4) & (a1c <  9  )] <- "6.4 -  9  "
    a1c_cat[(a1c >=  9  ) & (a1c < 12  )] <- "9   - 12  "
    a1c_cat[(a1c >= 12  )               ] <- "12  - 14+ "

    a1c_cat <- factor(a1c_cat
                    #, levels = 1:4
                    , levels = c( "0   -  5.7"
                                , "5.7 -  6.4"
                                , "6.4 -  9  "
                                , "9   - 12  "
                                , "12  - 14+ "
                                )
                    #, ordered = TRUE
                    )
  } # TRUE

  return(a1c_cat)
} # e_health_calc_a1c_categories



#' Calculate BMI
#'
#' @param weight    list of weights (kg or lb)
#' @param height    list of heights (cm or in)
#' @param system    "Metric" (kg, cm) or "English" (lb, in)
#'
#' @return  bmi     value
#' @export
#'
#' @examples
#' e_health_calc_bmi(
#'   weight = 160
#' , height = 71
#' , system = c("Metric", "English")[2]
#' )
e_health_calc_bmi <-
  function(
    weight
  , height
  , system = c("Metric", "English")[1]
  ) {

  if (system == "Metric") {
    # https://www.cdc.gov/healthyweight/assessing/bmi/adult_bmi/index.html
    # Formula: weight (kg) / [height (m)]^2
    # weight in kg
    # height in cm
    bmi <- weight / ((height / 100)^2)
  }

  if (system == "English") {
    # https://www.cdc.gov/nccdphp/dnpao/growthcharts/training/bmiage/page5_2.html
    # Formula: weight (lb) / [height (in)]^2 x 703
    # weight in lb
    # height in in
    bmi <- weight / (height^2) * 703
  }

  return(bmi)
} # e_health_calc_bmi


#' CDC classification for BMI category labels
#'
#' https://www.cdc.gov/healthyweight/assessing/bmi/adult_bmi/index.html
#'
#' @param bmi
#'
#' @return bmi_cat, categorical levels of BMI
#' @export
#'
#' @examples
#' e_health_calc_bmi(
#'   weight = 160
#' , height = 71
#' , system = c("Metric", "English")[2]
#' ) |>
#' e_health_bmi_categories()
e_health_calc_bmi_categories <- function(bmi) {

  # https://www.cdc.gov/healthyweight/assessing/bmi/adult_bmi/index.html
  # BMI
  # Weight          Status
  # Below 18.5      Underweight
  # 18.5 - 24.9     Normal or Healthy Weight
  # 25.0 - 29.9     Overweight
  # 30.0 and Above  Obese

  bmi_cat <- rep(NA, length(bmi))
  bmi_cat[                (bmi < 18.5)] <- "Underweight"
  bmi_cat[(bmi >= 18.5) & (bmi < 25  )] <- "Normal or Healthy Weight"
  bmi_cat[(bmi >= 25  ) & (bmi < 30  )] <- "Overweight"
  bmi_cat[(bmi >= 30  )               ] <- "Obese"

  bmi_cat <- factor(bmi_cat
                  #, levels = 1:4
                  , levels = c( "Underweight"
                              , "Normal or Healthy Weight"
                              , "Overweight"
                              , "Obese"
                              )
                  , ordered = TRUE
                  )

  return(bmi_cat)
} # e_health_calc_bmi_categories



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
#' e_health_calc_cahps_cat_to_val(
#'   cahps_cat       = c(1,2,1,2) |> factor(levels = c(1, 2), labels = c("No", "Yes"))
#' , cahps_scale     = c("neg1pos1", "other")[1]
#' , cahps_direction = c("low_neg", "low_pos")[1]
#' )
e_health_calc_cahps_cat_to_val <-
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
} # e_health_calc_cahps_cat_to_val



#' PHQ-2 Scores and Proposed Treatment Actions, Table 4 from Kroenke K, Spitzer RL, Psychiatric Annals 2002;32:509-521
#'
#' @param phq2_total_score    total score
#'
#' @return phq2_cat, categorical levels of PHQ-2
#' @export
#'
#' @examples
#' 0:6 |> e_health_calc_phq2_categories()
e_health_calc_phq2_categories <-
  function(
    phq2_total_score
  ) {

  # https://www.pcpcc.org/sites/default/files/resources/instructions.pdf
  # https://www-jstor-org.libproxy.unm.edu/stable/pdf/3768417.pdf
  # https://www.hiv.uw.edu/page/mental-health-screening/phq-2

  # Patient Health Questionnaire-2 (PHQ-2)
  # The PHQ-2 inquires about the frequency of depressed mood and anhedonia over the past two weeks. The PHQ-2 includes the first two items of the PHQ-9.
  #
  # The purpose of the PHQ-2 is to screen for depression in a "first-step" approach.
  #
  # Patients who screen positive should be further evaluated with the PHQ-9 to determine whether they meet criteria for a depressive disorder.
  #
  # Over the last 2 weeks, how often have you been bothered by the following problems?
  #
  #     0             +1              +2                        +3
  #     Not at all    Several days    More than half the days   Nearly every day
  # 1.Little interest or pleasure in doing things
  # 2.Feeling down, depressed or hopeless
  #
  # PHQ-2 score obtained by adding score for each question (total points)
  #
  # Interpretation:
  #   A PHQ-2 score ranges from 0-6. The authors identified a score of 3 as the optimal cutpoint when using the PHQ-2 to screen for depression.
  #   If the score is 3 or greater, major depressive disorder is likely.
  #   Patients who screen positive should be further evaluated with the PHQ-9, other diagnostic instruments, or direct interview to determine whether they meet criteria for a depressive disorder.

  # PHQ-2
  # Score   Depression Severity     Proposed Treatment Actions
  # 0 - 2   None-minimal            None
  # 3 - 6   Depressed               Follow up with PHQ-9

  phq2_cat <- rep(NA, length(phq2_total_score))
  phq2_cat[                           (phq2_total_score <  3)] <- "None-minimal"
  phq2_cat[(phq2_total_score >=  3) & (phq2_total_score <= 6)] <- "Depressed"

  phq2_cat <-
    factor(
      phq2_cat
    , levels =
        c(
          "None-minimal"
        , "Depressed"
        )
    , ordered = TRUE
    )

  return(phq2_cat)
} # e_health_calc_phq2_categories



#' PHQ-9 Scores and Proposed Treatment Actions, Table 4 from Kroenke K, Spitzer RL, Psychiatric Annals 2002;32:509-521
#'
#' @param phq9_total_score    total score
#'
#' @return phq9_cat, categorical levels of PHQ-9
#' @export
#'
#' @examples
#' 0:27 |> e_health_calc_phq9_categories()
e_health_calc_phq9_categories <-
  function(
    phq9_total_score
  ) {

  # https://www.pcpcc.org/sites/default/files/resources/instructions.pdf

    # Over the last 2 weeks, how often have you been bothered by any of the following:
    # a. Little interest or pleasure in doing things?
    # b. Feeling down, depressed, or hopeless?
    # c. Trouble falling or staying asleep, or sleeping too much?
    # d. Feeling tired or having little energy?
    # e. Poor appetite or overeating?
    # f. Feeling bad about yourself - or that you are a failure or have let yourself or your family down?
    # g. Trouble concentrating on things, such as reading the newspaper or watching television?
    # h. Moving or speaking so slowly that other people could have noticed? Or the opposite -- being so fidgety or restless that you have been moving around a lot more than usual?
    # i. Thoughts that you would be better off dead or of hurting yourself in some way?
    #
    # Responses
    # 0 Not at all
    # 1 Several days
    # 2 More than half the days
    # 3 Nearly every day

  # PHQ-9
  # Score   Depression Severity     Proposed Treatment Actions
  # 0 - 4   None-minimal            None
  # 5 - 9   Mild                    Watchful waiting; repeat PHQ-9 at follow-up
  # 10 - 14 Moderate                Treatment plan, considering counseling, follow-up and/or pharmacotherapy
  # 15 - 19 Moderately Severe       Active treatment with pharmacotherapy and/or psychotherapy
  # 20 - 27 Severe                  Immediate initiation of pharmacotherapy and, if severe impairment or poor response to therapy, expedited referral to a mental health specialist for psychotherapy and/or collaborative management

  phq9_cat <- rep(NA, length(phq9_total_score))
  phq9_cat[                           (phq9_total_score <  5)] <- "None-minimal"
  phq9_cat[(phq9_total_score >=  5) & (phq9_total_score < 10)] <- "Mild"
  phq9_cat[(phq9_total_score >= 10) & (phq9_total_score < 15)] <- "Moderate"
  phq9_cat[(phq9_total_score >= 15) & (phq9_total_score < 20)] <- "Moderately Severe"
  phq9_cat[(phq9_total_score >= 20)                          ] <- "Severe"

  phq9_cat <-
    factor(
      phq9_cat
    , levels =
        c(
          "None-minimal"
        , "Mild"
        , "Moderate"
        , "Moderately Severe"
        , "Severe"
        )
    , ordered = TRUE
    )

  return(phq9_cat)
} # e_health_calc_phq9_categories


#' Federal Poverty Level ratio
#'
#' @param income_annual   annual income in dollars
#' @param n_persons       number of persons in household
#' @param poverty_year    year for comparison (currently only for 2016)
#'
#' @return poverty_ratio, the ratio of a person's income to the Federal Poverty Level based on the number of people in the household
#' @export
#'
#' @examples
#' e_health_calc_poverty_ratio_FederalPovertyLevel(
#'     income_annual = seq(0, 60000, by = 10000)
#'   , n_persons     = 1:7
#'   , poverty_year  = 2016
#' )
e_health_calc_poverty_ratio_FederalPovertyLevel <-
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
} # e_health_calc_poverty_ratio_FederalPovertyLevel
