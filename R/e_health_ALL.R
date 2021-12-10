# Index for this file
# A1c
# BMI
# phq2
# phq9


#' A1c category labels using CDC classification or user-defined categories
#'
#' See CDC \code{https://www.cdc.gov/diabetes/managing/managing-blood-sugar/a1c.html}
#'
#' @param a1c         A1c values
#' @param a1c_breaks  Breaks defining labeled ranges, intervals are closed on the left
#' @param a1c_labels  Labels for values between break values including lower bound
#'
#' @return a1c_cat, categorical levels of A1c
#' @export
#'
#' @examples
#' a1c      <- c(NA, seq(0,15, by =1), NA)
#' a1c_cat  <- e_calc_a1c_categories(a1c)
#' data.frame(a1c, a1c_cat)
e_calc_a1c_categories <-
  function(
    a1c
  , a1c_breaks    = c(-Inf, 5.7, 6.4, Inf)
  , a1c_labels    = c("Neither", "Pre-diabetes", "Diabetes")
  ) {

  # https://www.cdc.gov/diabetes/managing/managing-blood-sugar/a1c.html
  # A1c            Category
  # Below 5.7%     Normal
  # 5.7% to 6.4%   Prediabetes
  # 6.5% or above  Diabetes

  a1c_cat <-
    cut(
      a1c
    , breaks = a1c_breaks
    , labels = a1c_labels
    , right  = FALSE
    )

  ## tidyverse method
  ### @param a1c_max       A max A1c for a separate label at least this value
  ### @param a1c_max_label Label for at least \code{a1c_max} value
  ###, a1c_max       = 14.1
  ###, a1c_max_label = "Diabetes"
  # a1c_cat <-
  #   dplyr::case_when(
  #     is.na(a1c)                      ~ NA %>% as.character()
  #   ,                 (a1c < 5.7 )    ~ "Neither"
  #   , (a1c >= 5.7 ) & (a1c < 6.4 )    ~ "Pre-diabetes"
  #   , (a1c >= 6.4 ) & (a1c < a1c_max) ~ "Diabetes"
  #   , (a1c >= a1c_max )               ~ a1c_max_label
  #   ) %>%
  #   factor(
  #     levels =
  #       c(
  #         "Neither"
  #       , "Pre-diabetes"
  #       , "Diabetes"
  #       , a1c_max_label
  #       ) %>% unique()
  #   )

  return(a1c_cat)
}


#' BMI Calculation
#'
#' @param weight in kg or lb
#' @param height in cm or in
#' @param system, either "Metric" (kg, cm) or "English" (lb, in)
#'
#' @return bmi value
#' @export
#'
#' @examples
#' e_calc_bmi(60, 150)
#' e_calc_bmi(132, 59, system = "English")
#' e_calc_bmi(seq(40, 70, by = 5), 150)
e_calc_bmi <-
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
}


#' BMI category labels using CDC classification
#'
#' @param bmi         BMI value
#' @param bmi_breaks  Breaks defining labeled ranges, intervals are closed on the left
#' @param bmi_labels  Labels for values between break values including lower bound
#'
#' @return bmi_cat, categorical levels of BMI
#' @export
#'
#' @examples
#' e_calc_bmi(60, 150) %>% e_calc_bmi_categories()
#' e_calc_bmi(seq(40, 70, by = 5), 150) %>% e_calc_bmi_categories()
e_calc_bmi_categories <-
  function(
    bmi
  , bmi_breaks    = c(-Inf, 18.5, 25, 30, Inf)
  , bmi_labels    = c("Underweight", "Normal or Healthy Weight", "Overweight", "Obese")
  ) {

  # https://www.cdc.gov/healthyweight/assessing/bmi/adult_bmi/index.html
  # BMI
  # Weight          Status
  # Below 18.5      Underweight
  # 18.5 - 24.9     Normal or Healthy Weight
  # 25.0 - 29.9     Overweight
  # 30.0 and Above  Obese

  bmi_cat <-
    cut(
      bmi
    , breaks = bmi_breaks
    , labels = bmi_labels
    , right  = FALSE
    )

  return(bmi_cat)
}


#' PHQ-2 category labels using Table 4 from Kroenke K, Spitzer RL, Psychiatric Annals 2002;32:509-521
#'
#' PHQ-2 Scores and Proposed Treatment Actions.
#' https://www.pcpcc.org/sites/default/files/resources/instructions.pdf
#'
#' The PHQ-2 inquires about the frequency of depressed mood and anhedonia over the past two weeks. The PHQ-2 includes the first two items of the PHQ-9.
#' The purpose of the PHQ-2 is to screen for depression in a "first-step" approach.
#' Patients who screen positive should be further evaluated with the PHQ-9 to determine whether they meet criteria for a depressive disorder.
#' PHQ-2 score obtained by adding score for each question (total points)
#'
#' Interpretation:
#'   A PHQ-2 score ranges from 0-6. The authors identified a score of 3 as the optimal cutpoint when using the PHQ-2 to screen for depression.
#'   If the score is 3 or greater, major depressive disorder is likely.
#'   Patients who screen positive should be further evaluated with the PHQ-9, other diagnostic instruments, or direct interview to determine whether they meet criteria for a depressive disorder.
#'
#' @param phq2         PHQ-2 scores (0 - 6)
#' @param phq2_breaks  Breaks defining labeled ranges, intervals are closed on the left (typically, 3 or greater indicates major depressive disorder is likely)
#' @param phq2_labels  Labels for values between break values including lower bound
#'
#' @return phq2_cat, categorical levels of PHQ-2
#' @export
#'
#' @examples
#' e_calc_phq2_categories(c(0, 2, 3, 5))
#'
#' dplyr::tibble(
#'   phq2 = seq(0, 6, by = 1)
#' , phq2_cat = phq2 %>% e_calc_phq2_categories()
#' ) %>%
#' print(n = Inf)
e_calc_phq2_categories <-
  function(
    phq2
  , phq2_breaks    = c(-Inf, 3, Inf)
  , phq2_labels    = c("None-minimal", "Depressed")
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

  phq2_cat <-
    cut(
      phq2
    , breaks = phq2_breaks
    , labels = phq2_labels
    , right  = FALSE
    )

  return(phq2_cat)
}


#' PHQ-9 category labels using Table 4 from Kroenke K, Spitzer RL, Psychiatric Annals 2002;32:509-521
#'
#' PHQ-9 Scores and Proposed Treatment Actions.
#' https://www.pcpcc.org/sites/default/files/resources/instructions.pdf
#'
#' @param phq9         PHQ-9 scores (0 - 27)
#' @param phq9_breaks  Breaks defining labeled ranges, intervals are closed on the left
#' @param phq9_labels  Labels for values between break values including lower bound
#'
#' @return phq9_cat, categorical levels of PHQ-9
#' @export
#'
#' @examples
#' e_calc_phq9_categories(c(0, 6, 12, 21))
#'
#' dplyr::tibble(
#'   phq9 = seq(0, 27, by = 1)
#' , phq9_cat = phq9 %>% e_calc_phq9_categories()
#' ) %>%
#' print(n = Inf)
e_calc_phq9_categories <-
  function(
    phq9
  , phq9_breaks    = c(-Inf, 5, 10, 15, 20, Inf)
  , phq9_labels    =
      c(
        "None-minimal"
      , "Mild"
      , "Moderate"
      , "Moderately Severe"
      , "Severe"
      )
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

  phq9_cat <-
    cut(
      phq9
    , breaks = phq9_breaks
    , labels = phq9_labels
    , right  = FALSE
    )

  return(phq9_cat)
}


