#' PHQ-2 Scores and Proposed Treatment Actions, Table 4 from Kroenke K, Spitzer RL, Psychiatric Annals 2002;32:509-521
#'
#' @param phq2_total_score
#'
#' @return phq2_cat, categorical levels of PHQ-2
#' @export
#'
#' @examples
phq2_categories <- function(phq2_total_score) {

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
}
