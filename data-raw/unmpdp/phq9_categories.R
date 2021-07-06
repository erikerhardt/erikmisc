#' PHQ-9 Scores and Proposed Treatment Actions, Table 4 from Kroenke K, Spitzer RL, Psychiatric Annals 2002;32:509-521
#'
#' @param phq9_total_score
#'
#' @return phq9_cat, categorical levels of PHQ-9
#' @export
#'
#' @examples
phq9_categories <- function(phq9_total_score) {

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
}
