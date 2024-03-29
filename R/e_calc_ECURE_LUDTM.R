#' ECURE Upper Division Expectation (UDE)
#'
#' See \code{e_calc_ECURE_LUDTM}
#'
#' @param T       total credits earned
#' @param C_min   start of UD (upper division) credits
#' @param C_max   credits required for graduation
#' @param UDC_min minimum UD credits at graduation
#'
#' @return UDE    Upper Division Expectation
#' @export
#'
#' @examples
#' # Starting your senior semester, expected number of upper-division credits
#' e_calc_ECURE_UDE(T = 90)
e_calc_ECURE_UDE <-
  function(
    T       =  NA
  , C_min   =  45
  , C_max   = 120
  , UDC_min =  54
  ) {
  # T is total credits
  if (T <= C_min) { UDE <- 0     }
  if (T >= C_max) { UDE <- UDC_min }
  if (C_min < T & T < C_max) {
    a <- (-C_min * UDC_min / (C_max - C_min))
    b <- (UDC_min / (C_max - C_min))
    UDE <- a + b * T
  }
  return(UDE)
}

#' ECURE scale the LUDTM when less than expected
#'
#' See \code{e_calc_ECURE_LUDTM}
#'
#' @param UD      number of upper division credits
#' @param UDE     upper division expectation for credits
#' @param T       total credits earned
#' @param C_min   start of UD (upper division) credits
#' @param C_max   credits required for graduation
#'
#' @return LUDTM  Lower- to upper-division transition metric when less than expected
#' @export
e_calc_ECURE_scale_LUDTM <-
  function(
    UD    =  NA
  , UDE   =  NA
  , T     =  NA
  , C_min =  45
  , C_max = 120
  ) {
  # Calculate the proportion of UDE courses completed
  prop <- UD / UDE
  # Transform the proportion from [0,1] to exclude 0 and 1, say [0.01, 0.99]
  prop_scale <- prop * 0.98 + 0.01

  # Transform to logit scale
  z <- e_convert_logit(prop_scale)
  # Apply penalty for total credits
  z_scale <- z - ((T - C_min) / (C_max - C_min))^2
  # Transform back to probability
  prop_2 <- e_convert_logistic(z_scale)

  # Assign as our metric and return that value
  LUDTM <- prop_2

  return(LUDTM)
}


#' ECURE Lower- to upper-division transition metric
#'
#' NSF "Expanding Undergraduate Research Participation in General Education
#'   Courses to Improve STEM Persistence and Graduation Rates" (Award #1953349)
#'
#' Our goal is
#'
#' * to develop an undergraduate student-specific metric
#'   to quantify a student's possible struggle
#'   to transition from lower-division courses to upper-division courses
#'   through fulfillment of their degree requirements
#' * which improves early detection of students who need support and intervention
#' * to increase each student's success by supporting a timely graduation.
#'
#' We have derived a metric capturing the main desired features below,
#'   and we can continue to develop the metric to be major-specific
#'   and to be more sensitive in certain underperforming domains.
#'
#'
#' At The University of New Mexico (UNM),
#'   for most majors in order to graduate "on time" (within 4 years),
#'   an undergraduate student is expected to
#'
#' 1. earn 15 credits per semester for 8 semesters culminating in graduation at 120 credits,
#' 2. begin earning __upper-division (\eqn{UD})__ credits starting after their
#'    third semester (after 45 credits), and
#' 3. complete earning roughly 54 UD credits by graduation (\eqn{C_{UD Grad} = 54}),
#'   based on 6 credits above the 48 UD credit minimum of the
#'   UNM College of Arts and Sciences.
#'
#' This translates to an average of just under 11 UD credits per semester
#'   for their fourth through eighth semesters.
#' This description can be summarized in the __upper-division expectation (\eqn{C_{UD Exp}})__,
#'   defined as the minimum number of UD credits conditional on the
#'   __total number of credits earned (\eqn{C_{Total}})__.
#' The piecewise function for the \eqn{C_{UD Exp}} can be derived from these assumptions as:
#'
#' * For \eqn{C_{Total} \le 45}: \eqn{C_{UD Exp} = 0}.
#' * For \eqn{C_{Total} \ge 120}: \eqn{C_{UD Exp} = C_{UD Grad} = 54}.
#' * For \eqn{45 < C_{Total} < 120}:
#'      \eqn{C_{UD Exp} = -45 * 54 /
#'            (120 - 45) + 54 / (120 - 45) C_{Total} = -31.68 + 0.72 C_{Total}}.
#'
#' A student above the \eqn{C_{UD Exp}} line is meeting expectations,
#'   while a student's distance below the \eqn{C_{UD Exp}} line quantifies their struggle to
#'   transition from lower-division courses to upper-division courses.
#'
#' We define the __lower- to upper-division transition metric (\eqn{LUDTM})__
#'   to represent the degree of fulfillment (0 to 1)
#'   of the upper division expectation (\eqn{C_{UD Exp}}).
#' Qualitatively, the \eqn{LUDTM} is the proportion of the
#'   upper division expectation (\eqn{C_{UD Exp}})
#'   that as student has achieved relative to their total credits (\eqn{C_{Total}})
#'   scaled to be more sensitive close to the \eqn{C_{UD Exp}};
#'   that is, a \eqn{LUDTM} of 0.5 is closer to 1 than 0
#'   as encouragement to get "over the line" when close.
#' **Figure XXX** (produced with example code) illustrates the \eqn{LUDTM}.
#'
#' The LUDTM needs two numbers of credits to date to be calculated:
#'   the student's total credits (\eqn{C_{Total}})
#'   and their number of UD credits earned (\eqn{C_{UD Earn}}).
#' First, we calculate the proportion of the total upper-division credits completed
#'   that are required for graduation,
#'   \eqn{p = C_{UD Earn} / C_{UD Grad}}.
#' A common technique in logistic regression
#'   is used to rescale this ratio relative to the total credits
#'   and to make the metric more sensitive
#'   when a student is close to but under the \eqn{C_{UD Exp}} line.
#' The technique is to convert a proportion \eqn{p}
#'   to the logit scale, \eqn{z = logit(p) = log(p / (1 - p))},
#'   then perform a transformation on \eqn{z}, such as \eqn{f(z)},
#'   and finally, transform it back to a proportion with the logistic transformation,
#'   \eqn{p_{new} = logistic(f(z)) = exp(f(z)) / (1 + exp(f(z)))}.
#' Next, we rescale \eqn{p} so that the logit transformation will be defined
#'   at the extremes of 0 and 1,
#'   \eqn{p_s = p * 0.98 + 0.01}, which shrinks the interval \eqn{[0,1]}
#'   to \eqn{[0.01,0.99]}.
#' Then, on the logit scale, we include a penalty
#'   \eqn{z = logit(p_s) - ((C_{Total} - C_min)/(C_max - C_min))^2},
#'   where \eqn{C_min=45} and \eqn{C_max=120} are the expected
#'   total number of credits for starting and completing the UD course requirements.
#' This ratio is used to scale the effect of the total number of credits, \eqn{C_{Total}},
#'   and this quantity is then squared to impose and increasingly stronger penalty
#'   as \eqn{C_{Total}} increases,
#'   representing the increasing difficulty of making up more UD credits later
#'   in the student's career for a timely graduation.
#' Finally, we transform this quantity back to a proportion
#'   with the logistic transformation, \eqn{LUDTM = logistic(z)}.
#'
#'
#' @param UD      number of upper division credits
#' @param T       total credits earned
#' @param C_min   start of UD (upper division) credits
#' @param C_max   credits required for graduation
#' @param UDC_min minimum UD credits at graduation
#'
#' @return LUDTM  Lower- to upper-division transition metric
#' @export
#'
#' @examples
#' # Example: UD=15 starting your senior semester gives:
#' e_calc_ECURE_LUDTM(UD = 15, T = 90)
#'
#' ## Plot
#' # Constants
#' C_min   =  45 # start of UD credits
#' C_max   = 120 # graduation
#' UDC_min =  54 # minimum UD credits at graduation
#'
#' xlim = c(0, 150)
#' ylim = c(0, 60)
#' interval = 15
#'
#' dat <-
#'   expand.grid(T  = seq(xlim[1], xlim[2], by = 1)
#'             , UD = seq(ylim[1], ylim[2], by = 1)
#'             , LUDTM = NA
#'               )
#' for (i in 1:nrow(dat)) {
#'   dat$LUDTM[i] <-
#'     e_calc_ECURE_LUDTM(UD = dat$UD[i], T = dat$T[i], C_min, C_max, UDC_min)
#' }
#'
#' last_chance <-
#'   tibble::tibble(
#'     T = seq(C_max, C_max - UDC_min, by = -15)
#'   , UD = seq(UDC_min, 0, by = -15)
#'   , LUDTM = NA
#'   )
#' for (i in 1:nrow(last_chance)) {
#'   last_chance$LUDTM[i] <-
#'     e_calc_ECURE_LUDTM(UD = last_chance$UD[i], T = last_chance$T[i], C_min, C_max, UDC_min)
#' }
#'
#' library(ggplot2)
#' p <- ggplot(dat, aes(x = T, y = UD, z = LUDTM, fill = LUDTM))
#' p <- p + geom_tile()
#' p <- p + scale_x_continuous(breaks = seq(xlim[1], xlim[2], by = interval))
#' p <- p + scale_y_continuous(breaks = c(seq(ylim[1], ylim[2], by = interval), UDC_min))
#' p <- p + coord_equal(expand = FALSE, xlim = xlim, ylim = ylim)
#' p <- p + geom_vline(xintercept = c(45, 120), linetype = 2, colour = "white")
#' p <- p + geom_hline(yintercept = c(UDC_min), linetype = 2, colour = "white")
#' p <- p + scale_fill_distiller(palette = "Spectral", direction = 1, na.value = "white"
#'                                , breaks = seq(0, 1, by = 0.2), limits = c(0, 1))
#' p <- p + geom_contour(color = "gray80", binwidth = 0.10)
#' p <- p + geom_contour(color = "gray60", binwidth = 0.20)
#' p <- p + geom_contour(color = "gray40", binwidth = 0.50, size = 0.8)
#' p <- p + geom_segment(aes(x = xlim[1], y = ylim[1], xend = C_min  , yend = ylim[1])
#'                        , colour = "black", size = 2)
#' p <- p + geom_segment(aes(x = C_min  , y = ylim[1], xend = C_max  , yend = UDC_min)
#'                        , colour = "black", size = 2)
#' p <- p + geom_segment(aes(x = C_max  , y = UDC_min, xend = xlim[2], yend = UDC_min)
#'                        , colour = "black", size = 2)
#' p <- p + geom_segment(aes(x = (C_max - UDC_min), y = ylim[1], xend = C_max, yend = UDC_min)
#'                        , colour = "darkgreen", linetype = 2, size = 0.1)
#' p <- p + geom_point(data = last_chance, mapping = aes(x = T, y = UD), shape = 3
#'                        , colour = "darkgreen", size = 2)
#' p <- p + theme_bw()
#' p <- p + labs(title = "Lower- to upper-division transition metric (LUDTM)")
#' p <- p + labs(subtitle = NULL)
#' #p <- p + labs(caption=paste0( bquote(Bold~line~is~upper-division~expectation~(C[UD Exp])), "."
#' p <- p + labs(caption = paste0( "Bold black line is upper-division expectation (C_[UD Exp])."
#'                               , "\nGray contour lines of LUDTM are at every 0.1."
#'                               , "\nGreen plus signs indicate last chance to graduate on time."
#'                               ))
#' p <- p + labs(x = bquote(Total~credits~(C[Total])))
#' p <- p + labs(y = bquote(Upper-division~credits~(C[UD~Earn])))
#' #p <- p + labs(x = "Total credits (C_Total)")
#' #p <- p + labs(y = "Upper-division credits (C_[UD Earn]")
#' p <- p + labs(fill = "LUDTM")
#' p <- p + theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1, Caption align left
#' print(p)
#'
e_calc_ECURE_LUDTM <-
  function(
    UD      = NA
  , T       = NA
  , C_min   =  45 # start of UD credits
  , C_max   = 120 # graduation
  , UDC_min =  54 # minimum UD credits at graduation
  ) {

  # UD is upper division credits
  UDE <- e_calc_ECURE_UDE(T, C_min, C_max, UDC_min)
  if (UDE == 0) {
    LUDTM <- 1
  } else {

    # If you're below the UDE, calculate the LUDTM, otherwise, you're a 1
    LUDTM <-
      ifelse(
        (UD / UDE < 1) & (T > C_min)
      , e_calc_ECURE_scale_LUDTM(UD, UDE, T, C_min, C_max)
      , 1
      )

  }
  LUDTM <- min(LUDTM, 1)
  return(LUDTM)
}


#' ECURE Lower- to upper-division transition metric, v2
#'
#' NSF "Expanding Undergraduate Research Participation in General Education
#'   Courses to Improve STEM Persistence and Graduation Rates" (Award #1953349)
#'
#' This metric measures the number of semesters ahead of on-time graduation
#'    a student is for taking upper-division courses.
#'
#' * 0 indicates the last chance for on-time graduation;
#'   all remaining courses must be upper-division
#' * +1 indicates one semester ahead
#' * -1 indicates one semester behind (the student will graduate at least 1 semester "late")
#'
#' @param student_UpperDiv_cred      student's current number of upper-division credits
#' @param student_Total_cred         student's current total credits earned
#' @param program_UpperDiv_cred_min  program's minimum upper-division credits at graduation
#' @param program_Total_cred_grad    program's total number of credits for graduation
#' @param program_cred_per_semester  program's expected number of credits per semester
#'
#' @return LUDTM  Lower- to upper-division transition metric
#' @export
#'
#' @examples
#' # Example: UD = 15 starting your senior semester gives:
#' e_calc_ECURE_LUDTM2(
#'     student_UpperDiv_cred     =  15
#'   , student_Total_cred        =  90
#'   )
#'
#'
#' ## Plot
#' # Constants
#' program_Total_cred_grad   = 120 # graduation
#' program_UpperDiv_cred_min =  54 # minimum UD credits at graduation
#' program_last_chance = program_Total_cred_grad - program_UpperDiv_cred_min # start of UD credits
#'
#' xlim = c(0, 150)
#' ylim = c(0, 60)
#' program_cred_per_semester = 15
#'
#' dat <-
#'   expand.grid(student_Total_cred  = seq(xlim[1], xlim[2], by = 1)
#'             , student_UpperDiv_cred = seq(ylim[1], ylim[2], by = 1)
#'             , LUDTM = NA
#'               )
#' for (i in 1:nrow(dat)) {
#'   dat$LUDTM[i] <-
#'     e_calc_ECURE_LUDTM2(
#'         student_UpperDiv_cred     =  dat$student_UpperDiv_cred[i]
#'       , student_Total_cred        =  dat$student_Total_cred[i]
#'       #, program_UpperDiv_cred_min =  54
#'       #, program_Total_cred_grad   = 120
#'       #, program_cred_per_semester =  15
#'       )
#'
#'     # e_calc_ECURE_LUDTM2(
#'     #   student_UpperDiv_cred = dat$student_UpperDiv_cred[i]
#'     # , student_Total_cred = dat$student_Total_cred[i]
#'     # , program_last_chance
#'     # , program_Total_cred_grad
#'     # , program_UpperDiv_cred_min
#'     # )
#' }
#'
#' range_min = -4.1
#' range_max = +2.1
#'
#' dat <-
#'   dat |>
#'   dplyr::mutate(
#'     LUDTM =
#'       dplyr::case_when(
#'         LUDTM > range_max ~ range_max
#'       , LUDTM < range_min ~ range_min
#'       , TRUE              ~ LUDTM
#'       )
#'   )
#'
#' library(ggplot2)
#' p <- ggplot(dat, aes(x = student_Total_cred, y = student_UpperDiv_cred
#'                , z = LUDTM, fill = LUDTM))
#' p <- p + geom_tile()
#' p <- p + scale_x_continuous(breaks = seq(xlim[1], xlim[2]
#'                               , by = program_cred_per_semester))
#' p <- p + scale_y_continuous(breaks = c(seq(ylim[1], ylim[2]
#'                                      , by = program_cred_per_semester)
#'                                      , program_UpperDiv_cred_min))
#' p <- p + coord_equal(expand = FALSE, xlim = xlim, ylim = ylim)
#' p <- p + geom_vline(xintercept = c(45, 120), linetype = 2, colour = "white")
#' p <- p + geom_hline(yintercept = c(program_UpperDiv_cred_min)
#'                        , linetype = 2, colour = "white")
#' p <- p + scale_fill_distiller(palette = "Spectral", direction = 1
#'                                , na.value = "white"
#'                                , breaks = seq(-10, 10, by = 1)
#'                                , limits = c(range_min, range_max))
#' p <- p + geom_contour(color = "gray50", binwidth = 0.5)
#' p <- p + geom_contour(color = "gray25", binwidth = 1.0, size = 0.8)
#' p <- p + geom_segment(aes(x = xlim[1], y = ylim[1], xend = program_last_chance
#'                                , yend = ylim[1])
#'                                , colour = "black", size = 2)
#' p <- p + geom_segment(aes(x = program_last_chance  , y = ylim[1]
#'                                , xend = program_Total_cred_grad
#'                                , yend = program_UpperDiv_cred_min)
#'                                , colour = "black", size = 2)
#' p <- p + geom_segment(aes(x = program_Total_cred_grad
#'                                , y = program_UpperDiv_cred_min, xend = xlim[2]
#'                                , yend = program_UpperDiv_cred_min)
#'                                , colour = "black", size = 2)
#' p <- p + geom_segment(aes(x = (program_Total_cred_grad - program_UpperDiv_cred_min)
#'                                , y = ylim[1]
#'                                , xend = program_Total_cred_grad
#'                                , yend = program_UpperDiv_cred_min)
#'                                , colour = "darkgreen", linetype = 2, size = 0.1)
#' p <- p + theme_bw()
#' p <- p + labs(title = "Lower- to upper-division transition metric (LUDTM), v2")
#' p <- p + labs(subtitle = NULL)
#' #p <- p + labs(caption=paste0( bquote(Bold~line~is~upper-division~expectation~(C[UD Exp])),"."
#' #p <- p + labs(caption = paste0(
#'                  "Bold black line is upper-division expectation (C_[UD Exp])."
#' #              , "\nGray contour lines of LUDTM are at every 0.1."
#' #              , "\nGreen plus signs indicate last chance to graduate on time."
#' #              ))
#' p <- p + labs(x = bquote(Total~credits~(C[Total])))
#' p <- p + labs(y = bquote(Upper-division~credits~(C[UD~Earn])))
#' #p <- p + labs(x = "Total credits (C_Total)")
#' #p <- p + labs(y = "Upper-division credits (C_[UD Earn]")
#' p <- p + labs(fill = "LUDTM")
#' p <- p + theme(plot.caption = element_text(hjust = 0)) # Default is hjust=1, Caption align left
#' print(p)
#'
e_calc_ECURE_LUDTM2 <-
  function(
    student_UpperDiv_cred     =  NA
  , student_Total_cred        =  NA
  , program_UpperDiv_cred_min =  54
  , program_Total_cred_grad   = 120
  , program_cred_per_semester =  15
  ) {

  LUDTM <-
    (
     (student_UpperDiv_cred - program_UpperDiv_cred_min) -
     (student_Total_cred    - program_Total_cred_grad  )
    ) / program_cred_per_semester

  return(LUDTM)
}

