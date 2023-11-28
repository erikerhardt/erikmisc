## Convert Units


#' convert letter grades to GPA equivalent
#'
#' Using UNM standard: <https://unm-student.custhelp.com/app/answers/detail/a_id/3461>
#'
#' @param Grade character string letter grades with trailing + or -: "F", "D-", "D", "D+", ..., "A-", "A", "A+"
#'
#' @return GPA  numeric from F = 0 to A+ = 4.33, NA if not a letter grade
#' @importFrom dplyr case_when
#' @export
#'
#' @examples
#' e_convert_Grade_to_GPA(
#'     Grade =
#'       c("A+", "A", "A-"
#'       , "B+", "B", "B-"
#'       , "C+", "C", "C-"
#'       , "D+", "D", "D-"
#'       , "F"
#'       , NA, 0, "hello"
#'       )
#'   )
e_convert_Grade_to_GPA <-
  function(
    Grade
  ) {

  GPA <-
    dplyr::case_when(
      Grade == "A+" ~ 4.33
    , Grade == "A"  ~ 4.00
    , Grade == "A-" ~ 3.67
    , Grade == "B+" ~ 3.33
    , Grade == "B"  ~ 3.00
    , Grade == "B-" ~ 2.67
    , Grade == "C+" ~ 2.33
    , Grade == "C"  ~ 2.00
    , Grade == "C-" ~ 1.67
    , Grade == "D+" ~ 1.33
    , Grade == "D"  ~ 1.00
    , Grade == "D-" ~ 0.67
    , Grade == "F"  ~ 0.00
    , TRUE          ~ NA |> as.numeric()
    )

  return(GPA)
}





#' Convert inches (in) to centimeters (cm)
#'
#' @param   inches  list of values in inches (in)
#'
#' @return  cm      list of values in centimeters (cm)
#' @export
#'
#' @examples
#' e_convert_in_to_cm(1)
e_convert_in_to_cm <-
  function(
    inches
  ) {
  cm <- 2.54 * inches
  return(cm)
}

#' Convert centimeters (cm) to inches (in)
#'
#' @param   cm      list of values in centimeters (cm)
#'
#' @return  inches  list of values in inches (in)
#' @export
#'
#' @examples
#' e_convert_cm_to_in(2.54)
e_convert_cm_to_in <-
  function(
    cm
  ) {
  inches <- cm / 2.54
  return(inches)
}





#' Convert pounds (lb) to kilograms (kg)
#'
#' @param   lb  list of values in pounds (lb)
#'
#' @return  kg  list of values in kilograms (kg)
#' @export
#'
#' @examples
#' e_convert_lb_to_kg(1)
e_convert_lb_to_kg <-
  function(
    lb
  ) {
  kg <- 0.45359237 * lb
  return(kg)
}

#' Convert kilograms (kg) to pounds (lb)
#'
#' @param   kg  list of values in kilograms (kg)
#'
#' @return  lb  list of values in pounds (lb)
#' @export
#'
#' @examples
#' e_convert_kg_to_lb(0.45359237)
e_convert_kg_to_lb <-
  function(
    kg
  ) {
  lb <- kg / 0.45359237
  return(lb)
}

