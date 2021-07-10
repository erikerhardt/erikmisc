## Convert Units

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

