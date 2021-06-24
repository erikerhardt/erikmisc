#' Title
#'
#' @param weight
#' @param height
#' @param system, either "Metric" (kg, cm) or "English" (lb, in)
#'
#' @return
#' @export
#'
#' @examples
bmi_calc <- function(weight, height, system = "Metric") {

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
