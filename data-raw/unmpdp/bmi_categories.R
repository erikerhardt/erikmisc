#' CDC classification for BMI category labels
#'
#' @param bmi
#'
#' @return bmi_cat, categorical levels of BMI
#' @export
#'
#' @examples
bmi_categories <- function(bmi) {

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
}
