library(tidyverse)

## dat_mtcars_e
dat_mtcars_e <-
  datasets::mtcars %>%
  tibble::as_tibble(
    rownames = "model"
  ) %>%
  dplyr::mutate(
    cyl = cyl %>% factor(levels = c(4, 6, 8), labels = c("four", "six", "eight"))
  , vs  = vs  %>% factor(levels = c(0, 1), labels = c("V-shaped", "straight"))
  , am  = am  %>% factor(levels = c(0, 1), labels = c("automatic", "manual"))
  )

# Label columns
dat_labels <-
  tibble::tribble(
    ~var    , ~label
  , "model" , "Model"
  , "mpg"   , "Miles/(US) gallon"
  , "cyl"   , "Number of cylinders"
  , "disp"  , "Displacement (cu.in.)"
  , "hp"    , "Gross horsepower"
  , "drat"  , "Rear axle ratio"
  , "wt"    , "Weight (1000 lbs)"
  , "qsec"  , "1/4 mile time"
  , "vs"    , "Engine"                     # (0 = V-shaped, 1 = straight)"
  , "am"    , "Transmission"               # (0 = automatic, 1 = manual)"
  , "gear"  , "Number of forward gears"
  , "carb"  , "Number of carburetors"
  )

for (i_row in 1:nrow(dat_labels)) {
  labelled::var_label(dat_mtcars_e[[dat_labels[["var"]][i_row] ]]) <- dat_labels[["label"]][i_row]
}

str(dat_mtcars_e)

usethis::use_data(
    dat_mtcars_e
  , overwrite = TRUE
  )

# save(
#     dat_mtcars_e
#   , file = "data/dat_mtcars_e.RData"
#   )
