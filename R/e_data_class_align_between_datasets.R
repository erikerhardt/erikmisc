#' For two datasets with some matching variable names, align classes before row binding together
#'
#' @details Logic:
#' \enumerate{
#'  \item Determine common variable names between \code{dat1} and \code{dat2}.
#'  \item Determine classes of common variables, retain list of different classes or factor variables with different levels.
#'  \item If both factors and different levels, expand list of factors to include those in order from \code{dat1} then \code{dat2}.
#'  \item If one dataset has all NA, then it is set to the class of the other dataset. If both all NA, then set to dat1 class.
#'  \item Otherwise, the more specific class is converted to the more general class:
#'        logical < integer < numeric < complex < factor (labels) < character.
#' }
#'
#' If a variable has multiple classes, only the first is used.
#'
#' @param dat1        data.frame or tibble, treated as primary dataset for class
#' @param dat2        data.frame or tibble, treated as secondary dataset for class
#' @param sw_bind_rows T/F whether to return \code{dplyr::bind_rows(dat1, dat2)} after class alignment
#' @param sw_return_only_bind_rows T/F return only \code{dplyr::bind_rows(dat1, dat2)} after class alignment, useful for binding several datasets in a single pipe |> workflow.
#'
#' @return out        list with \code{dat1} and \code{dat2}, and \code{dat12} if specified, and \code{dat_class} at the start, differences, and end of process
#' @import dplyr
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#' # Create list of variable names for all combinations of data classes and factor levels
#' #lab <- c("L", "I", "N", "X", "F", "f", "C", "n") # labels for datatypes; X = complex, F and f have different factor levels, n = all NAs
#' #lab_pairs <- expand.grid(lab, lab)
#' #paste0("Var_", lab_pairs$Var1, "_", lab_pairs$Var2)
#'
#' n_obs <- 100
#'
#' # Variables are named as "Var1_[dat1 class]_[dat2 class]" for all class comparisons.
#' dat1 <-
#'   tibble::tibble(
#'     Var_d1  = rep("dat1 only", n_obs)
#'   , Var_L_L = rbinom(n_obs, size = 1, prob = 0.5) |> as.logical()
#'   , Var_I_L = rbinom(n_obs, size = 1, prob = 0.5) |> as.integer()
#'   , Var_N_L = rbinom(n_obs, size = 1, prob = 0.5) |> as.numeric()
#'   , Var_X_L = complex(real = stats::rnorm(n_obs), imaginary = stats::rnorm(n_obs))
#'   , Var_F_L = sample(letters[ 1: 6], n_obs, replace = TRUE) |> factor(levels = letters[ 1: 6])
#'   , Var_f_L = sample(letters[21:26], n_obs, replace = TRUE) |> factor(levels = letters[21:26])
#'   , Var_C_L = sample(letters[11:16], n_obs, replace = TRUE)
#'   , Var_n_L = rep(NA, n_obs)
#'   , Var_L_I = Var_L_L, Var_I_I = Var_I_L, Var_N_I = Var_N_L, Var_X_I = Var_X_L, Var_F_I = Var_F_L, Var_f_I = Var_f_L, Var_C_I = Var_C_L, Var_n_I = Var_n_L
#'   , Var_L_N = Var_L_L, Var_I_N = Var_I_L, Var_N_N = Var_N_L, Var_X_N = Var_X_L, Var_F_N = Var_F_L, Var_f_N = Var_f_L, Var_C_N = Var_C_L, Var_n_N = Var_n_L
#'   , Var_L_X = Var_L_L, Var_I_X = Var_I_L, Var_N_X = Var_N_L, Var_X_X = Var_X_L, Var_F_X = Var_F_L, Var_f_X = Var_f_L, Var_C_X = Var_C_L, Var_n_X = Var_n_L
#'   , Var_L_F = Var_L_L, Var_I_F = Var_I_L, Var_N_F = Var_N_L, Var_X_F = Var_X_L, Var_F_F = Var_F_L, Var_f_F = Var_f_L, Var_C_F = Var_C_L, Var_n_F = Var_n_L
#'   , Var_L_f = Var_L_L, Var_I_f = Var_I_L, Var_N_f = Var_N_L, Var_X_f = Var_X_L, Var_F_f = Var_F_L, Var_f_f = Var_f_L, Var_C_f = Var_C_L, Var_n_f = Var_n_L
#'   , Var_L_C = Var_L_L, Var_I_C = Var_I_L, Var_N_C = Var_N_L, Var_X_C = Var_X_L, Var_F_C = Var_F_L, Var_f_C = Var_f_L, Var_C_C = Var_C_L, Var_n_C = Var_n_L
#'   , Var_L_n = Var_L_L, Var_I_n = Var_I_L, Var_N_n = Var_N_L, Var_X_n = Var_X_L, Var_F_n = Var_F_L, Var_f_n = Var_f_L, Var_C_n = Var_C_L, Var_n_n = Var_n_L
#'   )
#'
#' dat2 <-
#'   tibble::tibble(
#'     Var_d2  = rep("dat2 only", n_obs)
#'   , Var_L_L = rbinom(n_obs, size = 1, prob = 0.5) |> as.logical()
#'   , Var_L_I = rbinom(n_obs, size = 1, prob = 0.5) |> as.integer()
#'   , Var_L_N = rbinom(n_obs, size = 1, prob = 0.5) |> as.numeric()
#'   , Var_L_X = complex(real = stats::rnorm(n_obs), imaginary = stats::rnorm(n_obs))
#'   , Var_L_F = sample(letters[ 1: 6], n_obs, replace = TRUE) |> factor(levels = letters[ 1: 6])
#'   , Var_L_f = sample(letters[21:26], n_obs, replace = TRUE) |> factor(levels = letters[21:26])
#'   , Var_L_C = sample(letters[11:16], n_obs, replace = TRUE)
#'   , Var_L_n = rep(NA, n_obs)
#'   , Var_I_L = Var_L_L, Var_I_I = Var_L_I, Var_I_N = Var_L_N, Var_I_X = Var_L_X, Var_I_F = Var_L_F, Var_I_f = Var_L_f, Var_I_C = Var_L_C, Var_I_n = Var_L_n
#'   , Var_N_L = Var_L_L, Var_N_I = Var_L_I, Var_N_N = Var_L_N, Var_N_X = Var_L_X, Var_N_F = Var_L_F, Var_N_f = Var_L_f, Var_N_C = Var_L_C, Var_N_n = Var_L_n
#'   , Var_X_L = Var_L_L, Var_X_I = Var_L_I, Var_X_N = Var_L_N, Var_X_X = Var_L_X, Var_X_F = Var_L_F, Var_X_f = Var_L_f, Var_X_C = Var_L_C, Var_X_n = Var_L_n
#'   , Var_F_L = Var_L_L, Var_F_I = Var_L_I, Var_F_N = Var_L_N, Var_F_X = Var_L_X, Var_F_F = Var_L_F, Var_F_f = Var_L_f, Var_F_C = Var_L_C, Var_F_n = Var_L_n
#'   , Var_f_L = Var_L_L, Var_f_I = Var_L_I, Var_f_N = Var_L_N, Var_f_X = Var_L_X, Var_f_F = Var_L_F, Var_f_f = Var_L_f, Var_f_C = Var_L_C, Var_f_n = Var_L_n
#'   , Var_C_L = Var_L_L, Var_C_I = Var_L_I, Var_C_N = Var_L_N, Var_C_X = Var_L_X, Var_C_F = Var_L_F, Var_C_f = Var_L_f, Var_C_C = Var_L_C, Var_C_n = Var_L_n
#'   , Var_n_L = Var_L_L, Var_n_I = Var_L_I, Var_n_N = Var_L_N, Var_n_X = Var_L_X, Var_n_F = Var_L_F, Var_n_f = Var_L_f, Var_n_C = Var_L_C, Var_n_n = Var_L_n
#'   )
#'
#' # Can't bind by row, variables with same names but different classes
#' try(dat12 <- dplyr::bind_rows(dat1, dat2))
#'
#' # align classes
#' out <- e_data_class_align_between_datasets(dat1, dat2)
#' out[[ "dat_class_all"     ]] |> print(n=Inf)
#' out[[ "dat_class_diff_NA" ]] |> print(n=Inf)
#' out[[ "dat_class_final"   ]] |> print(n=Inf)
#' out[[ "dat1"              ]] |> str()
#' out[[ "dat2"              ]] |> str()
#'
#' # now they can bind by row
#' dat12 <- dplyr::bind_rows(out$dat1, out$dat2)
#'
e_data_class_align_between_datasets <-
  function(
    dat1          = NULL
  , dat2          = NULL
  , sw_bind_rows  = FALSE
  , sw_return_only_bind_rows = FALSE
  ) {
  #### ADRC data
  ## #dat1       = dat_adrc4
  ## dat1       = dat_adrc3
  ## dat2       = dat_markvcid2
  ## sw_bind_rows  = TRUE
  ## sw_return_only_bind_rows = FALSE

  out <- list()

  # common variable names between datasets
  var_common <-
    names(dat1)[names(dat1) %in% names(dat2)]

  # if multiple classes, return first (typically for timestamp variables with both "POSIXct" and "POSIXt" )
  dat_class <-
    tibble::tibble(
      var         = var_common
    , class_d1    = lapply(dat1[, var_common ], class) |> sapply("[[", 1) |> as.character()
    , class_d2    = lapply(dat2[, var_common ], class) |> sapply("[[", 1) |> as.character()
    , class_diff  = !(class_d1 == class_d2)
    , all_NA_d1   = NA
    , all_NA_d2   = NA
    , factor_diff = FALSE
    )

  # assess all NA and factor levels
  for (i_var in seq_len(nrow(dat_class))) {
    ## i_var = 1
    dat_class$all_NA_d1[i_var] <- all(is.na(dat1[[ dat_class$var[i_var] ]]))
    dat_class$all_NA_d2[i_var] <- all(is.na(dat2[[ dat_class$var[i_var] ]]))
    if (dat_class$class_d1[i_var] == "factor" &
        dat_class$class_d2[i_var] == "factor"
      ) {
      levels_d1 <- levels(dat1[[ dat_class$var[i_var] ]])
      levels_d2 <- levels(dat2[[ dat_class$var[i_var] ]])
      dat_class$factor_diff[i_var] <-
        !all(
          levels_d1 %in% levels_d2
        , levels_d2 %in% levels_d1
        )
    }
  } # i_var

  out[[ "dat_class_all" ]] <- dat_class


  # xtabs(~ class_d1 + class_d2, dat_class)

  # just the difference rows
  dat_class <-
    dat_class |>
    dplyr::filter(
      class_diff  |
      factor_diff |
      all_NA_d1   |
      all_NA_d2
    )

  out[[ "dat_class_diff_NA" ]] <- dat_class


  for (i_var in seq_len(nrow(dat_class))) {
    ## i_var = 1
    ## i_var = 7  # d1 NA
    ## i_var = 9  # factor

    # both factors, align factor levels
    if (dat_class$factor_diff[i_var]) {
      # if the factor levels differ,
      #   use factor level list from dat1, then append other levels from dat2
      #   apply to both datasets

      levels_d1 <- levels(dat1[[ dat_class$var[i_var] ]])
      levels_d2 <- levels(dat2[[ dat_class$var[i_var] ]])

      dat1[[ dat_class$var[i_var] ]] <-
        dat1[[ dat_class$var[i_var] ]] |>
        factor(
          levels = c(levels_d1, levels_d2) |> unique()
        )
      dat2[[ dat_class$var[i_var] ]] <-
        dat2[[ dat_class$var[i_var] ]] |>
        factor(
          levels = c(levels_d1, levels_d2) |> unique()
        )

      next
    } # factor_diff

    # if one or both all NAs
    if (dat_class$all_NA_d1[i_var] | dat_class$all_NA_d2[i_var]) {

      # both, dat2 inherits dat1 class
      if(dat_class$all_NA_d1[i_var] & dat_class$all_NA_d2[i_var]) {
        class(dat2[[ dat_class$var[i_var] ]]) <-
          class(dat1[[ dat_class$var[i_var] ]])
        next
      }

      # dat2 inherits dat1 class
      if(dat_class$all_NA_d2[i_var]) {
        if (class(dat1[[ dat_class$var[i_var] ]]) == "factor") {
          dat2[[ dat_class$var[i_var] ]] <-
            dat2[[ dat_class$var[i_var] ]] |>
            factor(
              levels = levels(dat1[[ dat_class$var[i_var] ]])
            )
        } else {
          class(dat2[[ dat_class$var[i_var] ]]) <-
            class(dat1[[ dat_class$var[i_var] ]])
        }
        next
      }

      # dat1 inherits dat2 class
      if(dat_class$all_NA_d1[i_var]) {
        if (class(dat2[[ dat_class$var[i_var] ]]) == "factor") {
          dat1[[ dat_class$var[i_var] ]] <-
            dat1[[ dat_class$var[i_var] ]] |>
            factor(
              levels = levels(dat2[[ dat_class$var[i_var] ]])
            )
        } else {
          class(dat1[[ dat_class$var[i_var] ]]) <-
            class(dat2[[ dat_class$var[i_var] ]])
        }
        next
      }
    } # NAs


    # align classes: logical < integer < numeric < complex < factor (labels) < character
    if (dat_class$class_diff[i_var]) {
      # if either is character, then both to character
      if (dat_class$class_d1[i_var] == "character" | dat_class$class_d2[i_var] == "character") {
        dat1[[ dat_class$var[i_var] ]] <- as.character(dat1[[ dat_class$var[i_var] ]])
        dat2[[ dat_class$var[i_var] ]] <- as.character(dat2[[ dat_class$var[i_var] ]])
        next
      }

      # if either is    factor, then both to character
      if (dat_class$class_d1[i_var] == "factor" | dat_class$class_d2[i_var] == "factor") {
        dat1[[ dat_class$var[i_var] ]] <- as.character(dat1[[ dat_class$var[i_var] ]])
        dat2[[ dat_class$var[i_var] ]] <- as.character(dat2[[ dat_class$var[i_var] ]])
        next
      }

      # if either is   complex, then both to   complex
      if (dat_class$class_d1[i_var] == "complex" | dat_class$class_d2[i_var] == "complex") {
        dat1[[ dat_class$var[i_var] ]] <- as.complex(dat1[[ dat_class$var[i_var] ]])
        dat2[[ dat_class$var[i_var] ]] <- as.complex(dat2[[ dat_class$var[i_var] ]])
        next
      }

      # if either is   numeric, then both to   numeric
      if (dat_class$class_d1[i_var] == "numeric" | dat_class$class_d2[i_var] == "numeric") {
        dat1[[ dat_class$var[i_var] ]] <- as.numeric(dat1[[ dat_class$var[i_var] ]])
        dat2[[ dat_class$var[i_var] ]] <- as.numeric(dat2[[ dat_class$var[i_var] ]])
        next
      }

      # if either is   integer, then both to   integer
      if (dat_class$class_d1[i_var] == "integer" | dat_class$class_d2[i_var] == "integer") {
        dat1[[ dat_class$var[i_var] ]] <- as.integer(dat1[[ dat_class$var[i_var] ]])
        dat2[[ dat_class$var[i_var] ]] <- as.integer(dat2[[ dat_class$var[i_var] ]])
        next
      }

      # otherwise             , both are already logicial

      next
    } # class_diff

    ## print(
    ##   paste0(
    ##     i_var
    ##   , "  "
    ##   , var_common[i_var]
    ##   , " class difference.  "
    ##   , "adrc: "
    ##   , class(dat1[[ var_common[i_var] ]])
    ##   , "   vci: "
    ##   , class(dat2[[ var_common[i_var] ]])
    ##   , ".  All NAs, "
    ##   , "adrc: "
    ##   , all_na_adrc4
    ##   , "   vci: "
    ##   , all_na_adrc3
    ##   , "."
    ##   )
    ## )

    # for loop should never get down here
    print("e_data_class_align_between_datasets(), unhandled class")
  } # i_var

  dat_class <-
    tibble::tibble(
      var         = var_common
    , class_d1    = lapply(dat1[, var_common ], class) |> sapply("[[", 1) |> as.character()
    , class_d2    = lapply(dat2[, var_common ], class) |> sapply("[[", 1) |> as.character()
    , class_diff  = !(class_d1 == class_d2)
    , all_NA_d1   = NA
    , all_NA_d2   = NA
    , factor_diff = FALSE
    )

  # assess all NA and factor levels
  for (i_var in seq_len(nrow(dat_class))) {
    ## i_var = 1
    dat_class$all_NA_d1[i_var] <- all(is.na(dat1[[ dat_class$var[i_var] ]]))
    dat_class$all_NA_d2[i_var] <- all(is.na(dat2[[ dat_class$var[i_var] ]]))
    if (dat_class$class_d1[i_var] == "factor" &
        dat_class$class_d2[i_var] == "factor"
      ) {
      levels_d1 <- levels(dat1[[ dat_class$var[i_var] ]])
      levels_d2 <- levels(dat2[[ dat_class$var[i_var] ]])
      dat_class$factor_diff[i_var] <- !all(levels_d1 == levels_d2)
    }
  } # i_var

  out[[ "dat_class_final" ]] <- dat_class

  out[[ "dat1" ]] <- dat1
  out[[ "dat2" ]] <- dat2

  if(sw_bind_rows | sw_return_only_bind_rows) {
    out[[ "dat12" ]] <-
      dplyr::bind_rows(
        dat1
      , dat2
      )
    if(sw_return_only_bind_rows) {
      return(out[[ "dat12" ]])
    }
  }

  return(out)

} # e_data_class_align_between_datasets
