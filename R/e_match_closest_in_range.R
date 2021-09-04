#' Match the closest observations from one dataset to a key dataset.
#'
#' Similar to \code{survival::neardate} but chooses closest in both directions restricted to an asymmetrical range.
#'
#' Can also be used to match closest within a range of dates in the future by setting \code{diff_lower} and \code{diff_upper} to be positive numbers, e.g., 5 and 7.
#'
#' @param dat_key           key dataset
#' @param dat_to_match      data to match to the key dataset
#' @param id_vars_key       ID variables in key dataset
#' @param id_vars_to_match  associated ID variables in data to match
#' @param val_var_key       value variable to determine closeness in key dataset
#' @param val_var_to_match  associated value variable in data to match
#' @param diff_lower        match from data to match can be no lower than the key data by this amount
#' @param diff_upper        match from data to match can be no higher than the key data by this amount
#' @param sw_criteria       criteria for match proximity (useful when range values \code{diff_lower} and \code{diff_upper} are used): closest, minimum, or maximum.
#' @param sw_return_key_vars T/F return the key value for use in matching if multiple records per ID
#'
#' @return dat_to_match restricted to only those unique observations that are closest to the key data
#' @import dplyr
#' @export
#'
#' @examples
#'
#' set.seed(1)
#'
#' dat_key <-
#'   tidyr::expand_grid(
#'     key1 = c("a", "b", "c")
#'   , key2 = c("x", "y")
#'   ) %>%
#'   dplyr::mutate(
#'     value = 1:dplyr::n()
#'   )
#'
#' dat_to_match <-
#'   tidyr::expand_grid(
#'     key1_m = c("a", "b")      # no "c"
#'   , key2_m = c("x", "y", "z") # added "z"
#'   ) %>%
#'   dplyr::slice(
#'     sample.int(n = 2*3, size = 4 * 2*3, replace = TRUE) # produce multiple per obs
#'   ) %>%
#'   dplyr::mutate(
#'     value_m = runif(n = dplyr::n(), min = -5, max = 10)
#'   , other1  = rnorm(dplyr::n())
#'   , other2  = rnorm(dplyr::n())
#'   ) %>%
#'   dplyr::arrange(
#'     key1_m, key2_m
#'   )
#'
#' dat_to_match_sub <-
#'   e_match_closest_in_range(
#'     dat_to_match      = dat_to_match
#'   , id_vars_to_match  = c("key1_m", "key2_m")
#'   , val_var_to_match  = "value_m"
#'   , dat_key           = dat_key
#'   , id_vars_key       = c("key1"  , "key2"  )
#'   , val_var_key       = "value"
#'   , diff_lower        = -Inf
#'   , diff_upper        = +Inf
#'   )
#'
#' dat_key          %>% print()
#' dat_to_match     %>% print(n = Inf)
#' dat_to_match_sub %>% print()
#'
#'
#' # within specified range
#' e_match_closest_in_range(
#'   dat_to_match      = dat_to_match
#' , id_vars_to_match  = c("key1_m", "key2_m")
#' , val_var_to_match  = "value_m"
#' , dat_key           = dat_key
#' , id_vars_key       = c("key1"  , "key2"  )
#' , val_var_key       = "value"
#' , diff_lower        = -2
#' , diff_upper        = +4
#' , sw_return_key_vars = TRUE
#' )
#'
#' # within specified range, maximum value
#' e_match_closest_in_range(
#'   dat_to_match      = dat_to_match
#' , id_vars_to_match  = c("key1_m", "key2_m")
#' , val_var_to_match  = "value_m"
#' , dat_key           = dat_key
#' , id_vars_key       = c("key1"  , "key2"  )
#' , val_var_key       = "value"
#' , diff_lower        = -2
#' , diff_upper        = +4
#' , sw_criteria       = "maximum"
#' , sw_return_key_vars = TRUE
#' )
#'
#' # within specified range, minimum value
#' e_match_closest_in_range(
#'   dat_to_match      = dat_to_match
#' , id_vars_to_match  = c("key1_m", "key2_m")
#' , val_var_to_match  = "value_m"
#' , dat_key           = dat_key
#' , id_vars_key       = c("key1"  , "key2"  )
#' , val_var_key       = "value"
#' , diff_lower        = -2
#' , diff_upper        = +4
#' , sw_criteria       = "minimum"
#' , sw_return_key_vars = TRUE
#' )
e_match_closest_in_range <-
  function(
    dat_to_match
  , id_vars_to_match
  , val_var_to_match
  , dat_key
  , id_vars_key
  , val_var_key
  , diff_lower          = -Inf
  , diff_upper          = +Inf
  , sw_criteria         = c("closest", "minimum", "maximum")[1]
  , sw_return_key_vars  = FALSE
  ) {

  dat_key <-
    dat_key %>%
    dplyr::rename(
      val_var_key__ = !!sym(val_var_key)
    )

  # for *_join(by=)
  names(id_vars_key) <- id_vars_to_match

  # restrict rows to those that match in dat_key
  dat_temp_to_match <-
    dat_to_match %>%
    dplyr::semi_join(
      dat_key
    , by = id_vars_key
    ) %>%
    dplyr::left_join(
      dat_key
    , by = id_vars_key
    ) %>%
    # sort by ID and value
    dplyr::arrange(
      dplyr::across(
        .cols = c(all_of(id_vars_to_match), all_of(val_var_to_match))
      )
    )

  # calculate difference in values, restrict to range, determine closest
  dat_temp_to_match_pre <-
    dat_temp_to_match %>%
    dplyr::mutate(
      val_diff__ = !!sym(val_var_to_match) - val_var_key__
    ) %>%
    # filter to be within difference range
    dplyr::filter(
      val_diff__ %>% dplyr::between(diff_lower, diff_upper)
    ) %>%
    # find closest
    dplyr::group_by(
      across( {{id_vars_to_match}} )
    )

  if        (sw_criteria == c("closest", "minimum", "maximum")[1]) {
    dat_temp_to_match_post <-
      dat_temp_to_match_pre %>%
      # filter matches
      dplyr::filter(
        (abs(val_diff__) == min(abs(val_diff__)))
      )
  } else if (sw_criteria == c("closest", "minimum", "maximum")[2]) {
    dat_temp_to_match_post <-
      dat_temp_to_match_pre %>%
      # filter matches
      dplyr::filter(
        (val_diff__ == min(val_diff__))
      )
  } else if (sw_criteria == c("closest", "minimum", "maximum")[3]) {
    dat_temp_to_match_post <-
      dat_temp_to_match_pre %>%
      # filter matches
      dplyr::filter(
        (val_diff__ == max(val_diff__))
      )
  }

  dat_temp_to_match <-
    dat_temp_to_match_post %>%
    dplyr::ungroup() %>%
    # make unique (in case multiple matches of same distance)
    dplyr::distinct(
      across(all_of(id_vars_to_match))
    , .keep_all = TRUE
    )



  if (!sw_return_key_vars) {
    dat_temp_to_match <-
      dat_temp_to_match %>%
      # remove variables not originally part of dat_to_match
      dplyr::select(
        -val_var_key__
      , -val_diff__
      )
  }

  return(dat_temp_to_match)
}
