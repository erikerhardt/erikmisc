## Aeronautical Pilot functions


#' Compute Koch Chart density altitude, takeoff roll, and climb rate
#'
#' Approximation for Koch Chart density altitude for
#'   an increase in takeoff roll and decrease in climb rate
#'   based on pressure altitude and temperature.
#'
#' \code{dens_alt} = (145426 * (1 - (((288.16 - \code{pres_alt} * 0.001981) /
#'                   288.16)^5.2563 / ((273.16 + \code{temp_C}) / 288.16))^0.235))
#'
#'     Equations and approximations from "Axioms of Flight",
#'     James Embree, Flight Information Publications,
#'     St. Louis MO, 1984. ISBN 0-9601062-7-8
#'
#' Approximation (good to 8000 feet density altitude):
#'
#' * Takeoff distance:
#'     * For fixed pitch prop, increase sea level standard day takeoff distance
#'       15% for each 1000 foot increase in density altitude.
#'     * For constant speed prop, increase by 13%.
#' * Climb rate
#'     * For fixed pitch prop, decrease sea level standard day climb rate
#'       7.5% for each 1000 foot increase in density altitude.
#'     * For constant speed prop, decrease by 7%.
#'
#' Example takeoff for fixed pitch prop:
#'
#' * dist_takeoff_dens_alt = dist_takeoff_sealevel * (1 + ((dens_alt / 1000) * 0.15))
#'
#' (https://groups.google.com/g/rec.aviation.piloting/c/SDlMioBVqaA)
#'
#'
#' @param pres_alt               pressure altitude, inches of Mg
#' @param temp_C                 temperature, degrees celsius
#' @param dist_takeoff_sea_level POH distance for takeoff at
#'                               standard temperature and pressure (STP)
#' @param climb_rate_sea_level   POH distance for climb at
#'                               standard temperature and pressure (STP)
#' @param sw_propeller           Select propeller to be "fixed_pitch" or "constant_speed"
#'
#' @return out                   named list including \code{density_altitude},
#'                               \code{dist_takeoff_dens_alt},
#'                               \code{climb_rate_dens_alt},
#'                               and all the input parameters.
#' @export
#'
#' @examples
#'
#' e_pilot_density_altitude(
#'     pres_alt               = 0
#'   , temp_C                 = 15
#'   , dist_takeoff_sea_level = 1000
#'   , climb_rate_sea_level   = 500
#'   , sw_propeller = c("fixed_pitch", "constant_speed")[1]
#'   )
#' e_pilot_density_altitude(
#'     pres_alt               = 7170
#'   , temp_C                 = 30
#'   , dist_takeoff_sea_level = 770
#'   , climb_rate_sea_level   = 900
#'   , sw_propeller = c("fixed_pitch", "constant_speed")[1]
#'   )
#' e_pilot_density_altitude(
#'     pres_alt               = 7170
#'   , temp_C                 = 30
#'   , dist_takeoff_sea_level = 770
#'   , climb_rate_sea_level   = 900
#'   , sw_propeller = c("fixed_pitch", "constant_speed")[2]
#'   )
#'
#'
#' ## Tables
#' # 1972 Piper Arrow II
#' dist_takeoff_sea_level <- 770
#' climb_rate_sea_level   <- 900
#' sw_propeller           <- c("fixed_pitch", "constant_speed")[2]
#'
#' list_out <- list()
#' i_list <- 0
#'
#' for (i_pres_alt in seq(0, 10000, by = 2000)) {
#'   ## i_pres_alt = 0
#'   for (i_temp_C in c(0, seq(15, 40, by = 5))) {
#'     ## i_temp_C = 0
#'
#'     i_list <- i_list + 1
#'
#'     list_out[[ i_list ]] <-
#'       e_pilot_density_altitude(
#'         pres_alt               = i_pres_alt
#'       , temp_C                 = i_temp_C
#'       , dist_takeoff_sea_level = dist_takeoff_sea_level
#'       , climb_rate_sea_level   = climb_rate_sea_level
#'       , sw_propeller           = sw_propeller
#'       )
#'   }
#' }
#'
#' dat_out <-
#'   list_out %>%
#'   dplyr::bind_rows()
#' dat_out %>% print(n = 10)
#'
#' # Note: pres_alt = pressure altitude at 29.92 inches Mg
#'
#' # density altitude
#' dat_density_altitude <-
#'   dat_out %>%
#'   dplyr::select(
#'     density_altitude
#'   , pres_alt
#'   , temp_C
#'   ) %>%
#'   tidyr::pivot_wider(
#'     id_cols     = temp_C
#'   , names_from  = pres_alt
#'   , values_from = density_altitude
#'   )
#'
#' # takeoff roll
#' dat_takeoff_roll <-
#'   dat_out %>%
#'   dplyr::select(
#'     dist_takeoff_dens_alt
#'   , pres_alt
#'   , temp_C
#'   ) %>%
#'   tidyr::pivot_wider(
#'     id_cols     = temp_C
#'   , names_from  = pres_alt
#'   , values_from = dist_takeoff_dens_alt
#'   )
#'
#' # climb rate
#' dat_climbrate <-
#'   dat_out %>%
#'   dplyr::select(
#'     climb_rate_dens_alt
#'   , pres_alt
#'   , temp_C
#'   ) %>%
#'   tidyr::pivot_wider(
#'     id_cols     = temp_C
#'   , names_from  = pres_alt
#'   , values_from = climb_rate_dens_alt
#'   )
#'
#' dat_density_altitude
#' dat_takeoff_roll
#' dat_climbrate
#'
e_pilot_density_altitude <-
  function(
    pres_alt               = 0
  , temp_C                 = 15
  , dist_takeoff_sea_level = 1000
  , climb_rate_sea_level   = 500
  , sw_propeller           = c("fixed_pitch", "constant_speed")[2]
  ) {

  density_altitude <-
    145426 *
    (1 -
      (
        ( (288.16 - pres_alt * 0.001981) / 288.16 )^5.2563
          /
        ( (273.16 + temp_C             ) / 288.16 )
      )^0.235
    )

  if(sw_propeller == c("fixed_pitch", "constant_speed")[1]) {
    dist_takeoff_dens_alt <-
      dist_takeoff_sea_level * (1 + ((density_altitude / 1000) * 0.150))

    climb_rate_dens_alt <-
      climb_rate_sea_level   * (1 - ((density_altitude / 1000) * 0.075))
  }
  if(sw_propeller == c("fixed_pitch", "constant_speed")[2]) {
    dist_takeoff_dens_alt <-
      dist_takeoff_sea_level * (1 + ((density_altitude / 1000) * 0.130))

    climb_rate_dens_alt <-
      climb_rate_sea_level   * (1 - ((density_altitude / 1000) * 0.070))
  }

  out <-
    c(
      density_altitude       = round(density_altitude)
    , dist_takeoff_dens_alt  = round(dist_takeoff_dens_alt)
    , climb_rate_dens_alt    = round(climb_rate_dens_alt)
    , pres_alt               = pres_alt
    , temp_C                 = temp_C
    , dist_takeoff_sea_level = dist_takeoff_sea_level
    , climb_rate_sea_level   = climb_rate_sea_level
    , sw_propeller           = sw_propeller
    )

  return(out)
}


