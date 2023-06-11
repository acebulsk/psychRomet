
#' Thermal conductivity of air
#'
#' Helper function for the ice bulb temp calculation that calculates the thermal conductivity of air [J m-1 s-1 K-1]
#'
#' @param Tai
#'
#' @return thermal conductivity of air [J m-1 s-1 K-1]
#' @export
#'
#' @examples thermal_conductivity_air(10)
thermal_conductivity_air <-function(Tai){
  Ka<-0.000063*(Tai+273.15)+0.00673
}

#' Diffusivity of water vapour in air
#'
#' Helper function for ice bulb temp calculation calculates the diffusivity of water vapour in air.
#'
#' @param T_c air temperature in celcius
#'
#' @return diffusivity of water vapour in air [m2 s-1]
#' @export
#'
#' @examples diffusivity_water_vapour(10)
diffusivity_water_vapour <-function(T_c){
  Dv<-2.06*10^-5*((T_c+273.15)/273.15)^1.75
  return(Dv)
}

#' Absolute Humidity (water vapour density)
#'
#' A rearangement of actual_vapour_pressure Stull et al., 2017 eq. 1.19
#'
#' @param T_c temperature Celsius
#' @param rh as fraction
#' @param R_v kPa·K–1·m3·kg–1
#'
#' @return density of water vapour in the air kg m-3
#' @export
#'
#' @examples absolute_humidity(2, .20)
absolute_humidity <- function(T_c, rh, R_v = 0.4615){
  e_sat <- tetens(T_c) # kpa
  e_act <- actual_vapour_pressure(e_sat, rh) # kpa
  K <- T_c + 273.15 # Celsius to Kelvin
  e_act / (R_v * K)
}

#' Density Dry Air
#'
#' Density Dry Air From Stull et al., 2017 eq. 1.18.
#' Note: Need to subtract e_act from p_atm so that we are just left with the dry air fraction. From 836 lecture 1 slide 6.
#'
#' @param T_c temperature Celsius
#' @param rh relative humidity as fraction
#' @param p_atm atmosphereic pressure, kPa
#' @param R_d gas constant for unsaturated air kPa·K–1·m3·g–1
#'
#' @return density of dry air kg / m3
#' @export
#'
#' @examples density_dry_air(15, 0, 101.325)
density_dry_air <- function(T_c, rh, p_atm, R_d = 0.287053){
  e_act <- actual_vapour_pressure(tetens(T_c), rh)
  (p_atm - e_act) / (R_d * (T_c + 273.15))
}

#' Density Moist Air
#'
#' Since total pressure = pressure dry air + pressure water vapour (from lecture 1 slide 6)
#'
#'
#' @param T_c temperature Celsius
#' @param p_atm atmosphereic pressure, kPa
#' @param R_v gas constant for moist air kPa·K–1·m3·kg–1
#'
#' @return density of moist air kg / m3
#' @export
#'
#' @examples density_moist_air(15, 0, 101.325)
density_moist_air <- function(T_c, rh, p_atm, R_v = 0.4615){
  rho_da <- density_dry_air(T_c, rh, p_atm)

  rho_wv <- absolute_humidity(T_c, rh) # kg m-3

  rho_da + rho_wv
}

#' Density Air
#'
#' Density Air using virtual temp, hardly different from above in practice (g / m3). From Stull et al., 2017 eq. 1.23.
#'
#'
#' @param T_c temperature Celsius
#' @param p_atm atmospheric pressure, kPa
#' @param mixing_ratio kg / m3
#' @param mixing_ratio_liquid kg / m3 usually assume 0
#' @param mixing_ratio_ice kg / m3 usually assume 0
#' @param R_d gas constant for unsaturated air kPa·K–1·m3·kg–1
#'
#' @return total density dry + wet g / m3
#' @export
#'
#' @examples density_dry_air(15, 101.325, 0.03)
density_air_virtual <- function(T_c, p_atm, mixing_ratio, mixing_ratio_liquid = 0, mixing_ratio_ice = 0, R_d = 0.287053){
  T_v <- virtual_temp(T_c, mixing_ratio, mixing_ratio_liquid, mixing_ratio_ice)
  p_atm / (R_d * (T_v + 273.15))
}

#' Air Density at Elevation
#'
#' @param T_c Temperature Celsius
#' @param m_asl Elevation (metres)
#' @param a 0.040 K m –1
#' @param p_0 average sea level density 1.2250 kg·m –3
#'
#' @return kg / m3
#' @export
#'
#' @examples air_density(15, 2000)
air_density <- function(T_c, m_asl, a = 0.04, p_0 = 1.2250){
  k <- T_c + 273.15
  p_0 * exp(-a * m_asl / k)
}
