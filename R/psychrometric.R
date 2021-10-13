# list of psychometric functions compiled from Stull et al., 2017 and 2001 ASHRAE Fundamentals Handbook (SI)

#### functions ####

#' Pressure Uniform
#' Return air pressure given height above sea level, assumes uniform temp and pressure. From Stull et al., 2017 eq. 1.9b
#' @param m_asl height in metres
#'
#' @return pressure in kPa
#' @export
#'
#' @examples pressure_uniform(m_asl = 10^4)
#'
pressure_uniform <- function(m_asl){
  101.325 * exp(1)^{-m_asl/7.29}
}

#' Actual Vapour Pressure
#'
#' Calculated using relative humidity and saturated vapour pressure.
#' @param e_sat PA or KPA
#' @param rh as fraction
#'
#' @return e_act in units of e_sat
#' @export
#'
#' @examples actual_vapour_pressure(e_sat = 4.360379, rh = 0.35)
actual_vapour_pressure <- function(e_sat, rh){
  e_sat * rh
}

#' Actual Vapour Pressure (using absolute humidity)
#'
#' Another way to get actual vapour pressure as a funciton of absolute humidity. From Stull et al., 2017 eq. 1.19.
#' @param absolute_humidity g/g
#' @param T_c temperature Celsius
#' @param R_v # kPa·K–1·m3·kg–1
#'
#' @return kPa
#' @export
#'
#' @examples actual_vapour_pressure_ah(absolute_humidity = 0.0148, T_c = 20, R_v = 0.4615)
actual_vapour_pressure_ah <- function(absolute_humidity, T_c,  R_v = 0.4615) {
  K <- T_c + 273.15

  absolute_humidity * R_v * K
}

#' Absolute Humidity (water vapour density)
#'
#' A rearangement of actual_vapour_pressure Stull et al., 2017 eq. 1.19
#'
#' @param T_c temperature Celsius
#' @param R_v kPa·K–1·m3·g–1
#'
#' @return density of water vapour in the air kg m-3
#' @export
#'
#' @examples absolute_humidity(2, 20)
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
#' @param R_v gas constant for moist air kPa·K–1·m3·g–1
#'
#' @return density of moist air g / m3
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
#' @param mixing_ratio g / m3
#' @param mixing_ratio_liquid g / m3 usually assume 0
#' @param mixing_ratio_ice g / m3 usually assume 0
#' @param R_d gas constant for unsaturated air kPa·K–1·m3·g–1
#'
#' @return total density dry + wet kg / m3
#' @export
#'
#' @examples density_dry_air(15, 101.325, 0.03)
density_air_virtual <- function(T_c, p_atm, mixing_ratio, mixing_ratio_liquid = 0, mixing_ratio_ice = 0, R_d = 0.287053){
  T_v <- virtual_temp(T_c, mixing_ratio, mixing_ratio_liquid, mixing_ratio_ice)
  p_atm / (R_d * (T_v + 273.15))
}

#' Saturated Vapour Pressure
#'
#' Returns the saturated vapour pressure using the Clausius-Clapeyron equation. From Stull et al., 2017 eq. 4.1
#'
#' @param T_c temperature in Celsius, function does kelvin conversion
#' @param e_o vapour pressure constant kpa
#'
#' @return e_sat in kPa
#' @export
#'
#' @examples clausius_clapeyron(T_c = 30, e_o = 0.6113)
clausius_clapeyron <- function(T_c, e_o = 0.6113){

  K <- T_c + 273.15 # Celsius to Kelvin

  dplyr::if_else(T_c >= 0,
                 e_o * exp((5423)*((1/273.15) - (1/K))), # true
                 e_o * exp((6139)*((1/273.15) - (1/K))) # false
  )
}

#' Actual Vapour Pressure
#'
#' Returns the actual or unsaturated vapour pressure using the Clau-sius-Clapeyron equation.
#'
#' @param T_c temperature in Celsius
#' @param T_d dew point temperature
#' @param e_o vapour pressure constant kpa
#'
#' @return e_act in kPa
#' @export
#'
#' @examples clausius_clapeyron_act(T_c = 30, T_d = 12.8, e_o = 0.6113)
clausius_clapeyron_act <- function(T_c, T_d, e_o = 0.6113) { # Stull et al., 2017 eq. 4.1b
  K_d <- T_d + 273.15
  dplyr::if_else(T_c >= 0,
                 e_o * exp((5423) * ((1/273.15) - 1/K_d)), # true
                 e_o * exp((6139) * ((1/273.15) - 1/K_d)) # false
  )
}

#' Tetens' formula for saturated vapour pressure
#'
#' Change e_o to 610.8 for pa. From Stull et al., 2017 eq. 4.2
#'
#' @param T_c temperature Celsius
#' @param e_o vapour pressure constant kpa
#'
#' @return kPa or pa if e_o == 610.8 pa
#' @export
#'
#' @examples tetens(30)
tetens <- function(T_c, e_o = 0.6113){
  dplyr::if_else(T_c >= 0,
                 e_o*exp((17.27*T_c)/(T_c+237.3)), # true
                 e_o*exp((21.87*T_c)/(T_c+265.5)) # false
  )
}

#' Specific Humidity
#'
#' Ratio of mass of water vapour to total air pressure. From Stull et al., 2017 eq. 4.7
#'
#' @param e_s saturated vapour pressure kPa
#' @param total_pressure total pressure kPa
#' @param e_o R_d over R_v (g/g) Vapour pressure constant Stull et al., eq 4.5
#'
#' @return g g-1 change e_o to 622 for g / kg
#' @export
#'
#' @examples specific_humidity(e_s = 0.6114, total_pressure = 101.325, e_o = 0.622)
specific_humidity <- function(e_s, total_pressure, e_o = 0.622) {
  (e_o * e_s) / total_pressure
}


#' Relative Humidity
#'
#' @param e_act kPa or PA
#' @param e_s same units as e_act
#'
#' @return fraction
#' @export
#'
#' @examples relative_humidity(e_act = 1.286, e_s = 2.369)
relative_humidity <- function(e_act, e_s) { # Stull et al., 2017 eq. 4.14a
  e_act / e_s
}

#' Mixing Ratio (Humidity Ratio)
#'
#' Ratio of mass water vapour to the mass of dry air. calculate hr using mostly temp - from psychometrics chapter 6 ASHRAE. Useful for wetbulb iteration calculation.
#'
#' @param T_c temperature Celsius
#' @param T_wb wet bulb temperature
#' @param W_wb wet bulb humidity ratio
#'
#' @return g / kg
#' @export
#'
#' @examples NA
mixing_ratio_t <- function(T_c, T_wb, W_wb){

  ((2501 - 2.381 * T_wb) * W_wb - 1.006 * (T_c - T_wb)) / (2501 + 1.805 * T_c - 4.18 * T_wb)

}

#' Mixing Ratio (Humidity Ratio)
#'
#' Calculate the mixing ratio aka humidity ratio using mostly pressure. From Stull et al., 2017 eq. 4.4/4.5. Can use either saturated or unsaturated for e. You get what you put in.
#'
#' @param e_act vapour pressure, can be saturated or unsaturated (kPa)
#' @param p_atm kPa
#' @param e_o R_d over R_v (g/g) Vapour pressure constant Stull et al., eq 4.5
#'
#'
#' @return  g/g if e_o == 0.622 or g / kg if 622 (saturated or unsaturated depending on e)
#' @export
#'
#' @examples mixing_ratio_p(e_act = 0.6113, p_atm = 101.325)
mixing_ratio_p <- function(e_act, p_atm, e_o = 0.622){
  e_o * e_act / (p_atm - e_act)
}

#' Dew Point Temperature
#'
#' Calculates dew point temperature using actual vapour pressure. From Stull et al., 2017 eq. 4.15a.
#'
#' @param e_act unstaturated / actual vapour pressure
#' @param R_v_L_v water-vapour gas constant for dry air over the latent heat of vapourization (units are ^ K -1)
#' @param e_o 0.6113 kPa
#'
#' @return Temperature in Celsius
#' @export
#'
#' @examples dew_point_temp_e_act(1.286)
dew_point_temp_e_act <- function(e_act, R_v_L_v = 1.844e-4, e_o = 0.6113){ # Stull et al., 2017 eq. 4.15a
  K_dp <- ((1/273.15) - (R_v_L_v) * (log((e_act/e_o))))^-1

  K_dp - 273.15 # return Celsius
}

#' Dew Point Temperature
#'
#' Calculated using mixing ratio and atmospheric pressure. From Stull et al., 2017 eq. 4.15b.
#'
#' @param mixing_ratio in grams
#' @param p_atm pressure atmosphere kpa
#' @param R_v_L_v ratio constant in K
#' @param e_o R_d over R_v (g/g) Vapour pressure constant Stull et al., eq 4.5
#'
#'
#' @return Celsius
#' @export
#'
#' @examples dew_point_temp_r(0.01, 80)
dew_point_temp_r <- function(mixing_ratio, p_atm, R_v_L_v = 1.844e-4, e_o = 0.622){ #

  K_dp <- ((1/273.15) - (R_v_L_v) * (log(((mixing_ratio*p_atm)/(e_o * (mixing_ratio+e_o))))))^-1

  K_dp - 273.15 # return Celsius
}

#' Virtual Temperature
#'
#' Options to include fraction liquid and ice but usually assume to be 0. Stull et al., 2017 eq. 1.22.
#'
#' @param T_c air temperature Celsius
#' @param mixing_ratio mixing ratio in grams
#' @param mixing_ratio_liquid mixing ratio in grams
#' @param mixing_ratio_ice mixing ratio in grams
#'
#' @return Temperature in Celsius
#' @export
#'
#' @examples virtual_temp(35, 0.03, mixing_ratio_liquid = 0.01)
virtual_temp <- function(T_c, mixing_ratio, mixing_ratio_liquid = 0, mixing_ratio_ice = 0){
  K <- T_c + 273.15

  K_v <- K * (1 + (0.61 * mixing_ratio) - mixing_ratio_liquid - mixing_ratio_ice) # make sure are in grams

  K_v - 273.15 # return Celsius
}

#' Potential Temperature
#'
#' @param T_c Celsius
#' @param P kpa
#'
#' @return Temperature Celsius
#' @export
#'
#' @examples potential_temp(10, 70)
potential_temp <- function(T_c, P){ # Stull et al., 2017 eq. 3.12

  K <- T_c + 273.15

  K_0 <- K * (100 / P) ^ 0.28571

  K_0 - 273.15 # return Celsius

}

# Empirical Approximation at sea level
#' Wet Bulb Temperature
#'
#' Estimated using an empirical function from Stull et al., 2017 eq. 4.19. Need either RH or esat and eact.
#'
#' @param T_c temperature Celsius
#' @param RH relative humidity fraction
#' @param e_act kpa or pa
#' @param e_sat same as above
#'
#' @return temperature celsius
#' @export
#'
#' @examples wet_bulb_empirical(20, .5)
wet_bulb_empirical <- function(T_c, RH = NA, e_act = NA, e_sat = NA){ # Stull et al., 2017 eq. 4.19

  if (is.na(RH)){
    RH <- relative_humidity(e_act, e_sat) * 100
    T_c * atan(0.151977 * (RH + 8.313659)^0.5) - 4.686035 +
      atan(T_c + RH) - atan(RH - 1.676331) +
      0.00391838 * RH^{3/2} * atan(0.023101 * RH)

  } else {
    RH <- RH * 100
    T_c * atan(0.151977 * (RH + 8.313659)^0.5) - 4.686035 +
      atan(T_c + RH) - atan(RH - 1.676331) +
      0.00391838 * RH^{3/2} * atan(0.023101 * RH)
  }
}

#' Wet Bulb Iterator
#'
#' Uses a convergence variable to find the wet bulb temp by solving for. Warren ran through this in class.
#'
#' @param T_c air temperature in celsius
#' @param rh relative humidity as fraction
#' @param p_atm pressure atmosphere (PA)
#' @param iter number of iterations
#' @param e_o R_d over R_v (g/g) Vapour pressure constant Stull et al., eq 4.5
#'
#' @return temperature celsius
#' @export
#'
#' @examples wet_bulb_iter(21, 0.37, 101325)
wet_bulb_iter <- function(T_c, rh, p_atm, iter = 3000, e_o = 0.622){

  # define matrices
  residual_mtx <- matrix()
  wet_bulb_trials <- matrix()

  # calculate baseline humidity ratio we will try to converge on
  e_sat <- tetens(T_c, 610.8)
  e_act <-  e_sat * rh
  W_base <- mixing_ratio_p(e_act, p_atm, e_o)

  # Initial value for T_wb
  wet_bulb_trials[1]  <-  T_c

  for(i in 1:iter){

    # calculate sat vap press at guessed wet bulb temp
    e_wb <-  tetens(wet_bulb_trials[i], e_o = 610.8)

    # convert the sat vapour pressure to humidity ratio
    W_wb <- mixing_ratio_p(e_wb, p_atm, e_o)

    W_iter <- mixing_ratio_t(T_c, wet_bulb_trials[i], W_wb)

    # convergence variable

    residual_mtx[i] <- abs(W_base - W_iter)

    wet_bulb_trials[i+1] <- wet_bulb_trials[i] - 0.01 # wet bulb temp is less than equal to dry bulb
  }

  index <- which.min(residual_mtx) # where is the lowest residual

  wet_bulb_trials[index] # return wet bulb temp at index with lowest residual

}

#' Standard Temperature
#'
#' Non-linear version, Stull et al., 2017 eq. 1.16
#'
#' @param m_asl metres above sea level.
#'
#' @return standard temperature (celsius)
#' @export
#'
#' @examples standard_temperature(1000)
standard_temperature <- function(m_asl) { # Stull et al., 2017 eq. 1.16

  H <- m_asl / 1000

  K <- dplyr::if_else(H <= 11,
                      288.15 - (6.5) * H,

                      dplyr::if_else(H >= 11 & H <= 20,
                                     216.65,

                                     dplyr::if_else(H >= 20 & H <= 32,
                                                    16.65 + (1) * (H - 20),

                                                    dplyr::if_else(H >= 32 & H <= 47,
                                                                   228.65 + (2.8) * (H - 32),

                                                                   dplyr::if_else(H >= 47 & H <= 51,
                                                                                  270.65, 0
                                                                   )))))

  return(K - 273.15)
}

# Pressure at Altitude
#' Non-uniform equation from Stull et al., 2017 eq. 1.17
#'
#' @param m_asl metres above sea level
#'
#' @return kpa
#' @export
#'
#' @examples pressure_atmosphere(1000)
pressure_atmosphere <- function(m_asl){ # Stull et al., 2017 eq. 1.17
  H <- m_asl / 1000

  st <- standard_temperature(m_asl) + 273.15
  dplyr::if_else(H <= 11,
                 101.325 * (288.15 / st)^-5.255877,

                 dplyr::if_else(H >= 11 & H <= 20,
                                (22.632)* exp(-0.1577*(H-11)),

                                dplyr::if_else(H >= 20 & H <= 32,
                                               (5.4749)*(216.65/st)^34.16319,

                                               dplyr::if_else(H >= 32 & H <= 47,
                                                              (0.868)*(228.65/st)^12.2011,

                                                              dplyr::if_else(H >= 47 & H <= 51,
                                                                             (0.1109)*exp(-0.1262*(H-47)), 0
                                                              )))))

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
