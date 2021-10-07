# list of psychometric functions compiled from Stull et al., 2017 and 2001 ASHRAE Fundamentals Handbook (SI)

#### constants ####
R_d <- 2.871e-4 # kPa·K–1·m3·g–1 aka gas constant for dry air
R_v <- 4.61e-4 # kPa·K–1·m3·g–1

vap_pressure <- 622 # g kg–1

T_o <- 273.15 # K
e_o <- 0.6113 # kpa warren uses 610.8 (pa)

P_tot <- 101.325 # pressure at sea level (kpa)

#### functions ####

#' Pressure Uniform
#' Return air pressure given height above sea level, assumes uniform temp and pressure. From Stull et al., 2017 eq. 1.9b
#' @param H height in metres
#'
#' @return pressure in kPa
#' @export
#'
#' @examples
#' pressure_uniform(10^4)
pressure_uniform <- function(H){
  101.325 * exp(1)^{-H/7.29}
}


#' Saturated Vapour Pressure
#'
#' Returns the saturated vapour pressure using the Clausius-Clapeyron equation.
#'
#' @param T_c temperature in celcius, function does kelvin conversion
#' @param e_o vapour pressure constant kpa
#'
#' @return e_sat in kPa
#' @export
#'
#' @examples
clausius_clapeyron <- function(T_c, e_o = 0.6113){ # Stull et al., 2017 eq. 4.1

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
#' @examples
clausius_clapeyron_act <- function(T_c, T_d, e_o = 0.6113) { # Stull et al., 2017 eq. 4.1b
  dplyr::if_else(T_c >= 0,
  e_o * exp((5423) * ((1/273.15) - 1/T_d)), # true
  e_o * exp((6139) * ((1/273.15) - 1/T_d)) # false
  )
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
#' @examples
actual_vapour_pressure <- function(e_sat, rh){
  e_sat * rh
}

#' Actual Vapour Pressure (using absolute humidity)
#'
#' Another way to get actual vapour pressure as a funciton of absolute humidity. From Stull et al., 2017 eq. 1.19.
#' @param absolute_humidity fraction
#' @param T_c temperature celcius
#' @param R_v # kPa·K–1·m3·g–1
#'
#' @return
#' @export
#'
#' @examples
actual_vapour_pressure_ah <- function(absolute_humidity, T_c, R_v = 4.61e-4) {
  K <- T_c + 273.15
  ah <- absolute_humidity * 1000 # kg to g
  ah * R_v * K
}

#' Tetens' formula for saturated vapour pressure
#'
#' Change e_o to 610.8 for pa. From Stull et al., 2017 eq. 4.2
#'
#' @param T_c temperature celcius
#' @param e_o vapour pressure constant kpa
#'
#' @return kPa or pa if e_o == 610.8 pa
#' @export
#'
#' @examples
tetens <- function(T_c, e_o = 0.6113){
  dplyr::if_else(T_c >= 0,
          e_o*exp((17.27*T_c)/(T_c+237.3)), # true
          e_o*exp((21.87*T_c)/(T_c+265.5)) # false
  )
}


#' Absolute Humidity (water vapour density)
#'
#' Stull et al., 2017 eq. 1.20
#'
#' @param e_s saturated vapour pressure kPa
#' @param T_c temperature celcius
#' @param R_v kPa·K–1·m3·g–1
#'
#' @return
#' @export kg m-3
#'
#' @examples
absolute_humidity <- function(e_s, T_c, R_v = 4.61e-4){ # Stull et al., 2017 eq. 1.20
  K <- T_c + 273.15 # Celsius to Kelvin
  e_s / (R_v * K)
}

#' Specific Humidity
#'
#' From Stull et al., 2017 eq. 4.7
#'
#' @param e_s saturated vapour pressure kPa
#' @param total_pressure total pressure kPa
#' @param vap_pressure constant kPa
#'
#' @return g kg-1
#' @export
#'
#' @examples
specific_humidity <- function(e_s, total_pressure, vap_pressure = 622) { #
  (vap_pressure * e_s) / total_pressure
}


#' Relative Humidity
#'
#' @param e_act kPa or PA
#' @param e_s same units as e_act
#'
#' @return fraction
#' @export
#'
#' @examples
relative_humidity <- function(e_act, e_s) { # Stull et al., 2017 eq. 4.14a
  e_act / e_s
}

#' Mixing Ratio (Humidity Ratio)
#'
#' calculate hr using mostly temp - from psychometrics chapter 6 ASHRAE. Useful for wetbulb iteration calculation.
#'
#' @param T_c temperature celcius
#' @param T_wb wet bulb temperature
#' @param W_wb wet bulb humidity ratio
#'
#' @return g / kg
#' @export
#'
#' @examples
humidity_ratio_t <- function(T_c, T_wb, W_wb){

  ((2501 - 2.381 * T_wb) * W_wb - 1.006 * (T_c - T_wb)) / (2501 + 1.805 * T_c - 4.18 * T_wb)

}

#' Mixing Ratio (Humidity Ratio)
#'
#' Calculate the mixing ratio aka humidity ratio using mostly pressure. From Stull et al., 2017 eq. 4.4/4.5. Can use either saturated or unsaturated for e. You get what you put in.
#'
#' @param e_act vapour pressure, can be saturated or unsaturated (kPa)
#' @param p_atm kPa
#' @param E R_d over R_v g/kg is default or g/g with 0.622
#'
#'
#' @return g / kg (saturated or unsaturated depending on e) or g/g if E == 0.622
#' @export
#'
#' @examples
humidity_ratio_p <- function(e_act, p_atm, E = 622){
  E * e_act / (p_atm - e_act)
}

#' Density Moist Air using temperature and atmospheric pressure.
#'
#' Stull et al., 2017 eq. 1.20
#'
#' @param T_c temperature celcius
#' @param p_atm kPa
#' @param R_v kPa·K–1·m3·g–1
#'
#' @return g / m3
#' @export
#'
#' @examples
density_moist_air <- function(T_c, p_atm, R_v = 4.61e-4){
  p_atm / (R_v * (T_c + 273.15))
}

#' Density Moist Air
#'
#' Density Moist Air using virtual temp, hardly different from above in practice (g / m3). From Stull et al., 2017 eq. 1.23.
#'
#' Stull et al., 2017 eq. 1.23
#'
#' @param T_c temperature celcius
#' @param p_atm kPa
#' @param R_v kPa·K–1·m3·g–1
#' @param mixing_ratio g / m3
#' @param mixing_ratio_liquid g / m3 usually assume 0
#' @param mixing_ratio_ice g / m3 usually assume 0
#'
#' @return g / m3
#' @export
#'
#' @examples
density_moist_air_virtual <- function(T_c, p_atm, R_v = 4.61e-4, mixing_ratio, mixing_ratio_liquid = 0, mixing_ratio_ice = 0){
  T_v <- virtual_temp(T_c, mixing_ratio, mixing_ratio_liquid, mixing_ratio_ice)
  p_atm / (R_v * (T_v +273.15))
}

#' Density Moist Air (water vapour density)
#'
#' Density Moist Air using mixing ratio, hardly different from above in practice (g / m3). From 2001 ASHRAE Fundamentals Handbook (SI) eqn 11 + 27 gives g/m3.
#'
#'
#' @param T_c temperature celcius
#' @param p_atm pressure atmosphere kPa
#' @param e_sat saturated vapour pressure kPa
#' @param W mixing ratio g/kg
#'
#' @return kg m-3
#' @export
#'
#' @examples
density_moist_air_hr <- function(T_c, p_atm, e_sat, W){ # from 2001 ASHRAE Fundamentals Handbook (SI) eqn 11 + 27 gives g/m3
  (1/((R_d * (T_c + 273.15))/ (p_atm - e_sat)))*(1+W)
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
#' @examples dew_point_temp(1.286)
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
#'
#' @return Celsius
#' @export
#'
#' @examples dew_point_temp_r(0.01, 80)
dew_point_temp_r <- function(mixing_ratio, p_atm, R_v_L_v = 1.844e-4, e_o = 0.6113){ #

 K_dp <- ((1/273.15) - (R_v_L_v) * (log(((mixing_ratio*p_atm)/(e_o * (mixing_ratio+0.622))))))^-1

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
#' @param T_c temperature celcius
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
#'
#' @return temperature celsius
#' @export
#'
#' @examples wet_bulb_iter(21, 0.37, 101325)
wet_bulb_iter <- function(T_c, rh, p_atm, iter = 3000){

  # define matrices
  residual_mtx <- matrix()
  wet_bulb_trials <- matrix()

  # calculate baseline humidity ratio we will try to converge on
  e_sat <- tetens(T_c, 610.8)
  e_act <-  e_sat * rh
  W_base <- humidity_ratio_p(e_act, p_atm, 0.622)

  # Initial value for T_wb
  wet_bulb_trials[1]  <-  T_c

  for(i in 1:iter){

    # calculate sat vap press at guessed wet bulb temp
    e_wb <-  tetens(wet_bulb_trials[i], e_o = 610.8)

    # convert the sat vapour pressure to humidity ratio
    W_wb <- humidity_ratio_p(e_wb, p_atm, 0.622)

    W_iter <- humidity_ratio_t(T_c, wet_bulb_trials[i], W_wb)

    # convergence variable

    residual_mtx[i] <- abs(W_base - W_iter)

    wet_bulb_trials[i+1] <- wet_bulb_trials[i] - 0.01 # wet bulb temp is less than equal to dry bulb
  }

  index <- which.min(residual_mtx) # where is the lowest residual

  wet_bulb_trials[index] # return wet bulb temp at index with lowest residual

}

# convert specific humidity to humidity ratio
mr_sh <- function(mr){x <- mr / (1+mr)}

# Standard temperature at given altitude
standard_temperature <- function(H) { # Stull et al., 2017 eq. 1.16
  dplyr::if_else(H <= 11,
          288.15 - (6.5) * H,

          dplyr::if_else(H >= 11 & H <= 20,
                  216.65,

                  dplyr::if_else(H >= 20 & H <= 32,
                          216.65 + (1) * (H - 20),

                          dplyr::if_else(H >= 32 & H <= 47,
                                  228.65 + (2.8) * (H - 32),

                                  dplyr::if_else(H >= 47 & H <= 51,
                                          270.65, 0
                                  )))))
}

# Pressure for given altitude
pressure_atmosphere <- function(H){ # Stull et al., 2017 eq. 1.17
  st <- standard_temperature(H)
  dplyr::if_else(H <= 11,
          101.325 * (288.15 / st)^5.255877,

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
