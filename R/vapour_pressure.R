#' Tetens' formula for saturated vapour pressure
#'
#' Change e_o to 610.8 for pa. From Stull et al., 2017 eq. 4.2
#'
#' @param T_c temperature Celsius
#' @param e_o vapour pressure constant kpa
#' @param b1 empirical parameter
#' @param b2 empirical parameter
#'
#' @return kPa or pa if e_o == 610.8 pa
#' @export
#'
#' @examples tetens(30)
tetens <- function(T_c, e_o = 0.6113, b1 = 17.27, b2 = 21.87){
  dplyr::if_else(T_c >= 0,
                 e_o*exp((b1*T_c)/(T_c+237.3)), # true
                 e_o*exp((b2*T_c)/(T_c+265.5)) # false
  )
}

#' Clausius-Clapeyron Equation for Saturated Vapour Pressure
#'
#' Returns the saturated vapour pressure using the Clausius-Clapeyron equation. From Stull et al., 2017 eq. 4.1
#'
#' @param T_c temperature in Celsius, function does kelvin conversion
#' @param e_o vapour pressure constant kpa
#' @param Lv_Rv latent heat of vapourization 2.5e6 j kg-1 (for water) over water vapour gas constant 461 J K -1 kg -1
#' @param Ld_Rv latent heat of diffusion 2.83e6 J kg-1 (for ice) over water vapour gas constant 461 J K -1 kg -1
#' @return e_sat in kPa
#' @export
#'
#' @examples clausius_clapeyron(T_c = 30, e_o = 0.6113)
clausius_clapeyron <- function(T_c, Lv_Rv = 5423, Ld_Rv = 6139, e_o = 0.6113){

  K <- T_c + 273.15 # Celsius to Kelvin

  dplyr::if_else(T_c >= 0,
                 e_o * exp((Lv_Rv)*((1/273.15) - (1/K))), # true
                 e_o * exp((Ld_Rv)*((1/273.15) - (1/K))) # false
  )
}

#' Actual Vapour Pressure
#'
#' Returns the actual or unsaturated vapour pressure using the Clau-sius-Clapeyron equation.
#'
#' @param T_c temperature in Celsius
#' @param T_d dew point temperature
#' @inheritParams clausius_clapeyron
#'
#' @return e_act in kPa
#' @export
#'
#' @examples clausius_clapeyron_act(T_c = 30, T_d = 12.8, e_o = 0.6113)
clausius_clapeyron_act <- function(T_c, T_d, Lv_Rv = 5423, Ld_Rv = 6139, e_o = 0.6113) { # Stull et al., 2017 eq. 4.1b
  K_d <- T_d + 273.15
  dplyr::if_else(T_c >= 0,
                 e_o * exp((Lv_Rv) * ((1/273.15) - 1/K_d)), # true
                 e_o * exp((Ld_Rv) * ((1/273.15) - 1/K_d)) # false
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
#' @examples actual_vapour_pressure(e_sat = 4.360379, rh = 0.35)
actual_vapour_pressure <- function(e_sat, rh){
  e_sat * rh
}

#' Actual Vapour Pressure (using absolute humidity)
#'
#' Another way to get actual vapour pressure as a funciton of absolute humidity. From Stull et al., 2017 eq. 1.19.
#' @param absolute_humidity kg/kg or g/g
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
