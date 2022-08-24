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

  # check all required inputs are valid
  if ((is.na(T_c) | is.na(rh) | is.na(p_atm))) {
    return(NA)
  }
  else{
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

    return(wet_bulb_trials[index]) # return wet bulb temp at index with lowest residual
  }
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



