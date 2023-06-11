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

  if(all(is.na(RH))){
    stopifnot(!all(is.na(e_act)) & !all(is.na(e_act)))
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

#' Ice Bulb Temperature Calculator
#'
#' Uses the Newton-Raphston Iteration Functions to converge on an ice bulb temperature. This code was sent from Phil Harder within his phase correction script.
#'
#'
#' @param Ta air temperature in deg C
#' @param RH relative humidity as a fraction
#'
#' @author Phillip Harder
#'
#' @return a vector of ice bulb temperatures in deg C
#' @export
#'
#' @examples ice_bulb_iter(-10, 0.5)
ice_bulb_iter <- function(Ta, RH){

  # Newton-Raphston Iteration Functions
  ffun<-function(Tai,Ti1){
    ff <- -Ti1+Tai-(Li*diffusivity_water_vapour(Tai)/thermal_conductivity_air(Tai))*(absolute_humidity(T_c = Tai, rh = 1)-absolute_humidity(T_c = Tai, rh = RHi))
  }

  fpfun<-function(Tai,Ti1){
    T1 <- Ti1+0.001*Ti1
    T2 <- Ti1-0.001*Ti1
    fp <- (ffun(Tai,T1)-ffun(Tai,T2))/(0.002*Ti1)
  }

  Tifun<-function(Ti1){
    Ti2<-Ti1-ffun(Tai,Ti1)/fpfun(Tai,Ti1)
  }

  #Latent Heat [J kg-1]
  L <- 1000*(2501-(2.361*Ta))
  L[which(Ta<0)] <- 1000*(2834.1-0.29*Ta[which(Ta<0)]-0.004*Ta[which(Ta<0)]^2)

  RH[which(Ta<0)] <- buckfun(RH[which(Ta<0)], Ta[which(Ta<0)])

  Ti <- matrix(ncol=1,nrow=length(Ta),NA)

  #Ti Iterative Solution
  for(i in 1:length(Ta)){
    Tai <- Ta[i]
    RHi <- RH[i]
    Li <- L[i]
    Ti1 <- Tai-5.0001 #Initial guess of Ti
    crit <- 99999 #Initial critical value for while loop
    while(crit>0.000001){
      Ti2 <- Tifun(Ti1)
      crit <- abs(Ti1-Ti2)
      Ti1 <- Ti2
    }
    Ti[i] <- Ti1
  }
  return(Ti[,1])
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



