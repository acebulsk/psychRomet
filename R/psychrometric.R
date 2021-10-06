# list of psychometric functions compiled from Stull et al., 2017 and 2001 ASHRAE Fundamentals Handbook (SI)

#### constants ####
R_d <- 2.871e-4 # kPa·K–1·m3·g–1 aka gas constant for dry air
R_v <- 4.61e-4 # kPa·K–1·m3·g–1

vap_pressure <- 622 # g kg–1

T_o <- 273.15 # K
e_o <- 0.6113 # kpa warren uses 610.8 (pa)

P_tot <- 101.325 # pressure at sea level (kpa)

#### functions ####

# Assumes uniform temp and pressure (kPa)
pressure_uniform <- function(H){ # Stull et al., 2017 eq. 1.9b
  101.325 * exp(1)^{-H/7.29}
}

unsat_vap_pressure_liq <- function(T_d) { # Stull et al., 2017 eq. 4.1
  e_o * exp((5423) * ((1/T_o) - 1/T_d))
}

# Tetens' formula for saturated vapour pressure (kPa)
tetens <- function(T_C){ # Stull et al., 2017 eq. 4.2
  if_else(T_C >= 0,
          e_o*exp((17.27*T_C)/(T_C+237.3)), # true
          e_o*exp((21.87*T_C)/(T_C+265.5)) # false
  )
}

# Clausius-Clapeyron equation for saturated vapour pressure (kPa)
clausius_clapeyron <- function(T_C){ # Stull et al., 2017 eq. 4.1
  K <- T_C + 273.15 # Celsius to Kelvin
  e_o * exp((5423)*((1/273.15) - (1/K)))
}

# Partial Pressure due to water vapour

partial_vap_pressure_ah <- function(absolute_humidity, T_C) { # Stull et al., 2017 eq. 1.19
  K <- T_C + 273.15
  ah <- absolute_humidity * 1000 # kg to g
  ah * R_v * K
}

# aka e_act (kPa)
partial_vap_pressure <- function(e_sat, rh){ # shown in tutorial
  e_sat * rh
}

# aka water vapour density (kg / m3)
absolute_humidity <- function(e_s, T_C){ # Stull et al., 2017 eq. 1.20
  K <- T_C + 273.15 # Celsius to Kelvin
  e_s / (R_v * K)
}

# Specific Humidity (g / kg)
specific_humidity <- function(e_s, total_pressure) { # Stull et al., 2017 eq. 4.7
  (vap_pressure * e_s) / total_pressure
}

# Relative Humidity
relative_humidity1 <- function(e_act, e_s) { # Stull et al., 2017 eq. 4.14a
  e_act / e_s
}

relative_humidity2 <- function(T_wb, P_tot, r) { # Stull et al., 2017 eq. 4.14c
  e_s <- sat_vap_pressure2(T_C)
  r_s <- mixing_ratio(e_s, P_tot)

  100 * (r / r_s)
}

# Mixing Ratio (g / kg)
humidity_ratio_t <- function(T_C, T_wb, W_wb){ # calculate hr using mostly temp - from psychometrics chapter 6 ASHRAE

  ((2501 - 2.381 * T_wb) * W_wb - 1.006 * (T_C - T_wb)) / (2501 + 1.805 * T_C - 4.18 * T_wb)

}

# Mixing Ratio for saturated air (g / kg)
humidity_ratio_p <- function(e_act, p_atm){ # Stull et al., 2017 eq. 4.5
  622 * e_act / (p_atm - e_act)
}

# Density Moist Air (g / m3)
density_moist_air <- function(T_C, p_atm){ # Stull et al., 2017 eq. 1.20
  p_atm / (R_v * (T_C +273.15))
}

# Density Moist Air using virtual temp, hardly different from above in practice (g / m3)
density_moist_air_virtual <- function(T_C, p_atm, mixing_ratio, mixing_ratio_liquid = 0, mixing_ratio_ice = 0){ # Stull et al., 2017 eq. 1.23
  T_v <- virtual_temp(T_C, mixing_ratio, mixing_ratio_liquid, mixing_ratio_ice)
  p_atm / (R_v * (T_v +273.15))
}

# Density Moist Air using mixing ratio, hardly different from above in practice (g / m3)
density_moist_air_hr <- function(T_C, p_atm, e_sat, W){ # from 2001 ASHRAE Fundamentals Handbook (SI) eqn 11 + 27 gives g/m3
  (1/((R_d * (T_C + 273.15))/ (p_atm - e_sat)))*(1+W)
}

# Dew Point Temperature (Celsius)
dew_point_temp_e <- function(e_act){ # Stull et al., 2017 eq. 4.15a
  K_dp <- ((1/T_o) - (1.844e-4) * (log((e_act/e_o))))^-1

  K_dp - 273.15 # return Celsius
}

# Dew Point Temperature (Celsius)
# dew_point_temp_r <- function(mixing_ratio, p_atm){ # Stull et al., 2017 eq. 4.15b
#   r_g <- mixing_ratio / 1000 # convert to kg
#
#  K_dp <- ((1/T_o) - (1.844e-4) * (log(((r_g*p_atm)/(e_o * (r_g+0.622))))))^-1
#
#  K_dp - 273.15 # return Celsius
# }

# Temperature is dependent on the ambient humidity
virtual_temp <- function(T_C, mixing_ratio, mixing_ratio_liquid = 0, mixing_ratio_ice = 0){ # Stull et al., 2017 eq. 1.22
  K <- T_C + 273.15

  mr_g <- mixing_ratio / 1000
  mr_g_l <- mixing_ratio_liquid / 1000
  mr_g_i <- mixing_ratio_ice / 1000

  K_v <- K * (1 + (0.61 * mr_g) - mr_g_l - mr_g_i) # make sure are in grams

  K_v - 273.15 # return Celsius
}

# Potential Temperature
potential_temp<- function(Temp, P){ # Stull et al., 2017 eq. 3.12
  Temp * (100 / P) ^ 0.28571
}

# Empirical Approximation at sea level
wet_bulb_empirical <- function(T_C, RH = NA, e_act = NA, e_sat = NA){ # Stull et al., 2017 eq. 4.19

  if (is.na(RH)){
    RH <- relative_humidity1(e_act, e_sat) * 100
    T_C * atan(0.151977 * (RH + 8.313659)^0.5) - 4.686035 +
      atan(T_C + RH) - atan(RH - 1.676331) +
      0.00391838 * RH^{3/2} * atan(0.023101 * RH)

  } else {
    T_C * atan(0.151977 * (RH + 8.313659)^0.5) - 4.686035 +
      atan(T_C + RH) - atan(RH - 1.676331) +
      0.00391838 * RH^{3/2} * atan(0.023101 * RH)
  }
}

# wet buld iter - uses a convergence variable to find the wet bulb temp by solving for

# wet_bulb_iter <- function(T_C, rh, p_atm, iter = 3000){
#
#   # define matrices
#   residual_mtx <- matrix()
#   wet_bulb_trials <- matrix()
#
#   # calculate baseline humidity ratio we will try to converge on
#   e_sat <- tetens(T_C)
#   e_act <-  e_sat * rh
#   W_base <- humidity_ratio_p(e_act, p_atm)
#
#   # Initial value for T_wb
#   wet_bulb_trials[1]  <-  T_C
#
#   for(i in 1:iter){
#
#     # calculate sat vap press at guessed wet bulb temp
#     e_wb <-  tetens(wet_bulb_trials[i])
#
#     # convert the sat vapour pressure to humidity ratio
#     W_wb <- humidity_ratio_p(e_wb, p_atm)
#
#     # W_iter <- humidity_ratio_t(T_C, wet_bulb_trials[i], W_wb)
#
#     # convergence variable
#
#     residual_mtx[i] <- abs(W_base - W_wb)
#
#     wet_bulb_trials[i+1] <- wet_bulb_trials[i] - 0.01 # wet bulb temp is less than equal to dry bulb
#   }
#
#   index <- which.min(residual_mtx) # where is the lowest residual
#
#   wet_bulb_trials[index] # return wet bulb temp at index with lowest residual
# }

# convert specific humidity to humidity ratio
mr_sh <- function(mr){x <- mr / (1+mr)}

# Standard temperature at given altitude
standard_temperature <- function(H) { # Stull et al., 2017 eq. 1.16
  if_else(H <= 11,
          288.15 - (6.5) * H,

          if_else(H >= 11 & H <= 20,
                  216.65,

                  if_else(H >= 20 & H <= 32,
                          216.65 + (1) * (H - 20),

                          if_else(H >= 32 & H <= 47,
                                  228.65 + (2.8) * (H - 32),

                                  if_else(H >= 47 & H <= 51,
                                          270.65, 0
                                  )))))
}

# Pressure for given altitude
pressure_atmosphere <- function(H){ # Stull et al., 2017 eq. 1.17
  st <- standard_temperature(H)
  if_else(H <= 11,
          101.325 * (288.15 / st)^5.255877,

          if_else(H >= 11 & H <= 20,
                  (22.632)* exp(-0.1577*(H-11)),

                  if_else(H >= 20 & H <= 32,
                          (5.4749)*(216.65/st)^34.16319,

                          if_else(H >= 32 & H <= 47,
                                  (0.868)*(228.65/st)^12.2011,

                                  if_else(H >= 47 & H <= 51,
                                          (0.1109)*exp(-0.1262*(H-47)), 0
                                  )))))

}
