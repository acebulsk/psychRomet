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
