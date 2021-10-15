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
  km <- m_asl / 1000
  101.325 * exp(1)^{-km/7.29}
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
