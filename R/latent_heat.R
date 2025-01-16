#' Calculate the latent heat of vaporisation
#'
#' From CRHM ClassevapD_Resist.cpp line 258
#'
#' @param air_temp Air temperature in deg. C.
#'
#' @return Latent heat of vaporisation in j kg
#' @export
#'
#' @examples latent_heat_vaporisation(0)
latent_heat_vaporisation <- function(air_temp){

  Lv <- (2.501 - 0.002361 * air_temp) * 1e6 # Mj kg -1 to j kg -1

  return(Lv)
}

#' Calculate the latent heat of sublimation
#'
#' From CRHM NewModules_old.cpp line 1249
#'
#' @param air_temp Air temperature in deg. C.
#'
#' @return Latent heat of sublimation in Mj kg
#' @export
#'
#' @examples latent_heat_vaporisation(0)
latent_heat_sublimation <- function(air_temp){

  Ls <- 1000*(2834.1-0.29*air_temp-0.004*air_temp^2) # j kg -1

   return(Ls)
}
