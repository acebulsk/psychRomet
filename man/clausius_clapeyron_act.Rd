% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vapour_pressure.R
\name{clausius_clapeyron_act}
\alias{clausius_clapeyron_act}
\title{Actual Vapour Pressure}
\usage{
clausius_clapeyron_act(T_c, T_d, Lv_Rv = 5423, Ld_Rv = 6139, e_o = 0.6113)
}
\arguments{
\item{T_c}{temperature in Celsius}

\item{T_d}{dew point temperature}

\item{Lv_Rv}{latent heat of vapourization 2.5e6 j kg-1 (for water) over water vapour gas constant 461 J K -1 kg -1}

\item{Ld_Rv}{latent heat of diffusion 2.83e6 J kg-1 (for ice) over water vapour gas constant 461 J K -1 kg -1}

\item{e_o}{vapour pressure constant kpa}
}
\value{
e_act in kPa
}
\description{
Returns the actual or unsaturated vapour pressure using the Clau-sius-Clapeyron equation.
}
\examples{
clausius_clapeyron_act(T_c = 30, T_d = 12.8, e_o = 0.6113)
}
