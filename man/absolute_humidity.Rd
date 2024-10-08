% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/density.R
\name{absolute_humidity}
\alias{absolute_humidity}
\title{Absolute Humidity (water vapour density)}
\usage{
absolute_humidity(T_c, rh, R_v = 0.4615)
}
\arguments{
\item{T_c}{temperature Celsius}

\item{rh}{as fraction}

\item{R_v}{kPa·K–1·m3·kg–1}
}
\value{
density of water vapour in the air kg m-3
}
\description{
A rearangement of actual_vapour_pressure Stull et al., 2017 eq. 1.19
}
\examples{
absolute_humidity(2, .20)
}
