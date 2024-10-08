% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/temperature.R
\name{dew_point_temp_r}
\alias{dew_point_temp_r}
\title{Dew Point Temperature}
\usage{
dew_point_temp_r(mixing_ratio, p_atm, R_v_L_v = 0.0001844, e_o = 0.622)
}
\arguments{
\item{mixing_ratio}{in grams}

\item{p_atm}{pressure atmosphere kpa}

\item{R_v_L_v}{ratio constant in K}

\item{e_o}{R_d over R_v (g/g) Vapour pressure constant Stull et al., eq 4.5}
}
\value{
Celsius
}
\description{
Calculated using mixing ratio and atmospheric pressure. From Stull et al., 2017 eq. 4.15b.
}
\examples{
dew_point_temp_r(0.01, 80)
}
