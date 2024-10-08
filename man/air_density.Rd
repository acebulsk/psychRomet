% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/density.R
\name{air_density}
\alias{air_density}
\title{Air Density at Elevation}
\usage{
air_density(T_c, m_asl, a = 0.04, p_0 = 1.225)
}
\arguments{
\item{T_c}{Temperature Celsius}

\item{m_asl}{Elevation (metres)}

\item{a}{0.040 K m –1}

\item{p_0}{average sea level density 1.2250 kg·m –3}
}
\value{
kg / m3
}
\description{
Air Density at Elevation
}
\examples{
air_density(15, 2000)
}
