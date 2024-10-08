% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/temperature.R
\name{standard_temperature}
\alias{standard_temperature}
\title{Standard Temperature}
\usage{
standard_temperature(m_asl)
}
\arguments{
\item{m_asl}{metres above sea level.}
}
\value{
standard temperature (celsius)
}
\description{
Non-linear version, Stull et al., 2017 eq. 1.16
}
\examples{
standard_temperature(1000)
}
