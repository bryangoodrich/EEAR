\docType{data}
\name{MeterData}
\alias{MeterData}
\alias{OfficeMeter}
\title{Example Meter Data Sets}
\format{A data table with 140352 rows and 3 variables}
\description{
Three datasets containing 15-minute commercial smart meter
interval data from Sacramento area. The data sets include

A dataset containing 15-minute smart meter interval data
for an office space in the Sacramento region. Dates range
from Midnight January 1, 2012 to end of day December 31,
2013
}
\details{
\itemize{ \item OfficeMeter. A commercial office space.
\item SchoolMeter. A school campus.  \item ManMeter. A
manufacturer with early, tight usage. }

Each example represents the intended data model for
imported meter data.  The variables are as follows:

\itemize{ \item meter. The identifier of the meter reading.
\item timestamp. The 15-minute time stamp, imported as GMT.
\item kW. The monitored 15-minute demand over the meter. }

The template format for imported meter data is as follows:

\itemize{ \item meter. The identifier of the meter reading.
\item timestamp. The 15-minute time stamp, imported as GMT.
\item kW. The monitored 15-minute demand over the meter. }
}
\keyword{datasets}

