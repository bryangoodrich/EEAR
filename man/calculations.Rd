\name{loadfactor}
\alias{eui}
\alias{eui2}
\alias{kwh}
\alias{loadfactor}
\alias{mean2}
\title{Various kW calculations}
\usage{
loadfactor(x)

kwh(x, h = 0.25)

eui(x, sqft)

eui2(x, sqft)

mean2(x, ...)
}
\arguments{
  \item{x}{A numeric vector representing kW.}

  \item{h}{A numeric value representing proportion of an
  hour represented.}

  \item{sqft}{The square-footage of the represented
  structure.}

  \item{...}{additional arguments passed onto other
  functions.}
}
\value{
A numeric vector representing a calculated value.
}
\description{
This collection transforms a vector of kW into a vector
normalized by some factor, such as load factor and the max
consumption, the energy use index and the square-footage of
the structure, or kWh as kW normalized by its proportion of
an hour.
}

