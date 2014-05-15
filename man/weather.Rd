\name{weather}
\alias{get_temperature_handle}
\alias{set_temperature_handle}
\alias{temperature_join}
\alias{tj}
\alias{weather}
\alias{with_handle}
\alias{with_temperature}
\title{A suite of commands for managing weather data and meters.}
\usage{
set_temperature_handle(x, name = "temperatures")

get_temperature_handle()

with_handle(FUN, ..., h = get_temperature_handle())

temperature_join(t, interval)

tj(t, interval)

with_temperature(x, ...)
}
\arguments{
  \item{x}{a table object.}

  \item{name}{The name of the temperature object stored.
  Default "temperatures"}

  \item{FUN}{a function with first argument a temperature
  table}

  \item{h}{A temperature handle}

  \item{t}{a temperature table}

  \item{interval}{a timestamp for meter intervals.}

  \item{\dots}{arguments passed onto further functions.}
}
\description{
A suite of commands for managing weather data and meters.
}

