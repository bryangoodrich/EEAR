\name{cast}
\alias{as_interval}
\alias{as_schedule}
\alias{as_season}
\alias{as_weekday}
\alias{cast}
\title{Creates or coerces a vector of type POSIXct}
\usage{
as_interval(t, tz = "America/Los_Angeles")

as_season(t)

as_weekday(t)

as_schedule(t, type = 1)
}
\arguments{
  \item{t}{a POSIXct object.}

  \item{type}{A number indicating which type of schedule to
  create.}

  \item{tz}{A timezone specificaton to be used for the
  conversion. Defaults to America/Los_Angeles.}
}
\value{
An ordered factor of equal length.

For \code{as_interval}, this is the '\%H:\%M' component t.

For \code{as_season}, this is the '\%m' component of t
accordingly \itemize{ \item{"Winter"}{Jan, Feb, Mar, and
Dec} \item{"Summer"}{Jun, Jul, Aug, Sep}
\item{"Shoulder"}{Apr, May, Oct, Nov} }

For \code{as_weekday}, this is the \code{\link{weekdays}}
conversion of t, ordered from Sunday to Saturday.
}
\description{
These functions coerce a POSIX time stamp into bins.
\code{as_interval} creates a factor for each 15 minute
interval \code{as_season} creates a factor for each season
\code{as_weekday} creates a factor for each day of the week
}

