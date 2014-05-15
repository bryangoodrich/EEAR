\name{by_time}
\alias{by_day}
\alias{by_hour}
\alias{by_month}
\alias{by_quarter}
\alias{by_time}
\alias{by_week}
\alias{by_year}
\title{Breaking up a time stamp by different periods}
\usage{
by_time(breaks, start.on.monday = FALSE)

by_hour(x)

by_day(x)

by_week(x)

by_month(x)

by_quarter(x)

by_year(x)
}
\arguments{
  \item{breaks}{One of numerous options for splitting a
  POSIX time object up into bins according to the specified
  breaks. See \code{\link{cut.POSIXt}} for more details.}

  \item{x}{a POSIXct object.}

  \item{start.on.monday}{logical. If breaks = "weeks",
  should the week start on Mondays or Sundays?}
}
\value{
A factor representing the bins, as equivalent POSIX time
stamps, for the first date in that bin according to the
specified breaks.
}
\description{
Creates functions to break a time stamp into periods by the
specified breaks. Many options are created already for
common periods, such as by hour, week, month, and quarter.
}
\examples{
by_4months <- by_time("4 month")
    day_seq <- seq(as.POSIXct("2012-01-01 00:00:00 -0800"),
                   as.POSIXct("2013-01-01 00:00:00 -0800"), by = 'day')
    by_4months(day_seq)
    by_quarter(day_seq)
}

