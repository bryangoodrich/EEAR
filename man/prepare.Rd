\name{prepare}
\alias{prepare}
\title{Prepare A Raw Meter Data Set for Analysis}
\usage{
prepare(x, totalized = TRUE, meters = NULL, days = NULL,
  start_date = NULL, end_date = NULL, start_time = "00:00",
  end_time = "23:45")
}
\arguments{
  \item{x}{a data table}

  \item{totalized}{Logical. Should kW be aggregated across
  meters.  (Default TRUE)}

  \item{meters}{Optional. Meters to keep in data. Defaults
  to all in data.}

  \item{days}{Optional. Days of week to include. Accepts
  numeric, with "Sunday" equals to 1. Defaults to every
  day.}

  \item{start_date}{The earliest date to include.}

  \item{end_date}{The latest date to include.}

  \item{start_time}{The beginning time of day to include.
  Default "00:00"}

  \item{end_time}{The ending time of day to include.
  Default "23:45"}
}
\value{
A data table with 4 fields \itemize{ \item timestamp. The
15-minute time stamp.  \item interval. The interval time of
day.  \item weekday. The timestamp day of the week.  \item
kw. The total or mean kW across all selected meters.  }
}
\description{
A general function for filtering the raw data set according
to available meters, selected days of the week, date and
time of day range, and whether the data should be
aggregated (sum) or averaged (mean) over all the indicated
meters.
}

