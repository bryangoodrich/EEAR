\name{write_meter}
\alias{write_meter}
\title{Write Copied IEE Data to CSV file}
\usage{
write_meter(meter, filename = NULL, filepath = ".", append = FALSE,
  header = FALSE, ...)
}
\arguments{
  \item{meter}{The identifier (meter number) for the meter.
  Acts as the default file name when `filename` parameter
  unspecified.}

  \item{filename}{The name of the file. Default is NULL.
  Will use `meter` to provide default file name.}

  \item{filepath}{The path to where the file should be
  saved. Defaults to the current working directly (".")}

  \item{append}{Logical. Should the meter data be appended
  to the specified file.}

  \item{header}{Logical. Does the data contain headers.
  Default is FALSE.}

  \item{\dots}{All other parameters passed to read.delim,
  where clipboard content is read in and processed before
  writing out to CSV.}
}
\description{
Write meter data from IEE copied onto windows clipboard
into a CSV file.
}

