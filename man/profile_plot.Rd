\name{profile_plot}
\alias{profile_plot}
\alias{profile_prepare}
\title{Plot Usage Profile Lines}
\usage{
profile_plot(x, line = c("weekday", "workweek", "day", "month", "season",
  "year"), facets = NULL, legend = TRUE, title = "",
  xlab = "Time of Day", ylab = "kWh", plot = TRUE, ...)

profile_prepare(x, line, facets, ...)
}
\arguments{
  \item{x}{a data table}

  \item{line}{The type of line to be drawn. Default day of
  week. Possible values include every `day`, `month`,
  `season`, and `year`.}

  \item{facets}{The type of facets to panel the profiles
  by. The avaiable facets depend upon which line is chosen.
  Day profiles can be plotted for each week, month,
  quarter, season, or year. Weekday profiles for all but
  week.  Month for quarter, season, or year. Season by
  year. Year takes no facets.}

  \item{legend}{Logical. Should the legend be included in
  plot.}

  \item{title}{The title on the plot.}

  \item{xlab}{The label for the x-axis. Default is "Time of
  Day".}

  \item{ylab}{The label for the y-axis. Default is "kW".}

  \item{plot}{Logical. Should the plot be drawn.}

  \item{\dots}{Further arguments passed to data
  preparation.}
}
\value{
Invisibly returns the ggplot plotting object.
}
\description{
Generates profile plots, averaged for the given line
period, paneled for the given facets.
}

