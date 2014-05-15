\name{weather_plot}
\alias{weather_plot}
\alias{weather_prepare}
\title{Plot Demand against Temperature}
\usage{
weather_plot(x, point = c("day", "interval"), facets = c("none", "weekday",
  "workweek", "quarter", "year"), seasonality = TRUE, legend = seasonality,
  title = "", xlab = "Temperature (F)", ylab = "kW", plot = TRUE)

weather_prepare(x, point, facets, seasonality)
}
\arguments{
  \item{x}{a data table}

  \item{point}{The type of point to plot, daily averages or
  instance intervals.}

  \item{facets}{How to panel the plot; default "none" for
  entire data set.}

  \item{seasonality}{Logical. Should points be coloured by
  season}

  \item{legend}{Logical. Should the legend be included in
  plot.}

  \item{title}{The title on the plot.}

  \item{xlab}{The label for the x-axis. Default is "Time of
  Day".}

  \item{ylab}{The label for the y-axis. Default is "kW".}

  \item{plot}{Logical. Should the plot be drawn or}
}
\value{
Invisibly returns the ggplot plotting object.
}
\description{
Generates scatter plots kW ~ temperature
}

