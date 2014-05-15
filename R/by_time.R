#' Breaking up a time stamp by different periods
#' 
#' Creates functions to break a time stamp into periods by the specified breaks.
#' Many options are created already for common periods, such as by hour, week,
#' month, and quarter.
#' 
#' @param breaks One of numerous options for splitting a POSIX time object
#'   up into bins according to the specified breaks. See
#'   \code{\link{cut.POSIXt}} for more details.
#' @param x a POSIXct object.
#' @param start.on.monday logical. If breaks = "weeks", should the week start
#'   on Mondays or Sundays?
#' @return A factor representing the bins, as equivalent POSIX time stamps,
#'   for the first date in that bin according to the specified breaks.
#' @export
#' @examples
#'     by_4months <- by_time("4 month")
#'     day_seq <- seq(as.POSIXct("2012-01-01 00:00:00 -0800"),
#'                    as.POSIXct("2013-01-01 00:00:00 -0800"), by = 'day')
#'     by_4months(day_seq)
#'     by_quarter(day_seq)
by_time <- function(breaks, start.on.monday = FALSE) {
    function (x) cut(x, breaks=breaks, start.on.monday = start.on.monday)
}



#' @export
#' @rdname by_time
by_hour <- by_time('hour')

#' @export
#' @rdname by_time
by_day <- by_time('DSTday')

#' @export
#' @rdname by_time
by_week <- by_time('week')

#' @export
#' @rdname by_time
by_month <- by_time('month')

#' @export
#' @rdname by_time
by_quarter <- by_time('quarter')

#' @export
#' @rdname by_time
by_year <- by_time('year')
