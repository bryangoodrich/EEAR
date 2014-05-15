#' @name cast
#' @rdname cast
#' 
#' @title Creates or coerces a vector of type POSIXct
#' 
#' @description These functions coerce a POSIX time stamp into bins.
#' \code{as_interval} creates a factor for each 15 minute interval
#' \code{as_season} creates a factor for each season
#' \code{as_weekday} creates a factor for each day of the week
#' 
#' @param t a POSIXct object.
#' @param type A number indicating which type of schedule to create.
#' @param tz A timezone specificaton to be used for the conversion. Defaults
#'   to America/Los_Angeles.
#' @return An ordered factor of equal length. 
#' 
#'   For \code{as_interval}, this is the '\%H:\%M' component t.
#'   
#'   For \code{as_season}, this is the '\%m' component of t accordingly
#'   \itemize{
#'     \item{"Winter"}{Jan, Feb, Mar, and Dec}
#'     \item{"Summer"}{Jun, Jul, Aug, Sep}
#'     \item{"Shoulder"}{Apr, May, Oct, Nov}
#'   }
#'   
#'   For \code{as_weekday}, this is the \code{\link{weekdays}} conversion 
#'   of t, ordered from Sunday to Saturday.
NULL



#' @export
#' @rdname cast
as_interval <- function(t, tz = "America/Los_Angeles") {
    TIME <- c(
        "00:00", "00:15", "00:30", "00:45", "01:00", "01:15", "01:30",
        "01:45", "02:00", "02:15", "02:30", "02:45", "03:00", "03:15",
        "03:30", "03:45", "04:00", "04:15", "04:30", "04:45", "05:00",
        "05:15", "05:30", "05:45", "06:00", "06:15", "06:30", "06:45",
        "07:00", "07:15", "07:30", "07:45", "08:00", "08:15", "08:30",
        "08:45", "09:00", "09:15", "09:30", "09:45", "10:00", "10:15",
        "10:30", "10:45", "11:00", "11:15", "11:30", "11:45", "12:00",
        "12:15", "12:30", "12:45", "13:00", "13:15", "13:30", "13:45",
        "14:00", "14:15", "14:30", "14:45", "15:00", "15:15", "15:30",
        "15:45", "16:00", "16:15", "16:30", "16:45", "17:00", "17:15",
        "17:30", "17:45", "18:00", "18:15", "18:30", "18:45", "19:00",
        "19:15", "19:30", "19:45", "20:00", "20:15", "20:30", "20:45",
        "21:00", "21:15", "21:30", "21:45", "22:00", "22:15", "22:30",
        "22:45", "23:00", "23:15", "23:30", "23:45")
    factor(strftime(t, '%H:%M', tz = tz),
           levels=TIME, ordered = TRUE)
}

#' @export
#' @rdname cast
as_season <- function(t) {
    factor(Month2Season <- c(
        "01" = "Winter",   "02" = "Winter",   "03" = "Winter",
        "04" = "Shoulder", "05" = "Shoulder", "06" = "Summer",
        "07" = "Summer",   "08" = "Summer",   "09" = "Summer",
        "10" = "Shoulder", "11" = "Shoulder", "12" = "Winter"),
           ordered = TRUE)
    Month2Season[format(t, '%m')]
}

#' @export
#' @rdname cast
as_weekday <- function(t) {
    WEEKDAYS <- c(
        "Sunday", "Monday", "Tuesday", "Wednesday", 
        "Thursday", "Friday", "Saturday")
    factor(weekdays(t), WEEKDAYS, ordered=TRUE)
}

#' @export
#' @rdname cast
as_schedule <- function(t, type = 1) {
    type <- match.arg(as.character(type), choices = c("1", "2", "3"))
    
    x <- as_weekday(t)
    f <- switch(type,
        "1" = c("Sunday", "Monday", rep("Workweek", 4), "Saturday"),
        "2" = c("Sunday", rep("Weekday", 5), "Saturday"),
        "3" = c("Weekend", rep("Weekday", 5), "Weekend")
    )
    levels(x) <- f
    
    x <- switch(type,
        "1" = factor(x, c("Monday", "Workweek", "Saturday", "Sunday", ordered = TRUE)),
        "2" = factor(x, c("Weekday", "Saturday", "Sunday", ordered = TRUE)),
        "3" = factor(x, c("Weekday", "Weekend", ordered = TRUE))
    )
    x
} 

