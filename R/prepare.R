#' Prepare A Raw Meter Data Set for Analysis
#' 
#' A general function for filtering the raw data set according to 
#'   available meters, selected days of the week, date and time of day
#'   range, and whether the data should be aggregated (sum) or 
#'   averaged (mean) over all the indicated meters.
#' 
#' @param x a data table
#' @param totalized Logical. Should kW be aggregated across meters. 
#'   (Default TRUE)
#' @param meters Optional. Meters to keep in data. Defaults to all in data.
#' @param days Optional. Days of week to include. Accepts numeric, with
#'   "Sunday" equals to 1. Defaults to every day.
#' @param start_date The earliest date to include.
#' @param end_date The latest date to include.
#' @param start_time The beginning time of day to include. Default "00:00"
#' @param end_time The ending time of day to include. Default "23:45"
#' @return A data table with 4 fields
#'   \itemize{
#'     \item timestamp. The 15-minute time stamp.
#'     \item interval. The interval time of day.
#'     \item weekday. The timestamp day of the week. 
#'     \item kw. The total or mean kW across all selected meters.
#'   }
#' @export
prepare <- 
function(x, totalized = TRUE, meters = NULL, days = NULL, 
         start_date = NULL, end_date = NULL,
         start_time = "00:00", end_time = "23:45") 
{
    # Local binding for dplyr NSE
    meter <- weekday <- interval <- kW <- NULL
    
    if (totalized) {
        measure <- function(x, ...) sum(x, na.rm = TRUE, ...)
    } else {
        measure <- function(x, ...) mean(x, na.rm = TRUE, ...)
    }
    
    day_list <- c("Sunday", "Monday", "Tuesday",
                  "Wednesday", "Thursday", "Friday", "Saturday")
    names(day_list) <- day_list
    if (is.null(days)) days <- 1:7
    days <- day_list[days]
    
    meter_list <- meters(x)
    if (is.null(meters)) meters <- meter_list
    if (any(!meters %in% meter_list)) stop("At least one meter not in data.")
    
    setdate <- function(d, f = "%Y-%m-%d") strftime(d, f)
    date_range <- range(x$timestamp, na.rm = TRUE)
    if (is.null(start_date)) start_date <- setdate(date_range[1])
    if (is.null(end_date)) end_date <- setdate(date_range[2])
    
    start_date <- as.POSIXct(paste(start_date, "00:00:00"), tz = "America/Los_Angeles")
    end_date   <- as.POSIXct(paste(end_date,   "23:45:00"), tz = "America/Los_Angeles")
    
    ungroup(x %.%
        mutate (interval = as_interval(timestamp),
                weekday  = as_weekday(timestamp)) %.% 
        filter (meter %in% meters,
                weekday %in% days,
                timestamp %btw% c(start_date, end_date),
                interval %btw% c(start_time, end_time)) %.%
        group_by (timestamp, interval, weekday) %.%
        summarise(kW = measure(kW)) %.%
        arrange(timestamp))
}
