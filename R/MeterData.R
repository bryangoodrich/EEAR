#' Example Meter Data Sets
#' 
#' Three datasets containing 15-minute commercial smart meter interval data
#'   from Sacramento area. The data sets include
#'   
#' \itemize{
#'   \item OfficeMeter. A commercial office space.
#'   \item SchoolMeter. A school campus.
#'   \item ManMeter. A manufacturer with early, tight usage.
#' }
#' 
#' Each example represents the intended data model for imported meter data.
#'   The variables are as follows:
#' 
#' \itemize{
#'   \item meter. The identifier of the meter reading.
#'   \item timestamp. The 15-minute time stamp, imported as GMT.
#'   \item kW. The monitored 15-minute demand over the meter. 
#' }
#' 
#' @docType data
#' @keywords datasets
#' @name MeterData
#' @rdname OfficeMeter
#' @rdname SchoolMeter
#' @rdname ManMeter
NULL