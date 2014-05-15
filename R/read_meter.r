#' Read a meter data set
#' 
#' Imports a csv file containing a 3-tuple (meter id, time stamp, kW). 
#' Timestamps are expected to be provided as GMT (UTC) without DST.
#' 
#' @param file A path to a meter data file. Defaults to a GUI selection.
#' @param header Logical. Are there first row headers. (Default FALSE)
#' @param tz A timezone specificaton to be used for the conversion. Defaults
#'   to America/Los_Angeles.
#' @param \dots Further parameters passed to read.csv 
#' @return a data frame table object (dplyr)
#' @export
read_meter <- function(file = file.choose(), header = FALSE,
                       tz = "America/Los_Angeles", ...) {    
    FORMAT <- "%m/%d/%Y %H:%M"

    x <- read.csv(file, header = header, ...)
    stopifnot(ncol(x) == 3)
    
    names(x) <- c("meter", "timestamp", "kW")
    
    x$meter     <- factor(x$meter)
    x$kW        <- String2Numeric(x$kW)
    x$timestamp <- as.POSIXct(x$timestamp, format = FORMAT, tz='GMT')
    attr(x$timestamp, "tzone") <- tz
    tbl_df(x)
}
