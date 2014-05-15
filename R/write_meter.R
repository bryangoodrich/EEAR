#' Write Copied IEE Data to CSV file
#' 
#' Write meter data from IEE copied onto windows clipboard into a CSV file.
#' 
#' @param meter The identifier (meter number) for the meter.
#'   Acts as the default file name when `filename` parameter unspecified.
#' @param filename The name of the file. Default is NULL. Will use `meter`
#'   to provide default file name.
#' @param filepath The path to where the file should be saved. Defaults to 
#'   the current working directly (".")
#' @param append Logical. Should the meter data be appended to the specified file.
#' @param header Logical. Does the data contain headers. Default is FALSE.
#' @param \dots All other parameters passed to read.delim, where clipboard content
#'   is read in and processed before writing out to CSV.
#' @export
write_meter <- function(meter, filename = NULL, filepath = ".", 
                        append = FALSE, header = FALSE, ...) {
    if (is.null(filename))
        filename <- paste(as.character(meter), "csv", sep = '.')
    
    x <- read.delim("clipboard", header = header, ...)
    x <- data.frame(meter, x)
    write.table(x, file.path(filepath, filename), sep = ",", na = "",
                row.names = FALSE, col.names = FALSE, append = append)
    
}
