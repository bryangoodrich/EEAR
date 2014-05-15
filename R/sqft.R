#' Manage the SQFT of a customer data set
#' 
#' Functions for setting and extracting a data set sqft.
#' 
#' @param x a table object.
#' @param value the sqft value for this data set
#' @return Numeric value assigned to data; otherwise `NULL`.
#'   `has_sqft` returns a logical check whether this is `NULL`.
#' @export
#' @rdname sqft
sqft <- function(x) attr(x, 'sqft')

#' @export
#' @rdname sqft
"sqft<-" <- function(x, value) {
    stopifnot (length(value) == 1 && is.numeric(value))
    attr(x, "sqft") <- value
    x
}

#' @export
#' @rdname sqft
has_sqft <- function(x) !is.null(sqft(x))
