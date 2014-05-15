#' @name transformations
#' @rdname transformations
#' 
#' @title Transform the kW to another measurement (kWh, EUI, per-degree)
#' @param x a table object.
NULL


#' @export
#' @rdname transformations
to_kwh <- function(x) {
    # Local binding for dplyr NSE
    kW <- NULL
    y <- x %.% mutate(kW = kwh(kW))
    if(has_sqft(x)) sqft(y) <- sqft(x)
    y
}

#' @export
#' @rdname transformations
to_eui <- function(x) {
    # Local binding for dplyr NSE
    kW <- NULL
    
    # precondition should be part of EUI
    if (is.null(sqft)) stop("The sqft has not been set for this data")
    y <- x %.% mutate(kW = eui(kW, sqft(x)))
    sqft(y) <- sqft(x)
    y
}

#' @export
#' @rdname transformations
to_degree <- function(x) {
    # Local binding for dplyr NSE
    kW <- temperature <- NULL
    
    y <- with_temperature(x) %.% mutate(kW = kW / temperature)
    if(has_sqft(x)) sqft(y) <- sqft(x)
    y
}
