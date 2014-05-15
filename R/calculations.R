#' Various kW calculations
#' 
#' This collection transforms a vector of kW into a vector normalized by
#' some factor, such as load factor and the max consumption, the energy
#' use index and the square-footage of the structure, or kWh as kW normalized
#' by its proportion of an hour. 
#' 
#' @param x A numeric vector representing kW.
#' @param h A numeric value representing proportion of an hour represented.
#' @param sqft The square-footage of the represented structure.
#' @param ... additional arguments passed onto other functions.
#' @return A numeric vector representing a calculated value.
#' @rdname calculations
#' @export
loadfactor <- function(x) {
    mean(x) / max(x)
}

#' @rdname calculations
#' @export
kwh <- function(x, h=0.25) {
    x * h
}

#' @rdname calculations
#' @export
eui <- function(x, sqft) {
    kwh(x) / sqft
}

#' @rdname calculations
#' @export
eui2  <- function(x, sqft) {
    x / sqft
}

#' @rdname calculations
#' @export
mean2 <- function(x, ...) {
    mean(x, na.rm = TRUE, ...)
}
