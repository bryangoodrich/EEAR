#' @name weather
#' @rdname weather
#' 
#' @title A suite of commands for managing weather data and meters.
#' 
#' @param x a table object.
#' @param name The name of the temperature object stored. Default "temperatures"
#' @param FUN a function with first argument a temperature table
#' @param h A temperature handle
#' @param t a temperature table
#' @param interval a timestamp for meter intervals.
#' @param \dots arguments passed onto further functions.
NULL

#' @export
#' @rdname weather
set_temperature_handle <- function(x, name = "temperatures") {
    e <- new.env()
    assign(name, x, envir=e)
    options("temperature_handle" = list(obj = name, env = e))
    invisible()
}

#' @export
#' @rdname weather
get_temperature_handle <- function() {
    h <- getOption("temperature_handle")
    if (is.null(h)) stop("Temperature handle not set.")
    h
}

#' @export
#' @rdname weather
with_handle <- function(FUN, ..., h = get_temperature_handle()) {
    stopifnot(h$obj == "temperatures")
    h <- get(h$obj, h$env)
    FUN(h, ...)
}

#' @export
#' @rdname weather
temperature_join <- function(t, interval) {
    t$temperature[match(interval, t$timestamp)]
}

#' @export
#' @rdname weather
tj <- temperature_join

#' @export
#' @rdname weather
with_temperature <- function(x, ...) {
    y <- mutate(x, temperature = with_handle(tj, timestamp), ...)
    if(!is.null(sqft(x))) sqft(y) <- sqft(x)
    y
}


