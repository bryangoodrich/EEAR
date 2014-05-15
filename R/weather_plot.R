#' Plot Demand against Temperature
#' 
#' Generates scatter plots kW ~ temperature
#' 
#' @param x a data table
#' @param point The type of point to plot, daily averages or instance 
#'   intervals.
#' @param facets How to panel the plot; default "none" for entire data set.
#' @param seasonality Logical. Should points be coloured by season
#' @param legend Logical. Should the legend be included in plot.
#' @param title The title on the plot.
#' @param xlab The label for the x-axis. Default is "Time of Day".
#' @param ylab The label for the y-axis. Default is "kW".
#' @param plot Logical. Should the plot be drawn or
#' @return Invisibly returns the ggplot plotting object.
#' @export
#' @rdname weather_plot
weather_plot <- function(x, point = c("day", "interval"), 
                         facets = c("none", "weekday", "workweek", "quarter", "year"), 
                         seasonality = TRUE, legend = seasonality,
                         title = "", xlab = "Temperature (F)", ylab = "kW", plot = TRUE)
{
    point <- match.arg(point)
    facets <- match.arg(facets)
    colour <- NULL
    if (seasonality) colour <- "season"
    
    p <- ggplot(weather_prepare(x, point, facets, seasonality)) + 
        aes_string(x = "temperature", y = "kW", colour = colour) + 
        geom_point() + labs(x = xlab, y = ylab, title = title) +
        theme_bw()
    
    if (!legend) p <- p + theme(legend.position = "none")
    
    if (facets != "none") {
        fstr <- paste("~", paste(facets, collapse = " + "))
        p <- p + facet_wrap(as.formula(fstr))
    }
    
    if (plot) print(p)
    invisible(p)
}




#' @export
#' @rdname weather_plot
weather_prepare <- function(x, point, facets, seasonality) {
    # Local binding for dplyr NSE
    kW <- temperature <- NULL
    
    args <- alist(x = with_temperature(x))
    if (seasonality) args <- c(args, alist(season = as_season(timestamp)))
    if (facets != "none") {
        fargs <- switch(facets,
                        weekday = facets,
                        workweek = alist(as_schedule(timestamp)),
                        quarter  = alist(by_quarter(timestamp)),
                        year     = alist(by_year(timestamp))
        )
        names(fargs) <- facets
        args <- c(args, fargs)
    }
    
    args <- c(args, alist(day = by_day(timestamp)))
    if (point == "interval") 
        args <- c(args, alist(interval = quote(interval)))
    
    do.call(group_by, args) %.%
        summarise(kW = mean(kW), temperature = mean(temperature))
}
