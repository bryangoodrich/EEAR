#' Plot Usage Profile Lines
#' 
#' Generates profile plots, averaged for the given line period, paneled for
#' the given facets.
#' 
#' @param x a data table
#' @param line The type of line to be drawn. Default day of week. Possible 
#'   values include every `day`, `month`, `season`, and `year`.
#' @param facets The type of facets to panel the profiles by. The avaiable 
#'   facets depend upon which line is chosen. Day profiles can be plotted for each
#'   week, month, quarter, season, or year. Weekday profiles for all but week.
#'   Month for quarter, season, or year. Season by year. Year takes no facets.
#' @param legend Logical. Should the legend be included in plot.
#' @param title The title on the plot.
#' @param xlab The label for the x-axis. Default is "Time of Day".
#' @param ylab The label for the y-axis. Default is "kW".
#' @param plot Logical. Should the plot be drawn.
#' @param \dots Further arguments passed to data preparation.
#' @return Invisibly returns the ggplot plotting object.
#' @export
#' @rdname profile_plot
profile_plot <- function(x, line = c("weekday", "workweek", "day", "month", "season", "year"), 
                         facets = NULL, legend = TRUE,
                         title = "", xlab = "Time of Day", ylab = "kWh",
                         plot = TRUE, ...)
{
    # Local binding for dplyr NSE
    kW <- NULL
    
    line <- match.arg(line)
    colour <- NULL
    if (line != "day") colour <- line
    
    p <- ggplot(profile_prepare(x %.% mutate(kW = kwh(kW)), line, facets, ...)) +
        aes_string(x = "interval", y = "kW", colour = colour, group = line) + 
        geom_line() + labs(x = xlab, y = ylab, title = title)
    
    if (!legend) p <- p + theme(legend.position = "none")
    
    
    if (!is.null(facets)) {
        fstr <- paste("~", paste(facets, collapse = " + "))
        p <- p + facet_wrap(as.formula(fstr))
    }
    
    if (plot) print(p)
    invisible(p)
}

#' @export
#' @rdname profile_plot
profile_prepare <- function(x, line, facets, ...) {
    # Local binding for dplyr NSE
    kW <- NULL
    
    contains_facets <- function(facet) {
        function(hierarchy) {
            all(facet %in% names(hierarchy))
        }
    }
    validate_facet <- contains_facets(facets)
    
    time_hierarchy <- c("weekday" = as_weekday, "workweek" = as_schedule,
                        "week"    = by_week,    "month"    = by_month, 
                        "quarter" = by_quarter, "season"   = as_season,
                        "year"    = by_year)
    
    isValidated <- switch(line,
        day      = validate_facet(time_hierarchy[1:7]),
        weekday  = validate_facet(time_hierarchy[4:7]),
        workweek = validate_facet(time_hierarchy[4:7]),
        month    = validate_facet(time_hierarchy[5:7]),
        season   = validate_facet(time_hierarchy[7]),
        year     = is.null(facets)
    )
    
    stopifnot(isValidated)
    
    args <- switch(line,
                   weekday  = line,
                   workweek = alist(as_schedule(timestamp)),
                   season   = alist(as_season(timestamp)),
                   month    = alist(by_month(timestamp)),
                   year     = alist(by_year(timestamp)),
                   day      = alist(by_day(timestamp))
    )
    
    names(args) <- line
    
    args <- c(alist(x=x), args)
    for (t in facets) {
        tmp <- eval(substitute(alist(f(timestamp)), list(f = time_hierarchy[[t]])))
        names(tmp) <- t
        args <- c(args, tmp)
    }
    
    args <- c(args, alist(interval = quote(interval)))
    
    do.call(group_by, args) %.%
        summarise(kW = mean(kW))
}
