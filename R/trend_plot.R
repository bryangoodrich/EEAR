#' Plot Usage Trends
#' 
#' Generate plots of usage trends over time at various resolutions
#' 
#' @param x a data table
#' @param trend What level of resolution should be plotted. Default monthly.
#' @param title The title on the plot.
#' @param xlab The label for the x-axis. Default is "Time of Day".
#' @param ylab The label for the y-axis. Default is "kW".
#' @param plot Logical. Should the plot be drawn or
#' @return Invisibly returns the ggplot plotting object.
#' @export
#' @rdname trend_plot 
trend_plot <- function(x, trend = c("month", "week", "day"),
                       title = "", xlab = trend, ylab = "kWh", plot = TRUE)
{
    # Local binding for dplyr NSE
    kW <- ymin <- ymax <- NULL
    
    trend <- match.arg(trend)
    
    p <- ggplot(trend_prepare(x %.% mutate(kW = kwh(kW)), trend)) + 
        aes_string(x = trend, y = "kWh") + geom_point() +
        labs(x = xlab, y = ylab, title = title) +
        theme_bw()
    
    if (trend != "day") p <- p + geom_linerange(aes(ymax = ymax, ymin = ymin))
    
    if (plot) print(p)
    invisible(p)
}




#' @export
#' @rdname trend_plot
trend_prepare <- function(x, trend) {
    # Local binding for dplyr NSE
    kW <- NULL
    
    args <- switch(trend,
        month = alist(by_month(timestamp)),
        week  = alist(by_week(timestamp)),
        day   = alist(by_day(timestamp))
    )
    names(args) <- trend
    args <- c(alist(x=x), args)
    
    do.call(group_by, args) %.%
        summarise(kWh   = mean(kW),
                  ymin  = min(kW),
                  ymax  = max(kW))
}
