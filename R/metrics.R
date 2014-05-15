#' Calculates a Table of Metrics
#' 
#' A general function for producing tabular calculations according to given
#'   facets and provided metrics. 
#' 
#' @param x a data table
#' @param group By what groupings are these calculations to be based on.
#' @param metrics Vector of functions for which calculations will be generated.
#' @param trim The trim value for trimmed mean calculations
#' @param type The type of percentile to be calculated. See quantile for details.
#' @param na.rm Logical. Should NA values be removed before calculation. Default TRUE.
#' @return A data table
#' @export
metrics <- function(x, group = NULL,
                    metrics = NULL, 
                    trim = 0.1, type = 7, na.rm = TRUE) {
    # Local binding for dplyr NSE
    kW <- kWh <- NULL
    
    
    expr <- c("weekday"  = alist(as_weekday(timestamp)),
              "workweek" = alist(as_schedule(timestamp)),
              "week"     = alist(by_week(timestamp)),
              "month"    = alist(by_month(timestamp)),
              "quarter"  = alist(by_quarter(timestamp)),
              "season"   = alist(as_season(timestamp)),
              "year"     = alist(by_year(timestamp))
    )

    args <- alist(x=mutate(x, kWh = kwh(kW)))
    N <- length(group)
    if (N > 0) {
        for (g in group)
            args <- c(args, expr[[g]])
        names(args)[2:(1+N)] <- group
    }
    
    do.call(group_by, args) %.%
    summarise(
        "Count" = n()
        ,"Total kWh" = sum(kWh, na.rm = na.rm)
        ,"Min kWh" = min(kWh, na.rm = na.rm)
        ,"Max kWh" = max(kWh, na.rm = na.rm)
        ,"Mean kWh" = mean(kWh, na.rm = na.rm)
        ,"Median kWh" = median(kWh, na.rm = na.rm)
        ,"Trimmed Mean" = mean(kWh, trim = trim, na.rm = na.rm)
        ,"Std Dev" = sd(kWh, na.rm = na.rm)
        ,"Q25" = Q25(kWh, na.rm = na.rm, type = type)
        ,"Q75" = Q75(kWh, na.rm = na.rm, type = type)
        ,"Load Factor" = loadfactor(kW)
        ,"Peak kW" = max(kW)
    )
}




####### Helper Functions for specific measures ############

trim <- function(x, trim = 0.1, na.rm = TRUE) {
    mean(x, trim = trim, na.rm = na.rm)
}

Q <- function(q) {
    function(x, na.rm = TRUE, type = 7) 
        quantile(as.numeric(x), q, na.rm = na.rm, type = type, names = FALSE)
}
Q25 <- Q(0.25)
Q75 <- Q(0.75)
###########################################################

