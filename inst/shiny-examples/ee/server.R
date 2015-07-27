library(shiny)
library(EEAR)
library(RColorBrewer)



# Set Global Options and Variables
msg <- "Requires Input File"
options(shiny.maxRequestSize=60*1024^2)



# Begin Server
shinyServer (function(input, output) {
    
    # Raw Data Input
    original <- reactive({
        if (is.null(input$input_file)) return(NULL)
        
        inFile <- input$input_file
        read_meter(inFile$datapath, header = input$header)  
    })
    
    # Prepare Data Import for Processing
    prepared <- reactive({
        if (is.null(input$input_file)) return(NULL)
        
        days <- 1:7
        days <- days[c(input$sunday, input$monday, input$tuesday,
                       input$wednesday, input$thursday, input$friday,
                       input$saturday)]
        
        prepare(original(),
            totalized   = !input$averaged,
            meters      = input$meters,
            days        = days,
            start_date  = input$dates[1],
            end_date    = input$dates[2],
            start_time  = input$start_time,
            end_time    = input$end_time)
    })
    
    
    
    # Manage UI controls -- Post File Input
    # 1. Prepare data only once data is loaded
    # 2. Change prepare options to fit data
    #   - list meters
    #   - set start and end dates (TODO)
    output$file_loaded <- reactive({!is.null(input$input_file)})
    outputOptions(output, "file_loaded", suspendWhenHidden = FALSE)
    
    output$meterControls <- renderUI({
        meter_list <- levels(original()$meter)
        checkboxGroupInput("meters", "", choices = meter_list, selected = meter_list)
    })
    
    output$dateRangeControls <- renderUI ({
        date_range <- substr(as.character(range(original()$timestamp)), 1, 10)
        dateRangeInput("dates", "", start = date_range[1], end = date_range[2])
    })
    
    
    # Content of Data
#     output$contents <- renderTable ({
#         if (is.null(input$input_file))
#             return(NULL)
#             
#         head(prepared(), 6)
#     })
  
    # Building Summary Tab
    output$dataSummary <- renderPrint ({
        if (is.null(input$input_file)) return(msg)
        
        summary(prepared())
    })
    
    output$buildingMetrics <- renderDataTable ({
        if (is.null(input$input_file))
            return(NULL)
        metrics(prepared(),
                group = tolower(input$facets))
    })
    
    
    
    # Load Profiles Tab
    output$profilePlot <- renderPlot ({
        if (is.null(input$input_file))
            return(NULL)
        
        if (is.null(input$facets)) {
            facets <- NULL
        } else {
            facets <- tolower(input$facets)
        }
        p <- profile_plot(prepared(), 
                          line = tolower(input$profile_line), 
                          facets = facets,
                          legend = input$profile_legend,
                          title = input$profile_title,
                          xlab = input$profile_xlab,
                          ylab = input$profile_ylab,
                          plot = FALSE)
        print(p)
    })
    
    
    
    # Usage Trends Tab
    output$trendPlot <- renderPlot ({
        if (is.null(input$input_file))
            return(NULL)
        
        p <- trend_plot(prepared(),
                        trend = tolower(input$trend_type),
                        title = input$trend_title,
                        xlab = input$trend_xlab,
                        ylab = input$trend_ylab,
                        plot = FALSE)
        print(p)
    })
    
    
    
    # Demand vs. Weather Tab
    output$weatherPlot <- renderPlot ({
        if (is.null(input$input_file))
            return(NULL)
        
        p <- weather_plot(prepared(),
                          point = tolower(input$weather_type),
                          facets = tolower(input$weather_facets),
                          seasonality = input$weather_seasonality,
                          legend = input$weather_legend,
                          title = input$weather_title,
                          xlab = input$weather_xlab,
                          ylab = input$weather_ylab,
                          plot = FALSE)
        print(p)
    })
    
    
    
    # Density Map Tab
    output$densityMap <- renderPlot ({
        pal <- brewer.pal(11, "Spectral")
        V <- seq(0, 1, len = 12)
        #V <- c(0.0, 0.05, 0.1, 0.2, 0.4, 0.6, 0.7, 0.8, 0.9, 1.0)
        
        p <-
            ggplot(prepared() %.%
                       mutate(kWh = kwh(kW), day = by_day(timestamp))) +
            aes(interval, day, fill = kWh) + geom_tile() +
            scale_fill_gradientn(colours = rev(pal), values = V) +
            theme_bw()
        print(p)
    })
    
    # Calendar View Tab
    output$calendar <- renderPlot ({
        p <-
            ggplot(prepared() %.%
                    filter(strftime(timestamp, '%Y-%m') == '2013-03') %.%
                    mutate(kWh  = kwh(kW), 
                           day  = by_day(timestamp), 
                           week = by_week(timestamp)) %.%
                    arrange(timestamp)) + theme_bw() + 
            theme(panel.grid.major.x = element_blank()) + 
            aes(interval, kWh, group = day) + geom_step() + 
            facet_grid(week ~ weekday) +
            labs(title = "March, 2013")
        print(p)
    })
    
    
    
    # M & V Tab
    output$MV <- renderPlot ({
        return(NULL)
    })
})


