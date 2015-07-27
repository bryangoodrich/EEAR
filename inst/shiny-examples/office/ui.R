library(shiny)



# Custom Shiny Components
inlineSelectInput <- function(inputId, label, value = "", ...) {
    div(style = "display:inline-block",
        tags$label(label, `for` = inputId),
        tags$input(id = inputId, type = "text", value = value, ...))
}




# TIME <- c(
#     "00:00", "00:15", "00:30", "00:45", "01:00", "01:15", "01:30", "01:45",
#     "02:00", "02:15", "02:30", "02:45", "03:00", "03:15", "03:30", "03:45",
#     "04:00", "04:15", "04:30", "04:45", "05:00", "05:15", "05:30", "05:45",
#     "06:00", "06:15", "06:30", "06:45", "07:00", "07:15", "07:30", "07:45",
#     "08:00", "08:15", "08:30", "08:45", "09:00", "09:15", "09:30", "09:45", 
#     "10:00", "10:15", "10:30", "10:45", "11:00", "11:15", "11:30", "11:45", 
#     "12:00", "12:15", "12:30", "12:45", "13:00", "13:15", "13:30", "13:45",
#     "14:00", "14:15", "14:30", "14:45", "15:00", "15:15", "15:30", "15:45",
#     "16:00", "16:15", "16:30", "16:45", "17:00", "17:15", "17:30", "17:45",
#     "18:00", "18:15", "18:30", "18:45", "19:00", "19:15", "19:30", "19:45",
#     "20:00", "20:15", "20:30", "20:45", "21:00", "21:15", "21:30", "21:45",
#     "22:00", "22:15", "22:30", "22:45", "23:00", "23:15", "23:30", "23:45")

TIME <- structure(c("00:00", "00:15", "00:30", "00:45", "01:00", "01:15", 
"01:30", "01:45", "02:00", "02:15", "02:30", "02:45", "03:00", 
"03:15", "03:30", "03:45", "04:00", "04:15", "04:30", "04:45", 
"05:00", "05:15", "05:30", "05:45", "06:00", "06:15", "06:30", 
"06:45", "07:00", "07:15", "07:30", "07:45", "08:00", "08:15", 
"08:30", "08:45", "09:00", "09:15", "09:30", "09:45", "10:00", 
"10:15", "10:30", "10:45", "11:00", "11:15", "11:30", "11:45", 
"12:00", "12:15", "12:30", "12:45", "13:00", "13:15", "13:30", 
"13:45", "14:00", "14:15", "14:30", "14:45", "15:00", "15:15", 
"15:30", "15:45", "16:00", "16:15", "16:30", "16:45", "17:00", 
"17:15", "17:30", "17:45", "18:00", "18:15", "18:30", "18:45", 
"19:00", "19:15", "19:30", "19:45", "20:00", "20:15", "20:30", 
"20:45", "21:00", "21:15", "21:30", "21:45", "22:00", "22:15", 
"22:30", "22:45", "23:00", "23:15", "23:30", "23:45"), .Names = c("00:00 AM", 
"00:15 AM", "00:30 AM", "00:45 AM", "01:00 AM", "01:15 AM", "01:30 AM", 
"01:45 AM", "02:00 AM", "02:15 AM", "02:30 AM", "02:45 AM", "03:00 AM", 
"03:15 AM", "03:30 AM", "03:45 AM", "04:00 AM", "04:15 AM", "04:30 AM", 
"04:45 AM", "05:00 AM", "05:15 AM", "05:30 AM", "05:45 AM", "06:00 AM", 
"06:15 AM", "06:30 AM", "06:45 AM", "07:00 AM", "07:15 AM", "07:30 AM", 
"07:45 AM", "08:00 AM", "08:15 AM", "08:30 AM", "08:45 AM", "09:00 AM", 
"09:15 AM", "09:30 AM", "09:45 AM", "10:00 AM", "10:15 AM", "10:30 AM", 
"10:45 AM", "11:00 AM", "11:15 AM", "11:30 AM", "11:45 AM", "12:00 PM", 
"12:15 PM", "12:30 PM", "12:45 PM", "01:00 PM", "01:15 PM", "01:30 PM", 
"01:45 PM", "02:00 PM", "02:15 PM", "02:30 PM", "02:45 PM", "03:00 PM", 
"03:15 PM", "03:30 PM", "03:45 PM", "04:00 PM", "04:15 PM", "04:30 PM", 
"04:45 PM", "05:00 PM", "05:15 PM", "05:30 PM", "05:45 PM", "06:00 PM", 
"06:15 PM", "06:30 PM", "06:45 PM", "07:00 PM", "07:15 PM", "07:30 PM", 
"07:45 PM", "08:00 PM", "08:15 PM", "08:30 PM", "08:45 PM", "09:00 PM", 
"09:15 PM", "09:30 PM", "09:45 PM", "10:00 PM", "10:15 PM", "10:30 PM", 
"10:45 PM", "11:00 PM", "11:15 PM", "11:30 PM", "11:45 PM"))

WEEK <- list("Sun" = 1, "Mon" = 2, "Tue" = 3, "Wed" = 4,
          "Thu" = 5, "Fri" = 6, "Sat" = 7)



# Begin Shiny UI
shinyUI(pageWithSidebar(
headerPanel("Meter Data Analytics"),



#############################################
# Side Bar
#############################################
sidebarPanel(
    tags$head(
        tags$link(rel="stylesheet", type="text/css", href="override.css")
    ),

    
    conditionalPanel(
        condition = 'output.file_loaded',        

        
        # Prepare Data Well
        # Conditional on Loaded File
        wellPanel(
            tags$h3("Filters"),
            tags$strong("Available Meters"),
            uiOutput("meterControls"),
            
            tags$strong("Date Range"),
            uiOutput("dateRangeControls"),

            tags$strong("Time Range"),
            tags$br(),
            div(id = "time-controls",
                selectInput("start_time", "", choices = TIME, selected = "00:00"),
                tags$strong("  to "),
                selectInput("end_time", "", choices = TIME, selected = "23:45")
            ),
            
            tags$br(),
            
            tags$strong("Days of Week"),
            div(id = "inline-days", 
                checkboxInput("monday",    "Mon", value = TRUE),
                checkboxInput("tuesday",   "Tue", value = TRUE),
                checkboxInput("wednesday", "Wed", value = TRUE),
                checkboxInput("thursday",  "Thu", value = TRUE),
                checkboxInput("friday",    "Fri", value = TRUE),
                checkboxInput("saturday",  "Sat", value = TRUE),
                checkboxInput("sunday",    "Sun", value = TRUE)
            )
        ),
        
        
        conditionalPanel(
            condition = "input.conditionedPanels==1 || input.conditionedPanels==2",
            
            # Plotting Facets Well
            # Conditional on Loaded File and Selected Tabs
            wellPanel(
                checkboxGroupInput("facets", "Group by", 
                    choices = c("Weekday", "Workweek", "Week", "Month",
                                "Quarter", "Season", "Year"),
                    selected = NULL)
            )
        )
    )
),



#############################################
# Main Panel
#############################################
mainPanel(
    tabsetPanel(
        tabPanel("Building Summary",
            verbatimTextOutput("dataSummary"),
            dataTableOutput('buildingMetrics'),
            value = 1
        ),
        
        tabPanel("Load Profiles",
            div(id = "profile-controls", class = "inline-controls",
                selectInput("profile_line", "Line Type",
                            c("Weekday", "Workweek", "Day", "Month", "Season", "Year")),
                checkboxInput("profile_legend", "Legend", TRUE)
            ),
            textInput("profile_title", "Title", "Load Profile"),
            div(id = "profile-labels", class = "inline-controls",
                textInput("profile_xlab", "x-label", "Time of Day"),
                textInput("profile_ylab", "y-label", "Electricity Usage (kWh)")
            ),
            plotOutput("profilePlot"),
            value = 2
        ),
        
        tabPanel("Usage Trends",
            div(id = "trend-controls", class = "inline-controls",
                selectInput("trend_type", "Trend Type", c("Month", "Week", "Day"))),
            textInput("trend_title", "Title", "Usage Trend"),
            div(id = "trend-labels", class = "inline-controls",
                textInput("trend_xlab", "x-label", ""),
                textInput("trend_ylab", "y-label", "Electricity Usage (kWh)")
            ),
            plotOutput("trendPlot"),
            value = 3
        ),
        
        tabPanel("Demand vs Weather",
            div(id = "weather-controls", class = "inline-controls",
                selectInput("weather_type", "Points", c("Day", "Interval")),
                selectInput("weather_facets", "Facets",
                    c("None", "Weekday", "Workweek", "Quarter", "Year")),
                checkboxInput("weather_legend", "Legend", TRUE),
                checkboxInput("weather_seasonality", "Color by Season", TRUE)
            ),
            textInput("weather_title", "Title", "Demand vs. Temperature"),
            div(id = "weather-labels", class = "inline-controls",
                textInput("weather_xlab", "x-label", "Temperature (F)"),
                textInput("weather_ylab", "y-label", "Demand (kW)")
            ),
            plotOutput("weatherPlot"), 
            value = 4
        ),
        
        tabPanel("Heat Map (3D)",
            plotOutput("densityMap", width = "800px", height = "1600px"), 
            value = 5
        ),
        
        tabPanel("Calendar View",
            plotOutput("calendar"),
            value = 6
        ),
        
        tabPanel("M & V", 
            plotOutput("MV"), 
            value = 7
        ),
        
        id = "conditionedPanels"
    )
)



# Close PageWithSideBar and ShinyUI
))
