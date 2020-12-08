library(shiny)
library(DT)
library(highcharter)
library(plotly)
library(leaflet)

navbarPage(
    tabPanel(title = "Forecast",
             sidebarLayout(
                 sidebarPanel(
                     uiOutput("chosen_model_ui"),
                     textOutput("test"),
                     uiOutput("var_to_forecast_ui"),
                     sliderInput("n_to_predict", "Number of days to forecast", min = 1, max = 150, value = 30)
                 ),
                 mainPanel(
                     plotOutput("p_forecast")    
                 )
             )
    ), #tabPanel forecast
    title = "Coronavirus application",
    tabPanel(title = "Overview",
             sidebarLayout(
                 sidebarPanel(
                     uiOutput("country_filter"),
                     uiOutput("possible_vars_to_plot"),
                     radioButtons("scale_type", "Scale", choices = c("Linear" = "lin", "Logarithmic" = "log")),
                     uiOutput("multiple_vars_to_plot"),
                     uiOutput("max_date_info"),
                     tags$a("Source of data", href = "https://github.com/owid/covid-19-data/tree/master/public/data")
                 ),
                 mainPanel(
                     plotlyOutput("p_cases_pl"),
                     plotlyOutput("p_mult_pl"),
                     DTOutput("filtered_data_dt"),
                 )
             )
    ), #tabPanel overview
    tabPanel(title = "Grouped data",
             uiOutput("base_on_data_from"),
             highchartOutput("data_drill_hchart"),
             DTOutput("data_grp_continent_dt"),
             DTOutput("data_drilled_dt")
    ), #tabPanel grouped data
    tabPanel(title = "Map",
             leafletOutput("map", height = 800)
    ),
    tabPanel(title = "Correlation analysis"),
    tabPanel(title = "Codebook")
)