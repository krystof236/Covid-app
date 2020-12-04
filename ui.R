library(shiny)
library(DT)
library(highcharter)
library(plotly)

navbarPage(
    title = "Coronavirus application",
    tabPanel(title = "Overview",
             sidebarLayout(
                 sidebarPanel(
                     uiOutput("country_filter"),
                     uiOutput("possible_vars_to_plot"),
                     uiOutput("multiple_vars_to_plot"),
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
             h3("Data visualization using leaflet package")),
    tabPanel(title = "Forecast",
             h3("Forecasting using forecast package")),
    tabPanel(title = "Correlation analysis"),
    tabPanel(title = "Codebook")
)