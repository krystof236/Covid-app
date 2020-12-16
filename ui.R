library(shiny)
library(DT)
library(highcharter)
library(plotly)
library(leaflet)

navbarPage(
    title = "Coronavirus application",
    id = "panels",
    tabPanel(title = "Overview",
             sidebarLayout(
                 sidebarPanel(
                     uiOutput("country_filter"),
                     uiOutput("possible_vars_to_plot"),
                     uiOutput("date_range_ui"),
                     radioButtons("scale_type", "Scale", choices = c("Linear" = "lin", "Logarithmic" = "log")),
                     checkboxInput("ref_line", "Add a reference line", value = F),
                     conditionalPanel(
                         condition = "input.ref_line == true",
                         numericInput("ref_line_value", "Value of the reference line", value = 1)
                     ),
                     uiOutput("multiple_vars_to_plot"),
                     uiOutput("max_date_info"),
                     tags$a("Source of majority of data", href = "https://github.com/owid/covid-19-data/tree/master/public/data"),
                     br(),
                     tags$a("Source of data about recovered cases", href = "https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series"),
                     h5("For details please see the ", actionLink("link_to_codebook", "Codebook"), " tab")
                 ),
                 mainPanel(
                     plotlyOutput("p_cases_pl"),
                     plotlyOutput("p_mult_pl"),
                     DTOutput("filtered_data_dt"),
                 )
             )
    ), #tabPanel overview
    tabPanel(title = "Total cases break-up",
             uiOutput("country_tcbreakup_ui"),
             plotlyOutput("p_tcbreakup_pl")
    ), #tabPanel total cases break-up
    tabPanel(title = "Grouped data",
             uiOutput("base_on_data_from"),
             highchartOutput("data_drill_hchart"),
             DTOutput("data_grp_continent_dt"),
             DTOutput("data_drilled_dt")
    ), #tabPanel grouped data
    tabPanel(title = "Map",
             leafletOutput("map", height = 800)
    ),
    tabPanel(title = "Forecast",
             sidebarLayout(
                 sidebarPanel(
                     uiOutput("country_forecast_ui"),
                     uiOutput("chosen_model_ui"),
                     uiOutput("var_to_forecast_ui"),
                     sliderInput("n_to_predict", "Number of days to forecast", min = 1, max = 150, value = 30),
                     radioButtons("scale_type_forecast", "Scale", choices = c("Linear" = "lin", "Logarithmic" = "log")),
                 ),
                 mainPanel(
                     plotOutput("p_forecast")    
                 )
             )
    ), #tabPanel forecast
    tabPanel(title = "Czech republic detail",
             sidebarLayout(
                 sidebarPanel(
                     radioButtons("level", "Detail", choices = c("Kraj" = "kraj", "Okres" = "okres"), inline = T),
                     uiOutput("cz_kraj_level_ui"),
                     uiOutput("cz_okres_level_ui"),
                     conditionalPanel(condition = "input.level == 'okres'",
                                      actionButton("fill_okres", "Vypln okresy dle kraju")),
                     actionButton("apply_filters_cz_detail", "Apply"),
                     uiOutput("base_cz_detail_on_data_from_ui"),
                     uiOutput("max_date_cz_notice"),
                     tags$a("Datovy zdroj", href = "https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19")
                 ),
                 mainPanel(
                     highchartOutput("bar_cz_detail"),
                     plotlyOutput("p_cz_detail_pl")
                 )
             ),
    ), #tabPanel Czech republic detail
    tabPanel(title = "Correlation analysis",
             plotOutput("corr_graph"),
             textOutput("max_date_temp_notice"),
             tags$a("Source of temperature data", href = "http://www.meteo.jankovic.cz/")
    ),
    tabPanel(title = "Codebook",
             DTOutput("codebook"))
)
