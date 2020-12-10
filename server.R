library(tidyverse) #for data
library(DT) #package for interactive tables
library(ggplot2) #package for graphs
library(highcharter) #package for interactive graphs
library(plotly) #package for interactive graphs
library(scales) #package for various units
library(leaflet) #package for interactive maps
library(geojsonio) #package for .geojson file format 
# library(rgdal) #needed for shapefile map format
library(forecast) #package for forecasting, will be deprecated in future in favor of fable
library(lubridate)

# data preparation --------------------------------------------------------
data <- rio::import("data_shinyapps/owid-covid-data.csv") #using static data, when publishing to shinyapps, this data file has to be uploaded too
# data <- rio::import("https://covid.ourworldindata.org/data/owid-covid-data.csv") #data will be downloaded from the internet

#json source https://github.com/datasets/geo-countries/blob/master/data/countries.geojson
countries_json <- geojson_read("data_shinyapps/countries.geojson", what = "sp") #when publishing to shinyapps

#shapefile map data, source http://thematicmapping.org/downloads/world_borders.php, requires library(rgdal)
# countries_shapefile <- readOGR(dsn = "data/TM_WORLD_BORDERS",
#                                layer = "TM_WORLD_BORDERS-0.3",
#                                verbose = FALSE)

data <- data %>% 
  mutate(date = as.Date(date))
max_date <- max(data$date)
min_date <- min(data$date)
avail_countries <- data %>% distinct(location) %>% arrange(location)

# adding computed variables -----------------------------------------------
my_trailing_mean <- function(x, n = 7){stats::filter(x, rep(1/n,n), sides = 1)}

data <- data %>%
  group_by(location) %>% 
  mutate(week_rel_new_inc_now = new_cases/lag(new_cases, order_by = date, n = 7),
         week_rel_new_inc_now = ifelse(is.infinite(week_rel_new_inc_now), NA, week_rel_new_inc_now),
         #week_rel_tot_inc_wago = lag(total_cases, order_by = date, n = 6)/lag(total_cases, order_by = date, n = 13),
         week_rel_new_inc_wago = lag(week_rel_new_inc_now, order_by = date, n = 6),
         week_rel_new_inc_wago = ifelse(is.infinite(week_rel_new_inc_wago), NA, week_rel_new_inc_wago),
         avg_week_new_cases = my_trailing_mean(new_cases),
         adjusted_weekly_increase = (new_cases/lag(new_cases, order_by = date, n = 7))^(3/2)*sqrt(lag(new_tests, order_by = date, n = 7)/new_tests),
         adjusted_weekly_increase = ifelse(is.infinite(adjusted_weekly_increase), NA, adjusted_weekly_increase)) %>% 
  ungroup() #could cause some issues further down if not ungrouped

# variables for a smaller dataset -----------------------------------------
vars_small <- c("iso_code",
                "continent",
                "location",
                "date",
                "total_cases",
                "new_cases",
                "total_deaths",
                "new_deaths",
                "total_cases_per_million",
                "new_cases_per_million",
                "total_deaths_per_million",
                "new_deaths_per_million",
                "reproduction_rate",
                "week_rel_new_inc_now",
                "week_rel_new_inc_wago",
                "avg_week_new_cases",
                "adjusted_weekly_increase")

data_small <- data %>%
  select(all_of(vars_small))


# possible variables to plot preparation ----------------------------------
possible_vars_to_plot <- tribble(
  ~label, ~value,
  "Total cases", "total_cases",
  "New cases", "new_cases",
  "Total deaths", "total_deaths",
  "New deaths", "new_deaths",
  "Reproduction rate", "reproduction_rate",
  "Weekly relative increase", "week_rel_new_inc_now",
  "Weekly relative increase 1 week ago", "week_rel_new_inc_wago",
  "7-day average of new cases", "avg_week_new_cases",
  "Adjusted weekly increase", "adjusted_weekly_increase")



# possible models for forecasting -----------------------------------------
possible_models <- tribble(
  ~label, ~value,
  "Naive", "naive",
  "Simple exponential smoothing", "ses",
  "Holt", "holt",
  "ARIMA", "auto.arima",
  "Exponential smoothing state space model", "ets",
  "TBATS", "tbats"
)

# server function definition ----------------------------------------------
function(input, output) {

# user inputs defined via server -------------------------------------------------------------
  output$country_filter <- renderUI({
    selectInput("country", "Country", choices = avail_countries$location, multiple = T, selected = "Czechia")
  })
  output$possible_vars_to_plot <- renderUI({
    selectInput("var_to_plot", "Variable to plot", choices = deframe(possible_vars_to_plot)) 
  })
  output$base_on_data_from <- renderUI({
    dateInput("data_from", "Show situation according to data from", value = max_date, min = "2020-01-01", max = max_date)
  })
  output$multiple_vars_to_plot <- renderUI({
    selectInput("mult_vars_to_plot", "Multiple variables to plot", choices = deframe(possible_vars_to_plot), selected = possible_vars_to_plot[1:2,]$value, multiple = T) 
  })
  output$var_to_forecast_ui <- renderUI({
    selectInput("var_to_forecast", "Variable to forecast", choices = deframe(possible_vars_to_plot))
  })
  output$chosen_model_ui <- renderUI({
    selectInput("chosen_model", "Model to use", choices = deframe(possible_models))
  })
  
# time series, using ggplot and plotly -------------------------------------------------------------
  filtered_data <- reactive({
    data %>%
      filter(location %in% input$country)
  })
  
  filtered_data_small <- reactive({
    filtered_data() %>% select(all_of(vars_small))
  })
  
  # filtered_data_small <- data_small %>% filter(location == "Czech Republic")
  p_cases <- reactive({
    var_label <- possible_vars_to_plot %>% filter(value == input$var_to_plot) %>% select(label) %>% as.character()
    temp <- ggplot(filtered_data_small(),
           aes(x = date, y = .data[[input$var_to_plot]], group = location, color = location,
               text = paste0(
                 var_label, ': ', .data[[input$var_to_plot]],
                 '<br> Country: ', location,
                 '<br> Date: ', date
               )
           )
    )+
      geom_line()+
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")+
      labs(x = "Date", y = var_label, title = paste0("Single variable - ", var_label," time series"), color = "Country")
    
    if (input$scale_type == "log") {temp + scale_y_log10(labels = comma)}
    else {temp+scale_y_continuous(labels = comma)}
  })
  output$p_cases_pl <- renderPlotly({
    req(input$country)
    ggplotly(p_cases(), tooltip = "text")
    })
  

# multi-variable plot ------------------------------------------------------
  p_mult <- reactive({
    req(input$mult_vars_to_plot)
    req(input$country)
    my_data <- isolate(filtered_data_small())
    vars <- isolate(input$mult_vars_to_plot)
    no_layers <- length(vars)
    
    graphs <- c()
    graphs[[1]] <- ggplot(my_data, aes(x = date, group = location, color = location))
    
    for (i in 1:no_layers) {
      var_label <- possible_vars_to_plot %>% filter(value == vars[[i]]) %>% select(label) %>% as.character()
      graphs[[i+1]] <- graphs[[i]]+geom_line(aes(y = .data[[vars[[i]]]],
                                                 text = paste0(
                                                   var_label, ': ', .data[[vars[[i]]]],
                                                   '<br> Country: ', location,
                                                   '<br> Date: ', date
                                                 )))
    }
    temp <- graphs[[no_layers+1]]+
      labs(title = "Multiple variables", x = "Date", y = "", color = "Country")+
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")
    
    if (input$scale_type == "log") {temp + scale_y_log10(labels = comma)}
    else {temp+scale_y_continuous(labels = comma)}
  })
  
  output$p_mult_pl <- renderPlotly({
    ggplotly(p_mult(), tooltip = "text")
  })
  
  output$filtered_data_dt <- renderDT(filtered_data_small())
# drillable elements, bar chart (highcharter) and table (datatable) ---------------------------------------
  data_small_now <- reactive({
    req(input$data_from)
    data_small %>%
      filter(date == input$data_from) %>% 
      mutate(TotalCasesFormatted = format(total_cases, big.mark = " "),
             NewCasesFormatted = format(new_cases, big.mark = " "),
             TotalDeathsFormatted = format(total_deaths, big.mark = " "),
             NewDeathsFormatted = format(new_deaths, big.mark = " ")
      )
  })
    
  data_grp_continent <- reactive({
    data_small_now() %>%
      filter(continent != "") %>% 
      group_by(continent) %>% 
      summarise(total_cases = sum(total_cases, na.rm = T),
                new_cases = sum(new_cases, na.rm = T),
                total_deaths = sum(total_deaths, na.rm = T),
                new_deaths = sum(new_deaths, na.rm = T)) %>%
      mutate(TotalCasesFormatted = format(total_cases, big.mark = " "),
             NewCasesFormatted = format(new_cases, big.mark = " "),
             TotalDeathsFormatted = format(total_deaths, big.mark = " "),
             NewDeathsFormatted = format(new_deaths, big.mark = " ")
      )
  })
  
  output$data_grp_continent_dt <- renderDT({
    datatable(data_grp_continent() %>% 
                select(continent, TotalCasesFormatted, NewCasesFormatted, TotalDeathsFormatted, NewDeathsFormatted) %>% 
                rename(Continent = continent,
                       `Total cases` = TotalCasesFormatted,
                       `New cases` = NewCasesFormatted,
                       `Total deaths` = TotalDeathsFormatted,
                       `New deaths` = NewDeathsFormatted),
              options = list(searching = F, paging = F,
                             columnDefs = list(list(className = 'dt-right', targets = "_all"))
              )
    )
    })
  
  data_drilled <- reactive({
    data_small_now() %>%
      filter(continent %in% data_grp_continent()[input$data_grp_continent_dt_rows_selected,]$continent)
  })
  
  output$data_drilled_dt <- renderDT({
    req(input$data_grp_continent_dt_rows_selected)
    datatable(data_drilled() %>% 
                select(continent, location, total_cases_per_million, new_cases_per_million, total_deaths_per_million, new_deaths_per_million,
                       TotalCasesFormatted, NewCasesFormatted, TotalDeathsFormatted, NewDeathsFormatted, reproduction_rate) %>% 
                rename(Continent = continent,
                       Country = location,
                       `Total cases` = TotalCasesFormatted,
                       `New cases` = NewCasesFormatted,
                       `Total deaths` = TotalDeathsFormatted,
                       `New deaths` = NewDeathsFormatted,
                       `Total cases per M` = total_cases_per_million,
                       `New cases per M` = new_cases_per_million,
                       `Total deaths per M` = total_deaths_per_million,
                       'New deaths per M' = new_deaths_per_million,
                       `Reproduction rate` = reproduction_rate),
              options = list(searching = F, paging = F,
                             columnDefs = list(list(className = 'dt-right', targets = "_all"))
              )
    )
  })
  
  country_drilldown <- reactive({
    data_small_now() %>%
      filter(continent != "") %>%
      group_nest(continent) %>% 
      mutate(
        id = continent,
        type = "column", #type of highcharter graph
        data = map(data, mutate, name = location, y = total_cases),
        data = map(data, list_parse) #turn data.frame of counties for each state to a list, needed for highcharter
      )
  })
  
  output$data_drill_hchart <- renderHighchart({
    tooltip_category_text <- c("Total cases: ")
    tooltip_formatted_values <- c("{point.TotalCasesFormatted}")
    my_tooltips <- tooltip_table(tooltip_category_text, tooltip_formatted_values)
    
    hchart(
      data_grp_continent(),
      "column",
      hcaes(x = continent, y = total_cases, name = continent, drilldown = continent),
      name = "Total cases",
      colorByPoint = TRUE
    ) %>% 
      hc_drilldown(
        allowPointDrilldown = TRUE,
        series = list_parse(country_drilldown())
      ) %>%  
      hc_tooltip(
        pointFormat = my_tooltips,
        useHTML = TRUE
      ) %>% 
      hc_yAxis(title = "") %>% 
      hc_xAxis(title = "")
  })

# map, leaflet ------------------------------------------------------------
  output$max_date_info <- renderText(paste("Data from ", max_date))
  
  data_small_now_map <- data_small %>%
      filter(date == max_date) %>% 
      select(iso_code, location, new_cases_per_million, total_cases_per_million)
  
  countries_map <- countries_json
  countries_map@data <- countries_json@data %>% 
    left_join(data_small_now_map, by = c("ISO_A3" = "iso_code"))

  #function to return color based on new_cases_per_million
  # my_palette <- colorNumeric(palette = "YlOrBr", domain = countries_map@data$new_cases_per_million, na.color = "transparent")
  my_bins <- c(0,20,50,100,300,500,1000,Inf)
  my_palette <- colorBin(palette = "YlOrBr", domain = countries_map@data$new_cases_per_million, na.color = "transparent", bins = my_bins)
  
  my_text <- paste(
      "Country: ", countries_map@data$location,
      "<br> New cases per M: ", countries_map@data$new_cases_per_million,
      "<br> Total cases per M: ", countries_map@data$total_cases_per_million
    ) %>% lapply(htmltools::HTML)
  
  output$map <- renderLeaflet({
    leaflet(countries_map) %>% 
      addTiles() %>% 
      setView(lat = 50, lng = 15, zoom = 3) %>% 
      addPolygons(fillColor = ~my_palette(new_cases_per_million), stroke = F, label = my_text) %>% 
      addLegend(pal = my_palette, values = ~new_cases_per_million, title = "New cases per M", position = "bottomleft")
  })
    
# forecasting -------------------------------------------------------------
  n_frequency <- 365
  
  data_cz <- data_small %>% filter(location == "Czechia")
  
  data_ts <- reactive({
    min_avail_date_var_tbl <- data_cz %>%
      select(date, var_to_forecast) %>% 
      filter(!is.na(.data[[input$var_to_forecast]])) %>% 
      summarize(first(date))
    min_avail_date_var <- as.Date(min_avail_date_tbl[[1]])
    
    data_for_ts <- data_cz %>% filter(date >= min_avail_date_var) %>% select(input$var_to_forecast)
    
    ts(data_for_ts, start = decimal_date(min_avail_date_var), frequency = n_frequency)
  })
  
  model <- reactive({
    req(input$chosen_model)
    if (input$chosen_model %in% c('naive', 'ses', 'holt')) {lapply(data_ts(), input$chosen_model, h = input$n_to_predict)[[1]]} #needs parameter h to be trained
    else {lapply(data_ts(), input$chosen_model)[[1]]}
  })
  
  my_date_trans <- function(x) {date(date_decimal(x))}
  output$p_forecast <- renderPlot({
    req(input$var_to_forecast)
    var_label <- possible_vars_to_plot %>% filter(value == input$var_to_forecast) %>% select(label) %>% as.character()
    autoplot(forecast(model(), h = input$n_to_predict))+
      labs(x = "Date", y = var_label)+
      scale_y_continuous(labels = comma)+
      scale_x_continuous(labels = my_date_trans)
  })

  #visualise using ggplot via forecast's function autoplot, ggplotly doesn't handle it though as autoplot uses it's own geom
}
