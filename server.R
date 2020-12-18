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

# data import --------------------------------------------------------
deploying_to_shinyapps <- FALSE
#files to publish: countries.geojson, avg_temperatures_cz.csv, owid-covid-codebook.csv, CIS0109_cs.csv, cz_nuts_systematicka_cast.xlsx

#basic data
data <- if (deploying_to_shinyapps) {rio::import("https://covid.ourworldindata.org/data/owid-covid-data.csv")} else {rio::import("data/owid-covid-data.csv")}

#map data
#json source https://github.com/datasets/geo-countries/blob/master/data/countries.geojson
countries_json <- geojson_read("countries.geojson", what = "sp")

#shapefile map data, source http://thematicmapping.org/downloads/world_borders.php, requires library(rgdal)
# countries_shapefile <- readOGR(dsn = "data/TM_WORLD_BORDERS",
#                                layer = "TM_WORLD_BORDERS-0.3",
#                                verbose = FALSE)

#data about recovered patients
data_recovered_raw <- if (deploying_to_shinyapps) {rio::import("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")} else {rio::import("data/time_series_covid19_recovered_global.csv")}

#codebook
codebook <- rio::import("owid-covid-codebook.csv")

#average temperatures in Czech republic, Brno
avg_temperatures_cz <- rio::import("avg_temperatures_cz.csv")

#data for Czech republic by area
kraj_okres_cz <- if (deploying_to_shinyapps) {rio::import("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/kraj-okres-nakazeni-vyleceni-umrti.csv")} else {rio::import("data/kraj-okres-nakazeni-vyleceni-umrti.csv")}
okres_cis <- read_csv("CIS0109_CS.csv", locale = locale(encoding = "windows-1250")) %>%
  select(CHODNOTA, TEXT) %>% 
  rename(okres_lau_kod = CHODNOTA, okres_nazev = TEXT)
kraj_cis <- rio::import("cz_nuts_systematicka_cast.xlsx", which = 6) %>%
  select(Kód, 'NUTS 3') %>% 
  rename(kraj_nazev = 'NUTS 3', kraj_nuts_kod = Kód) %>% 
  filter(!is.na(kraj_nazev), !is.na(kraj_nuts_kod))

# data preparation --------------------------------------------------------
data <- data %>% 
  mutate(date = as.Date(date))
max_date <- max(data$date)
min_date <- min(data$date)
avail_countries <- data %>% distinct(location) %>% arrange(location)

#converting data_recovered_raw into a long format and data manipulation
to_pivot <- colnames(data_recovered_raw)[5:ncol(data_recovered_raw)]
data_recovered <- data_recovered_raw %>% 
  select(-Lat, -Long) %>% 
  pivot_longer(cols = all_of(to_pivot), names_to = "date", values_to = "total_recovered") %>% 
  mutate(date = as.Date(date, format = "%m/%d/%y")) %>% 
  rename(location = `Country/Region`) %>% 
  group_by(location, date) %>% 
  summarize(total_recovered = sum(total_recovered)) %>% 
  ungroup()

data_recovered_world <- data_recovered %>% 
  group_by(date) %>% 
  summarise(total_recovered = sum(total_recovered)) %>% 
  mutate(location = "World") %>% 
  ungroup()

data_recovered <- rbind(data_recovered, data_recovered_world)
  
#names of locations corrections
data_recovered[data_recovered$location == "US",]$location <- "United States"
data_recovered[data_recovered$location == "Cabo Verde",]$location <- "Cape Verde"
data_recovered[data_recovered$location == "Korea, South",]$location <- "South Korea"
data_recovered[data_recovered$location == "Taiwan*",]$location <- "Taiwan"

#adding total_recovered to main data
data <- data %>% 
  left_join(data_recovered, by = c("location", "date"))

avg_temperatures_cz$date <- as.Date(avg_temperatures_cz$date)

kraj_okres_cz$datum <- as.Date(kraj_okres_cz$datum)
kraj_okres_cz <- kraj_okres_cz %>% 
  left_join(kraj_cis, by = c("kraj_nuts_kod")) %>% 
  left_join(okres_cis, by = c("okres_lau_kod"))

max_date_cz <- max(kraj_okres_cz$datum)
min_date_cz <- min(kraj_okres_cz$datum)

# adding computed variables -----------------------------------------------
my_trailing_mean <- function(x, n = 7){stats::filter(x, rep(1/n,n), sides = 1)}

data <- data %>%
  group_by(location) %>% 
  mutate(avg_week_new_cases = my_trailing_mean(new_cases), #defaultne v datech jako _smoothed
         avg_week_new_tests = my_trailing_mean(new_tests), #defaultne v datech jako _smoothed
         week_rel_new_inc = avg_week_new_cases/lag(avg_week_new_cases, order_by = date, n = 7),
         week_rel_new_inc = ifelse(is.infinite(week_rel_new_inc), NA, week_rel_new_inc),
         adjusted_weekly_increase = (avg_week_new_cases/lag(avg_week_new_cases, order_by = date, n = 7))^(3/2)*sqrt(lag(avg_week_new_tests, order_by = date, n = 7)/avg_week_new_tests),
         adjusted_weekly_increase = ifelse(is.infinite(adjusted_weekly_increase), NA, adjusted_weekly_increase),
         positive_tests_ratio = new_cases/new_tests,
         active_cases = total_cases-total_recovered-total_deaths,
         hosp_patients_ratio = hosp_patients/active_cases,
         reproduction_rate_est = avg_week_new_cases/lag(avg_week_new_cases, order_by = date, n = 5),
         avg_week_new_cases_per_M = my_trailing_mean(new_cases_per_million)) %>% 
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
                "avg_week_new_cases",
                "avg_week_new_cases_per_M",
                "avg_week_new_tests",
                "week_rel_new_inc",
                "adjusted_weekly_increase",
                "positive_tests_ratio",
                "positive_rate",
                "active_cases",
                "total_recovered",
                "hosp_patients",
                "hosp_patients_ratio",
                "reproduction_rate_est")

data_small <- data %>%
  select(all_of(vars_small))

# possible variables to plot preparation ----------------------------------
possible_vars_to_plot <- tribble(
  ~label, ~value,
  "Total cases", "total_cases",
  "New cases", "new_cases",
  "Total deaths", "total_deaths",
  "New deaths", "new_deaths",
  "Total cases per M", "total_cases_per_million",
  "New cases per M", "new_cases_per_million",
  "Total deaths per M", "total_deaths_per_million",
  "New deaths per M", "new_deaths_per_million",
  "Reproduction rate", "reproduction_rate",
  "Reproduction rate (estimate)", "reproduction_rate_est",
  "Weekly relative increase", "week_rel_new_inc",
  "7-day average of new cases", "avg_week_new_cases",
  "7-day average of new cases per M", "avg_week_new_cases_per_M",
  "7-day average of new tests", "avg_week_new_tests",
  "Adjusted weekly increase", "adjusted_weekly_increase",
  "Positive tests ratio (computed)", "positive_tests_ratio",
  "Positive tests ratio (reported)", "positive_rate",
  "Active cases", "active_cases",
  "Total recovered", "total_recovered",
  "Hospitalized patients", "hosp_patients",
  "Hospitalized patients ratio", "hosp_patients_ratio")

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

# codebook preparation ----------------------------------------------------
#corrections
codebook[1,3] <- "ISO 3166-1 alpha-3 - three-letter country codes"

#adding further variables to the codebook
my_notice <- "Computed in this application"
added_variables <- tribble(
  ~column, ~source, ~description,
 "total_recovered",
 "Center for Systems Science and Engineering (CSSE) at John Hopkins University, estimates based on local media reports",
 "Total number of recovered cases",
 "avg_week_new_cases", my_notice, "Average of new cases during last 7 days",
 "avg_week_new_tests", my_notice, "Average of new tests during last 7 days",
 "week_rel_increase", my_notice, "Ratio of avg_week_new_cases today and week ago",
 "adjusted_weekly_increase", my_notice,
 "Our metric for the state of the pandemic taking volume of testing into account, (P2/P1)^(3/2)*(T1/T2)^(1/2),
 P2 avg_week_new_cases, P1 avg_week_new_cases week ago, T2 avg_week_new_tests, T1 avg_week_new_tests week ago",
 "positive_tests_ratio", my_notice, "Ratio of new cases and new tests",
 "active_cases", my_notice, "Currently active cases, computed as total_cases-total_recovered-total_deaths",
 "hosp_patients_ratio", my_notice, "Ratio of hospitalized patients and active cases",
 "reproduction_rate_est", my_notice, "An estimate of the reproduction number R, calculated as ratio of avg_week_new_cases and avg_week_new_cases 5 days ago"
)

codebook <- rbind(codebook, added_variables)

# SERVER FUNCTION DEFINITION----------------------------------------------
function(input, output, session) {

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
    selectInput("chosen_model", "Model to use", choices = deframe(possible_models), selected = "auto.arima")
  })
  output$date_range_ui <- renderUI({
    dateRangeInput("date_range", "Date range", start = min_date, end = max_date, min = min_date, max = max_date)
  })
  output$country_forecast_ui <- renderUI({
    selectInput("country_forecast", "Country", choices = avail_countries$location, selected = "Czechia")
  })
  output$country_tcbreakup_ui <- renderUI({
    selectInput("country_tcbreakup", "Country", choices = avail_countries$location, selected = "Czechia", multiple = T)
  })
  output$cz_kraj_level_ui <- renderUI({
    selectInput("kraj_filter", "Kraj", deframe(kraj_cis %>% select(kraj_nazev, kraj_nuts_kod)), selected = "CZ010", multiple = T)
  })
  output$cz_okres_level_ui <- renderUI({
    req(input$level == "okres")
    selectInput("okres_filter", "Okres", deframe(okres_cis %>% select(okres_nazev, okres_lau_kod)), multiple = T)
  })
  output$base_cz_detail_on_data_from_ui <- renderUI({
    dateInput("base_cz_detail_on_data_from", "Datum, ke kteremu vizualizovat barchart", value = max_date_cz, max = max_date_cz, min = min_date_cz)
  })
  
# navigation in the app ---------------------------------------------------
observeEvent(input$link_to_codebook, {
  updateNavbarPage(session, "panels", "Codebook")
})
  
# time series, using ggplot and plotly -------------------------------------------------------------
  filtered_data <- reactive({
    req(input$country)
    req(input$date_range)
    data %>% filter(location %in% input$country,
                    date >= input$date_range[[1]],
                    date <= input$date_range[[2]])
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
    
    temp <- if (input$scale_type == "log") {temp + scale_y_log10(labels = comma)}
    else {temp+scale_y_continuous(labels = comma)}
    
    if (input$ref_line) {temp + geom_hline(yintercept = input$ref_line_value)}
    else {temp}
  }) #p_cases
  output$p_cases_pl <- renderPlotly({
    req(input$country)
    ggplotly(p_cases(), tooltip = "text")
    })
  

# multi-variable plot ------------------------------------------------------
  p_mult <- reactive({
    req(input$mult_vars_to_plot)
    req(input$country)
    req(input$date_range)
    my_data <- isolate(filtered_data_small())
    vars <- input$mult_vars_to_plot
    no_layers <- length(vars)
    
    graphs <- c()
    graphs[[1]] <- ggplot(my_data, aes(x = date, group = location, color = location))
    
    for (i in 1:no_layers) {
      var_label <- possible_vars_to_plot %>% filter(value == vars[[i]]) %>% select(label) %>% as.character()
      graphs[[i+1]] <- graphs[[i]]+geom_line(aes(y = .data[[vars[[i]]]],
                                                 text = paste0(
                                                   !!var_label, ': ', .data[[vars[[i]]]], #this !! corrects the issue with tooltips with same var_label
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
  output$max_date_info <- renderText(paste("Data from ", max_date-1)) #because with max_date there is sometimes bad data, TODO date filter for the map
  
  data_small_now_map <- data_small %>%
    filter(date == max_date-1) %>% 
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
  
  data_forecast <- reactive({
    data_small%>%
      filter(location == input$country_forecast)
  })
  
  data_ts <- reactive({
    min_avail_date_var_tbl <- data_forecast() %>%
      select(date, input$var_to_forecast) %>% 
      filter(!is.na(.data[[input$var_to_forecast]])) %>% 
      summarize(first(date))
    min_avail_date_var <- as.Date(min_avail_date_var_tbl[[1]])
    
    data_for_ts <- data_forecast() %>% filter(date >= min_avail_date_var) %>% select(input$var_to_forecast)
    
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
    temp <- autoplot(forecast(model(), h = input$n_to_predict))+
      labs(x = "Date", y = var_label)+
      scale_x_continuous(labels = my_date_trans)
    
    if (input$scale_type_forecast  == "log") {temp + scale_y_log10(labels = comma)}
    else {temp+scale_y_continuous(labels = comma)}
  })

  #visualise using ggplot via forecast's function autoplot, ggplotly doesn't handle it though as autoplot uses it's own geom
# total cases break-up ----------------------------------------------------
  cols <- c("total_recovered" = "green", "active_cases" = "blue", "total_deaths" = "red")
  data_tcbreakup <- reactive({
    req(input$country_tcbreakup)
    data_small %>% 
      filter(location %in% input$country_tcbreakup) %>% 
      select(date, active_cases, total_deaths, total_recovered)
  })
  
  data_tcbreakup_long <- reactive({
    data_tcbreakup() %>% 
      pivot_longer(cols = c(active_cases, total_deaths, total_recovered), names_to = "cases_type", values_to = "value") %>% 
      mutate(cases_type = fct_reorder(as.factor(cases_type), desc(cases_type)))
  })
  
  p_tcbreakup <- reactive({
    ggplot(data_tcbreakup_long(), aes(x = date, y = value, fill = cases_type,
                                    text = paste0(
                                      "Date: ", date,
                                      "<br>Number of cases: ", value,
                                      "<br>Cases type: ", cases_type
                                    )))+
      geom_col()+
      scale_fill_manual(values = cols)+
      labs(fill = "Cases type", x = "Date", y = "Number of cases")+
      scale_y_continuous(labels = comma)+
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")
  })
  
  output$p_tcbreakup_pl <- renderPlotly({
    ggplotly(p_tcbreakup(), tooltip = "text", height = 600)
  })
# codebook ----------------------------------------------------------------
  output$codebook <- renderDT({
    datatable(codebook, options = list(searching = F, paging = F))
  })
  
# correlation analysis ----------------------------------------------------
  data_temperature_cz <- data_small %>% 
    filter(location == "Czechia") %>% 
    left_join(avg_temperatures_cz, by = c("date")) %>% 
    mutate(avg_week_temperature = my_trailing_mean(avg_temperature))
  
  temp_color <- "red"
  new_case_color <- "blue"
  mult_coeff <- 500
  p_temperature <- ggplot(data_temperature_cz, aes(x = date))+
    geom_line(aes(y = avg_week_new_cases), color = new_case_color)+
    geom_line(aes(y = (avg_week_temperature)*mult_coeff), color = temp_color)+
    scale_y_continuous(
      name = "Weekly average of new cases",
      sec.axis = sec_axis(~./mult_coeff, name = "Weekly average temperature (°C)")
    )+
    theme(
      axis.title.y = element_text(color = new_case_color),
      axis.title.y.right = element_text(color = temp_color)
    )+
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")
  
  output$corr_graph <- renderPlot(p_temperature)
  
  max_date_temp <- max(data_temperature_cz$date)
  output$max_date_temp_notice <- renderText(paste0("Temperature data available for Czech republic only so far, updated on ", max_date_temp))
# czech republic by area --------------------------------------------------
  data_kraj_cz <- kraj_okres_cz %>% 
    group_by(kraj_nuts_kod, kraj_nazev, datum) %>% 
    summarise(kumulativni_pocet_nakazenych = sum(kumulativni_pocet_nakazenych),
              kumulativni_pocet_vylecenych = sum(kumulativni_pocet_vylecenych),
              kumulativni_pocet_umrti = sum(kumulativni_pocet_umrti)) %>% 
    ungroup()
  
  observeEvent(input$fill_okres, {
    kraje <- input$kraj_filter
    updateSelectInput(session, "okres_filter",
                      selected = okres_cis %>% select(okres_lau_kod) %>% filter(substr(okres_lau_kod,1,5) %in% kraje) %>% deframe()
    )
  }) #fill of okres_filter based on kraje_filter values
  
  p_cz_detail <- eventReactive(input$apply_filters_cz_detail, {
    req(input$level)
    if (input$level == "kraj") {
      data_filtered <- data_kraj_cz %>%
        filter(kraj_nuts_kod %in% input$kraj_filter)
      color_var <- "kraj_nazev"
      group_var <- "kraj_nuts_kod"
      color_label <- "Kraj"
      group_label <- "Kod kraje"
    } else {
      data_filtered <- kraj_okres_cz %>%
        filter(okres_lau_kod %in% input$okres_filter)
      color_var <- "okres_nazev"
      group_var <- "okres_lau_kod"
      color_label <- "Okres"
      group_label <- "Kod okresu"
      } #end else

    ggplot(data = data_filtered, aes(x = datum, y = kumulativni_pocet_nakazenych, group = .data[[group_var]], color = .data[[color_var]]))+
      geom_line(aes(text = paste0('Datum: ', datum,
                                  '<br>Kumulativni pocet nakazenych: ', kumulativni_pocet_nakazenych,
                                  '<br>', color_label, ': ', .data[[color_var]],
                                  '<br>', group_label, ': ', .data[[group_var]])))+
      labs(x = "Datum", y = "Kumulativni pocet nakazenych", color = color_label)+
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")
  })
  
  output$p_cz_detail_pl <- renderPlotly({
    ggplotly(p_cz_detail(), tooltip = "text")
  })
  
  output$max_date_cz_notice <- renderText(paste0("Data zverejnovana s tydenni periodou, aktualni data z ", max_date_cz))
  
  data_kraj_cz_now <- reactive({
    data_kraj_cz %>% 
      filter(datum == input$base_cz_detail_on_data_from)
  })
  
  kraj_okres_cz_now <- reactive({
    kraj_okres_cz %>% 
      filter(datum == input$base_cz_detail_on_data_from)
  })
  
  output$bar_cz_detail <- renderHighchart({
    req(input$level)
    req(input$base_cz_detail_on_data_from)
    if (input$level == "kraj") {
      data <- data_kraj_cz_now()
      x_var <- "kraj_nazev"
      x_label <- "Nazev kraje"
    } else {
      data <- kraj_okres_cz_now()
      x_var <- "okres_nazev"
      x_label <- "Nazev okresu"
    } #end else
    
    tooltip_category_text <- c("Kumulativni pocet nakazenych: ", "Kraj: ")
    tooltip_formatted_values <- c("{point.kumulativni_pocet_nakazenych}", "{point.kraj_nazev}")
    my_tooltips <- tooltip_table(tooltip_category_text, tooltip_formatted_values)
    
    hchart(
      data,
      "column",
      hcaes(x = .data[[x_var]], y = kumulativni_pocet_nakazenych, color = kraj_nuts_kod),
      name = "Kumulativni pocet nakazenych",
      colorByPoint = TRUE
    ) %>% 
      hc_tooltip(
        pointFormat = my_tooltips,
        useHTML = TRUE
      ) %>% 
      hc_yAxis(title = list(text = "Kumulativni pocet nakazenych")) %>% 
      hc_xAxis(title = list(text = x_label))
  })
}

