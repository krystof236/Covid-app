library(tidyverse)
library(DT)
library(ggplot2)
library(highcharter)
library(plotly)
library(scales)


# data preparation --------------------------------------------------------
data <- rio::import("data/owid-covid-data.csv")
data <- data %>% 
  mutate(date = as.Date(date))
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
                "reproduction_rate")
data_small <- data %>%
  select(all_of(vars_small))

avail_countries <- data %>% distinct(location) %>% arrange(location)
max_date <- max(data$date)


# possible variables to plot preparation ----------------------------------
possible_vars_to_plot <- tribble(
  ~label, ~value,
  "Total cases", "total_cases",
  "New cases", "new_cases",
  "Total deaths", "total_deaths",
  "New deaths", "new_deaths",
  "Reproduction rate", "reproduction_rate")

# server function definition ----------------------------------------------
function(input, output) {

# user inputs -------------------------------------------------------------
  output$country_filter <- renderUI({
    selectInput("country", "Country", choices = avail_countries$location, multiple = T, selected = "Czech Republic")
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
  
# time series -------------------------------------------------------------
  filtered_data <- reactive({
    data %>%
      filter(location %in% input$country)
  })
  
  filtered_data_small <- reactive({
    filtered_data() %>% select(all_of(vars_small))
  })
  
  # filtered_data_small <- data_small %>% filter(location == "Czech Republic")
  p_cases <- reactive({
    max_y <- reactive(max(filtered_data_small()[[input$var_to_plot]], na.rm = T))
    var_label <- possible_vars_to_plot %>% filter(value == input$var_to_plot) %>% select(label) %>% as.character()
    ggplot(filtered_data_small(),
           aes(x = date, y = .data[[input$var_to_plot]], group = location, color = location,
               text = paste0(
                 var_label, ': ', .data[[input$var_to_plot]],
                 '<br> Country: ', location,
                 '<br> Date: ', date
               )
           )
    )+
      geom_line()+
      scale_y_continuous(breaks = seq(0,max_y(), 10^(floor(log10(max_y())))), labels = comma)+
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")+
      labs(x = "Date", y = var_label, title = paste0("Single variable - ", var_label," time series"), color = "Country")
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
    graphs[[no_layers+1]]+
      labs(title = "Multiple variables", x = "Date", y = "", color = "Country")+
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")+
      scale_y_continuous(labels = comma)
  })
  
  output$p_mult_pl <- renderPlotly({
    ggplotly(p_mult(), tooltip = "text")
  })
  
  output$filtered_data_dt <- renderDT(filtered_data_small())
# drillable highchart and datatable ---------------------------------------
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
}
