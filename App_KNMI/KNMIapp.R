# Shiny app for the RLadies Amsterdam Workshop R in Production
# 26-02-2020
# Author: Inderdeep Singh

# load the libraries
library(shiny)
library(shinydashboard)
library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(httr)
library(magrittr)
library(lubridate)
library(leaflet)
library(DT)

#####
# Load data
stations <- read_tsv('./data/stations.tsv')
variables <- read_tsv('./data/variables.tsv')

station_list  <- stations$STN
names(station_list) <- stations$NAME

variable_list  <- variables$VAR
names(variable_list) <- variables$DESC

#####
# Create Shiny app
# create UI interface
ui <- dashboardPage(
    dashboardHeader(title = 'KNMI Meteo Dashboard'),
    dashboardSidebar(),
    dashboardBody(
        fluidRow(
            box(width = 12, plotOutput('plot', height = 250))
        ),
        fluidRow(
            box(
                selectInput('station',  'Station',  station_list),
                selectInput('variable', 'Variable', variable_list),
                dateRangeInput('dates', 'Date Range'),
                width = 4
            ),
            box(leafletOutput('map', height = 450), width = 4),
            box(dataTableOutput('table'), width = 4)
        )
    )
)

# create server shiny backend
server <- function(input, output) {

    get_data <- reactive({
        # load weather data from dutch weather institute API
        baseurl <- 'http://projects.knmi.nl/klimatologie/uurgegevens/getdata_uur.cgi'
        #start <- (today() - lubridate::duration(1, 'day')) %>% format('%Y%m%d')
        #end <- today() %>% format('%Y%m%d')

        start <- input$dates[1] %>% as_date() %>% format('%Y%m%d')
        end   <- input$dates[2] %>% as_date() %>% format('%Y%m%d')

        print(input$dates)
        print(start)
        print(end)

        vars <- input$variable
        stns <- input$station

        url = paste0(baseurl, '?start=', start, '01', '&end=', end, '24', '&vars=', vars, '&stns=', stns)

        print(url)

        res <- GET(url)
        data <- content(res, as = 'text')

        df <- readr::read_csv(file = data, skip = 13, col_names = FALSE)
        if(nrow(df) == 0) { return(data.frame(time = list(NULL), value = list(NULL))) }
        colnames(df) <- c('station', 'date', 'hour', 'value')
        #df %<>% rename(station = X1, date = X2, hour = X3, value = X4)
        df %<>% mutate(time = paste0(date, hour - 1))
        df %<>% mutate(time = as.POSIXct(time, format = '%Y%m%d%H'))
        df %<>% select(time, value)

    })

    output$plot <- renderPlot({

        df <- get_data()

        df %>% ggplot() + aes(time, value) + geom_line()

    })

    output$map <- renderLeaflet({

        leaflet() %>%
          addProviderTiles(providers$Stamen.TonerLite,
            options = providerTileOptions(noWrap = TRUE)
          ) %>% addMarkers(data = stations, lat = ~LAT, lng = ~LON, popup = ~NAME)
      })

    output$table <- renderDataTable({
        stations
    })

}
# serve app
app <- shinyApp(ui, server)

runApp(app, port = 12092, host = '0.0.0.0')
