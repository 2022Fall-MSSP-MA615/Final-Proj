library(shiny)
library(dplyr)
library(DT)
library(shinyWidgets)
library(tidyverse)
library(magrittr)
library(kableExtra)
library(sf)
library(readxl)
library(rgdal)
library(maptools) 
library(sp) 
library(leaflet)
library(shinycssloaders)
library(plotly)
library(htmltools)
library(shinyjs)


RTimes <- read.csv("RTimes.csv", header = T)
RTimes <- RTimes[,-c(1)]
Bus <- read.csv("Bus.csv", header = T)
Bus <- Bus[,-c(1,2,4,5,8,13,14)]
Bus$scheduled <- substr(Bus$scheduled,12,19)
Bus$actual <- substr(Bus$actual,12,19)
Bus$route_id <- as.character(Bus$route_id)
RTimes$direction_id <- as.character(RTimes$direction_id)
MBTA_Data <- read.csv("Final_Data_MBTA.csv")

Season <- unique(MBTA_Data$Season)
Route <- unique(MBTA_Data$route_id)
Start <- unique(MBTA_Data$departure_time)
Time <- unique(Bus$scheduled)

ui <- fluidPage(
  titlePanel("MBTA Map"),
  sidebarLayout(
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Map",leafletOutput("map")),
                  tabPanel("Train",tableOutput("table1")),
                  tabPanel("BUS",tableOutput("table2")),
      sidebarPanel(selectInput("Route", "Select the route",Route),
                   br(),
                   selectInput("Start", "Select departure time",Start),
                   br(),
                   checkboxGroupInput("Season","Select the season",Season),
                   br(),
                   br(),
                   selectInput("Bus Start", "Select the Bus departure time", Time)
                  )
      )
    )
  )
)

server <- function(input,output){
  table_df1 <- reactive({
    MBTA_Data %>% filter(Season%in%input$Season, route_id%in%input$Route, 
                      departure_time%in%input$Start)
  })
  table_df2 <- reactive({
    Bus %>% filter(scheduled%in%input$Time)
  })
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lat = table_df1()$stop_lat, lng = table_df1()$stop_lon,
                 popup= table_df1()$checkpoint_name)
  })
  output$table1 <- renderTable({table_df1()})
  output$table2 <- renderTable({table_df2()})
}

shinyApp(ui = ui, server = server)

