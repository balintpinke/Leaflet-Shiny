library(leaflet)
library(shiny)
library(shinydashboard)
library(DT)
library(geojsonio)
library(plyr)

setwd("D:/R_WD/Leaflet-Shiny")
counties <- geojsonio::geojson_read("hun_counties.json", what = "sp")
counties_df <- read.csv("leaflet_ksh_megye.csv", sep = ";", header = TRUE, dec = ",", check.names=FALSE)
counties@data <- plyr::join(counties@data, counties_df, by = "megye")
choices=names(counties@data)[2:4]


pal <- colorNumeric("Reds", NULL)

ui <-  dashboardPage(skin = "red",
  dashboardHeader(title = "Map of Hungary", titleWidth = 250),
  dashboardSidebar(collapsed = TRUE,
                   sidebarMenu()),
  dashboardBody(tags$style(HTML("

                                
                                .box.box-solid.box-primary>.box-header {
                                color:#fff;
                                background:#d73925
                                }
                                
                                .box.box-solid.box-primary{
                                border-bottom-color:#666666;
                                border-left-color:#666666;
                                border-right-color:#666666;
                                border-top-color:#666666;
                                }
                                
                                ")),
    selectInput("komorb_input", "Choose variable to map:", choices = choices, selected = choices[3]),
    fluidRow(
    box(title = "Leaflet map", width = 9, status = "primary", solidHeader = TRUE,
      leafletOutput("counties_map",height = 500)
      ),
    box(title = "Datatable", width = 3, status = "primary", solidHeader = TRUE,
        dataTableOutput("counties_dt")
    )
    )
    )
    )

server <- function(input, output, session) {


  output$counties_map <- renderLeaflet({

    leaflet(counties) %>%
      addTiles() %>%
      addPolygons(stroke = TRUE, smoothFactor = 1, fillOpacity = 0.8,  color = "black",   dashArray = 1,
                  fillColor = ~pal(counties@data[[input$komorb_input]]),
                  label = ~paste(counties@data[["megye"]], "county", input$komorb_input, ":", counties@data[[input$komorb_input]])) %>%
      addLegend(pal = pal,  title = input$komorb_input, values = ~counties@data[[input$komorb_input]], opacity = 1.0)

  })
  
  
  df <- reactive({
    counties_df2 <- counties_df[, c("megye", input$komorb_input)]
    counties_df2 <- counties_df2[order(-counties_df2[2]),]
    counties_df2
  })
  
  
  
  output$counties_dt <- renderDataTable({datatable(
    df(), rownames=FALSE, options = list(searching = FALSE)
  )
  })

  



}

shinyApp(ui, server)

