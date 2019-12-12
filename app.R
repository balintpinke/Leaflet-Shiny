library(leaflet)
library(shiny)
library(shinydashboard)
library(DT)
library(geojsonio)
library(plyr)

# setwd("D:/R_WD/Leaflet-Shiny")
counties <- geojsonio::geojson_read("hun_counties.json", what = "sp")
counties_df <- read.csv("leaflet_ksh_megye.csv", sep = ";", header = TRUE, dec = ",", check.names=FALSE)
# counties@data <- plyr::join(counties@data, counties_df, by = "megye")
choices <- unique(counties_df$year)
choices2 <- names(counties_df)[3:5]

pal <- colorNumeric("Reds", NULL)

ui <-  dashboardPage(skin = "red",
  dashboardHeader(title = "Wheat production in Hungary", titleWidth = 450),
  dashboardSidebar(collapsed = TRUE,
                   sidebarMenu()),
  dashboardBody(tags$style(HTML(".box.box-solid.box-primary>.box-header {
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
    fluidRow(
      column(width = 3,
      selectInput("stat_type", "Choose statistics to map:", choices = choices2, selected = choices[1]),
      tags$a(href="https://www.ksh.hu/docs/hun/xstadat/xstadat_eves/i_omn012b.html", "Source of data")
      
        ),
      column(width = 3,
      selectInput("year_input", "Choose year to map:", choices = choices, selected = choices[1])
          ),
      column(width = 3#,
            # Harvested area hectare

      ),
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
)

server <- function(input, output, session) {

  counties_df_plot <- reactive({
    
    counties_df=counties_df[counties_df$year == input$year_input, c("megye", "year", input$stat_type)]
    
    counties_df
  })
  
  
  output$counties_map <- renderLeaflet({

    leaflet(counties) %>%
      addTiles() %>%
      addPolygons()
  })
  
  observe({
    if(!is.null(input$year_input)){
      
      counties@data <- plyr::join(counties@data, counties_df_plot(), by = "megye")
      
      leafletProxy("counties_map", data = counties) %>%
        clearControls() %>%
        addTiles() %>%
        addPolygons(stroke = TRUE, smoothFactor = 1, fillOpacity = 0.8,  color = "black",   dashArray = 1,
                    fillColor = ~pal(get(input$stat_type)),
                    label = ~paste(counties@data[["megye"]], "county", input$year_input, ":", counties@data[[input$stat_type]])) %>%
        addLegend(pal = pal,  title = paste("Year:", input$year_input), values = ~get(input$stat_type), opacity = 1.0)
    }
  })
  
  
  df <- reactive({
    plot_table=counties_df_plot()
    
    plot_table <- plot_table[order(-plot_table[3]),]
    plot_table
  })
  
  
  
  output$counties_dt <- renderDataTable({datatable(
    df(), rownames=FALSE, options = list(searching = FALSE)
  )
  })

  

}

shinyApp(ui, server)

