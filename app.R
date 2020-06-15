library(shiny)
library(leaflet)
library(stringr)
library(tidyverse)
library(tidycensus)
library(sf)
source('census_data_example.R')


ui <- fluidPage(
  
  # App title ----
  titlePanel("Providence Census Information"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Choose variable ----
      radioButtons("varToPlot", "Variable:",
                   c('Male','White', 'Black','Hispanic',"Age"))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Map ----
      leafletOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  # Return the requested dataset ----
  output$distPlot <- renderLeaflet({
    pal <- colorQuantile(palette = "viridis", domain = ri_pop$input$varToPlot, n = 10)
    ri_pop %>% 
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0,
                  fillOpacity = 0.7,
                  color = ~ pal(get(input$varToPlot))) %>%
      addLegend("bottomright", 
                pal = pal, 
                values = ~ get(input$varToPlot),
                title = "Population percentiles",
                opacity = 1)
  })
  
  
}

shinyApp(ui=ui, server=server)