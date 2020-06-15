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
                   c('Male','White', 'Black','Hispanic',"Age")) #Defines which checkboxes to use
      
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
  # print(input)
  output$distPlot <- renderLeaflet({
    # pal <- colorNumeric(palette = "viridis", domain = ri_pop$input$varToPlot, n = 10) #This line scales the colors from the minimum to the maximum value for the selected variable
    pal <- colorNumeric(palette = "viridis", domain = 0:1, n = 10) #This line scales the colors from 0 to 1 for all variables, allowing us to see different info. 
    ri_pop %>% 
      st_transform(crs = "+init=epsg:4326") %>% #Defines the geography info format
      leaflet() %>% #Creates leaflet pane
      addProviderTiles(provider = "CartoDB.Positron") %>% #No clue what this does tbh
      addPolygons(stroke = FALSE, #Creates the polygons to overlay on the map, with parameters
                  smoothFactor = 0,
                  fillOpacity = 0.7,
                  color = ~ pal(get(input$varToPlot))) %>%
      addLegend("bottomright", #Adds the legend
                pal = pal, 
                values = ~ get(input$varToPlot),
                title = "Population fraction",
                opacity = 1)
  })
  
  
}

shinyApp(ui=ui, server=server)