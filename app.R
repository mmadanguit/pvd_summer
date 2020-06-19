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
      checkboxGroupInput("varToPlot", "Variable:",
                   c('Male','White', 'Black','Hispanic',"Age")) #Defines which checkboxes to use
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Map ----
      tags$div(id = 'placeholderConstraint')
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  makeReactiveBinding("aggregConstraintObserver")
  aggregConstraintObserver <- list() #I literally have no idea what they do
  
  
  currentIds <- c()
  # Return the requested dataset ----
  observeEvent(input$varToPlot, { #Trigger all this mapping when the checkboxes change
    for (constraintId in currentIds){ #Start by getting rid of all the maps
      removeUI(selector = paste0('#',constraintId))
      aggregConstraintObserver[[constraintId]] <<- NULL
    } 
    currentIds <<- c()
    for(item in input$varToPlot){ #For every checked checkbox
      constraintId <- paste0('Constraint_', item) #Make this constraint ID
      currentIds <<- c(constraintId, currentIds) #And add this ID to this list
      # pal <- colorNumeric(palette = "viridis", domain = ri_pop$input$varToPlot[index], n = 10) #This line scales the colors from the minimum to the maximum value for the selected variable
      pal <- colorNumeric(palette = "viridis", domain = 0:1, n = 10) #This line scales the colors from 0 to 1 for all variables, allowing us to see different info.
      
      insertUI( #Add a UI element
        selector = '#placeholderConstraint', #After the placeholderConstraint div
        ui = tags$div(id = constraintId, #Give this UI div an ID of the previously defined ID name
              ri_pop %>% #Put this big ol map thing inside this div
                st_transform(crs = "+init=epsg:4326") %>% #Defines the geography info format
                leaflet() %>% #Creates leaflet pane
                addProviderTiles(provider = "CartoDB.Positron") %>% #No clue what this does tbh
                addPolygons(stroke = FALSE, #Creates the polygons to overlay on the map, with parameters
                            smoothFactor = 0,
                            fillOpacity = 0.7,
                            color = ~ pal(get(item))) %>%
                addLegend("bottomright", #Adds the legend
                          pal = pal,
                          values = ~ get(item),
                          title = "Population fraction",
                          opacity = 1)
              
        )
      )
    }
  })
    
    
    
    
    
  # output$plot1 <- renderLeaflet({
  #     item <- "White"
  #     print(item)
  #     # pal <- colorNumeric(palette = "viridis", domain = ri_pop$input$varToPlot[index], n = 10) #This line scales the colors from the minimum to the maximum value for the selected variable
  #     pal <- colorNumeric(palette = "viridis", domain = 0:1, n = 10) #This line scales the colors from 0 to 1 for all variables, allowing us to see different info. 
  #     ri_pop %>% 
  #       st_transform(crs = "+init=epsg:4326") %>% #Defines the geography info format
  #       leaflet() %>% #Creates leaflet pane
  #       addProviderTiles(provider = "CartoDB.Positron") %>% #No clue what this does tbh
  #       addPolygons(stroke = FALSE, #Creates the polygons to overlay on the map, with parameters
  #                   smoothFactor = 0,
  #                   fillOpacity = 0.7,
  #                   color = ~ pal(get(item))) %>%
  #       addLegend("bottomright", #Adds the legend
  #                 pal = pal, 
  #                 values = ~ get(item),
  #                 title = "Population fraction",
  #                 opacity = 1)
  # })
}
  

shinyApp(ui=ui, server=server)