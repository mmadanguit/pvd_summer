library(shiny)
library(leaflet)
library(stringr)
library(tidyverse)
library(tidycensus)
library(sf)
library(mapview)
library(shinyjs)
library(pracma)
library(rsconnect)
library(shinyBS)
source('census_data_example.R')
source('demand/availability/model.R')


ui <- fluidPage(
  useShinyjs(),
  tabsetPanel(
    # tabPanel("Census Data Map",
    #   # App title ----
    #   titlePanel("Providence Census Information"),
    #   
    #   # Sidebar layout with input and output definitions ----
    #   sidebarLayout(
    #     
    #     # Sidebar panel for inputs ----
    #     sidebarPanel(
    #       
    #       # Input: Choose variable ----
    #       checkboxGroupInput("varToPlot", "Variable:",
    #         c(
    #           'Pop',
    #           'Male',
    #           'Female',
    #           'White', 
    #           'Black',
    #           'Other', 
    #           'Hispanic', 
    #           'Citizen',
    #           'NotCitizen',
    #           'engOnly',
    #           'spanish',
    #           'spanishStrE',
    #           'spanishWeakE',
    #           'medFamInc',
    #           'perCapitaInc',
    #           'Poverty',
    #           'abovePoverty',
    #           'InternetAccess',
    #           'noInternetAccess',
    #           'Disability',
    #           'NoDisability',
    #           'auto',
    #           'public',
    #           'walk',
    #           'other',
    #           'college'
    #         ),
    #         selected = 'Pop'
    #       ), #Defines which checkboxes to use
    #       width = 3
    #     ),
    #     
    #     # Main panel for displaying outputs ----
    #     mainPanel(
    #       # Output: Maps will go here
    #       tags$div(id = 'censusPlaceholderConstraint') #Placeholder constraint to know where to put maps
    #       
    #     )
    #   )
    # ),

    tabPanel("Availability",
             sidebarPanel(
               fileInput("demandTRACT", "Choose demandTRACT.csv",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values")),
               fileInput("demandLatLng", "Choose demandLatLng.csv",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values")),
               dateRangeInput("availabilityDateRange", "Date Range to model",
                              start = "2019-5-01",
                              end = "2019-10-31",
                              min = "2018-11-01",
                              max = "2019-10-31"),
               checkboxInput("includeWeekdays", "Include Weekdays", value = TRUE),
               checkboxInput("includeWeekends", "Include Weekends", value = TRUE),
               radioButtons("tractOrLatLng", "Tract or Lat/Long?", choices = c("Model by Tract" = "tract", "Model by Latitude and Longitude" = "latLng")),
               radioButtons("zCol", "View Variable", choices = c(
                 "Mean Trips" = "meanTrips", 
                 "Median Trips" = "medTrips",
                 "Standard Deviation Trips" = "stdTrips",
                 "Zero Trips" = "zeroTrips"
                 )),
               bsTooltip("zCol", "tooltip text"),
               p("Built by Nolan Flynn, Marion Madanguit, Hyunkyung Rho, Maeve Stites, and Alice Paul")
             ),
             # titlePanel("Availability Maps"),
             mainPanel(
               h3("Estimated Demand Map"),
               leafletOutput("demandMapPlot"), #This is where the map will go 
               hr(),
               h3("Pickup Map"),
               leafletOutput("pickupMapPlot"), #This is where the map will go
               hr(),
               h3("Difference Map"),
               leafletOutput("differenceMapPlot"), #This is where the map will go
             )
      )
    )
  
  )

# Define server logic to load, map, and label selected datasets
server <- function(input, output) {
  options(shiny.maxRequestSize = 30*1024^2)
  # shinyjs::hide(id = "tractOrLatLng")
  # currentIds <- c()
  # observeEvent(input$varToPlot, { #Trigger all this mapping when the checkboxes change
  #   for (constraintId in currentIds){ #Start by getting rid of all the maps
  #     removeUI(selector = paste0('#',constraintId))
  #   } 
  #   currentIds <<- c()
  #   for(item in input$varToPlot){ #For every checked checkbox
  #     constraintId <- paste0('Constraint_', item) #Make this constraint ID
  #     horizontalId <- paste0('Constraint2_', item) #Make this constraint ID
  #     currentIds <<- c(constraintId, horizontalId, currentIds) #And add this ID to this list
  #     if(item == 'Pop' | item == 'medFamInc' | item == 'perCapitaInc'){ #If the item is any of these, scale according to the domain becuz these are the variables that are not 0 to 1 range
  #       pal <- colorNumeric(palette = "plasma", domain = ri_pop$item, n = 10) #This line scales the colors from the minimum to the maximum value for the selected variable
  #     } else { #For all the 0 to 1 range percentage things, do one of these
  #       # pal <- colorNumeric(palette = "plasma", domain = ri_pop$item, n = 10) #This line scales the colors from the minimum to the maximum value for the selected variable
  #       pal <- colorNumeric(palette = "plasma", domain = 0:1, n = 10) #This line scales the colors from 0 to 1 for all variables, allowing us to see different info.
  #     }
  #     labels <- sprintf( #Make a list of labels with HTML styling for each census tract
  #       "<strong>%s</strong><br/>variable = %g",
  #       str_extract(ri_pop$NAME, "^([^,]*)"), ri_pop[[item]] #Fill in the tract name, formatted to just the census tract number, and the value of this column
  #     ) %>% lapply(htmltools::HTML)
  #     insertUI( #Add a UI element
  #       selector = '#censusPlaceholderConstraint', #After the censusPlaceholderConstraint div
  #       ui = tags$div(id = constraintId, #Give this UI div an ID of the previously defined ID name
  #         ri_pop %>% #Put this big ol map thing inside this div
  #           st_transform(crs = "+init=epsg:4326") %>% #Defines the geography info format
  #           leaflet() %>% #Creates leaflet pane
  #           addProviderTiles(provider = "CartoDB.Positron") %>% #No clue what this does tbh
  #           addPolygons(
  #             stroke = FALSE, #Creates the polygons to overlay on the map, with parameters
  #             smoothFactor = 0,
  #             fillOpacity = 0.7,
  #             color = ~ pal(get(item)),
  #             label = labels, #Add the label
  #             labelOptions = labelOptions( #Add label styling
  #               style = list("font-weight" = "normal", padding = "3px 8px"),
  #               textsize = "15px",
  #               direction = "auto")
  #           ) %>%
  #           addLegend("bottomright", #Adds the legend
  #             pal = pal,
  #             values = ~ get(item),
  #             title = item,
  #             opacity = 1)
  #       )
  #     )
  #     insertUI( #Add a horizontal line
  #       selector = '#censusPlaceholderConstraint', #Add the line after the placeholderConstraint div
  #       ui = tags$hr(id=horizontalId) #This is the horizontal line btw
  #     )
  #   }
  # })
  
  
  setupPlots <- reactive({ #Reactive function to read the CSVs and filter based on the inputs. Runs once every time the inputs are changed, instead of 3 times (1 for each map)
    req(input$demandTRACT) #Require the 2 modeled demand data files to be uploaded
    req(input$demandLatLng)
    
    date1 = input$availabilityDateRange[1] #Get the start and end dates to filter the demand data output with
    date2 = input$availabilityDateRange[2]
    
    
    daysToInclude <- c() #Figure out what days to include to filter the demand data output with
    if(input$includeWeekdays[1]){
      daysToInclude <- c(daysToInclude, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    }
    if(input$includeWeekends[1]){
      daysToInclude <- c(daysToInclude, c("Saturday", "Sunday"))
    }
    
    latLng = FALSE #Convert latlng radio buttons to a boolean
    if(input$tractOrLatLng == "latLng"){
      latLng = TRUE
    }
    
    demand <- read.csv(input$demandTRACT$datapath) #Read the demand files, defaulting to tract edition
    if(latLng == TRUE){
      demand <- read.csv(input$demandLatLng$datapath)
    }
    filteredDemand <- demand %>%
      filter(DATE >= date1 & DATE <= date2) #%>% #Do the filtering from above
      # filter(DAY %in% daysToInclude)
    
  })
  
  output$demandMapPlot <- renderLeaflet({ #Render the mapview into the leaflet thing. Mapview is based on Leaflet so this works.
    latLng = FALSE #Convert latlng radio buttons to a boolean
    if(input$tractOrLatLng == "latLng"){
      latLng = TRUE
    }
    mvDemand <- req(setupPlots()) %>% genMap(latLng = latLng, zcol=input$zCol, type = "demand") #Get the output from setupPlots and generate the map based on that
    mvDemand@map #Get the contents of the leaflet map of the mapview output
  })
  
  
  output$pickupMapPlot <- renderLeaflet({ #Render the mapview into the leaflet thing. Mapview is based on Leaflet so this works.
    latLng = FALSE #Convert latlng radio buttons to a boolean
    if(input$tractOrLatLng == "latLng"){
      latLng = TRUE
    }
    mvPickup <- req(setupPlots()) %>% genMap(latLng = latLng, zcol=input$zCol, type = "pickup") #Get the output from setupPlots and generate the map based on that
    mvPickup@map #Get the contents of the leaflet map of the mapview output
    })
  output$differenceMapPlot <- renderLeaflet({ #Render the mapview into the leaflet thing. Mapview is based on Leaflet so this works.
    latLng = FALSE #Convert latlng radio buttons to a boolean
    if(input$tractOrLatLng == "latLng"){
      latLng = TRUE
    }
    mvDifference <- req(setupPlots()) %>% genMap(latLng = latLng, zcol=input$zCol, type = "difference") #Get the output from setupPlots and generate the map based on that
    mvDifference@map #Get the contents of the leaflet map of the mapview output
  })
}

shinyApp(ui=ui, server=server)