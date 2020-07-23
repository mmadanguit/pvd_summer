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
source('census_data_example.R')
# source('demand/dropoffDemandModel.R')
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
    # tabPanel("Dropoff Demand Map",
    #   titlePanel("Dropoff Demand Map"),
    #     mainPanel(
    #       leafletOutput("dropoffDemandMapPlot"), #This is where the demand map will go
    #     )
    #   ),
    tabPanel("Availability",
             sidebarPanel(
               dateRangeInput("availabilityDateRange", "Date Range to model",
                              start = "2018-11-01",
                              end = "2019-10-31",
                              min = "2018-11-01",
                              max = "2019-10-31"),
               checkboxInput("includeWeekdays", "Include Weekdays", value = TRUE),
               checkboxInput("includeWeekends", "Include Weekends", value = TRUE),
               radioButtons("tractOrLatLng", "Tract or Lat/Long?", choices = c("Model by Tract" = "tract", "Model by Latitude and Longitude" = "latLng")),
               radioButtons("zColDemand", "zColDemand", choices = c(
                 "meanTrips" = "meanTrips", 
                 "medTrips" = "medTrips",
                 "stdTrips" = "stdTrips",
                 "zeroTrips" = "zeroTrips",
                 "meanAvailTime" = "meanAvailTime",
                 "medAvailTime" = "medAvailTime",
                 "stdAvailTime" = "stdAvailTime",
                 "zeroAvailTime" = "zeroAvailTime",
                 "naAvailTime" = "naAvailTime",
                 "meanAvail" = "meanAvail",
                 "medAvail" = "medAvail",
                 "stdAvail" = "stdAvail",
                 "zeroAvail" = "zeroAvail"
                 )),
               radioButtons("zColPickup", "zColPickup", choices = c(
                 "meanTrips" = "meanTrips", 
                 "medTrips" = "medTrips",
                 "stdTrips" = "stdTrips",
                 "zeroTrips" = "zeroTrips"
               )),
             ),
             titlePanel("Availability Maps"),
             mainPanel(
               h3("Demand Map"),
              leafletOutput("demandMapPlot"), #This is where the map will go 
               hr(),
               h3("Pickup Map"),
               leafletOutput("pickupMapPlot"), #This is where the map will go
             )
      )
    )
  
  )

# Define server logic to load, map, and label selected datasets
server <- function(input, output) {
  # shinyjs::hide(id = "tractOrLatLng")
  # output$placeholder <- renderText("Placeholder")
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
  

  
  fol <- "" #Folder containing pickupsSummary.csv, availIntervals.csv, and tripsPerTract.csv
  # output$dropoffDemandMapPlot <- renderLeaflet({ #Render the mapview into the leaflet thing. Mapview is based on Leaflet so this works.
  #   mv <- dropoffDemandMap(fol)
  #   mv@map #Get the contents of the "map" slot of the formal mapview object. Ngl don't totally know what this means.
  # })
  
  output$demandMapPlot <- renderLeaflet({ #Render the mapview into the leaflet thing. Mapview is based on Leaflet so this works.
    date1 = input$availabilityDateRange[1] #Get the start and end dates to calculate model.R with
    date2 = input$availabilityDateRange[2]
    
    
    daysToInclude <- c() #Figure out what days to include to filter model.R output with
    if(input$includeWeekdays[1]){
      daysToInclude <- c(daysToInclude, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    }
    if(input$includeWeekends[1]){
      daysToInclude <- c(daysToInclude, c("Saturday", "Sunday"))
    }
    latLng = FALSE
    if(input$tractOrLatLng == "latLng"){
      latLng = TRUE
    }
    print(input$tractOrLatLng)
    
    demand <- constData(fol, latLng = latLng) #The actual modeling stuff from model.R
    mvDemand <- demand %>%
      filter(DATE >= date1 & DATE <= date2) %>% #Do the filtering from above
      filter(DAY %in% daysToInclude) %>%
      genMap(latLng = latLng, zcol=input$zColDemand)
    mvDemand@map #Get the contents of the "map" slot of the formal mapview object. Ngl don't totally know what this means.
  })
  
  
  output$pickupMapPlot <- renderLeaflet({ #Render the mapview into the leaflet thing. Mapview is based on Leaflet so this works.
    date1 = input$availabilityDateRange[1] #All pretty much the same as above.
    date2 = input$availabilityDateRange[2]
    
    
    daysToInclude <- c()
    if(input$includeWeekdays[1]){
      daysToInclude <- c(daysToInclude, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    }
    if(input$includeWeekends[1]){
      daysToInclude <- c(daysToInclude, c("Saturday", "Sunday"))
    }
    latLng = FALSE
    if(input$tractOrLatLng == "latLng"){
      latLng = TRUE
    }
    
    pickup <- constData(fol, pickup = TRUE, latLng = latLng)
      mvPickup <- pickup %>%
        filter(DATE >= date1 & DATE <= date2) %>%
        filter(DAY %in% daysToInclude) %>%
        genMap(latLng = latLng, zcol=input$zColPickup)
      mvPickup@map #Get the contents of the "map" slot of the formal mapview object. Ngl don't totally know what this means.
    })
}

shinyApp(ui=ui, server=server)