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


scooterVarChoices <- c(
  "Mean Trips/Day" = "meanTrips", 
  "Median Trips/Day" = "medTrips",
  "Standard Deviation Trips/Day" = "stdTrips",
  #################################################
  "Mean Scooters Available/Day" = "meanAvail",
  "Median Scooters Available/Day" = "medAvail",
  "Standard Deviation of Mean Available/Day" = "stdAvail",
  "Mean % of Day with Scooters Available" = "meanAvailPct",
  "Median % of Day with Scooters Available" = "medAvailPct",
  "Standard Deviation of Mean Available %" = "stdAvailPct",
  "% of Days with Zero Trips" = "zeroTrips",
  "% of Days with Zero Available Scooters" = "zeroAvailPct"
)
censusVarChoices <- c(
  "Population" = 'Pop',
  "% Male" = 'Male',
  "% Female" = 'Female',
  "% White" = 'White',
  "% Black" = 'Black',
  "% Other Race" = 'Other',
  "% Hispanic" = 'Hispanic',
  "% Citizen" = 'Citizen',
  "% Non-Citizen" = 'NotCitizen',
  "% Engligh-Speaking Only" = 'engOnly',
  "% Spanish-Speaking Only" = 'spanish',
  "% Spanish with Strong English" = 'spanishStrE',
  "% Spanish with Weak English" ='spanishWeakE',
  "Median Family Income" = 'medFamInc',
  "Per Capita Income" = 'perCapitaInc',
  "% Poverty" = 'Poverty',
  "% Above Poverty" = 'abovePoverty',
  "% Internet Access" = 'InternetAccess',
  "% No Internet Access" = 'noInternetAccess',
  "% Disability" = 'Disability',
  "% No Disability" = 'NoDisability',
  "% Commute by Auto" = 'auto',
  "% Commute by Public Transport" = 'public',
  "% Commute by Walk" = 'walk',
  "% Commute by Other" = 'other',
  "% In College" = 'college'
)
zColTooltipText <- "The Available/Day variables are the ones summarizing our model, and therefore show the Estimated Demand and Difference maps. All other variables summarize existing data and therefore only show the first map."
tripsVariables <- c("meanTrips", "medTrips", "stdTrips")

ui <- fluidPage(
   useShinyjs(),

   sidebarPanel(
     fileInput("demandTRACT", "Choose demandTRACT.csv",
               multiple = FALSE,
               accept = c("text/csv",
                          "text/comma-separated-values")),
     fileInput("demandLatLng", "Choose demandLatLng.csv",
               multiple = FALSE,
               accept = c("text/csv",
                          "text/comma-separated-values")),
     dateRangeInput("availabilityDateRange", "Date Range to Include",
                    start = "2019-5-01",
                    end = "2019-9-30",
                    min = "2018-11-01",
                    max = "2019-10-31"),
     checkboxInput("includeWeekdays", "Include Weekdays", value = TRUE),
     checkboxInput("includeWeekends", "Include Weekends", value = TRUE),
     radioButtons("tractOrLatLng", "Tract or Lat/Long?", choices = c("Model by Tract" = "tract", "Model by Latitude and Longitude" = "latLng")),
     radioButtons("zCol", "Scooter Variable", choices = scooterVarChoices),
     bsTooltip("zCol", zColTooltipText),
     checkboxGroupInput("varToPlot", "Census Variables:", choices = censusVarChoices, selected = 'Pop'), #Defines which checkboxes to use
     p("Built by Nolan Flynn, Marion Madanguit, Hyunkyung Rho, Maeve Stites, and Alice Paul")
   ),
   # titlePanel("Availability Maps"),
   mainPanel(
     column(6,
      h3("Census Maps"),
      # Output: Maps will go here
      tags$div(id = 'censusPlaceholderConstraint') #Placeholder constraint to know where to put maps
     ),
     column(6,
      div(id = "pickupMapDiv",
        h3("Scooter Variable Map"),
        leafletOutput("pickupMapPlot"), #This is where the map will go
        hr(),
      ),
      div(id = "demandMapDiv",
        h3("Estimated Demand Map"),
        leafletOutput("demandMapPlot"), #This is where the map will go
        hr(),
      ),
      div(id = "differenceMapDiv",
        h3("Difference Map"),
        leafletOutput("differenceMapPlot"), #This is where the map will go
        hr(),
      )
     )
  )
)

# Define server logic to load, map, and label selected datasets
server <- function(input, output) {
  options(shiny.maxRequestSize = 30*1024^2)
  
  
  currentIds <- c()
  observeEvent(input$varToPlot, { #Trigger all this mapping when the checkboxes change
    for (constraintId in currentIds){ #Start by getting rid of all the maps
      removeUI(selector = paste0('#',constraintId))
    }
    currentIds <<- c()
    for(item in input$varToPlot){ #For every checked checkbox
      constraintId <- paste0('Constraint_', item) #Make this constraint ID
      horizontalId <- paste0('Constraint2_', item) #Make this constraint ID
      currentIds <<- c(constraintId, horizontalId, currentIds) #And add this ID to this list
      if(item == 'Pop' | item == 'medFamInc' | item == 'perCapitaInc'){ #If the item is any of these, scale according to the domain becuz these are the variables that are not 0 to 1 range
        pal <- colorNumeric(palette = "plasma", domain = ri_pop$item, n = 10) #This line scales the colors from the minimum to the maximum value for the selected variable
      } else { #For all the 0 to 1 range percentage things, do one of these
        # pal <- colorNumeric(palette = "plasma", domain = ri_pop$item, n = 10) #This line scales the colors from the minimum to the maximum value for the selected variable
        pal <- colorNumeric(palette = "plasma", domain = 0:1, n = 10) #This line scales the colors from 0 to 1 for all variables, allowing us to see different info.
      }
      labels <- sprintf( #Make a list of labels with HTML styling for each census tract
        "<strong>%s</strong><br/>variable = %g",
        str_extract(ri_pop$NAME, "^([^,]*)"), ri_pop[[item]] #Fill in the tract name, formatted to just the census tract number, and the value of this column
      ) %>% lapply(htmltools::HTML)
      insertUI( #Add a UI element
        selector = '#censusPlaceholderConstraint', #After the censusPlaceholderConstraint div
        ui = tags$div(id = constraintId, #Give this UI div an ID of the previously defined ID name
                      ri_pop %>% #Put this big ol map thing inside this div
                        st_transform(crs = "+init=epsg:4326") %>% #Defines the geography info format
                        leaflet() %>% #Creates leaflet pane
                        addProviderTiles(provider = "CartoDB.Positron") %>% #No clue what this does tbh
                        addPolygons(
                          stroke = FALSE, #Creates the polygons to overlay on the map, with parameters
                          smoothFactor = 0,
                          fillOpacity = 0.7,
                          color = ~ pal(get(item)),
                          label = labels, #Add the label
                          labelOptions = labelOptions( #Add label styling
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto")
                        ) %>%
                        addLegend("bottomright", #Adds the legend
                                  pal = pal,
                                  values = ~ get(item),
                                  title = item,
                                  opacity = 1)
        )
      )
      insertUI( #Add a horizontal line
        selector = '#censusPlaceholderConstraint', #Add the line after the placeholderConstraint div
        ui = tags$hr(id=horizontalId) #This is the horizontal line btw
      )
    }
  })
  
  
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
    if(!input$zCol %in% tripsVariables){
      hide("demandMapDiv")
      hide("differenceMapDiv")
    } else {
      show("demandMapDiv")
      show("differenceMapDiv")
    }
    demand <- read.csv(input$demandTRACT$datapath) #Read the demand files, defaulting to tract edition
    if(latLng == TRUE){
      demand <- read.csv(input$demandLatLng$datapath)
    }
    filteredDemand <- demand %>%
      filter(DATE >= date1 & DATE <= date2) %>% #Do the filtering from above
      filter(DAY %in% daysToInclude)
  })
  
  
  output$pickupMapPlot <- renderLeaflet({ #Render the mapview into the leaflet thing. Mapview is based on Leaflet so this works.
    latLng = FALSE #Convert latlng radio buttons to a boolean
    if(input$tractOrLatLng == "latLng"){
      latLng = TRUE
    }
    mvPickup <- req(setupPlots()) %>% genMapCol(latLng = latLng, zcol=input$zCol, type = "pickup") #Get the output from setupPlots and generate the map based on that
    mvPickup #Get the contents of the leaflet map of the mapview output
  })
  
  
  output$demandMapPlot <- renderLeaflet({ #Render the mapview into the leaflet thing. Mapview is based on Leaflet so this works.
    latLng = FALSE #Convert latlng radio buttons to a boolean
    if(input$tractOrLatLng == "latLng"){
      latLng = TRUE
    }
    mvDemand <- req(setupPlots()) %>% genMapCol(latLng = latLng, zcol=input$zCol, type = "demand") #Get the output from setupPlots and generate the map based on that
    mvDemand #Get the contents of the leaflet map of the mapview output
  })
  
  
  output$differenceMapPlot <- renderLeaflet({ #Render the mapview into the leaflet thing. Mapview is based on Leaflet so this works.
    latLng = FALSE #Convert latlng radio buttons to a boolean
    if(input$tractOrLatLng == "latLng"){
      latLng = TRUE
    }
    mvDifference <- req(setupPlots()) %>% genMapCol(latLng = latLng, zcol=input$zCol, type = "difference") #Get the output from setupPlots and generate the map based on that
    mvDifference #Get the contents of the leaflet map of the mapview output
  })
}

shinyApp(ui=ui, server=server)