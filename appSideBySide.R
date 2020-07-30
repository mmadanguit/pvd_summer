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
  "Standard Deviation of Available/Day" = "stdAvail",
  "Mean % of Day with Scooters Available" = "meanAvailPct",
  "Median % of Day with Scooters Available" = "medAvailPct",
  "Standard Deviation of Available %" = "stdAvailPct",
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

makeCensusPlot <- function(item, varToPlot){
  # print("hi")
  if(item %in% varToPlot){
    if(item == 'Pop' | item == 'medFamInc' | item == 'perCapitaInc'){ #If the item is any of these, scale according to the domain becuz these are the variables that are not 0 to 1 range
      pal <- colorNumeric(palette = "plasma", domain = ri_pop$item, n = 10) #This line scales the colors from the minimum to the maximum value for the selected variable
    } else { #For all the 0 to 1 range percentage things, do one of these
      # pal <- colorNumeric(palette = "plasma", domain = ri_pop$item, n = 10) #This line scales the colors from the minimum to the maximum value for the selected variable
      pal <- colorNumeric(palette = "plasma", domain = 0:1, n = 10) #This line scales the colors from 0 to 1 for all variables, allowing us to see different info.
    }
    popupHTML <- sprintf( #Make a list of labels with HTML styling for each census tract
      "<strong>%s</strong><br/>variable = %g",
      str_extract(ri_pop$NAME, "^([^,]*)"), ri_pop[[item]] #Fill in the tract name, formatted to just the census tract number, and the value of this column
    ) %>% lapply(htmltools::HTML)
    
    censusPlot <- ri_pop %>% #Put this big ol map thing inside this div
      # st_transform(crs = "+init=epsg:4326") %>% #Defines the geography info format
      leaflet() %>% #Creates leaflet pane
      addProviderTiles(provider = "CartoDB.Positron") %>% #No clue what this does tbh
      addPolygons(
        stroke = FALSE, #Creates the polygons to overlay on the map, with parameters
        smoothFactor = 0,
        fillOpacity = 0.7,
        color = ~ pal(get(item)),
        popup = popupHTML #Add the label
        # labelOptions = labelOptions( #Add label styling
        #   style = list("font-weight" = "normal", padding = "3px 8px"),
        #   textsize = "15px",
        #   direction = "auto")
      ) %>%
      addLegend("bottomright", #Adds the legend
                pal = pal,
                values = ~ get(item),
                title = item,
                opacity = 1
      )
    # print(item)
    return(censusPlot)
  } else {
    return(NULL)
  }
}

ui <- fluidPage(
  useShinyjs(),
  br(),
  sidebarPanel(style = "overflow-y:scroll; height: 95vh",
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
    checkboxGroupInput("varToPlot", "Census Variables:", choices = censusVarChoices, selected = c()), #Defines which checkboxes to use
    p("Built by Nolan Flynn, Marion Madanguit, Hyunkyung Rho, Maeve Stites, and Alice Paul")
  ),
  # titlePanel("Availability Maps"),
  mainPanel(
    column(6,
        wellPanel(style = "overflow-y:scroll; height: 95vh",
           h3("Census Maps"),
           # Output: Maps will go here
           # tags$div(id = 'censusPlaceholderConstraint') #Placeholder constraint to know where to put maps
           div(id = "Pop", leafletOutput("PopPlot"), br()),
           div(id = "Male", leafletOutput("MalePlot"), br()),
           div(id = "Female", leafletOutput("FemalePlot"), br()),
           div(id = "White", leafletOutput("WhitePlot"), br()),
           div(id = "Black", leafletOutput("BlackPlot"), br()),
           div(id = "Other", leafletOutput("OtherPlot"), br()),
           div(id = "Hispanic", leafletOutput("HispanicPlot"), br()),
           div(id = "Citizen", leafletOutput("CitizenPlot"), br()),
           div(id = "NotCitizen", leafletOutput("NotCitizenPlot"), br()),
           div(id = "engOnly", leafletOutput("engOnlyPlot"), br()),
           div(id = "spanish", leafletOutput("spanishPlot"), br()),
           div(id = "spanishStrE", leafletOutput("spanishStrEPlot"), br()),
           div(id = "spanishWeakE", leafletOutput("spanishWeakEPlot"), br()),
           div(id = "medFamInc", leafletOutput("medFamIncPlot"), br()),
           div(id = "perCapitaInc", leafletOutput("perCapitaIncPlot"), br()),
           div(id = "Poverty", leafletOutput("PovertyPlot"), br()),
           div(id = "abovePoverty", leafletOutput("abovePovertyPlot"), br()),
           div(id = "InternetAccess", leafletOutput("InternetAccessPlot"), br()),
           div(id = "noInternetAccess", leafletOutput("noInternetAccessPlot"), br()),
           div(id = "Disability", leafletOutput("DisabilityPlot"), br()),
           div(id = "NoDisability", leafletOutput("NoDisabilityPlot"), br()),
           div(id = "auto", leafletOutput("autoPlot"), br()),
           div(id = "public", leafletOutput("publicPlot"), br()),
           div(id = "walk", leafletOutput("walkPlot"), br()),
           div(id = "other", leafletOutput("otherPlot"), br()),
           div(id = "college", leafletOutput("collegePlot"), br()),
        )
    ),
    column(6,
        wellPanel(style = "overflow-y:scroll; height: 95vh",
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
)

# Define server logic to load, map, and label selected datasets
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30*1024^2)
  
  varsToPlot <- censusVarChoices
  output$PopPlot <- renderLeaflet({makeCensusPlot("Pop", varsToPlot)})
  output$MalePlot <- renderLeaflet({makeCensusPlot("Male", varsToPlot)})
  output$FemalePlot <- renderLeaflet({makeCensusPlot("Female", varsToPlot)})
  output$WhitePlot <- renderLeaflet({makeCensusPlot("White", varsToPlot)})
  output$BlackPlot <- renderLeaflet({makeCensusPlot("Black", varsToPlot)})
  output$OtherPlot <- renderLeaflet({makeCensusPlot("Other", varsToPlot)})
  output$HispanicPlot <- renderLeaflet({makeCensusPlot("Hispanic", varsToPlot)})
  output$CitizenPlot <- renderLeaflet({makeCensusPlot("Citizen", varsToPlot)})
  output$NotCitizenPlot <- renderLeaflet({makeCensusPlot("NotCitizen", varsToPlot)})
  output$engOnlyPlot <- renderLeaflet({makeCensusPlot("engOnly", varsToPlot)})
  output$spanishPlot <- renderLeaflet({makeCensusPlot("spanish", varsToPlot)})
  output$spanishStrEPlot <- renderLeaflet({makeCensusPlot("spanishStrE", varsToPlot)})
  output$spanishWeakEPlot <- renderLeaflet({makeCensusPlot("spanishWeakE", varsToPlot)})
  output$medFamIncPlot <- renderLeaflet({makeCensusPlot("medFamInc", varsToPlot)})
  output$perCapitaIncPlot <- renderLeaflet({makeCensusPlot("perCapitaInc", varsToPlot)})
  output$PovertyPlot <- renderLeaflet({makeCensusPlot("Poverty", varsToPlot)})
  output$abovePovertyPlot <- renderLeaflet({makeCensusPlot("abovePoverty", varsToPlot)})
  output$InternetAccessPlot <- renderLeaflet({makeCensusPlot("InternetAccess", varsToPlot)})
  output$noInternetAccessPlot <- renderLeaflet({makeCensusPlot("noInternetAccess", varsToPlot)})
  output$DisabilityPlot <- renderLeaflet({makeCensusPlot("Disability", varsToPlot)})
  output$NoDisabilityPlot <- renderLeaflet({makeCensusPlot("NoDisability", varsToPlot)})
  output$autoPlot <- renderLeaflet({makeCensusPlot("auto", varsToPlot)})
  output$publicPlot <- renderLeaflet({makeCensusPlot("public", varsToPlot)})
  output$walkPlot <- renderLeaflet({makeCensusPlot("walk", varsToPlot)})
  output$otherPlot <- renderLeaflet({makeCensusPlot("other", varsToPlot)})
  output$collegePlot <- renderLeaflet({makeCensusPlot("college", varsToPlot)})
  
  observeEvent(input$varToPlot, {
    # print("hello")
    shown <- input$varToPlot
    for(item in censusVarChoices){
      if(!item %in% shown){
        # print(item)
        hide(item)
      }
    }
    for(item in shown){
      show(item)
    }
    # varsToPlot <- shown
  }, ignoreInit = FALSE)
  updateCheckboxGroupInput(session, "varToPlot", selected = c("Pop"))
  
  
  
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