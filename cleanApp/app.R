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
source('census_data.R')
source('model.R')

#To enable labels: uncomment lines 86 and 87


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
    #st_transform(crs = 4326) %>% #Defines the geography info format
    leaflet() %>% #Creates leaflet pane
    addProviderTiles(provider = "CartoDB.Positron") %>% #No clue what this does tbh
    addPolygons(#Creates the polygons to overlay on the map, with parameters
      weight = 1,
      color = "#000000",
      opacity = 1,
      smoothFactor = 0,
      fillOpacity = 0.7,
      fillColor = ~ pal(get(item)),
      # label = ~round(get(item), 3),
      # labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE),
      popup = popupHTML, #Add the label
      highlightOptions = highlightOptions(color = "#FFFFFF", weight = 2, bringToFront = TRUE)
    ) %>%
    addLegend("bottomright", #Adds the legend
              pal = pal,
              values = ~ get(item),
              title = item,
              opacity = 1
    )
  return(censusPlot)
}

ui <- fluidPage(
  useShinyjs(),
  column(4, style = "padding: 5px; width: 30%", #Forcibly set width so the overall width stays the same when changing the scooterCol width
         wellPanel(style = "overflow-y:scroll; height: 95vh", #Independently scroll this column
                   fileInput("demandTRACT", "Choose demandTRACT.csv", #Two CSV only file inputs
                             multiple = FALSE,
                             accept = c("text/csv", "text/comma-separated-values")),
                   fileInput("demandLatLng", "Choose demandLatLng.csv",
                             multiple = FALSE,
                             accept = c("text/csv", "text/comma-separated-values")),
                   dateRangeInput("availabilityDateRange", "Date Range to Include", #Date range picker for filtering
                                  start = "2019-5-01", #Default start date
                                  end = "2019-9-30", #Default end date
                                  min = "2018-11-01", #Earliest allowed start date
                                  max = "2019-10-31" #Latest allowed end date
                   ),
                   checkboxInput("includeWeekdays", "Include Weekdays", value = TRUE), #Weekend and weekday filtering checkboxes
                   checkboxInput("includeWeekends", "Include Weekends", value = TRUE),
                   radioButtons("tractOrLatLng", "Tract or Lat/Long?", choices = c("Model by Tract" = "tract", "Model by Latitude and Longitude" = "latLng")), #Tract/latlng picker
                   radioButtons("zCol", "Scooter Variable", choices = scooterVarChoices), #Scooter variables
                   bsTooltip("zCol", zColTooltipText), #Explainer for above zcol picker
                   checkboxGroupInput("varToPlot", "Census Variables:", choices = censusVarChoices, selected = c()), #Defines which checkboxes to use
                   p("Built by Nolan Flynn, Marion Madanguit, Hyunkyung Rho, Maeve Stites, and Alice Paul") #Hi!
         )
  ),
  column(4, id = "censusCol", style = "padding: 5px; width: 35%", #Forcibly set width so the overall width stays the same when changing the scooterCol width
         wellPanel(style = "overflow-y:scroll; height: 95vh", #Independently scroll this column
                   h3('Census Maps'),
                   
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
  column(4, id = "scooterCol", style = "padding: 5px; width: 35%", #Forcibly set width so the overall width stays the same when changing the scooterCol width
         wellPanel(style = "overflow-y:scroll; height: 95vh", #Independently scroll this column
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
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30*1024^2) #Increase the max CSV upload size
  
  output$PopPlot <- renderLeaflet({makeCensusPlot("Pop")}) #Render the output of makeCensusPlot(), at top of file, into each plot
  output$MalePlot <- renderLeaflet({makeCensusPlot("Male")})
  output$FemalePlot <- renderLeaflet({makeCensusPlot("Female")})
  output$WhitePlot <- renderLeaflet({makeCensusPlot("White")})
  output$BlackPlot <- renderLeaflet({makeCensusPlot("Black")})
  output$OtherPlot <- renderLeaflet({makeCensusPlot("Other")})
  output$HispanicPlot <- renderLeaflet({makeCensusPlot("Hispanic")})
  output$CitizenPlot <- renderLeaflet({makeCensusPlot("Citizen")})
  output$NotCitizenPlot <- renderLeaflet({makeCensusPlot("NotCitizen")})
  output$engOnlyPlot <- renderLeaflet({makeCensusPlot("engOnly")})
  output$spanishPlot <- renderLeaflet({makeCensusPlot("spanish")})
  output$spanishStrEPlot <- renderLeaflet({makeCensusPlot("spanishStrE")})
  output$spanishWeakEPlot <- renderLeaflet({makeCensusPlot("spanishWeakE")})
  output$medFamIncPlot <- renderLeaflet({makeCensusPlot("medFamInc")})
  output$perCapitaIncPlot <- renderLeaflet({makeCensusPlot("perCapitaInc")})
  output$PovertyPlot <- renderLeaflet({makeCensusPlot("Poverty")})
  output$abovePovertyPlot <- renderLeaflet({makeCensusPlot("abovePoverty")})
  output$InternetAccessPlot <- renderLeaflet({makeCensusPlot("InternetAccess")})
  output$noInternetAccessPlot <- renderLeaflet({makeCensusPlot("noInternetAccess")})
  output$DisabilityPlot <- renderLeaflet({makeCensusPlot("Disability")})
  output$NoDisabilityPlot <- renderLeaflet({makeCensusPlot("NoDisability")})
  output$autoPlot <- renderLeaflet({makeCensusPlot("auto")})
  output$publicPlot <- renderLeaflet({makeCensusPlot("public")})
  output$walkPlot <- renderLeaflet({makeCensusPlot("walk")})
  output$otherPlot <- renderLeaflet({makeCensusPlot("other")})
  output$collegePlot <- renderLeaflet({makeCensusPlot("college")})
  
  observeEvent(input$varToPlot, {
    shown <- input$varToPlot
    for(item in censusVarChoices){
      if(!item %in% shown){
        hide(item) #Hide anything not in input$varToPlot
      }
    }
    for(item in shown){
      show(item) #Show everything in input$varToPlot
    }
    if(is.null(shown)){ #If no census variables checked, hide the census column and make the scooter column wider
      hide("censusCol")
      shinyjs::runjs("document.getElementById('scooterCol').style.width = '70%';")
    } else { #If any census variables checked, show the census column and shrink the scooter column to its original size
      show("censusCol")
      shinyjs::runjs("document.getElementById('scooterCol').style.width = '35%';")
    }
  }, ignoreInit = TRUE, ignoreNULL = FALSE) #Don't run this function on init, and run this function even when input$varToPlot is empty, aka no census variables checked
  updateCheckboxGroupInput(session, "varToPlot", selected = c("Pop")) #After the initial rendering, during which the leaflet outputs must be shown cuz otherwise they're blank, update the selected variables, which triggers the above hiding function
  
  
  
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