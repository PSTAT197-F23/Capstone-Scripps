## shinyApp for CalCOFI data!
# show observational data + eDNA data on map
# LMB MNA
# last udpated: 10/13/23

# load the libraries
# remotes::install_github("dreamRs/shinytreeview")
library(shiny)
library(shinyjs)
library(shinyBS)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(leaflet.extras)
library(shinythemes)
library(ggplot2)
library(stringr)
library(rapport)
library(RColorBrewer)
library(viridis)
library(htmltools)
library(rsconnect)
library(shinytreeview)
library(shinyWidgets)


species_list <- data.frame(
  Suborder = c(rep('Odontocete',16), rep('Mysticete',6), rep('Unidentified',10)),
  Family = c('Delphinidae',
             'Ziphiidae',
             'Delphinidae',
             'Ziphiidae',
             'Delphinidae',
             'Delphinidae',
             'Physeteridae',
             'Delphinidae',
             'Delphinidae',
             'Phocoenidae',
             'Phocoenidae',
             'Delphinidae',
             'Delphinidae',
             'Delphinidae',
             'Delphinidae',
             'Delphinidae',
             'Blue whale',
             'Fin whale',
             'Humpback whale',
             'Gray whale',
             'Minke whale',
             'Sei whale',
             'Unidentified common dolphin',
             'Unidentified large whale',
             'Unidentified dolphin',
             'Unidentified beaked whale',
             'Unidentified small cetacean',
             'Unidentified cetacean',
             'Unidentified small whale',
             'Unidentified ziphid',
             'Unidentified odontocete',
             'Other'
  ),
  Species = c('Short-beaked common dolphin',
              'Cuviers beaked whale',
              'Pacific white-sided dolphin',
              'Bairds beaked whale',
              'Rissos dolphin',
              'Bottlenose dolphin',
              'Sperm whale',
              'Striped dolphin',
              'Long-beaked common dolphin',
              'Dalls porpoise',
              'Harbor porpoise',
              'Northern right whale dolphin',
              'Rough toothed dolphin',
              'Killer whale',
              'Short-finned pilot whale',
              'False killer whale',
              rep(NA,16))
)

adjustSize <- function(value) {
  baseSize <- 2000
  scaledValue <- log(value)
  if (!is.na(value)){
    if (scaledValue < 1) {
      return(baseSize * 2)
    } else {
      return(baseSize * scaledValue)  
    }
  }
}


# import my data, obtained from CalCOFI
whale <- read.csv("CalCOFI_2004-2022_CombinedSightings.csv")
whale$Season <- trimws(whale$Season)
whale$Year <- as.numeric(format(as.POSIXct(whale$DateTimeLocal, format = "%m/%d/%Y %H:%M"), format='%Y'))
whale = whale[-1105,]
station <- read.csv("CalCOFIStationOrder.csv")
edna <- read.csv("edna.csv")
colnames(edna)[colnames(edna) == "year"] ="Year"
viz <- read.csv("CalCOFI_2004-2021_Effort_OnTransectOnEffortONLY_MNA.csv")
acoustic <- read.csv("acoustic-ready.csv")
#acoustic <- acoustic %>%
#  mutate(SpeciesName = ifelse(is.na(SpeciesName), NA, 
#                              sapply(SpeciesName, function(x) {
#                                x <- paste(toupper(substring(x, 1, 1)), tolower(substring(x, 2)), sep="")
#                                if(x == "Fin") "Fin whale" else x
#                              }))) 

seasons_dataframe <- data.frame(
  Season = c(rep("Summer", 18), 
             rep("Fall", 18), 
             rep("Winter", 16), 
             rep("Spring", 15)
             #rep("Cruise with eDNA data", length(unique(edna$cruise))), 
             #rep("Cruise without eDNA", length(setdiff(unique(whale$Cruise), unique(edna$cruise)))),
             #rep("Cruise with acoustic data", length(unique(acoustic$cruise)))
  ),
  Cruise_Id = c(unique(whale$Cruise[whale$Season == "summer"]), 
                unique(whale$Cruise[whale$Season == "fall"]), 
                unique(whale$Cruise[whale$Season == "winter"]), 
                unique(whale$Cruise[whale$Season == "spring"])
                #unique(edna$cruise),
                #setdiff(unique(whale$Cruise), unique(edna$cruise)),
                #unique(acoustic$cruise)
  ))

# Define the data frame for cruises with eDNA data
# cruise_edna_dataframe <- data.frame(
#   cruise_with_eDNA = rep("Cruise with eDNA", length(unique(edna$cruise))),
#   cruise_Id_edna = unique(edna$cruise)
# )

# # Define custom star marker icon
# starIcon <- makeIcon(
#   iconUrl = "http://leafletjs.com/examples/custom-icons/star.png",
#   iconWidth = 32, iconHeight = 32
# )

#build the app!

themeSelector <- function() {
  div(
    div(
      style = "display: flex; align-items: center;",
      tags$i(class = "fas fa-moon", style = "margin-right: 5px;"),  # Moon icon
      materialSwitch(inputId = "theme-toggle", label = "Dark Mode", status = "primary")
    ),
    tags$script(
      "$('#theme-toggle').on('change', function(el) {
        var theme = el.target.checked ? 'darkly' : 'flatly';
        $('link[href^=\"shinythemes/css\"]').attr('href', 'shinythemes/css/' + theme + '.min.css');
      });"
    )
  )
}



# #Begin with the user interface (ui). This is where we will create the inputs and outputs that the user will be able to interact with.
ui <- fluidPage(
  #shinythemes::themeSelector(),
  tags$head(
    tags$style(
      type = 'text/css',
      '.modal-dialog { width: fit-content !important; }'
    ),
    tags$style(HTML("
      #info_button {
        position: fixed;
        bottom: 20px;
        left: 20px;
        z-index: 1000; /* Ensure it's on top of other elements */
        width: 40px; /* Set width */
        height: 40px; /* Set height */
        border-radius: 50%; /* Make the border circular */
        display: flex; /* Use flexbox */
        justify-content: center; /* Center horizontally */
        align-items: center; /* Center vertically */
        font-size: 20px; /* Increase icon size */
      }
      #info_button span {
        margin-left: -1px; /* Adjust icon position */
      }
    "),
    tags$style(HTML("
      .custom-modal .modal-dialog {
        width: 600px; /* Set the width */
        height: 400px; /* Set the height */
      }
      .custom-modal-content {
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        text-align: center;
      }
    "))
    ),
    tags$style(HTML("
  #sidebar-container {
    height: calc(100vh - 100px); /* Adjust the 100px to account for your page's specific header/footer sizes */
    overflow-y: auto;
  }
"))
    
  ),
  
  actionButton("info_button", icon("info-circle"), style = "color: #007bff;"),
  
  #choose a CSS theme -- you can also create a custom theme if you know CSS
  theme = shinytheme("flatly"),
  #create a navigation bar for the top of the app, and give it a main title
  
  navbarPage("SAEL CalCOFI ShinyApp",
             tabPanel("Species Map",
                      tags$h2("Interactive Cetacean Species Map", align = "center"),
                      tags$h6("Species presence data from CalCOFI."),
                      sidebarLayout(
                        sidebarPanel(id = "sidebar-container",
                          sliderInput(inputId = 'years', 
                                      label = 'Years', 
                                      min = min(whale$Year, na.rm = TRUE), 
                                      max = max(whale$Year, na.rm = TRUE), 
                                      value = c(2004, 2006),
                                      step = 1,
                                      sep = "", 
                                      animate = animationOptions(
                                        interval = 500,
                                        loop = FALSE,
                                        playButton = icon("play", "fa-2x"),
                                        pauseButton = icon("pause", "fa-2x")
                                      )
                          ),
                          treecheckInput(
                            inputId =  "all_cruises",
                            label = "Choose Cruise by Season:",
                            choices = make_tree(seasons_dataframe, c("Season", "Cruise_Id")),
                            width = "100%",
                            borders = TRUE
                          ),
                          
                          
                          # treecheckInput(
                          #   inputId =  "all_cruises_eDNA",
                          #   label = "Choose Cruise by eDNA:",
                          #   choices = make_tree(cruise_edna_dataframe, c("cruise_with_eDNA", "cruise_Id_edna")),
                          #   width = "100%",
                          #   borders = TRUE
                          # ),
                          
                          
                          # display sightings toggle:
                          materialSwitch(inputId = "sightings", label = "Display Sightings", value = TRUE, status = "primary"),
                          
                          # display stations toggle:
                          materialSwitch(inputId = "sites", label = "Display Stations", status = "warning"),
                          
                          # display eDNA toggle:
                          materialSwitch(inputId = "edna", label = "Display eDNA Data", status = "success"),
                          
                          # display visual effort toggle:
                          materialSwitch(inputId = "viz", label = "Display Visual Effort", status = "danger"),
                          
                          # display acoustic data toggle:
                          materialSwitch(inputId = "acoustic", label = "Display Acoustic Data", status = "info"),
                          
                          
                          
                          
                          # add collapsible checkboxes for suborders and species:
                          
                          treecheckInput(
                            inputId = "all_species",
                            label = "Choose Species:",
                            choices = make_tree(species_list, c('Suborder', 'Family', 'Species')),
                            width = '100%',
                            borders = TRUE
                          ),
                          # Add reset map zoom button here
                          div(
                            style = "margin-bottom: 5px; text-align: left;",
                            tags$label("Map Settings:")
                          ),
                          div(
                            style = "margin-bottom: 10px; text-align: left;",  # Increase margin for more space
                            actionButton("resetZoom", "Reset Map", width = '150px',
                                         style = 'border-color: #565655; background-color: #007bff; padding: 3px')
                          ),
                          div(
                            style = "margin-bottom: 10px; text-align: left;",
                            selectInput("provider", label = "Select Map Provider:", 
                                        choices = c("CartoDB.Positron", "CartoDB.DarkMatter","OpenStreetMap.Mapnik",
                                                    "Esri.WorldPhysical", "Esri.WorldImagery",
                                                    "Esri.WorldTerrain", "Esri.NatGeoWorldMap",
                                                    "USGS.USImageryTopo"),
                                        selected = "OpenStreetMap.Mapnik"),
                          ),
                          div(
                            style = "margin-bottom: 5px; text-align: left;",
                            tags$label("UI Settings:")
                          ),
                          themeSelector(),
                        ),
                        
                        
                        
                        mainPanel(
                          tags$style(type = "text/css", "#mymap {height: calc(100vh - 200px) !important;}"),
                          
                          leafletOutput(outputId = "mymap")),
                        
                      )
             ),
             
             tabPanel("More information",
                      tags$h1("Data description"),
                      tags$h5('California Cooperative Oceanic Fisheries Investigation (CalCOFI) has been conducting marine ecosystem surveys in the California Current since 1949. More information about the CalCOFI program can be found on
                              the CalCOFI website:'), 
                      tags$a(href="https://calcofi.org/","https://calcofi.org/", style='color:#FFFFFF'),
                      tags$h5("The purpose of this Shiny App is to provide scientists with an interactive tool to 
                              visualize marine mammal data collected onboard CalCOFI. Here we integrate multiple datastreams, 
                              highlighting how marine mammal visual sightings and eDNA detections are represented through time and space. 
                              Please stay tuned for the addition of acoustic detections in a future release. By integrating visual, acoustic, and genetic sampling methods, 
                              we hope to better understand the detection capabilities of each method for detecting marine mammals in their environment."),
                      
                      
                      
                      tags$h3("CalCOFI marine mammal visual survey data"),
                      tags$h5("Marine mammal visual line-transect surveys have been conducted on quarterly CalCOFI cruises since 2004. Visual surveys are 
                              conducted during daylight hours while the ship is in transit between CalCOFI stations. More information about visual
                              survey protocol can be found in Campbell et al. (2015):"),
                      tags$a(href="https://doi.org/10.1016/j.dsr2.2014.10.008","https://doi.org/10.1016/j.dsr2.2014.10.008", style='color:#FFFFFF'),
                      tags$h5("Per-cruise marine mammal visual survey effort is visible by clicking 'Display Visual Effort'. Additionally, sighting group size estimates are visible by selecting a species from the drop-down menu, 
                              where circle size on the map is proportional to group size. Only cetacean sightings are included in this interactive map. By selecting a sighting on the map, more information will pop up about that specific sighting."),
                      
                      tags$h3("CalCOFI marine mammal eDNA data"),
                      tags$h5("The NOAA CalCOFI Genomic Program (NCOG) has collected envrionmental DNA samples (eDNA) since 2014. Here we used metabarcoding assays to 
                              detect cetacean species from water samples collected at 10, 20, or 40 meters. The 'Display eDNA' function will plot eDNA sampling effort as opaque black circles, and eDNA detections as blue flags. By selecting a detection on the map, more information about that specific detection will pop up.  "), 
                      
                      tags$h4("Co-authors"),
                      tags$h5("Michaela Alksne, Lauren Baggett, Julie Dinasquet, Bryce Ellman, 
                              Erin Satterthwaite, Brice Semmens, and Simone Baumann-Pickering "),
                      
                      tags$h4("Funding Sources"),
                      tags$h5("This material is based upon research supported by the Office of Naval Research under Award Number (N00014-22-1-2719)"),
                      tags$h5("Office of Naval Research, US Navy Pacific Fleet"),
                      
                      
             )
             
             
  )
  
  
)

# #Next, we add the server. This is where we will actually create all of our plots, and add reactivity to our inputs and outputs.
server <- function(input, output, session) {
  
  
  observeEvent(input$resetZoom, {
    # Use leafletProxy to interact with the Leaflet map named 'mymap'
    leafletProxy("mymap", session) %>%
      setView(lng = -121, lat = 34, zoom = 6.5) # Reset to default view
  })
  

  
  sightingsCleared <- reactiveVal(FALSE)
  ednaCleared <- reactiveVal(FALSE)
  acousticCleared <- reactiveVal(FALSE)
  
  # info button
  observeEvent(input$info_button, {
    showModal(modalDialog(
      title = "CalCOFI eDNA Sampling",
      div(class = "custom-modal-content",
          div(img(src = "edna_poster.jpg", height = 600, width = 900)),
          div(style = "margin-bottom: 20px;"), # Empty div for spacing
          div(style = "width: 900px; margin: 0 auto;",
              div(style = "text-align: left; padding-left: 20px; padding-right: 20px;",
                  "   Environmental DNA (eDNA) sampling for marine mammals involves collecting water samples from various locations within a study area, 
                typically ranging from coastal waters to open ocean environments. These samples are then processed to extract both intracellular and 
                extracellular DNA shed by marine mammals into their surroundings, which can include skin cells, feces, urine, and other bodily fluids. 
                Once the DNA is extracted, it undergoes amplification using polymerase chain reaction (PCR) techniques targeting specific genetic markers, 
                such as mitochondrial DNA (mtDNA) genes like the control region (D-loop), 12s rRNA gene, 16s rRNA gene, or cytochrome b.
                The amplified DNA sequences are then analyzed to detect the presence of target species, 
                assess biodiversity by identifying multiple species simultaneously through eDNA metabarcoding 
                using universal primers coupled with next-generation sequencing (NGS), and characterize intraspecific genetic diversity. 
                This eDNA approach offers a non-invasive, cost-effective, and sensitive method for monitoring marine mammal populations, especially for rare, elusive, 
                or threatened species that are challenging to detect using traditional visual and acoustic methods. However, challenges remain in optimizing sampling strategies, 
                assay design, and data interpretation to maximize the reliability and accuracy of eDNA-based monitoring programs for marine mammal assessment and conservation.")
          ),
          actionButton("next_button", "Next Page")
      ),
      size = 'l',
      easyClose = TRUE,
      footer = NULL,
      class = "custom-modal" # Add custom class to the modal dialog
    ))
  })
  
  observeEvent(input$next_button, {
    showModal(modalDialog(
      title = "CalCOFI Actual Cruise Track: 2001RL Example",
      div(class = "custom-modal-content",
          div(img(src = "2001Ancil_North_actual.png", height = 950, width = 700)),
          div(style = "margin-bottom: 20px;"), # Empty div for spacing
          div(style = "width: 900px; margin: 0 auto;",
              div(style = "text-align: left; padding-left: 20px; padding-right: 20px;",
                  "CalCOFI 2001RL sailed on NOAA FSV Reuben Lasker on 04 Jan 2020 at 1400PDT from 10th Avenue Marine Terminal, San Diego. 
                  All 104 science stations were successfully occupied. CTD casts and various net tows were completed at each science station. 
                  Underway visual observations of marine mammals were conducted while under transit and sonobuoys deployed before stations as the acoustic component. 
                  Other underway science included continuous pCO2/pH and meteorological measurements. The cruise ended in San Francisco at Pier 30/32 on 26 Jan 2020 at 1300PDT.")
          ),
    ),
      size = 'l',
      easyClose = TRUE,
      footer = NULL,
      class = "custom-modal" # Add custom class to the modal dialog
    ))
  })
  
  
  
  
  
  observeEvent(input$sites, { # when the user selects the display sites input button
    if (input$sites > 0) {
      leafletProxy("mymap", session) %>% # add a layer to the map
        clearGroup("sites") %>%
        addCircleMarkers( # add circular markers for site locations
          lng = station$Lon..dec., lat = station$Lat..dec.,
          color = 'black',
          stroke = TRUE,
          popup = paste("Line:",station$Line,
                        "<br>Station:",station$Sta) %>% # popup with information about line + station
            lapply(htmltools::HTML), # read this html code
          radius = 2,
          weight = 1,
          group = "sites"
        )
    }
  })
  
  # add layer to clear sites
  observeEvent(input$sites, { # when the user clicks the clear sites button
    if (input$sites < 1) {
      leafletProxy("mymap") %>%
        clearGroup("sites")
    }
  })
  
  # add reactive filter for visual effort per cruise
  vizFilter <- reactive({filter(viz, viz$cruise %in% input$all_cruises
                                & viz$Year >= input$years[1] 
                                & viz$Year <= input$years[2])})
  
  
  # observe event for vizEffort data reactivity
  observeEvent(input$viz, { # put cruise filtering within vizeffort observe event so that we can display visual effort 
    # per cruise. 
    
    observeEvent(input$all_cruises, {
      if (input$viz > 0) {
        leafletProxy("mymap", session) %>%
          clearGroup("viz")
        
        for (i in 1:nrow(vizFilter())) {
          leafletProxy("mymap", session) %>%
            addPolylines(
              lng = c(vizFilter()$StartLon[i], vizFilter()$EndLon[i]),
              lat = c(vizFilter()$StartLat[i], vizFilter()$EndLat[i]),
              weight = 2,
              color = "blue",
              group = "viz"  # Group name for effort polylines
            )
        }
      }
    })
  })
  
  observeEvent(input$viz, { # clear the observational line when input button is clicked
    if (input$viz < 1) {
      leafletProxy("mymap") %>%
        clearGroup("viz")
    }
  })
  
  # when user selects a suborder from "Choose suborder" dropdown, this observe function will be triggered
  # if suborder == All, then species_choices can be any of them
  # if suborder == suborder, then it filters suborder choices based on suborder column 
  
  
  # Update SELECTIZE INPUT
  
  #create the base map using leaflet
  # output$mymap <- renderLeaflet({
  #   leaflet() %>%
  #     setView(lng = -121, lat = 34, zoom = 6.5) %>%
  #     addProviderTiles(providers$CartoDB.Positron, layerId = "base")
  # })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(lng = -121, lat = 34, zoom = 6.5) %>%
      addProviderTiles(input$provider)
  })

  # OBS DATA
  # create reactivity for obs data
  # reactive expression filters dataset based on input conditions and returns filtered subset of the data
  obsFilter <- reactive({
    filter(whale , whale$Cruise %in% input$all_cruises 
           & whale$SpeciesName %in% input$all_species 
           & whale$Year >= input$years[1] 
           & whale$Year <= input$years[2])
  })
  
  
  species_to_color <- c(
    "Short-beaked common dolphin" = "cyan4",
    "Blue whale" = "darkorange3",
    "Unidentified common dolphin" = "yellow",
    "Unidentified large whale" = "springgreen3",
    "Fin whale" = "blueviolet",
    "Cuviers beaked whale" = "chartreuse1",
    "Unidentified dolphin" = "red1",
    "Pacific white-sided dolphin" = "deeppink3",
    "Bairds beaked whale" = "coral1",
    "Unidentified beaked whale" = "lightpink",
    "Rissos dolphin" = "blue1",
    "Bottlenose dolphin" = "chocolate4",
    "Sperm whale" = "gold2",
    "Striped dolphin" = "orchid1",
    "Long-beaked common dolphin" = "aquamarine1",
    "Dalls porpoise" = "burlywood1",
    "Humpback whale" = "seagreen1",
    "Harbor porpoise" = "darkolivegreen",
    "Unidentified small cetacean" = "bisque3",
    "Gray whale" = "grey27",
    "Northern right whale dolphin" = "cadetblue1",
    "Unidentofied cetacean" = "darkred",
    "Rough toothed dolphin" = "deepskyblue2",
    "Minke whale" = "orange",
    "Unidentified small whale" = "slategray3",
    "Killer whale" = "violetred1",
    "Short-finned pilot whale" = "slateblue1",
    "Unidentified ziphid" = "mistyrose",
    "False killer whale" = "darkcyan",
    "Unidentified odontocete" = "green2",
    "Sei Whale" = "plum4",
    "Other" = "azure2"
  )
  
  # Define the number of colors for observational whale points
  num_colors = length(unique(whale$SpeciesName))  # there are 33 unique cetacean codes in this dataset
  
  # icon for eDNA detections on map
  greenHelixIcon <- makeIcon(
    iconUrl = "https://cdn-icons-png.flaticon.com/512/922/922105.png",
    iconWidth = 35, iconHeight = 35
  )
  
  
  # observe layer for obs data reactivity
  observe({
    pal = colorFactor(palette = species_to_color, levels = as.factor(unique(whale$SpeciesName)))
    values = obsFilter()$SpeciesName
    
    if (input$sightings > 0) {
      leafletProxy("mymap") %>%
        clearGroup("sightings") %>%
        clearControls() %>%
        addMapPane("layer1", zIndex = 430) %>%
        addCircles(data = obsFilter(),
                   lat = as.numeric(obsFilter()$DecLat), lng = as.numeric(obsFilter()$DecLong),
                   # radius = 5000,
                   color = ~pal(values),
                   fillColor = ~pal(values),
                   radius = sapply(obsFilter()$Best, adjustSize),
                   popup = ~paste("Sighting:",as.character(obsFilter()$SpeciesName),
                                  "<br>Group Size Estimate:", as.character(obsFilter()$Best),
                                  "<br>Date (Local):",as.character(obsFilter()$DateTimeLocal),
                                  "<br>Lat:",as.character(obsFilter()$DecLat)," Lon:",as.character(obsFilter()$DecLong)) %>%
                     lapply(htmltools::HTML),
                   opacity = 1,
                   fillOpacity = 0.7,
                   group = "sightings",
                   options = pathOptions(pane = "layer1", weight = 1)
        )
    } %>% 
      addLegend("topright", pal = pal, values = values, group="sightings", title="Cetacean visual sightings",layerId = "sightings_legend")
    # Check if eDNA legends are currently displayed
    if (input$edna > 0) {
      # Add eDNA effort legend if it was displayed
      leafletProxy("mymap", session) %>%
        addLegend("bottomleft",
                  colors = "black",
                  labels = "eDNA Effort",
                  opacity = 1,
                  layerId = "edna_effort_legend",
        )
      
      # Add eDNA detection legend if it was displayed
      leafletProxy("mymap", session) %>%
        addLegend("bottomleft",
                  colors = "lightgreen",
                  labels = "eDNA Detection",
                  opacity = 1,
                  layerId = "edna_detection_legend"
        ) # This might seem counter intuitive, but it is to 
      # take care of the edge case where selecting all will clear eDNA legends.
    }
    # Check if acoustic legends are currently displayed
    if (input$acoustic > 0) {
      # Add acoustic effort legend if it was displayed
      leafletProxy("mymap", session) %>%
        addLegend("topleft",
                  colors = "black",
                  labels = "Acoustic Effort",
                  opacity = 1,
                  layerId = "acoustic_effort_legend"
        )
      
      # Add acoustic detection legend if it was displayed
      leafletProxy("mymap", session) %>%
        addLegend("topleft",
                  colors = "#6D00BE",
                  labels = "Acoustic Detection",
                  opacity = 1,
                  layerId = "acoustic_detection_legend"
        ) # This might seem counter intuitive, but it is to 
      # take care of the edge case where selecting all will clear acoustic legends.
    }
    
  })
  
  # observe event for clearing sightings
  observeEvent(input$sightings, {
    req(input$sightings < 1)  # Require input$sightings to be off
    leafletProxy("mymap", session) %>%
      clearGroup("sightings") %>% 
      removeControl("sightings_legend") # Remove legend associated with sightings
    sightingsCleared(TRUE)
  })
  
  observeEvent(input$sightings, {
    sightingsCleared(FALSE)
  })
  
  
  # eDNA effort filter for plotting eDNA effort per cruise. Plot as black circle 
  ednaEffortFilter <- reactive({filter(edna, edna$cruise %in% input$all_cruises 
                                       & edna$Year >= input$years[1] 
                                       & edna$Year <= input$years[2])})
  
  
  ednaDetectionFilter <- reactive({filter(edna, edna$cruise %in% input$all_cruises & edna$SpeciesName!="NA" 
                                          & edna$Year >= input$years[1] 
                                          & edna$Year <= input$years[2])
    
  })
  
  # observe layer for eDNA effort data reactivity
  observe({
    pal = colorFactor(palette = species_to_color, levels = as.factor(unique(whale$SpeciesName)))
    values = obsFilter()$SpeciesName
    
    req(input$edna > 0)  # Require input$edna to be greater than 0 to proceed
    if(input$edna > 0) {
      leafletProxy("mymap", session) %>%
        clearGroup("edna") # clear existing edna first
      if (!is.null(ednaEffortFilter()$longitude)) {
        leafletProxy("mymap", session) %>%
          addCircles(
            lng = as.numeric(ednaEffortFilter()$longitude),
            lat = as.numeric(ednaEffortFilter()$latitude),
            radius = 5000,  # Adjust the radius as needed
            color = "black",  # Border color
            fillColor = "black",  # Fill color
            popup = paste("eDNA Effort",
                          "<br>Sample Depth (m):", as.character(ednaEffortFilter()$depth),
                          "<br>Line:", as.character(ednaEffortFilter()$line),
                          "<br>Station:", as.character(ednaEffortFilter()$station)) %>%
              lapply(htmltools::HTML),
            opacity = 1,
            fillOpacity = 1,
            group = "edna"
          ) %>%
          clearControls() %>%
          addLegend("bottomleft",
                    colors = "black",
                    labels = "eDNA Effort",
                    opacity = 1,
                    layerId = "edna_effort_legend"
          )}
      # Check if acoustic legends are currently displayed
      if (input$acoustic > 0) {
        # Add acoustic effort legend if it was displayed
        leafletProxy("mymap", session) %>%
          addLegend("topleft",
                    colors = "black",
                    labels = "Acoustic Effort",
                    opacity = 1,
                    layerId = "acoustic_effort_legend"
          )
        
        # Add acoustic detection legend if it was displayed
        leafletProxy("mymap", session) %>%
          addLegend("topleft",
                    colors = "#6D00BE",
                    labels = "Acoustic Detection",
                    opacity = 1,
                    layerId = "acoustic_detection_legend"
          ) # This might seem counter intuitive, but it is to 
        # take care of the edge case where selecting all will clear eDNA legends.
      }
      # Add sightings legend if it was displayed
      if (input$sightings > 0) {
        leafletProxy("mymap", session) %>%
          addLegend("topright", pal = pal, values = values, group="sightings", title="Cetacean visual sightings", layerId = "sightings_legend")}
    }
    
  })
  
  # observe layer for eDNA detection data reactivity
  observe({
    pal = colorFactor(palette = species_to_color, levels = as.factor(unique(whale$SpeciesName)))
    values = obsFilter()$SpeciesName
    
    req(input$edna > 0)  # Require input$edna to be greater than 0 to proceed
    if(input$edna > 0) {
      leafletProxy("mymap", session) %>%
        clearGroup("edna_detection") # clear existing edna detection
      if (!is.null(ednaDetectionFilter()$longitude)){
        leafletProxy("mymap", session) %>%
          addMarkers(
            lng = as.numeric(ednaDetectionFilter()$longitude),
            lat = as.numeric(ednaDetectionFilter()$latitude),
            icon = greenHelixIcon,
            popup = paste("eDNA Detection:",as.character(ednaDetectionFilter()$SpeciesName),
                          "<br>Sample Depth (m):", as.character(ednaDetectionFilter()$depth),
                          "<br>Line:",as.character(ednaDetectionFilter()$line),
                          "<br>Station:",as.character(ednaDetectionFilter()$station)) %>%
              lapply(htmltools::HTML), 
            group = "edna_detection"
          ) %>%
          addLegend("bottomleft",
                    colors = "lightgreen",
                    labels = "eDNA Detection",
                    opacity = 1,
                    layerId = "edna_detection_legend"
          )
      }
      # Check if acoustic legends are currently displayed
      if (input$acoustic > 0) {
        # Add acoustic effort legend if it was displayed
        leafletProxy("mymap", session) %>%
          addLegend("topleft",
                    colors = "black",
                    labels = "Acoustic Effort",
                    opacity = 1,
                    layerId = "acoustic_effort_legend"
          )
        
        # Add acoustic detection legend if it was displayed
        leafletProxy("mymap", session) %>%
          addLegend("topleft",
                    colors = "#6D00BE",
                    labels = "Acoustic Detection",
                    opacity = 1,
                    layerId = "acoustic_detection_legend"
          ) # This might seem counter intuitive, but it is to 
        # take care of the edge case where selecting all will clear eDNA legends.
      }
      # Add sightings legend if it was displayed
      if (input$sightings > 0) {
        leafletProxy("mymap", session) %>%
          addLegend("topright", pal = pal, values = values, group="sightings", title="Cetacean visual sightings", layerId = "sightings_legend")}
    }
  })
  
  # observe event for clearing eDNA data
  observeEvent(input$edna, {
    req(input$edna < 1)  # Require input$clearedna to be greater than 0 to proceed
    leafletProxy("mymap", session) %>%
      clearGroup("edna") %>%
      clearGroup("edna_detection") %>%
      removeControl("edna_effort_legend") %>%
      removeControl("edna_detection_legend")# Remove both legends associated with eDNA data
    ednaCleared(TRUE)
  })
  
  observeEvent(input$edna, {
    ednaCleared(FALSE)
  })
  
  
  ############################################################
  
  # icon for acoustic detections on map
  musicNoteIcon <- makeIcon(
    iconUrl = "assests/music-note-purple.png",
    iconWidth = 35, iconHeight = 35
  )
  
  # acoustic effort filter for plotting acoustic effort per cruise. Plot as black circle 
  acousticEffortFilter <- reactive({filter(acoustic, acoustic$cruise %in% input$all_cruises
                                           & acoustic$SpeciesName %in% input$all_species
                                           & acoustic$Year >= input$years[1] 
                                           & acoustic$Year <= input$years[2])})
  
  acousticDetectionFilter <- reactive({
    
    filter(acoustic, acoustic$cruise %in% input$all_cruises 
           & acoustic$SpeciesName %in% input$all_species
           & acoustic$Year >= input$years[1] 
           & acoustic$Year <= input$years[2])
    
  })
  
  # observe layer for acoustic effort data reactivity
  observe({
    pal = colorFactor(palette = species_to_color, levels = as.factor(unique(whale$SpeciesName)))
    values = obsFilter()$SpeciesName
    
    req(input$acoustic > 0)  # Require input$acoustic to be greater than 0 to proceed
    if(input$acoustic > 0) {
      leafletProxy("mymap", session) %>%
        clearGroup("acoustic") # clear existing acoustic first
      if (!is.null(acousticEffortFilter()$longitude)) {
        leafletProxy("mymap", session) %>%
          addCircles(
            lng = as.numeric(acousticEffortFilter()$longitude),
            lat = as.numeric(acousticEffortFilter()$latitude),
            radius = 5000,  # Adjust the radius as needed
            color = "black",  # Border color
            fillColor = "black",  # Fill color
            popup = paste("Acoustic Effort",
                          "<br>Line:", as.character(acousticEffortFilter()$line),
                          "<br>Station:", as.character(acousticEffortFilter()$station)) %>%
              lapply(htmltools::HTML),
            opacity = 1,
            fillOpacity = 1,
            group = "acoustic"
          ) %>%
          addLegend("topleft",
                    colors = "black",
                    labels = "Acoustic Effort",
                    opacity = 1,
                    layerId = "acoustic_effort_legend"
          )
      }
      if (input$edna > 0) {
        # Add eDNA effort legend if it was displayed
        leafletProxy("mymap", session) %>%
          addLegend("bottomleft",
                    colors = "black",
                    labels = "eDNA Effort",
                    opacity = 1,
                    layerId = "edna_effort_legend"
          )
        
        # Add eDNA detection legend if it was displayed
        leafletProxy("mymap", session) %>%
          addLegend("bottomleft",
                    colors = "lightgreen",
                    labels = "eDNA Detection",
                    opacity = 1,
                    layerId = "edna_detection_legend"
          ) # This might seem counter intuitive, but it is to 
        # take care of the edge case where selecting all will clear eDNA legends.
      }
      if (input$sightings > 0) {
        leafletProxy("mymap", session) %>%
          addLegend("topright", pal = pal, values = values, group="sightings", title="Cetacean visual sightings", layerId="sightings_legend")}
    }
  })
  
  # observe layer for acoustic detection data reactivity
  observe({
    pal = colorFactor(palette = species_to_color, levels = as.factor(unique(whale$SpeciesName)))
    values = obsFilter()$SpeciesName
    
    req(input$acoustic > 0)  # Require input$acoustic to be greater than 0 to proceed
    if(input$acoustic > 0) {
      leafletProxy("mymap", session) %>%
        clearGroup("acoustic_detection") # clear existing acoustic detection
      if (!is.null(acousticDetectionFilter()$longitude)){
        # Define jitter amount
        #jitter_amount <- 0.045  # Adjust this value as needed
        
        # Apply jitter to longitude and latitude
        #jittered_lng <- as.numeric(acousticDetectionFilter()$longitude) + runif(length(acousticDetectionFilter()$longitude), -jitter_amount, jitter_amount)
        #jittered_lat <- as.numeric(acousticDetectionFilter()$latitude) + runif(length(acousticDetectionFilter()$latitude), -jitter_amount, jitter_amount)
        
        leafletProxy("mymap", session) %>%
          addMarkers(
            lng = as.numeric(acousticDetectionFilter()$longitude),
            lat = as.numeric(acousticDetectionFilter()$latitude),
            icon = musicNoteIcon,
            popup = paste("Acoustic Detection:", as.character(acousticDetectionFilter()$SpeciesName),
                          "<br>Duration (hours):", as.character(acousticDetectionFilter()$duration),
                          "<br>Line:", as.character(acousticDetectionFilter()$line),
                          "<br>Station:", as.character(acousticDetectionFilter()$station)) %>%
              lapply(htmltools::HTML), 
            group = "acoustic_detection"
          ) %>%
          addLegend("topleft",
                    colors = "#6D00BE",
                    labels = "Acoustic Detection",
                    opacity = 1,
                    layerId = "acoustic_detection_legend"
          )
      }
      if (input$edna > 0) {
        # Add eDNA effort legend if it was displayed
        leafletProxy("mymap", session) %>%
          addLegend("bottomleft",
                    colors = "black",
                    labels = "eDNA Effort",
                    opacity = 1,
                    layerId = "edna_effort_legend"
          )
        
        # Add eDNA detection legend if it was displayed
        leafletProxy("mymap", session) %>%
          addLegend("bottomleft",
                    colors = "lightgreen",
                    labels = "eDNA Detection",
                    opacity = 1,
                    layerId = "edna_detection_legend")}
      if (input$sightings > 0) {
        leafletProxy("mymap", session) %>%
          addLegend("topright", pal = pal, values = values, group="sightings", title="Cetacean visual sightings", layerId = "sightings_legend")}
    }
  })
  
  
  
  # observe event for clearing acoustic data
  observeEvent(input$acoustic, {
    req(input$acoustic < 1) # Require input$clearacoustic to be greater than 0 to proceed
    leafletProxy("mymap", session) %>%
      clearGroup("acoustic") %>%
      clearGroup("acoustic_detection") %>%
      removeControl("acoustic_effort_legend") %>%
      removeControl("acoustic_detection_legend") # Remove both legends associated with acoustic data
    acousticCleared(TRUE)
  })
  
  observeEvent(input$acoustic, {
    acousticCleared(FALSE)
  })
  
  
}

# #Finally, we tell R to use the user interface and the server together to build our app!
shinyApp(ui, server)