## shinyApp for CalCOFI data!
# show observational data + eDNA data on map
# LMB MNA
# last udpated: 10/13/23

# load the libraries
library(shiny)
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

# import my data, obtained from CalCOFI
whale <- read.csv("CalCOFI_2004-2022_CombinedSightings.csv")
whale$Season <- trimws(whale$Season)
whale = whale[-1105,]
station <- read.csv("CalCOFIStationOrder.csv")
edna <- read.csv("edna.csv")
viz <- read.csv("CalCOFI_2004-2021_Effort_OnTransectOnEffortONLY_MNA.csv")

# # Define custom star marker icon
# starIcon <- makeIcon(
#   iconUrl = "http://leafletjs.com/examples/custom-icons/star.png",
#   iconWidth = 32, iconHeight = 32
# )

#build the app!

# #Begin with the user interface (ui). This is where we will create the inputs and outputs that the user will be able to interact with.
ui <- fluidPage(
  #choose a CSS theme -- you can also create a custom theme if you know CSS
  theme = shinytheme("darkly"),
  #create a navigation bar for the top of the app, and give it a main title
  navbarPage("SAEL CalCOFI ShinyApp",
             #add the first tab panel (tab1) and annotate -- the tags$h command adds text at different sizes
             tabPanel("Species Map",
                      tags$h2("Interactive Cetacean Species Map", align = "center"),
                      tags$h6("Species presence data from CalCOFI."),
                      #create the sidebar that will hold the input functions that we add
                      sidebarLayout(
                        sidebarPanel(
                          #create inputs for species and date
                          # add input for observational data
                          # add action button for site markers
                          selectizeInput("cruise", "Choose CalCOFI Cruise (yy-mm):",
                                         choices = NULL, multiple = FALSE),
                          actionButton("sites", "Display Stations", width = '150px',
                                       style='border-color: #565655;
                                       background-color: #F47831;
                                       padding:3px'),
                          
                          # add action button to clear site markers
                          actionButton("clearsites", "Clear Stations",
                                       width = '150px',
                                       style='border-color: #565655;
                                       background-color: #F47831;
                                       padding:3px'),
                          
                          actionButton("edna", "Display eDNA Data", width = '150px',
                                       style='border-color: #565655;
                                       background-color: #008080;
                                       padding:3px'),
                          
                        #  HTML("<br>eDNA processed for cruises CC1402-CC1611"),  # Add your description here
                          
                          # add action button to clear site markers
                          actionButton("clearedna", "Clear eDNA Data",
                                       width = '150px',
                                       style='border-color: #565655;
                                       background-color: #008080;
                                       padding:3px'),
                          

                          # add action button for viz markers
                          actionButton("viz", "Display Visual Effort", width = '150px',
                                       style='border-color: #565655;
                                       background-color: #FF69B4;
                                       padding:3px'),
                          
                          # add action button to clear site markers
                          actionButton("clearviz", "Clear Visual Effort",
                                       width = '150px',
                                       style='border-color: #565655;
                                       background-color: #FF69B4;
                                       padding:3px'),
                        
                        selectInput("season", "Filter by Season:",
                                    choices = c("spring", "summer", "fall", "winter"), selected = "fall"),
                          
                          selectizeInput("suborder", "Choose cetacean suborder:",
                                         choices = c("All", unique(whale$SubOrder)), selected = "All"),  # Include "All" option
                          conditionalPanel( # creates a panel that is visible depending on conditional expression 
                            condition = 'input.suborder != "All"',  # Only show when a specific suborder is selected (not equal to "All")
                            checkboxGroupInput("species", "Select Species within Suborder:",
                                               choices = NULL, selected = NULL)
                          ),
                          conditionalPanel( 
                            condition = 'input.suborder == "All"',  # Show when "All" is selected
                            checkboxGroupInput("all_species", "Select All Species:",
                                               choices = NULL, selected = NULL)
                          ),
                        
                        
                          
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
  
  observeEvent(input$sites, { # when the user selects the display sites input button
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
    })
  
  # add layer to clear sites
  observeEvent(input$clearsites, { # when the user clicks the clear sites button
    leafletProxy("mymap") %>%
      clearGroup("sites")
  })
  
  # add reactive filter for visual effort per cruise
  vizFilter <- reactive({
    filter(viz, viz$cruise == input$cruise)
  })

  # observe event for vizEffort data reactivity
  observeEvent(input$viz, { # put cruise filtering within vizeffort observe event so that we can display visual effort 
    # per cruise. 
    
    observeEvent(input$cruise, {
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
  
  observeEvent(input$clearviz, { # clear the observational line when input button is clicked
    if (input$clearviz > 0) {
      leafletProxy("mymap") %>%
        clearGroup("viz")
    }
  })
  

  # when user selects a suborder from "Choose suborder" dropdown, this observe function will be triggered
  # if suborder == All, then species_choices can be any of them
  # if suborder == suborder, then it filters suborder choices based on suborder column 
  observe({
    # Update species choices based on selected suborder
    if (input$suborder == "All") {
      species_choices <- unique(whale$SpeciesName)
    } else {
      species_choices <- unique(whale$SpeciesName[whale$SubOrder == input$suborder])
    }
    
    # update checkbox inputs based on suborder selection
    if (input$suborder == "All") {
      updateCheckboxGroupInput(session, "all_species", choices = species_choices, selected = NULL)
      updateCheckboxGroupInput(session, "species", choices = NULL, selected = NULL)
    } else {
      updateCheckboxGroupInput(session, "all_species", choices = NULL, selected = NULL)
      updateCheckboxGroupInput(session, "species", choices = species_choices, selected = NULL)
    }
  })
  
  # Update SELECTIZE INPUT
  updateSelectizeInput(session = session, "cruise",
                       choices = unique(whale$Cruise),
                       selected = unique(whale$Cruise)[1], server = TRUE)
  
  # create the base map using leaflet
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(lng = -121, lat = 34, zoom = 6.5) %>%
      addProviderTiles(providers$CartoDB.Positron, layerId = "base")
  })
  
  # OBS DATA
  # create reactivity for obs data
  # reactive expression filters dataset based on input conditions and returns filtered subset of the data
  obsFilter <- reactive({
    if (input$suborder == "All") {
      filter(whale, whale$Season == input$season & whale$Cruise == input$cruise & whale$SpeciesName %in% input$all_species)
    } else {
      filter(whale, whale$Season == input$season & whale$Cruise == input$cruise & whale$SubOrder == input$suborder & whale$SpeciesName %in% input$species)
    }
  })
  
  # Define the number of colors for observational whale points
  num_colors = length(unique(whale$SpeciesName))  # there are 33 unique cetacean codes in this dataset
  # Generate the full viridis turbo palette
  full_palette = viridis(256, option = "turbo")
  # Randomly shuffle the colors
  set.seed(1)  # Set a seed for reproducibility
  random_colors = sample(full_palette, num_colors)
  
  # observe layer for obs data reactivity
  observe({
    pal = colorFactor(palette = random_colors, levels = as.factor(unique(whale$SpeciesName)))
    values = obsFilter()$SpeciesName
    
    leafletProxy("mymap") %>%
      clearGroup("sightings") %>%
      #clearGroup("edna") %>%
      clearControls() %>%
      addMapPane("layer1", zIndex = 430) %>%
      addCircles(data = obsFilter(),
                 lat = as.numeric(obsFilter()$DecLat), lng = as.numeric(obsFilter()$DecLong),
                 # radius = 5000,
                 color = ~pal(values),
                 fillColor = ~pal(values),
                 radius = (log(obsFilter()$Best))*2000,
                 popup = ~paste("Sighting:",as.character(obsFilter()$SpeciesName),
                                "<br>Group Size Estimate:", as.character(obsFilter()$Best),
                                "<br>Date (Local):",as.character(obsFilter()$DateTimeLocal),
                                "<br>Lat:",as.character(obsFilter()$DecLat)," Lon:",as.character(obsFilter()$DecLong)) %>%
                   lapply(htmltools::HTML),
                 opacity = 0.7,
                 fillOpacity = 0.7,
                 group = "sightings",
                 options = pathOptions(pane = "layer1")
      ) %>%
      addLegend("topright", pal = pal, values = values, group="sightings", title="Cetacean visual sightings")
  })

# filter eDNA data
  # ednaFilter <- reactive({
  #   if (input$suborder == "All") {
  #     filter(edna, edna$cruise == input$cruise & edna$SpeciesName %in% input$all_species)
  #   } else {
  #   filter(edna, edna$cruise == input$cruise & edna$SubOrder == input$suborder & edna$SpeciesName %in% input$species)
  #   }
  # 
  # })
  
  # eDNA effort filter for plotting eDNA effort per cruise. Plot as black circle 
  ednaEffortFilter <- reactive({
    
      filter(edna, edna$cruise == input$cruise)
  })

  #print(str(ednaEffortFilter()))
  
    ednaDetectionFilter <- reactive({
      
     filter(edna, edna$cruise == input$cruise & edna$SpeciesName!="NA")

    })
  

    # observe layer for eDNA effort data reactivity
    observe({
      if (input$edna > 0) {
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
            )
        }
      } 
    })
      observeEvent(input$clearedna, {
    if (input$clearedna > 0) {
      leafletProxy("mymap") %>%
        clearGroup("edna")
    }
  })
    
# observe layer for eDNA detection data reactivity
  observe({
  
    if (input$edna > 0) {
     # leafletProxy("mymap", session) %>%
       # clearGroup("edna") # clear existing edna first
      if (!is.null(ednaDetectionFilter()$longitude)){
        html_legend <- "<img src='http://leafletjs.com/examples/custom-icons/default.png'>eDNA Detection"
        
        leafletProxy("mymap", session) %>%
          addMarkers(
            lng = as.numeric(ednaDetectionFilter()$longitude),
            lat = as.numeric(ednaDetectionFilter()$latitude),
            popup = paste("eDNA Detection:",as.character(ednaDetectionFilter()$SpeciesName),
                          "<br>Sample Depth (m):", as.character(ednaDetectionFilter()$depth),
                          "<br>Line:",as.character(ednaDetectionFilter()$line),
                          "<br>Station:",as.character(ednaDetectionFilter()$station)) %>%
              lapply(htmltools::HTML), 
            group = "edna") #%>%
      }
     }
    })
    
  observeEvent(input$clearedna, {
    if (input$clearedna > 0) {
      leafletProxy("mymap") %>%
        clearGroup("edna")
    }
  })

}

# #Finally, we tell R to use the user interface and the server together to build our app!
shinyApp(ui, server)