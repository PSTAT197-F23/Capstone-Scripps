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
#install.packages("purrr")
library(purrr)
library(shinydashboard)
library(DT)
library(htmlwidgets)

jsfile <- "bundle.js" # the bundle.js file is in the `www` directory, pls do not change the directory name



# IMPORT DATA, obtained from CalCOFI:
source("data/data-cleaning.R")  # import data cleaning functions

whale <- read.csv("data/whale.csv")

station <- read.csv("data/CalCOFIStationOrder.csv")
edna_raw <- read.csv("data/edna-processed.csv")
edna <- clean_edna(edna_raw)
viz_raw <- read.csv("data/CalCOFI_2004-2021_Effort_OnTransectOnEffortONLY_MNA.csv")
viz <- bind_rows(clean_viz(viz_raw), clean_new_viz(new_raw))

acoustic_raw <- read.csv("data/acoustic_detections.csv")
acoustic_detections <- clean_acoustic(acoustic_raw)

station_acoustic_raw <- read.csv("data/acoustic_station.csv") # station data for plotting acoustic detentions
station_acoustic <- clean_acoustic(station_acoustic_raw)


# create dataframes for checkboxes:
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
              'Long-beaked common dolphin ',
              'Dalls porpoise',
              'Harbor porpoise',
              'Northern right whale dolphin',
              'Rough toothed dolphin ',
              'Killer whale',
              'Short-finned pilot whale',
              'False killer whale',
              rep(NA,16))
)

seasons_dataframe <- whale %>% select('Cruise', 'Season', 'Year') %>%
  distinct(Cruise, .keep_all = TRUE) %>% 
  mutate(Season = str_to_title(Season))



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
        width: 60%; /* Set the width */
        height: 40%; /* Set the height */
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
  
  dashboardPage(skin="blue",
  dashboardHeader(title="SAEL CalCOFI ShinyApp"),
  dashboardSidebar(width = 300,
                   sidebarMenu(
                     HTML(paste0(
                       "<br>",
                       "<a target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='CalCofi Logo.png' width = '186'></a>",
                       "<br>")),
                     menuItem("Home", tabName="info", icon = icon("home")),
                     menuItem("Species Map", tabName="map", icon = icon("thumbtack")),
                     menuItem("Bioacoustics", tabName="bioacoustics", icon = icon("music")),
                     menuItem("More Information", tabName="moreinfo", icon=icon("bell"))
                     
                   )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "info",
              includeMarkdown("data/welcome_page.Rmd"),
              HTML('<center><img src="opening_infographic.png", height="710", usemap="#edna_map">',
                   '<map name="edna_map">',
                   '<area shape="rect" coords="40,10,480,270" href="https://calcofi.org/sampling-info/ships/" title="CalCofi Ships">',
                   '<area shape="rect" coords="480,80,550,200" href="https://www.sciencedirect.com/science/article/pii/S0967064514002690" title="Visual Observers">',
                   '<area shape="rect" coords="50,250,200,587" href="https://calcofi.org/data/marine-ecosystem-data/e-dna/" title="CalCofi eDNA">',
                   '<area shape="rect" coords="500,130,770,320" href="https://voicesinthesea.ucsd.edu/" title="Voices in the Sea">',
                   '<area shape="rect" coords="300,380,920,680" href="https://voicesinthesea.ucsd.edu/" title="Voices in the Sea">',
                   '</map></center>'
                   )
              ),
      tabItem(tabName = "map",
              # DTOutput("myTable"), for testing, dont remove plsssssssssssssss :)
              sidebarLayout(
                sidebarPanel(id = "sidebar-container",
                             sliderInput(inputId = 'years',
                                         label = 'Years',
                                         min = min(whale$Year, na.rm = TRUE),
                                         max = max(whale$Year, na.rm = TRUE),
                                         value = c(min(whale$Year, na.rm = TRUE), 
                                                   max(whale$Year, na.rm = TRUE)),
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
                               choices = make_tree(seasons_dataframe, c("Season", "Cruise")),
                               width = "100%",
                               borders = TRUE
                             ),


                             # display sightings toggle:
                             materialSwitch(inputId = "sightings", label = "Display Sightings", status = "primary"),

                             # display stations toggle:
                             materialSwitch(inputId = "sites", label = "Display Stations", status = "warning"),

                             # display visual effort toggle:
                             materialSwitch(inputId = "viz", label = "Display Visual Effort", status = "danger"),

                             # display eDNA toggle:
                             materialSwitch(inputId = "edna", label = "Display eDNA Data", status = "success"),

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
                                           choices = c("CartoDB.Positron", "OpenStreetMap.Mapnik",
                                                       "Esri.NatGeoWorldMap", "Esri.WorldTerrain",
                                                       "Esri.WorldImagery"), #"CartoDB.DarkMatter", "Esri.WorldPhysical", "Esri.WorldImagery",
                                           #"Esri.WorldTerrain","USGS.USImageryTopo"

                                           selected = "CartoDB.Positron"),
                             ),
                             div(
                               style = "margin-bottom: 5px; text-align: left;",
                               tags$label("UI Settings:")
                             ),
                             themeSelector(),
                                        ),
                mainPanel(
                  tags$style(type = "text/css", "#mymap {height: calc(100vh - 200px) !important;}"), #this map size only applies to the interactive map
                  tags$head(tags$script(src = jsfile)), #jsfile contains the easyprint function to download map with labels
                  leafletOutput(outputId = "mymap", height="auto") #this map size is only applied to the downloaded map
                  #Using height = 'auto' causes the inconsistency in the downloaded map size
                  #However, setting the height to be dynamic is the only way to capture the current window size.
              )
      )
    ),
    tabItem(tabName = "bioacoustics",
            includeMarkdown("data/bioacoustics_page.Rmd")),
    tabItem(tabName = "moreinfo",
            includeMarkdown("data/more_info_page.Rmd"))
  ))))


# #Next, we add the server. This is where we will actually create all of our plots, and add reactivity to our inputs and outputs.
server <- function(input, output, session) {
  
  #output$myTable <- renderDT({ # for testing, dont REMOVE >:(
  #  insert data set
  #})
  
  
  observeEvent(input$resetZoom, {
    # Use leafletProxy to interact with the Leaflet map named 'mymap'
    leafletProxy("mymap", session) %>%
      setView(lng = -121, lat = 34, zoom = 6.5) # Reset to default view
  })
  
  sightingsCleared <- reactiveVal(FALSE)
  ednaCleared <- reactiveVal(FALSE)
  acousticCleared <- reactiveVal(FALSE)
  vizCleared <- reactiveVal(FALSE)
  
  # info button
  observeEvent(input$info_button, {
    showModal(modalDialog(
      title = "CalCOFI eDNA Sampling",
      div(class = "custom-modal-content",
          
          div(img(src = "edna_poster.jpg", style = "height: 60vh; width: 50vw;")), # Adjusted size using vh and vw units
          div(style = "margin-bottom: 20px;"), # Empty div for spacing
          div(style = "width: 50vw; margin: 0 auto;",
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
              assay design, and data interpretation to maximize the reliability and accuracy of eDNA-based monitoring programs for marine mammal assessment and conservation."),
              div(style = "margin-bottom: 20px;"), # Empty div for spacing
              div("Suarez-Bregua, Paula, et al. “Environmental DNA (Edna) for Monitoring Marine Mammals: Challenges and Opportunities.” Frontiers, Frontiers, 5 Sept. 2022, www.frontiersin.org/articles/10.3389/fmars.2022.987774/full.")
              
          ),
          actionButton("next_button", "Next Page")
      ),
      size = 'm',
      easyClose = TRUE,
      footer = NULL,
      class = "custom-modal" # Add custom class to the modal dialog
    ))
  })
  
  observeEvent(input$next_button, {
    showModal(modalDialog(
      title = "CalCOFI Actual Cruise Track: 2001RL Example",
      div(class = "custom-modal-content",
          div(img(src = "2001Ancil_North_actual.png", style = "height: 75vh; width: 40vw;")), # Adjusted size using vh and vw units
          div(style = "margin-bottom: 20px;"), # Empty div for spacing
          div(style = "width: 40vw; margin: 0 auto;",
              div(style = "text-align: left; padding-left: 20px; padding-right: 20px;",
                  "CalCOFI 2001RL sailed on NOAA FSV Reuben Lasker on 04 Jan 2020 at 1400PDT from 10th Avenue Marine Terminal, San Diego. 
              All 104 science stations were successfully occupied. CTD casts and various net tows were completed at each science station. 
              Underway visual observations of marine mammals were conducted while under transit and sonobuoys deployed before stations as the acoustic component. 
              Other underway science included continuous pCO2/pH and meteorological measurements. The cruise ended in San Francisco at Pier 30/32 on 26 Jan 2020 at 1300PDT."),
              div(style = "margin-bottom: 20px;"), # Empty div for spacing
              div("CalCOFI. 2001RL Cruise Summary. NOAA FSV Reuben Lasker, 04-26 Jan. 2020, 10th Avenue Marine Terminal, San Diego. Chief Scientist: Amy Hays, Technical Coordinator: Angela Klemmedson. CalCOFI, https://calcofi.org/2001rl/.")
          ),
      ),
      size = 'm',
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
  vizFilter <- reactive({filter(viz, viz$Cruise %in% input$all_cruises
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
  
  
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(lng = -121, lat = 34, zoom = 7.5) %>%
      addProviderTiles(input$provider) %>%
      onRender(
        "function(el, x) {
            this.zoomIn(1); // Increase the zoom level by 1
            this.zoomOut(1); //Atempt to resolve edge case
            var newCenter = L.latLng(this.getCenter().lat, this.getCenter().lng - 0.1);
            this.setView(newCenter, this.getZoom());
            L.easyPrint({
              sizeModes: ['Current', 'A4Landscape', 'A4Portrait'],
              filename: 'mymap',
              exportOnly: true,
              hideControlContainer: false
            }).addTo(this);
            }"
      )
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
    # Odontocetes (16)---------------------
    "Short-beaked common dolphin"= "cyan4",
    "Cuviers beaked whale"="#e66c2e",
    "Pacific white-sided dolphin"="lightpink",
    "Bairds beaked whale"="#bfef45",
    "Rissos dolphin"="#77dded",
    "Bottlenose dolphin"="#f032e6",
    "Sperm whale"= "#5e2210",
    "Striped dolphin"="#975b2b",
    "Long-beaked common dolphin "="#9100a8",
    "Dalls porpoise "="#8d9fdc",
    "Harbor porpoise"="#5f455a",
    "Northern right whale dolphin"="#41528d",
    "Rough toothed dolphin "="#a08b44",
    "Killer whale"="#469990",
    "Short-finned pilot whale"="#dcbeff",
    "False killer whale"="#efa864",
    # Mysticetes --------------------------
    "Blue whale"="#3cadde",
    "Fin whale"="#87ce43",
    "Humpback whale"="#c800e9",
    "Gray whale"="#4944b8",
    "Minke whale"="#fff8a7",
    "Sei Whale"="#164817",
    # Unidentified-------------------------
    "Unidentified common dolphin"="#935481",
    "Unidentified large whale"="#899aff",
    "Unidentified dolphin"="#dea1b9",
    "Unidentified beaked whale"="#f2baa0",
    "Unidentified small cetacean"="#9100a8",
    "Unidentified cetacean"="#8cc8c2",
    "Unidentified small whale"="#d2e595",
    "Unidentified ziphid"="#624367",
    "Unidentified odontocete"="#e8ebb5",
    "Other"="#2c2c7c"
  )
  species_levels <- as.factor(c(
    "Short-beaked common dolphin",
    "Cuviers beaked whale",
    "Pacific white-sided dolphin",
    "Bairds beaked whale",
    "Rissos dolphin",
    "Bottlenose dolphin",
    "Sperm whale",
    "Striped dolphin",
    "Long-beaked common dolphin ",
    "Dalls porpoise",
    "Harbor porpoise",
    "Northern right whale dolphin",
    "Rough toothed dolphin ",
    "Killer whale",
    "Short-finned pilot whale",
    "False killer whale",
    "Blue whale",
    "Fin whale",
    "Humpback whale",
    "Gray whale",
    "Minke whale",
    "Sei whale",
    "Unidentified common dolphin",
    "Unidentified large whale",
    "Unidentified dolphin",
    "Unidentified beaked whale",
    "Unidentified small cetacean",
    "Unidentified cetacean",
    "Unidentified small whale",
    "Unidentified ziphid",
    "Unidentified odontocete",
    "Other")
  )
  # Define the number of colors for observational whale points
  num_colors = length(unique(whale$SpeciesName))  # there are 33 unique cetacean codes in this dataset
  
  # icon for eDNA detections on map
  greenHelixIcon <- makeIcon(
    iconUrl = "https://cdn-icons-png.flaticon.com/512/922/922105.png",
    iconWidth = 35, iconHeight = 35
  )

  # icon for eDNA efforts on map
  blackHelixIcon <- makeIcon(
    iconUrl = "https://cdn-icons-png.flaticon.com/512/620/620401.png",
    iconWidth = 35, iconHeight = 35
  )
  
  # alternate icon for eDNA efforts on map (gray)
  # blackHelixIcon <- makeIcon(
  #   iconUrl = "https://cdn-icons-png.freepik.com/512/4102/4102105.png",
  #   iconWidth = 35, iconHeight = 35
  # )
  
  # observe layer for obs data reactivity
  observe({
    pal = colorFactor(palette = species_to_color, levels = species_levels)
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
                  colors = "gray",
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
  ednaEffortFilter <- reactive({filter(edna, edna$Cruise %in% input$all_cruises 
                                       & edna$Year >= input$years[1] 
                                       & edna$Year <= input$years[2])})
  
  
  ednaDetectionFilter <- reactive({
      data <- filter(edna, edna$Cruise %in% input$all_cruises 
                                          & edna$SpeciesName %in% input$all_species
                                          & edna$Year >= input$years[1] 
                                          & edna$Year <= input$years[2])
      
      transform_data <- function(df) {
        
        df_grouped <- df %>% group_by(line, station, latitude, longitude)
        
        df_transformed <- df_grouped %>%
          summarise(
            Detections = list(SpeciesName),
            .groups = 'drop' 
          )
        
        return(df_transformed)
      }
      
      format_species <- function(species_string) {
        if (length(species_string) == 1) {
          return(species_string)
        }
        combined_string <- paste(species_string, collapse = ", ")
        return(combined_string)
      }
      
      rslt <- transform_data(data)
      
      rslt$FormattedDetections <- sapply(rslt$Detections, format_species, USE.NAMES = FALSE)
      
      return(rslt)
    
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
        #get coordinates of detections
        detection_coords <- data.frame( 
          lng = as.numeric(ednaDetectionFilter()$longitude),
          lat = as.numeric(ednaDetectionFilter()$latitude)
        )
        #get coordinates of effort
        effort_coords <- data.frame(
          lng = as.numeric(ednaEffortFilter()$longitude),
          lat = as.numeric(ednaEffortFilter()$latitude)
        )
        #find coordinates where detections and efforts overlap
        overlap_coords <- merge(detection_coords, effort_coords, by = c("lng", "lat"), all = FALSE) 
        
        #filter effort markers only where there's no overlap with detections
        effort_markers <- ednaEffortFilter()[!paste(ednaEffortFilter()$longitude, ednaEffortFilter()$latitude) %in% paste(overlap_coords$lng, overlap_coords$lat), ]
        #add filtered "no overlap" effort markers
        leafletProxy("mymap", session) %>%
          addMarkers(
            lng = as.numeric(effort_markers$longitude),
            lat = as.numeric(effort_markers$latitude),
            icon = blackHelixIcon,
            popup = paste("eDNA Effort",
                          "<br>Sample Depth (m):", as.character(effort_markers$depth),
                          "<br>Line:", as.character(effort_markers$line),
                          "<br>Station:", as.character(effort_markers$station)) %>%
              lapply(htmltools::HTML),
            group = "edna"
          ) %>%
          clearControls() %>%
          addLegend("bottomleft",
                    colors = "black",
                    labels = "eDNA Effort",
                    opacity = 1,
                    layerId = "edna_effort_legend"
          )
      }
  
      # Check if acoustic legends are currently displayed
      if (input$acoustic > 0) {
        # Add acoustic effort legend if it was displayed
        leafletProxy("mymap", session) %>%
          addLegend("topleft",
                    colors = "gray",
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
            popup = paste("eDNA Detection:",as.character(ednaDetectionFilter()$FormattedDetections),
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
                    colors = "gray",
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
    iconUrl = "www/music-note-purple.png",
    iconWidth = 35, iconHeight = 35
  )
  
  
  # observe layer for acoustic effort data reactivity
  observe({
    pal = colorFactor(palette = species_to_color, levels = species_levels)
    values = obsFilter()$SpeciesName
    
    req(input$acoustic > 0)  # Require input$acoustic to be greater than 0 to proceed
    if(input$acoustic > 0) {
      leafletProxy("mymap", session) %>%
        clearGroup("acoustic") # clear existing acoustic first
      if (!is.null(acousticEffortFilter2())) {
        leafletProxy("mymap", session) %>%
          addCircles(
            
            # lng = as.numeric(acousticEffortFilter()$longitude),
            #  lat = as.numeric(acousticEffortFilter()$latitude),
            # radius = 5000,  # Adjust the radius as needed
            # color = "black",  # Border color
            # fillColor = "black",  # Fill color
            
            lng = as.numeric(acousticEffortFilter2()$Longitude),
            lat = as.numeric(acousticEffortFilter2()$Latitude),
            radius = normalize_effort(acousticEffortFilter2()$Effort)*6000, # Adjust the radius as needed
            color = "gray",  # Border color
            fillColor = "gray",  # Fill color
            
            popup = paste("Acoustic Effort",
                          "<br>Line:", as.character(acousticEffortFilter2()$Line),
                          "<br>Station:", as.character(acousticEffortFilter2()$Station),
                          "<br>Effort (hours):", as.character(acousticEffortFilter2()$Effort)) %>%
              lapply(htmltools::HTML),
            opacity = 1,
            fillOpacity = 1,
            group = "acoustic"
          ) %>%
          addLegend("topleft",
                    
                    colors = "gray",
                    
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
      
      if (!is.null(acousticDetectionFilter2()$Longitude)){
        
        leafletProxy("mymap", session) %>%
          addMarkers(
            lng = as.numeric(acousticDetectionFilter2()$Longitude),
            lat = as.numeric(acousticDetectionFilter2()$Latitude),
            icon = musicNoteIcon,
            popup = paste(gsub("\n", "<br>", acousticDetectionFilter2()$FormattedString)), 
            
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
  
  ################################
  
  
  acousticDetectionFilter2 <- reactive({
    
    data <- filter(acoustic_detections, acoustic_detections$Cruise %in% input$all_cruises 
                   & acoustic_detections$SpeciesName %in% input$all_species
                   & acoustic_detections$Year >= input$years[1] 
                   & acoustic_detections$Year <= input$years[2])
    
    
    if(nrow(data) == 0) {
      temp <- data.frame(Line = character(), Station = character(), SpeciesName = character(), Duration = numeric())
    } else {
      temp <- aggregate(Duration ~ Line + Station + SpeciesName, data = data, FUN = sum)
    }
    
    station_copy <- station
    
    # fixing line numbers
    station_copy[station_copy['Line'] == 63.3, 'Line'] = 63
    
    station_copy[station_copy['Line'] == 66.7, 'Line'] = 67
    
    station_copy[station_copy['Line'] == 73.3, 'Line'] = 73
    
    station_copy[station_copy['Line'] == 76.7, 'Line'] = 77
    
    station_copy[station_copy['Line'] == 83.3, 'Line'] = 83
    
    station_copy[station_copy['Line'] == 87.7, 'Line'] = 87
    
    station_copy[station_copy['Line'] == 93.3, 'Line'] = 93
    #station_copy$Line <- floor(station_copy$Line)
    #station_copy$Sta <- floor(station_copy$Sta)
    
    station_copy <- station_copy[, c('Line','Sta','Lat..dec.','Lon..dec.')]
    
    names(station_copy)[names(station_copy) == "Sta"] <- "Station"
    names(station_copy)[names(station_copy) == "Lon..dec."] <- "Longitude"
    names(station_copy)[names(station_copy) == "Lat..dec."] <- "Latitude"
    
    transform_data <- function(df) {
      
      df_grouped <- df %>% group_by(Line, Station, Latitude, Longitude)
      
      df_transformed <- df_grouped %>%
        summarise(
          Detections = list(map2(SpeciesName, Duration, ~c(.x, .y))),
          .groups = 'drop' 
        )
      
      return(df_transformed)
    }
    
    
    rslt<- transform_data(merge(temp, station_copy, by = c("Station", "Line")))
    
    format_detection <- function(detection) {
      paste(detection[1], ":", detection[2], "hours")
    }
    
    rslt %>%
      rowwise() %>% 
      mutate(
        FormattedString = paste(
          sprintf("Line %d, Station %s", Line, Station), 
          paste(sapply(Detections, format_detection), collapse = "\n"), 
          sep = "\n" 
        )
      )
    
    
  })
  
  
  acousticEffortFilter2 <- reactive({
    data <- filter(station_acoustic, station_acoustic$Cruise %in% input$all_cruises
                   & station_acoustic$Year >= input$years[1] 
                   & station_acoustic$Year <= input$years[2])
    
    if(nrow(data) == 0) {
      return(data.frame(Line = character(), Station = character(), Latitude = numeric(), 
                        Longitude = numeric(), Effort = numeric()))
    } else {
      return(aggregate(Effort ~ Line + Station + Latitude + Longitude, data = data, FUN = sum))
    }
  })
  

}

# #Finally, we tell R to use the user interface and the server together to build our app!
shinyApp(ui, server)