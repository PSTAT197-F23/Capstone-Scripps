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
library(purrr)
library(shinydashboard)
library(DT)
library(htmlwidgets)

jsfile <- "bundle.js" # the bundle.js file is in the `www` directory, pls do not change the directory name



# IMPORT DATA, obtained from CalCOFI:
source("data/data-cleaning.R")  # import data cleaning functions


new_raw <- read.csv("data/whale_visual_data/new-marine-mammal-data/CC-202311.txt", header = TRUE, na.strings = "NA")
whale <- read.csv("data/whale_visual_data/whale.csv")

station <- read.csv("data/CalCOFIStationOrder.csv")

edna_raw <- read.csv("data/edna_data/edna-processed.csv")
edna <- clean_edna(edna_raw)

viz <- read.csv("data/whale_visual_data/transect.csv")

acoustic_raw <- read.csv("data/acoustic_data/acoustic_detections.csv")
acoustic_detections <- clean_acoustic(acoustic_raw)

station_acoustic_raw <- read.csv("data/acoustic_data/acoustic_station.csv") # station data for plotting acoustic detentions
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
                     menuItem("Interactive Species Map", tabName="map", icon = icon("thumbtack")),
                     menuItem("Bioacoustics", tabName="bioacoustics", icon = icon("music")),
                     menuItem("More Information", tabName="moreinfo", icon=icon("bell"))
                     
                   )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "info",
              HTML('<body>
                    <h1>Welcome!</h1>
                    <p>California Cooperative Oceanic Fisheries Investigation (CalCOFI) has been conducting marine ecosystem surveys in the California Current since 1949. More information about the CalCOFI program can be found on the <a href="https://calcofi.org/">CalCOFI website</a>.</p>
                    <p>The purpose of this Shiny App is to provide scientists with an interactive tool to visualize marine mammal data collected onboard CalCOFI. Here we integrate multiple datastreams, highlighting how marine mammal visual sightings and eDNA detections are represented through time and space. Please stay tuned for the addition of acoustic detections in a future release. By integrating visual, acoustic, and genetic sampling methods, we hope to better understand the detection capabilities of each method for detecting marine mammals in their environment.</p>
                    <h2>How is CalCOFI Data Collected?</h2>
                    <div class="infographic">
                      <p>Below is an interactive infographic that details the sampling processes for collecting visual, eDNA, and bioacoustic data.</p>
                      <div style="margin-bottom: 30px;"></div> <!-- This will create space -->
                      <p style="text-align: center; font-weight: bold;">Click or hover on different parts of the visual to learn more about them!</p>
                    </div>
                    </body>'),
              HTML('<center><img src="opening-infographic.png", height="710", usemap="#edna_map">',
                   '<map name="edna_map">',
                   '<area shape="rect" coords="40,10,480,270" href="https://calcofi.org/sampling-info/ships/" title="CalCofi Ships">',
                   '<area shape="rect" coords="480,80,550,200" href="https://www.sciencedirect.com/science/article/pii/S0967064514002690" title="Visual Observers">',
                   '<area shape="rect" coords="530,250,640,285" href="https://voicesinthesea.ucsd.edu/species/dolphins/bottlenose.html" title="Bottlenose Dolphin">',
                   '<area shape="rect" coords="590,180,710,225" href="https://voicesinthesea.ucsd.edu/species/dolphins/common.html" title="Short-Beaked Common Dolphin">',
                   '<area shape="rect" coords="650,250,745,280" href="https://voicesinthesea.ucsd.edu/species/dolphins/pacificWhiteSided.html" title="Pacific White-Sided Dolphin">',
                   '<area shape="rect" coords="700,255,830,310" href="https://voicesinthesea.ucsd.edu/species/dolphins/rissos.html" title="Risso\'s Dolphin">',
                   '<area shape="rect" coords="50,250,200,587" href="https://calcofi.org/data/marine-ecosystem-data/e-dna/" title="CalCofi eDNA">',
                   '<area shape="rect" coords="400,425,740,490" href="https://voicesinthesea.ucsd.edu/species/baleenWhales/blue.html" title="Blue Whale">',
                   '<area shape="rect" coords="350,380,580,425" href="https://voicesinthesea.ucsd.edu/species/baleenWhales/minke.html" title="Minke Whale">',
                   '<area shape="rect" coords="400,560,735,610" href="https://voicesinthesea.ucsd.edu/species/baleenWhales/fin.html" title="Fin Whale">',
                   '<area shape="rect" coords="550,510,790,600" href="https://voicesinthesea.ucsd.edu/species/baleenWhales/gray.html" title="Gray Whale">',
                   '<area shape="rect" coords="280,510,550,580" href="https://voicesinthesea.ucsd.edu/species/baleenWhales/humpback.html" title="Humpback Whale">',
                   '<area shape="rect" coords="385,600,700,670" href="https://voicesinthesea.ucsd.edu/species/baleenWhales/sei.html" title="Sei Whale">',
                   '</map></center>'
                   )
              ),
      tabItem(tabName = "map",
              # DTOutput("myTable"), for testing, dont remove plsssssssssssssss :)
              h1("Interactive Species Map", 
                 style = "font-family: Arial, sans-serif; font-size: 36px; color: #333; font-weight: bold; text-align: center;"),
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
                  leafletOutput(outputId = "mymap", height="auto"), #this map size is only applied to the downloaded map
                  selectInput("data_type", "Select Data Type:", 
                              choices = c("Sightings" = "sightings", "eDNA" = "edna", "Acoustic" = "acoustic")),
                  downloadButton("downloadData", "Download Data")
                  #Using height = 'auto' causes the inconsistency in the downloaded map size
                  #However, setting the height to be dynamic is the only way to capture the current window size.
              )
      )
    ),
    tabItem(tabName = "bioacoustics",
            HTML("<h1><strong>CalCOFI Bioacoustics</strong></h1>
                  <center>
                  <img src='sonobuoy_deployment.png' height='380' width='600'>
                  <h6><em>CalCOFI sonobuoy deployment</em></h6>
                  </center>
                  <br/> 
                  <p>Bioacoustics refers to the study of the sounds produced by marine 
                  mammals and their underwater acoustic environment. Bioacoustic studies 
                  of marine mammals involve the collection, analysis, and interpretation 
                  of these sounds to gain insights into various aspects of their behavior, 
                  ecology, and conservation.</p>
                  <br/>
                  <center>
                  <img src='all_calls_fixed.png' height='220' width='820'>
                  <h6><em>Spectrogram with all identifiable calls present (blue whale A, 
                  B, and D calls / fin whale 20 hz and 40 hz calls)</em></h6>
                  </center>
                  <br/>
                  <h2><strong>How is Bioacoustic Data Collected and Visualized?</strong></h2>
                  <p>CalCOFI ships deploy sonobuoys at varying depths to record marine 
                  mammal acoustics for hours at a time. These devices typically pick up 
                  2000 samples of sound waves per second, which are saved as wav files 
                  for further manipulation. A series of Fourier transforms are then 
                  applied to short, overlapping segments of the recorded signals to 
                  convert the data from the time domain (amplitude varying with time) 
                  to the frequency domain. Additional features of the sound wave can 
                  then be extracted such as a signal's frequency spectrum and magnitude. 
                  Using this acquired information, a spectrogram is constructed by piecing 
                  together the audio segments into one continuous plot that provides a 
                  time-varying representation of a signal's frequency content. Duration
                  is on the x-axis, frequency is on the y-axis, and magnitude is represented 
                  by color. The end product is a colorful visual capture of recorded sound 
                  that contains anything from whale songs to white noise from ships, as you 
                  can see in the figure above.</p>
                  <br/><center>
                  <h4>Listen to actual recordings by pressing the play buttons!</h4>
                  </center>
                  <center>
                  <div style='display: flex; justify-content: center;'>
                  <div style='display: flex;'>
                  <figure style='margin-right: 20px;'>
                  <img src='blue_whale_B_call.png' height='200' width='450'>
                  <h6><em>Blue Whale B Call</em></h6>
                  <div class='audio-controls'>
                    <audio controls>
                      <source src='ringtoneBlue.mp3' type='audio/mp3'>
                    </audio>
                  </div>
                  </figure>
                  <figure>
                  <img src='fin_whale_pulse.png' height='200' width='450'>
                  <h6><em>Fin Whale Pulse</em></h6>
                  <div class='audio-controls'>
                    <audio controls>
                      <source src='ringtoneBlue.mp3' type='audio/mp3'>
                    </audio>
                  </div>
                  </figure>
                  </div>
                  </center>
                  <br/><br/><br/>"),
                  fluidRow(column(6,HTML("<h2><strong>Faster r-CNN Deep Learning Model</strong></h2>")), #remove fluidRow code to move button under heading
                  # Add a button to trigger the modal
            column(4,actionButton("show_modal", "How Does the Model Work?", style="margin-top: 15px;"))),
            
            # Define the modal
            tags$div(
              id = "myModal",
              class = "modal fade",
              tabindex = "-1",
              role = "dialog",
              tags$div(
                class = "modal-dialog",
                role = "document",
                tags$div(
                  class = "modal-content",
                  tags$div(
                    class = "modal-header",
                    tags$h5(class = "modal-title", "How Does the Model Work?"),
                    tags$button(
                      type = "button",
                      class = "close",
                      `data-dismiss` = "modal",
                      `aria-label` = "Close",
                      tags$span(`aria-hidden` = "true", HTML("&times;")))),
                  tags$div(
                    class = "modal-body",
                    tags$p("The Faster r-CNN ResNet-50 model is an object detection and 
                    classification model. The workflow starts with ResNet-50 (a pretrained 
                    50-layer CNN) extracting the high-level features from the training data. 
                    The Region Proposal Network (RPN) scans this feature map to propose 
                    candidate regions likely to contain the signals of interest. These 
                    proposed regions are converted into fixed-size feature maps through 
                    Region of Interest (ROI) pooling. Finally, the feature maps are processed 
                    to classify the object within each region and refine the bouding box 
                    coordinates which correspond to locations on the image that contain 
                    the whale calls. Our final output consists of bounding boxes with 
                    their associated class label and adjusted coordinates."),
                    tags$div(class="text-center",
                             tags$img(src='https://www.researchgate.net/publication/336243948/figure/fig3/AS:810083202109441@1570150294355/Our-Faster-R-CNN-framework-used-for-kidney-detection-as-the-first-stage-of-our-proposed.ppm', 
                                      alt="Model Workflow", style="width: 70%; height: auto; margin-top:10px;"),
                             tags$figcaption("Image from https://www.researchgate.net/figure/Our-Faster-R-CNN-framework-used-for-kidney-detection-as-the-first-stage-of-our-proposed_fig3_336243948", style = "text-align: center; font-style: italic;"))),
                  tags$div(
                    class = "modal-footer",
                    tags$button(
                      type = "button",
                      class = "btn btn-secondary",
                      `data-dismiss` = "modal",
                      "Close"))))),
            tags$script(HTML("
                 $(document).ready(function(){
                   $('#show_modal').click(function(){
                     $('#myModal').modal('show');
                   });
                   $('.close').click(function(){
                     $('#myModal').modal('hide');
                   });
                   $(window).click(function(event){
                     if (event.target.id === 'myModal') {
                       $('#myModal').modal('hide');
                     }
                   });
                 });
                 ")),
                  HTML("<p>A Faster r-CNN ResNet-50 model was built to automate the 
                  identification of whale calls and classify them given spectrograms 
                  transformed from wav files of collected bioacoustic data as input. 
                  Although functional, imperfect accuracy was likely due to the abundance 
                  of noise and insignificant signals scattered throughout the data as 
                  shown below. These factors made it difficult for the model to distinguish 
                  between the actual calls and undesired visuals.</p>
                  <center>
                  <img src='blue_whale_b_call_fixed.png' height='220' width='820'>
                  <h6><em>Noisy spectrogram containing blue whale B call</em></h6>
                  </center>
                  <br/>
                  <p>The solution was to produce a preprocessing pipeline that would 
                  eliminate this noise, and consequently, increase both model runtime
                  efficiency and classification accuracy. The following is a small scale 
                  demonstration of the final preprocessing pipeline followed by the training 
                  of the Faster r-CNN ResNet-50 model with code and output examples:</p>
                  <br/>
                  <h2><strong>Step 1: Preprocessing</strong></h2>
                  <h3><strong>Step 1a: Organizing the Data</strong></h3>
                  <p>We first iterate through our training data of spectrogram images to 
                  flatten each one into a 1-dimensional array that consists of the image's 
                  pixel values dependent on color intensity. The arrays are then vertically 
                  stacked on top of one another to create a single array containing all 
                  of the data. An example of an unmodified spectrogram from the original 
                  dataset is shown below.</p>
<pre><code>  #annotations saved in 'unique_annotation' variable
  #images navigated to via 'spectrogram_path'
  
  for index, row in unique_annotation.iterrows():
      image = Image.open(row['spectrogram_path'])
      pixel_values = np.array(image).flatten()
      data_matrix.append(pixel_values)
      
  original_data = np.vstack(data_matrix)</code></pre>
                  <center>
                  <figure>
                  <img src='step_1_fin_whale_40_fixed.png' height='220' width='820'>
                  <h6><em>Fin whale 40 Hz pulse (unprocessed)</em><h6>
                  </figure>
                  </center>
                  <br/>
                  <h3><strong>Step 1b: Principal Component Analysis (PCA)</strong></h3>
                  <p>To denoise the spectrograms and cut down on computation time, we 
                  decided to perform PCA to the training set. Singular value decomposition 
                  was done to separate our array's covariance matrix into three sub-matrices 
                  that together comprise the original data: U (eigenvector matrix), 
                  S (eigenvalue matrix), and T (feature matrix). Below is an example of a few 
                  features extracted from matrix T for a single spectrogram. In theory, 
                  combining those 10 separate components would construct 
                  something very closely resembling the original observation.</p>
<pre><code>  U, S, T = np.linalg.svd(original_data, full_matrices=False)
  US = U*S
  
  svd_data = US @ T
  svd_data_scaled = scaler.inverse_transform(svd_data)
  
  for i in range(0, 10):
      one_face = T[i]
      plt.subplot(2, 5, i + 1)
      draw_img_single(one_face)</code></pre>
                  <center>
                  <figure>
                  <img src='step_2_image.png' height='200' width='450'>
                  <h6><em>10 sub-spectrograms for one observation</em><h6>
                  </figure>
                  </center>
                  <br/>
                  <h3><strong>Step 1c: Noise Reduction
                  </strong></h3>
                  <p>With the artifacts and white noise now separated from the signals of 
                  interest, we performed a column-wise background subtraction on the 
                  sub-component images along with median blurring for further noise reduction. 
                  This removes the unwanted columns and messy noise from the spectrograms 
                  while accentuating the stronger, brighter whale calls. We can now use these 
                  filtered principal components to reconstruct much cleaner spectrograms.</p>
<pre><code>  for i in range(len(T)):
      feature = np.copy(T[i].reshape((141, 601)))
      feature = median_filter(feature, size = 3)
  
      for j in range(feature.shape[1]):
          column = feature[:, j]
          percentile_value = np.percentile(column, 10)
          feature[:, j] = column - percentile_value
          feature[:, j][feature[:, j] < 0] = 0
          
      signal_enhanced_features[i] = feature.flatten()
      
  for i in range(0, 10):
      one_face = signal_enhanced_features[i]
      plt.subplot(2, 5, i + 1)
      draw_img_single(one_face)</code></pre>
                  <center>
                  <figure>
                  <img src='step_3_image.png' height='200' width='450'>
                  <h6><em>Signal enhanced features</em><h6>
                  </figure>
                  </center>
                  <br/>
                  <h3><strong>Step 1d: Reconstruction</strong></h3>
                  <p>We reconstruct the full spectrogram images by combining the original 
                  U and S matrices from Step 1b with the 'signal_enhanced_features' created 
                  in Step 1c. This time, however, we keep only the first 150 principal 
                  components since those were sufficient in creating reconstructions of the 
                  original images that resembled them almost perfectly. The procured 
                  reconstructions contained almost no vertical artifacts and significantly 
                  fewer features for the model to have to analyze.</p>
<pre><code>  matrix = US[:, 0:150] @ signal_enhanced_features[0:150, :]
  matrix = US @ signal_enhanced_features
  matrix_scaled = scaler.inverse_transform(matrix)
  matrix_scaled = np.where(matrix_scaled < 0, 0, matrix_scaled)</code></pre>
                  <h3><strong>Step 1e: Noise Reduction on Reconstructions</strong></h3>
                  <p>We lastly apply another simple column-wise subtraction to 
                  these reconstructions to give us our finalalized preprocessed images. 
                  Below is an example of the image of the fin whale 40 Hz pulse from step 
                  1a before and after preprocessing.</p>
<pre><code>  matr_sub = np.zeros_like(matrix_scaled)
    
  for i in range(len(matrix_scaled)):
      spec = np.copy(matrix_scaled[i].reshape((141, 601)))
  
      for j in range(spec.shape[1]):
          column = spec[:, j]
          percentile_value = np.percentile(column, 60)
          spec[:, j] = column - percentile_value
          spec[:, j][spec[:, j] < 0] = 0
  
      matr_sub[i] = spec.flatten()</code></pre>
                  <center>
                  <figure>
                  <img src='step_5_comp_fixed.png' height='450' width='820'>
                  <h6><em>Before and after preprocessing comparison of fin whale 
                  40 Hz pulse</em><h6>
                  </figure>
                  </center>
                  <br/>
<pre><code>  for i in range(len(matr_sub)):
  processed_image = matr_sub[i].reshape(141, 601)
  image = Image.fromarray(processed_image.astype(np.uint8), 'L')
  image.save(Path(directory_path) / Path(filenames[i]))</code></pre>
                  <p>The final preprocessed arrays are converted back into images and 
                 saved to the directory path specified by the user, ready to be used 
                 for model training.</p>
                <br/><h2><strong>Step 2: Training the Model</strong></h2>
                <p>We first read our preprocessed spectrogram images into our data loader 
                to be used as training data.</p>
<pre><code>  train = DataLoader(AudioDetectionData(csv_file='../labeled_data/train_val_test_annotations/train.csv'),
      batch_size=16,
      shuffle = True,
      collate_fn = custom_collate, 
      pin_memory = True if torch.cuda.is_available() else False)</code></pre>
                <p>We then use the torchvision package from PyTorch to create our pretrained 
                Faster r-CNN model architecture with the specified parameters below. The 
                optimizer used for minimizing loss was Stochastic Gradient Descent with a 
                learning rate of 0.001, and the model will be trained for 20 epochs.</p>
<pre><code>  optimizer = torch.optim.SGD(model.parameters(), lr = 0.001, momentum = 0.9, weight_decay= 0.0005)
  num_epochs = 20</code></pre>
               <p>With our model ready, we can finally start its training where it 
               reiteratively learns how to correctly identify and label the whale calls
               in a given spectrogram. Training loss is logged after every epoch as a way
               to measure how well the model is fitting after each time the model's 
               parameters are updated based on the loss function.</p>
<pre><code>  for epochs in range(num_epochs):
    model.train()
    epoch_train_loss  = 0
    for data in train_d1:
        imgs = []
        targets = []
        for d in data:
            imgs.append(d[0].to(device))
            targ = {}
            targ['boxes'] = d[1]['boxes'].to(device)
            targ['labels'] = d[1]['labels'].to(device)
            targets.append(targ)
            
        loss_dict = model(imgs,targets)
        loss = sum(v for v in loss_dict.values())
        epoch_train_loss += loss.cpu().detach().numpy()
        optimizer.zero_grad()
        loss.backward()
        optimizer.step()
    print(f'training loss: {epoch_train_loss}')</code></pre>
           <p>Now that the model is trained, we can use our testing set to evaluate the
                 performance of the model. The results from our testing data can be 
                 visualized through a precision-recall curve which describes how good 
                 our model is at classifying each call.</p>")),
    tabItem(tabName = "moreinfo",
            HTML('<div id="calcofi-marine-mammal-visual-survey-data" class="section level3">
                  <h3><strong>CalCOFI Marine Mammal Visual Survey Data</strong></h3>
                  <p>Marine mammal visual line-transect surveys have been conducted on
                  quarterly CalCOFI cruises since 2004. Visual surveys are conducted
                  during daylight hours while the ship is in transit between CalCOFI
                  stations. More information about visual survey protocol can be found in
                  <a href="https://doi.org/10.1016/j.dsr2.2014.10.008%22,%22https://doi.org/10.1016/j.dsr2.2014.10.008">Campbell
                  et al. (2015)</a>. Per-cruise marine mammal visual survey effort is
                  visible by clicking ‘Display Visual Effort’. Additionally, sighting
                  group size estimates are visible by selecting a species from the
                  drop-down menu, where circle size on the map is proportional to group
                  size. Only cetacean sightings are included in this interactive map. By
                  selecting a sighting on the map, more information will pop up about that
                  specific sighting.</p>
                  <p><br /></p>
                  </div>
                  <div id="calcofi-marine-mammal-edna-data" class="section level3">
                  <h3><strong>CalCOFI Marine Mammal eDNA Data</strong></h3>
                  <p>The NOAA CalCOFI Genomic Program (NCOG) has collected envrionmental
                  DNA samples (eDNA) since 2014. Here we used metabarcoding assays to
                  detect cetacean species from water samples collected at 10, 20, or 40
                  meters. The ‘Display eDNA’ function will plot eDNA sampling effort as
                  opaque black circles, and eDNA detections as green dna helixes. By selecting a
                  detection on the map, more information about that specific detection
                  will pop up.</p>
                  <p><br /></p>
                  </div>
                  <div id="ucsb-data-science-capstone" class="section level3">
                  <h3><strong>UCSB Data Science Capstone</strong></h3>
                  <p>The Data Science Capstone is a three-course sequence at the
                  University of California, Santa Barbara (UCSB) in which students engage
                  in project-based learning with data-intensive methodologies with the
                  hopes of making a positive impact on the world. As their project, seven
                  students from the program upgraded this Shiny app to implement new data,
                  improve functionality, and enhance user experience.</p>
                  <p><br /></p>
                  </div>
                  <div id="ocean-observing-in-california" class="section level3">
                  <h3><strong>Ocean Observing in California Conference</strong></h3>
                  <h4>Celebrate the Past, Showcase the Present, Envision the Future</h4>
                  <p>This conference celebrated the rich history of ocean observation, 
                  marking the 75th anniversary of CalCOFI and CeNCOOS and the 20th 
                  anniversary of SCCOOS. With a goal of sharing insights on the California 
                  Current, showcasing successful collaborations between scientists and 
                  data users, and strengthening relationships across the ocean community.</p>
                  The poster can be accessed
                  <a href="UCSB_DS_OOC_Poster.pdf">here.</a>
                  <p><br /></p>
                  </div>
                  <div id="co-authors" class="section level3">
                  <h3><strong>Co-authors</strong></h3>
                  <p>Luis Barajas<sup>1</sup>, Sam Guimte<sup>1</sup>, Justin Kim<sup>1</sup>, 
                  Kaitlyn Lee<sup>1</sup>, Yoobin Won<sup>1</sup>, Ryan Yee<sup>1</sup>, 
                  Puyuan Zhang<sup>1</sup>, Michaela Alksne<sup>2</sup>, Lauren Baggett
                  <sup>2</sup>, Julie Dinasquet<sup>2</sup>, Bryce Ellman<sup>2</sup>, 
                  Erin Satterthwaite<sup>2</sup>, Brice Semmens<sup>2</sup>, 
                  Simone Baumann-Pickering<sup>2</sup>.</p>
                  <p><br /></p>
                  </div>
                  <div id="affilations" class="section level3">
                  <h3><strong>Affiliations</strong></h3>
                  <p><sup>1</sup>UC Santa Barbara, Santa Barbara, California 93106.</p>
                  <p><sup>2</sup>Scripps Institution of Oceanography, 8622 Kennel Way, La Jolla, CA 92037.</p>
                  <p><br /></p>
                  <div>
                  <div id="funding-sources" class="section level3">
                  <h3><strong>Funding Sources</strong></h3>
                  <p>This material is based upon research supported by the Office of Naval
                  Research under Award Number (N00014-22-1-2719).</p>
                  <p>Office of Naval Research, US Navy Pacific Fleet</p>
                  </div>'))
  ))))


# #Next, we add the server. This is where we will actually create all of our plots, and add reactivity to our inputs and outputs.
server <- function(input, output, session) {
  
  #output$myTable <- renderDT({ # for testing, dont REMOVE >:(
  #  insert data set
  #})
  
  # Define the download handler for multiple data types
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$data_type, "data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Decide which dataset to download based on user input
      if (input$data_type == "sightings") {
        data_to_download <- obsFilter()  # Assuming this returns the filtered sightings data
      } else if (input$data_type == "edna") {
        data_to_download <- downloadEdnaDetectionFilter()  # Assuming this returns the filtered eDNA detection data
      } else if (input$data_type == "acoustic") {
        data_to_download <- downloadAcousticDetectionFilter()  # Assuming this returns the filtered eDNA detection data
      }
      write.csv(data_to_download, file, row.names = FALSE)
    }
  )
  
  
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
  
  edna_legend <- "<img src='https://cdn-icons-png.flaticon.com/512/922/922105.png'
  style='width:20px;height:20px;'> eDNA Detection"
  
  acoustic_legend <- "<img src='music-note-purple.png'
  style='width:20px;height:20px;'> Acoustic Detection"
  
  edna_effort_legend <- HTML(
    '<div style="display: flex; align-items: center;">
          <div style="width: 15px; height: 15px; border-radius: 50%; background: black; margin-right: 5px;"></div>
          <div>eDNA Effort</div>
        </div>'
  )
  
  acoustic_effort_legend <- HTML(
    '<div style="display: flex; align-items: center;">
          <div style="width: 15px; height: 15px; border-radius: 50%; background: gray; margin-right: 5px;"></div>
          <div>Acoustic Effort</div>
        </div>'
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
        # addLegend("bottomleft",
        #           colors = "black",
        #           labels = "eDNA Effort",
        #           opacity = 1,
        #           layerId = "edna_effort_legend"
        # ) %>%
        addControl(html = edna_effort_legend, position = "bottomleft", layerId = "edna_effort_legend") %>%
        addControl(html = edna_legend, position = "bottomleft", layerId = "edna_legend")
      
      # Add eDNA detection legend if it was displayed
      # leafletProxy("mymap", session) %>%
      #   addLegend("bottomleft",
      #             colors = greenHelixIcon,
      #             labels = "eDNA Detection",
      #             #opacity = 1,
      #             layerId = "edna_detection_legend"
      #   ) # This might seem counter intuitive, but it is to 
        
      # take care of the edge case where selecting all will clear eDNA legends.
    }
    # Check if acoustic legends are currently displayed
    if (input$acoustic > 0) {
      # Add acoustic effort legend if it was displayed
      leafletProxy("mymap", session) %>%
        # addLegend("topleft",
        #           colors = "gray",
        #           labels = "Acoustic Effort",
        #           opacity = 1,
        #           layerId = "acoustic_effort_legend"
        # )
        addControl(html = acoustic_effort_legend, position = "topleft", layerId = "acoustic_effort_legend")
      
      # Add acoustic detection legend if it was displayed
      leafletProxy("mymap", session) %>%
        addControl(html = acoustic_legend, position = "topleft", layerId = "acoustic_legend")
        # addLegend("topleft",
        #           colors = "#6D00BE",
        #           labels = "Acoustic Detection",
        #           opacity = 1,
        #           layerId = "acoustic_detection_legend"
        # ) # This might seem counter intuitive, but it is to 
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
  
  downloadEdnaDetectionFilter <- reactive({filter(edna, edna$Cruise %in% input$all_cruises 
                                                  & edna$SpeciesName %in% input$all_species
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
          addCircleMarkers(
            lng = as.numeric(effort_markers$longitude),
            lat = as.numeric(effort_markers$latitude),
            #icon = blackHelixIcon,
            color = "black",
            fillColor = "black",
            fillOpacity = 0.7,
            stroke = FALSE,
            
            popup = paste("eDNA Effort",
                          "<br>Sample Depth (m):", as.character(effort_markers$depth),
                          "<br>Line:", as.character(effort_markers$line),
                          "<br>Station:", as.character(effort_markers$station)) %>%
              lapply(htmltools::HTML),
            group = "edna"
          ) %>%
          clearControls() %>%
          # addLegend("bottomleft",
          #           colors = "black",
          #           labels = "eDNA Effort",
          #           opacity = 1,
          #           layerId = "edna_effort_legend"
          # ) %>%
          addControl(html = edna_effort_legend, position = "bottomleft", layerId = "edna_effort_legend") %>%
          addControl(html = edna_legend, position = "bottomleft", layerId = "edna_legend")
      }
  
      # Check if acoustic legends are currently displayed
      if (input$acoustic > 0) {
        # Add acoustic effort legend if it was displayed
        leafletProxy("mymap", session) %>%
          # addLegend("topleft",
          #           colors = "gray",
          #           labels = "Acoustic Effort",
          #           opacity = 1,
          #           layerId = "acoustic_effort_legend"
          # )
          addControl(html = acoustic_effort_legend, position = "topleft", layerId = "acoustic_effort_legend")
        
        # Add acoustic detection legend if it was displayed
        leafletProxy("mymap", session) %>%
          addControl(html = acoustic_legend, position = "topleft", layerId = "acoustic_legend")
          # addLegend("topleft",
          #           colors = "#6D00BE",
          #           labels = "Acoustic Detection",
          #           opacity = 1,
          #           layerId = "acoustic_detection_legend"
          # ) # This might seem counter intuitive, but it is to 
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
        #removeControl(map = "mymap",layerId = "edna_legend")
        #clearControls() %>%
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
          # addLegend("bottomleft",
          #           colors = "black",
          #           labels = "eDNA Effort",
          #           opacity = 1,
          #           layerId = "edna_effort_legend") %>%
          # addLegend("bottomleft",
          #           colors = greenHelixIcon,
          #           labels = "eDNA Detection",
          #           #opacity = 1,
          #           layerId = "edna_detection_legend"
          # )
          addControl(html = edna_effort_legend, position = "bottomleft", layerId = "edna_effort_legend") %>%
          
          addControl(html = edna_legend, position = "bottomleft", layerId = "edna_legend")
      }
      # Check if acoustic legends are currently displayed
      if (input$acoustic > 0) {
        # Add acoustic effort legend if it was displayed
        leafletProxy("mymap", session) %>%
          # addLegend("topleft",
          #           colors = "gray",
          #           labels = "Acoustic Effort",
          #           opacity = 1,
          #           layerId = "acoustic_effort_legend"
          # )
          addControl(html = acoustic_effort_legend, position = "topleft", layerId = "acoustic_effort_legend")
        
        # Add acoustic detection legend if it was displayed
        leafletProxy("mymap", session) %>%
          addControl(html = acoustic_legend, position = "topleft", layerId = "acoustic_legend")
          # addLegend("topleft",
          #           colors = "#6D00BE",
          #           labels = "Acoustic Detection",
          #           opacity = 1,
          #           layerId = "acoustic_detection_legend"
          # ) # This might seem counter intuitive, but it is to 
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
      removeControl("edna_detection_legend") %>%# Remove both legends associated with eDNA data
      removeControl("edna_legend")
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
          # addLegend("topleft",
          #           
          #           colors = "gray",
          #           
          #           labels = "Acoustic Effort",
          #           opacity = 1,
          #           layerId = "acoustic_effort_legend"
          # )
          addControl(html = acoustic_effort_legend, position = "topleft", layerId = "acoustic_effort_legend")
      }
      if (input$edna > 0) {
        # Add eDNA effort legend if it was displayed
        leafletProxy("mymap", session) %>%
          # addLegend("bottomleft",
          #           colors = "black",
          #           labels = "eDNA Effort",
          #           opacity = 1,
          #           layerId = "edna_effort_legend"
          # )
          
        
        # Add eDNA detection legend if it was displayed
        #leafletProxy("mymap", session) %>%
          # addLegend("bottomleft",
          #           colors = c("url('https://cdn-icons-png.flaticon.com/512/922/922105.png')"),
          #           labels = "eDNA Detection",
          #           #opacity = 1,
          #           layerId = "edna_detection_legend"
          # ) # This might seem counter intuitive, but it is to 
          addControl(html = edna_effort_legend, position = "bottomleft", layerId = "edna_effort_legend") %>%
          addControl(html = edna_legend, position = "bottomleft", layerId = "edna_legend")
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
          addControl(html = acoustic_legend, position = "topleft", layerId = "acoustic_legend")
          # addLegend("topleft",
          #           colors = "#6D00BE",
          #           labels = "Acoustic Detection",
          #           opacity = 1,
          #           layerId = "acoustic_detection_legend"
          # )
      }
      if (input$edna > 0) {
        # Add eDNA effort legend if it was displayed
        leafletProxy("mymap", session) %>%
          # addLegend("bottomleft",
          #           colors = "black",
          #           labels = "eDNA Effort",
          #           opacity = 1,
          #           layerId = "edna_effort_legend"
          # )
          addControl(html = edna_effort_legend, position = "bottomleft", layerId = "edna_effort_legend")
        
        # Add eDNA detection legend if it was displayed
        leafletProxy("mymap", session) %>%
          # addLegend("bottomleft",
          #           colors = "url('https://cdn-icons-png.flaticon.com/512/922/922105.png')",
          #           labels = "eDNA Detection",
          #           opacity = 1,
          #           layerId = "edna_detection_legend")}
          addControl(html = edna_legend, position = "bottomleft", layerId = "edna_legend")}
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
      removeControl("acoustic_detection_legend") %>%# Remove both legends associated with acoustic data
      removeControl("edna_legend")
    acousticCleared(TRUE)
  })
  
  observeEvent(input$acoustic, {
    acousticCleared(FALSE)
  })
  
  ################################
  
  downloadAcousticDetectionFilter <- reactive({
    
    filter(acoustic_detections, acoustic_detections$Cruise %in% input$all_cruises 
                   & acoustic_detections$SpeciesName %in% input$all_species
                   & acoustic_detections$Year >= input$years[1] 
                   & acoustic_detections$Year <= input$years[2])
  })
  
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