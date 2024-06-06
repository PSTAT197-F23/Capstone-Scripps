library(tidyverse)
source("data/data-cleaning.R")  # import data cleaning functions



# Set the directory containing the txt files
directory <- "data/whale_visual_data/new-marine-mammal-data"

# List all txt files in the directory
txt_files <- list.files(directory, pattern = "\\.txt$", full.names = TRUE)

# Initialize an empty list to store data frames
data_list <- list()

# Iterate through each txt file
for (file in txt_files) {
  # Extract the file name
  file_name <- basename(file)
  
  # Read in the dataset
  data <- read.csv(file, header = TRUE, na.strings = "NA")
  
  # Store the dataset in the list with its file name
  data_list[[file_name]] <- data
}

# Merge all data sets together
merged_data <- do.call(rbind, data_list)

# Clean Sightings data from 2004-2022
whale_raw <- read.csv("data/whale_visual_data/CalCOFI_2004-2022_CombinedSightings.csv")
cleaned_whale <- clean_whale(whale_raw)

# Clean new sightings Data
cleaned_new <- clean_new_sightings(merged_data)

# Combine new sightings data with the current sightings data set
whale <- rbind(cleaned_whale, cleaned_new)

# Replace the existing whale data set with the new data set
write.csv(whale, file = "data/whale_visual_data/whale.csv", row.names = FALSE)

# read in current transect data
viz_raw <- read.csv("data/whale_visual_data/CalCOFI_2004-2021_Effort_OnTransectOnEffortONLY_MNA.csv")


# clean new transect data
transect_new <- clean_new_viz(merged_data)

create_mappings <- function(data) {
  temp_data <- data
  # create ending lat and lon
  temp_data$EndLon <- NA
  temp_data$EndLat <- NA
  
  for (i in 1:nrow(temp_data)) {
    # get the row's corresponding eff and trn values
    eff_trn <- c(temp_data[i,]$X1, temp_data[i,]$X2)
    # find eff = 0 and trn = 0
    if (eff_trn[1] == 0 & eff_trn[2] == 0) {
      print("Recording Effort")
      temp_data[i, ]$EndLon <- temp_data[i+1, ]$StartLon
      temp_data[i, ]$EndLat <- temp_data[i+1, ]$StartLat
    }
  }
  
  return(temp_data)
}
  
  
  


transect_new <- create_mappings(transect_new)
transect_new$cruise <- substr(transect_new$cruise, 1, nchar(transect_new$cruise) - 2)
transect_new$Cruise  <- paste0(
  substr(transect_new$cruise, 1, 2),  # Extracts the "CC", "DD", etc.
  "20",                          # Prepends "20" to the year
  substr(transect_new$cruise, 3, 4),  # Extracts the year part, assuming it's "23", "24", etc.
  "-",                           # Adds the dash
  substr(transect_new$cruise, 5, 6)   # Extracts the month part, "11", "12", etc.
)


# combine the new transect data with the current one
viz <- bind_rows(clean_viz(viz_raw), clean_viz(transect_new))

# Replace the existing transect data set with the new data set
write.csv(viz, file = "data/whale_visual_data/transect.csv", row.names = FALSE)

