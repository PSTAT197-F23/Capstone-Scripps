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

## Clean Sightings data from 2004-2022
#whale_raw <- read.csv("data/whale_visual_data/CalCOFI_2004-2022_CombinedSightings.csv")
#cleaned_whale <- clean_whale(whale_raw)

# Set path to the current whale data
whale_directory <- "data/whale_visual_data/whale.csv"

# read in current whale data set
whale <- read.csv(whale_directory)


# Clean new sightings Data
cleaned_new <- clean_new_sightings(merged_data)

# Combine new sightings data with the current sightings data set
whale <- rbind(whale, cleaned_new)

# Replace the existing whale data set with the new data set
write.csv(whale, file = "data/whale_visual_data/whale.csv", row.names = FALSE)
