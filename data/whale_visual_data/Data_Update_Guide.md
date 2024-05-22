
# Updating Data in the R Shiny App

This document outlines the process for incorporating new sightings and effort data into the existing R Shiny application, which is built around data collected from the years 2004 to 2022.

## Prerequisites

Ensure that any new data files adhere to the format specified in the **CountCOFI manual**. This is crucial for the scripts to process the data correctly.

## Steps for Uploading New Data

1. **Prepare the Data File:**
   - Confirm that the data file matches the required format as detailed in the CountCOFI manual.

2. **Upload the Data:**
   - Navigate to the `data/whale_visual_data` directory within the application's file structure.
   - Drag and drop the new dataset into the `new-marine-mammal-data` folder.

3. **Run the Processing Script:**
   - Execute the `clean_whale_data.R` script. This script will perform the following operations:
     - Pull new data from the `new-marine-mammal-data` folder.
     - Process the new and historical (2004 to 2022) data by applying necessary transformations.
     - Merge the new data with the historical dataset (2004 to 2022).
     - Output the combined data into two new datasets: `whale.csv` and `transect.csv`.
   - Do not delete any new files placed in the new-marine-mammal-data folder after running the script, as it does not permanently merge the new data with the historical data.

4. **Verify the Update:**
   - Ensure that the new `whale.csv` and `transect.csv` files are correctly formatted and contain up-to-date information.
   - Check that the R Shiny app reflects the changes and operates as expected with the new data.

## Troubleshooting

- If the data does not appear in the app after following these steps, revisit the format of your input files to ensure compliance with the CountCOFI manual specifications.
- Ensure that the `clean_whale_data.R` script executed without errors by checking the logs for any reported issues.

By following these steps, new sightings and effort data can be seamlessly integrated into the app, ensuring the application remains current and functional.
