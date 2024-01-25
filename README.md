# CalCOFI_eDNA_RShiny
Collaboration of UCSB Data Science Capstone students and CalCOFI to generate RShiny App for visualizing the distribution of marine mammal eDNA detections in time and space relative to oceanographic parameters and marine mammal visual observations. 
- app.R is the prototype RShiny App built by MNA to visualize marine mammal detections along CalCOFI grid
- "CalCOFI_2004-2022_CombinedSightings.csv" is the dataset of marine mammal visual observations from 2004 - 2022. Each row contains information about a unique sighting.
- "CalCOFI_2004-2021_Effort_OnTransectOnEffortONLY_MNA.csv" contains visual survey effort from 2004 - 2021. Each row contains information about location and length of a given transect line.
- "CalCOFI_species_codes.csv" contains all of the cetacean (whale, dolphin, and porpoise) species codes present in the dataset.
- "CalCOFIStationOrder.csv" contains the locations of each CalCOFI station.
- "edna.csv" contains the eDNA sequencing effort and detections. Each row represents eDNA effort at a given station and depth. Columns 10-16 represent species specific eDNA detections, with the detection code also in column 20 ("detection")

