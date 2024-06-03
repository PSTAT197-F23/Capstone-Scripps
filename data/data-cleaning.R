###########################
# WHALE:
# clean whale dataset:
clean_whale <- function(dataset){
  cleaned = subset(dataset, DecLat < 39)  # restrict latitude values
  cleaned = subset(cleaned, DecLong < -113)  # restrict longitude values
  cleaned = cleaned[-1105,]
  cleaned = cleaned[-2064,]  # removing inland bottlenose dolphin sighting 
  cleaned <- cleaned %>% 
    mutate(Year = as.integer(paste("20", substr(Cruise, 3, 4), sep = "")),  # create Year col
           Month = month.name[as.integer(substr(Cruise, 5, 6))]) %>%  # create Month col
    mutate(Cruise = paste(Month, Year)) %>%  # rename cruises
    mutate(Season = trimws(Season)) %>%   # trim whitespace for Season col
    mutate(SpeciesName = str_replace_all(SpeciesName, " $", "")) %>%
    select(Cruise, Season, Year, DateTimeLocal, DecLat, DecLong, Best, Min, Max, Calf, 
           Species1, Species2, SpeciesName, SubOrder)
  return(cleaned)
}


# clean raw data for whale:
species_code <- read.csv("data/CalCOFI_species_codes.csv")
clean_new_sightings <- function(new_sightings){
  cleaned <- new_sightings %>% 
    filter(EID != 'EID') %>%  # drop excess rows
    filter(ev == 'SIT' | ev == 'UPD') %>%  # only include sightings data
    filter(X1 == 'CETA') %>%   # only cetacean sightings
    select(cruise, 
           when, 
           Y, 
           X, 
           port, 
           star, 
           X13,
           X14,
           X15,
           X16,
           X17,
           X19
    ) %>% 
    rename(DecLat = Y,
           DecLong = X,
           PortObs = port,
           StbdObs = star,
           Min = X14,
           Max = X15,
           Calf = X16
    ) %>% 
    mutate(Year = as.integer(substr(when, 1, 4)),  # create Year col
           Month = month.name[as.integer(substr(when, 6, 7))]) %>%  # create Month col
    mutate(Cruise = paste(Month, Year),
           Season = as.character(sapply(as.integer(substr(when, 6, 7)), get_season)),
           when = as.POSIXct(when, format = "%Y-%m-%d %H:%M:%S"),
           DateTimeLocal = format(when, "%m/%d/%Y %H:%M"),
           Best = as.integer(X13),
           Species1 = str_to_title(trimws(X17)),
           Species2 = str_to_title(X19)
    ) %>% 
    left_join(species_code, by = c("Species1" = "speciesCode")) %>% 
    rename(SpeciesName = speciesName,
           SubOrder = taxonomy) %>% 
    select(Cruise, Season, Year, DateTimeLocal, DecLat, DecLong, Best, Min, Max, Calf, 
           Species1, Species2, SpeciesName, SubOrder)
  
  return(cleaned)
}


###########################
# EDNA:
clean_edna <- function(data){
  cleaned <- data %>% 
    mutate(Cruise = paste(month.name[as.integer(substr(cruise, 5, 6))], year))
  colnames(cleaned)[colnames(cleaned) == "year"] ="Year"
  
  return(cleaned)
}

###########################
# VIZ:
clean_viz <- function(data){
  cleaned <- data %>% mutate(Cruise = paste(month.name[as.integer(substr(Cruise, 8, 9))], 
                                                 substr(Cruise, 3, 6)))
  return(cleaned)
}

clean_new_viz <- function(new_data) {
  # This function returns year, season, cruise, and start/end cordinates from the input data
  # These coluns are the only ones used for visualizing effort in the app.R script
  cleaned <- new_data %>%
    filter(ev == 'EFF') %>% # get effort events
    select(cruise, when, X, Y, X2, X34, X35) %>% # select relevant columns
    mutate(Year = str_sub(when, 1, 4)) %>% # create year column
    filter(X2 == "0") %>% # get observations with transect effort ON
    filter(when != "when") %>% # remove 'when' obs
    mutate(Year = as.numeric(Year)) %>% # convert year to numeric
    mutate(X34 = ifelse(is_numeric(X34), X34, NA)) %>% # remove comments from X34
    mutate(X35 = ifelse(is_numeric(X35), X35, NA)) %>% # remove comments from X34
    mutate(
      X = as.numeric(X),
      Y = as.numeric(Y),
      X34 = as.numeric(X34),
      X35 = as.numeric(X35),
      Season = as.character(sapply(as.integer(substr(when, 6, 7)), get_season))
    ) %>% # make cordinates numeric
    select(-when) %>% # remove when column
    rename(
      StartLon = X,
      StartLat = Y,
      EndLon = X34,
      EndLat = X35,
      Cruise = cruise
    ) # rename columns
  
  cleaned
}

###########################
# ACOUSTIC:
clean_acoustic <- function(data){
  cleaned <- data %>% 
    mutate(Cruise = paste(month.name[as.integer(substr(Cruise, 5, 6))], Year))
  return(cleaned)
}



############################

# OTHER FUNCTIONS:
## Defining a function to convert month number to season
get_season <- function(month) {
  if (month %in% c(3,4,5)) {
    return("spring")
  } else if (month %in% c(06, 07, 08)) {
    return("summer")
  } else if (month %in% c(09, 10, 11)) {
    return("fall")
  } else if (month %in% c(12, 01, 02)) {
    return("winter")
  }
}


## function to scale the dots:
adjustSize <- function(value) {
  if (!is.na(value)){
    if (value < 10) {
      return(4750 + (1000 * log(value)))
    } 
    else {
      return((log(value) * 2000) + 3750)
      
    }
  }
}

## normalize effort function:
normalize_effort <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

## function to check if a string contains only numeric characters
is_numeric <- function(x) {
  grepl("^[0-9]+$", x)
}
