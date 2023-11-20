#### Example Case to use the information provided in OCCUR #### 

#### The following script contains basic steps for cleaning and filtering
#### biodiversity records after the downloading process.
#### It is organised by module as it is shown in OCCUR

#### Important: These example filters GBIF records of three species of mosses 
#### from Spain and Portugal and keep only those that are placed in Portugal

# Load packages ---------------------------------------------------------
library(rgbif)
library(bdc)
library(tidyverse)  # data manipulation
library(dplyr)
library(GADMTools)  # administrative units and GIS
library(CoordinateCleaner)  # data cleaning functions
library(countrycode)  # country names standardization
library(biogeo)
# Environment and data ----------------------------------------------------

rm(list = ls(all.names = TRUE))
wd <- ''  # Write working directory path
setwd(wd)

# Only run section a if you want to download your own dataset (needs user account in GBIF)
# a) Download the dataset from GBIF 
spList <- c('Homalothecium aureum', 'Syntrichia ruralis', 'Tortella squarrosa')
gbif_taxKeys <- name_backbone_checklist(spList) %>% 
                    filter(!matchType == 'NONE') %>% 
                    pull(usageKey)

occ_download(pred_in('taxonKey', gbif_taxKeys), # download 3 species names from our list
             pred('hasCoordinate', TRUE), # this example only download records with associated coordinates
             format = 'SIMPLE_CSV', 
             user = 'xxx', pwd = 'xxx',email = 'xxx') # include user information  
# Do not run:
occ_download_wait('xxxxxxx-xxxxxxx')
occData <- occ_download_get('xxxxxxx-xxxxxxx') %>% 
          occ_download_import()


# Start here using the data from GitHub
# b) Load dataset
# Read occurrences csv and check dimensions
data <- read.csv("recordsGBIFOCCUR.csv", encoding ='UTF-8', sep="\t") 

# Reduced version of the GBIF dataset -------------------------------------
# Filter fields 
colnames(data)
data <- data %>% select(c("gbifID", "family", "genus", "species", "infraspecificEpithet", 
             "taxonRank", "scientificName",
             "countryCode", "locality", "stateProvince", 
             "decimalLatitude",	"decimalLongitude", "coordinatePrecision", 
             "day", "month", "year", 
             "occurrenceStatus", "basisOfRecord", "recordedBy", "issue"))
colnames(data)

#####------------------ MODULE 1 Basis Of Record -----------------####
# What type of occurrences do we have?
unique(data$basisOfRecord)
# Remove records without appropriate basis of record:
data <- data %>% filter(basisOfRecord == 'PRESERVED_SPECIMEN') # in this example we keep only preserved specimens

#####------------------ MODULE 2 Taxonomy -----------------####
# 1. Is the record identified at a proper taxonomic rank?
# Let's see what do we have in our dataset:
unique(data$taxonRank)
# Filter those records with appropriate taxonomic rank
data <- data %>% filter(taxonRank == 'SUBSPECIES' | taxonRank == 'SPECIES' )
# Extract authors name included in scientifName information
data$authorshipName <- word(data$scientificName, 3, -1)
# Delete records with no name included
data <- data %>% filter(!is.na(authorshipName))

#####------------------ MODULE 3 Geography -----------------####
# 1. Check coordinates values: ----
# Discard records with latitude/longitude values equals to zero, exact same value or NULL
data <- data %>% filter(decimalLatitude != decimalLongitude) %>% 
                  filter(between(decimalLatitude, -90, 90)) %>% 
                  filter(between(decimalLongitude, -180, 180)) %>% 
                  filter(decimalLatitude != 0) %>% 
                  filter(decimalLongitude != 0)
# 2. Check coordinates precision: ----
# Number of decimal digits of coordinates as a measure of precision 
## Function to count number of decimals:
data <- bdc_coordinates_precision(data = data, 
                 lat ='decimalLatitude', 
                 lon = 'decimalLongitude', 
                 ndec = 1) # Filter coordinates with at least 1 decimal place
data <- data %>%  filter(.rou == 'TRUE')
data$.rou <- NULL
# 3. Check records that don't meet location criteria ----
# Check if coordinates are placed in the assigned country, ---- 
# in the following example we check whether records are located in Portugal

# Point in polygon by country analysis

# Import a shapefile of the study area with Adm. Units borders at country level 
# Download world / cpuntry shapefile from https://gadm.org/download_world.html
countries <- read_sf('gadm41_PRT.gpkg')
# Set the correct projection
st_crs(countries) <- "+proj=longlat +datum=WGS84 +no_defs"

# Transform occurrences into spatial points and project:
data$x <- data$decimalLongitude
data$y <- data$decimalLatitude

datapoints  <- st_as_sf(x = data,                         
                   coords = c("x", "y"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")

# Point in polygon: join each point to a polygon based on position:
data <- st_join(datapoints, countries)
# Exclude those that didn't fall in the country polygon or in 'SEA'
data <- data %>% filter(COUNTRY == "Portugal")
# Translate 'countryCode' information (ISO 3166-1-alpha-2 = "iso2c") 
# into country names of GADM 'countryCode'
data$countryName <- countrycode(data$countryCode, 
                                origin = "iso2c", 
                                destination = "country.name")


# Check the names obtained
unique(data$countryName)

# Match country names and label as 'FALSE' errors of location
data <- data %>% mutate(countryCheck = case_when(
                        COUNTRY != data$countryName ~ FALSE,
                        TRUE ~ TRUE))
# Subset and extract records located in country assigned by collector ('correct')
data <- data %>% filter(countryCheck == 'TRUE')
data <- as.data.frame(data)
# Delete Centroids
# Label coordinates placed in centroids of the country or capital
cap <- cc_cap (data,
               lon = "decimalLongitude",
               lat = "decimalLatitude",
               value = "flagged")
data <- cbind(data, cap)
cen <- cc_cen (data,
               lon = "decimalLongitude",
               lat = "decimalLatitude",
               value = "flagged")
data <- cbind(data, cen)
# Label coordinates placed in gbif headquarters
gbif <- cc_gbif(data,
                lon = "decimalLongitude",
                lat = "decimalLatitude",
                value = "flagged")
data <- cbind(data, gbif)

# Label coordinates from museums, gardens, institutions, zoo's... 
inst <- cc_inst(data,
                lon = "decimalLongitude",
                lat = "decimalLatitude",
                value = "flagged")
data <- cbind(data, inst)

# Filter and exclude centroids:
data <- data %>% 
          filter(cap == 'TRUE') %>% 
          filter(cen == 'TRUE') %>% 
          filter(inst == 'TRUE') %>% 
          filter(gbif == 'TRUE')

#####------------------ MODULE 4 Time -----------------####
# Choose level of temporal information:
data <- data %>% 
  filter(!is.na(year) & !is.na(month) & !is.na(day)) # only records with complete date of collection
data$date <- paste(data$day, data$month, data$year, sep = '/') # create a field that combines day/month/year information
# Set temporal range of time
data <- data %>% filter(year >= 1980 & year <= 2020)

#####------------------ MODULE 5 Duplicate -----------------####
# Here we have multiple combination to detect duplicate records
# Choose the appropriate combination of fields based on your study
### SpeciesName + Lat Lon + Date + recordedBy
dataDuplic1 <- cc_dupl (data,
                           lon = "decimalLongitude",
                           lat = "decimalLatitude",
                           species = "species",
                           additions = c('recordedBy', 'year'))

### SpeciesName + Lat Lon + recordedBy
dataDuplic2 <- cc_dupl (data,
                         lon = "decimalLongitude",
                         lat = "decimalLatitude",
                         species = "species",
                         additions = 'recordedBy')

### SpeciesName + Lat Lon + Year
dataDuplic3 <- cc_dupl (data,
                         lon = "decimalLongitude",
                         lat = "decimalLatitude",
                         species = "species",
                         additions = 'year')

### SpeciesName + Lat Lon
dataDuplic4 <- cc_dupl (data,
                         lon = "decimalLongitude",
                         lat = "decimalLatitude",
                         species = "species")

### SpeciesName + Lat Lon with buffer or rounded coordinates
data$lon_round <- round(data$decimalLongitude, 2)
data$lat_round <- round(data$decimalLatitude, 2)
dataDuplic5 <- cc_dupl (data,
                         lon = "lon_round",
                         lat = "lat_round",
                         species = "species")

### SpeciesName + cell
datbiogeo_b <- keepmainfields(data, 
                              ID='gbifID',Species='species', 
                              x='decimalLongitude', y='decimalLatitude')
dataDuplic6 <- duplicatesexclude(datbiogeo_b, 10) # spatial resolution in minutes
dataDuplic6 <- merge(data, dataDuplic6, by.x='gbifID', by.y='ID', all.x=TRUE)
dataDuplic6 <- dataDuplic6 %>% filter(Exclude == 0)

# SAVE THE CLEANED DATASET (Here SpeciesName + Lat Lon with rounded coordinates)
fwrite(dataDuplic5b, 'final_data.csv', sep="\t", row.names = FALSE)