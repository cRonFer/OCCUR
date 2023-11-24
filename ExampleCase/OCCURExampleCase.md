---
title: "Example Case to use the information provided in OCCUR"

output: 
  html_document:
    keep_md: true
---

The following script contains basic steps for cleaning and filtering biodiversity records after the downloading process. It is organised by module as it is shown in OCCUR.

Important: These example filters GBIF records of three species of mosses from Spain and Portugal and keep only those that are placed in Portugal

```{.r .klippy}
klippy::klippy(position = c('top', 'right'), color = 'darkred')
```

### Load packages


```{.r .klippy}
library(rgbif) 
library(bdc) 
library(tidyverse) # data manipulation 
library(dplyr) 
library(GADMTools) # administrative units and GIS 
library(CoordinateCleaner) # data cleaning functions 
library(countrycode) # country names standardization 
library(biogeo) # Environment and data 
```

### Prepare the environment


```{.r .klippy}
rm(list = ls(all.names = TRUE)) 

# wd <- '' # Write working directory path 
# setwd(wd)
```

### Get data

#### Only run section a if you want to download your own dataset (needs user account in GBIF)

#### a) Download the dataset from GBIF


```{.r .klippy}
spList <- c('Homalothecium aureum', 'Syntrichia ruralis', 'Tortella squarrosa')
gbif_taxKeys <- name_backbone_checklist(spList) %>% 
  filter(!matchType == 'NONE') %>% 
  pull(usageKey)

# occ_download(pred_in('taxonKey', gbif_taxKeys), # download 3 species names from our list 
# pred('hasCoordinate', TRUE), # this example only download records with associated coordinates 
# format = 'SIMPLE_CSV', user = 'xxx', pwd = 'xxx',email = 'xxx') 
# include user information

# Do not run: 
# occ_download_wait('xxxxxxx-xxxxxxx') 
# occData <- occ_download_get('xxxxxxx-xxxxxxx') %>% 
#             occ_download_import()
```

#### b) Load dataset: Start here using the data from GitHub

#### Read occurrences csv and check dimensions


```{.r .klippy}
data <- read.csv("recordsGBIFOCCUR.csv", encoding ='UTF-8', sep="\t")
```

#### Reduced version of the GBIF dataset

Filter fields


```{.r .klippy}
colnames(data) 
```

```
##  [1] "gbifID"                           "datasetKey"                      
##  [3] "occurrenceID"                     "kingdom"                         
##  [5] "phylum"                           "class"                           
##  [7] "order"                            "family"                          
##  [9] "genus"                            "species"                         
## [11] "infraspecificEpithet"             "taxonRank"                       
## [13] "scientificName"                   "verbatimScientificName"          
## [15] "verbatimScientificNameAuthorship" "countryCode"                     
## [17] "locality"                         "stateProvince"                   
## [19] "occurrenceStatus"                 "individualCount"                 
## [21] "publishingOrgKey"                 "decimalLatitude"                 
## [23] "decimalLongitude"                 "coordinateUncertaintyInMeters"   
## [25] "coordinatePrecision"              "elevation"                       
## [27] "elevationAccuracy"                "depth"                           
## [29] "depthAccuracy"                    "eventDate"                       
## [31] "day"                              "month"                           
## [33] "year"                             "taxonKey"                        
## [35] "speciesKey"                       "basisOfRecord"                   
## [37] "institutionCode"                  "collectionCode"                  
## [39] "catalogNumber"                    "recordNumber"                    
## [41] "identifiedBy"                     "dateIdentified"                  
## [43] "license"                          "rightsHolder"                    
## [45] "recordedBy"                       "typeStatus"                      
## [47] "establishmentMeans"               "lastInterpreted"                 
## [49] "mediaType"                        "issue"
```

```{.r .klippy}
data <- data %>% select(c("gbifID", "family", "genus", "species", "infraspecificEpithet", "taxonRank", "scientificName", "countryCode", "locality", "stateProvince", "decimalLatitude", "decimalLongitude", "coordinatePrecision", "day", "month", "year", "occurrenceStatus", "basisOfRecord", "recordedBy", "issue")) 
colnames(data)
```

```
##  [1] "gbifID"               "family"               "genus"               
##  [4] "species"              "infraspecificEpithet" "taxonRank"           
##  [7] "scientificName"       "countryCode"          "locality"            
## [10] "stateProvince"        "decimalLatitude"      "decimalLongitude"    
## [13] "coordinatePrecision"  "day"                  "month"               
## [16] "year"                 "occurrenceStatus"     "basisOfRecord"       
## [19] "recordedBy"           "issue"
```

------------------------------------------------------------------------

## MODULE 1: Basis Of Record

What type of occurrences do we have?


```{.r .klippy}
unique(data$basisOfRecord) 
```

```
## [1] "PRESERVED_SPECIMEN" "HUMAN_OBSERVATION"
```

Remove records without appropriate basis of record:


```{.r .klippy}
data <- data %>% filter(basisOfRecord == 'PRESERVED_SPECIMEN') # in this example we keep only preserved specimens
```

------------------------------------------------------------------------

## MODULE 2: Taxonomy

### 1. Is the record identified at a proper taxonomic rank?

Let's see what do we have in our dataset:


```{.r .klippy}
unique(data$taxonRank)
```

```
## [1] "SPECIES"    "VARIETY"    "SUBSPECIES"
```

Filter those records with appropriate taxonomic rank


```{.r .klippy}
data <- data %>% filter(taxonRank == 'SUBSPECIES' | taxonRank == 'SPECIES' )
```

### 2. Select only records that include authorship information in their original scientific names proposed

Extract authors name included in scientifName information


```{.r .klippy}
data$authorshipName <- word(data$scientificName, 3, -1)
```

Delete records with no name included


```{.r .klippy}
data <- data %>% filter(!is.na(authorshipName))
```

------------------------------------------------------------------------

## MODULE 3: Geography

### 1. Check coordinates values

Discard records with latitude/longitude values equals to zero, exact same value or NULL


```{.r .klippy}
data <- data %>% 
          filter(decimalLatitude != decimalLongitude) %>% 
          filter(between(decimalLatitude, -90, 90)) %>% 
          filter(between(decimalLongitude, -180, 180)) %>% 
          filter(decimalLatitude != 0) %>% 
          filter(decimalLongitude != 0)
```

### 2. Check coordinates precision

Number of decimal digits of coordinates as a measure of precision

Function to count number of decimals:


```{.r .klippy}
#Filter coordinates with at least 1 decimal place
data <- bdc_coordinates_precision(data = data, 
                                  lat ='decimalLatitude', 
                                  lon = 'decimalLongitude', ndec = 1) 
```

```
## bdc_coordinates_precision:
## Flagged 8 records
## One column was added to the database.
```

```{.r .klippy}
data <- data %>% filter(.rou == 'TRUE') 
data$.rou <- NULL 
```

### 3. Check records that don't meet location criteria

Check if coordinates are placed in the assigned country, in the following example we check whether records are located in Portugal

#### Point in polygon by country analysis

Import a shapefile of the study area with Adm. Units borders at country level

Download world / country shapefile from <https://gadm.org/download_world.html>


```{.r .klippy}
countries <- read_sf('gadm41_PRT.gpkg') # Set the correct projection 
st_crs(countries) <- "+proj=longlat +datum=WGS84 +no_defs"
```

#### Transform occurrences into spatial points and project:


```{.r .klippy}
data$x <- data$decimalLongitude 
data$y <- data$decimalLatitude

datapoints <- st_as_sf(x = data,coords = c("x", "y"), 
                       crs = "+proj=longlat +datum=WGS84 +no_defs")
```

#### Point in polygon: join each point to a polygon based on position


```{.r .klippy}
data <- st_join(datapoints, countries) # Exclude those that didn't fall in the country polygon or in 'SEA' 
data <- data %>% filter(COUNTRY == "Portugal") 
# Translate 'countryCode' information (ISO 3166-1-alpha-2 = "iso2c") into country names of GADM 'countryCode' 
data$countryName <- countrycode(data$countryCode, 
                                origin = "iso2c", 
                                destination = "country.name")
# Check the names obtained
unique(data$countryName)
# Match country names and label as 'FALSE' errors of location
data <- data %>% 
  mutate(countryCheck = case_when(COUNTRY != data$countryName ~ FALSE, 
                                                          TRUE ~ TRUE)) 
# Subset and extract records located in country assigned by collector ('correct') 
data <- data %>% filter(countryCheck == 'TRUE') 
data <- as.data.frame(data)
```

#### Delete Centroids:


```{.r .klippy}
# Label coordinates placed in centroids of the country or capital 
cap <- cc_cap (data, lon = "decimalLongitude", 
                     lat = "decimalLatitude", value = "flagged") 
data <- cbind(data, cap) 
cen <- cc_cen (data, lon = "decimalLongitude", 
                     lat = "decimalLatitude", value = "flagged") 
data <- cbind(data, cen)  
# Label coordinates placed in gbif headquarters
gbif <- cc_gbif(data, lon = "decimalLongitude", 
                    lat = "decimalLatitude", value = "flagged") 
data <- cbind(data, gbif)
# Label coordinates from museums, gardens, institutions, zoo's…
inst <- cc_inst(data, lon = "decimalLongitude",
                      lat = "decimalLatitude", value = "flagged")
data <- cbind(data, inst)
#Filter and exclude centroids:
data <- data %>% filter(cap == 'TRUE') %>% 
                  filter(cen == 'TRUE') %>% 
                  filter(gbif == 'TRUE') %>% 
                  filter(inst == 'TRUE')
```

------------------------------------------------------------------------

## MODULE 4: Time

#### a. Choose level of temporal information:


```{.r .klippy}
# Only records with complete date of collection
data <- data %>% 
        filter(!is.na(year) | !is.na(month) | !is.na(day))  
# Create a field that combines day/month/year information
data$date <- paste(data$day, data$month, data$year, sep = '/')
```

#### b. Set temporal range of time:


```{.r .klippy}
data <- data %>% filter(year >= 1980 & year <= 2020)
```

------------------------------------------------------------------------

## MODULE 5: Duplicates

Here we have multiple combination to detect duplicate records .

Choose the appropriate combination of fields based on your study:

#### SpeciesName + Lat Lon + Date + recordedBy


```{.r .klippy}
dataDuplic1 <- cc_dupl (data, lon = "decimalLongitude", 
                        lat = "decimalLatitude", 
                        species = "species", 
                        additions = c('recordedBy', 'year'))
```

```
## Testing duplicates
```

```
## Removed 115 records.
```

#### SpeciesName + Lat Lon + recordedBy


```{.r .klippy}
dataDuplic2 <- cc_dupl (data, lon = "decimalLongitude", 
                        lat = "decimalLatitude", 
                        species = "species", 
                        additions = 'recordedBy')
```

```
## Testing duplicates
```

```
## Removed 117 records.
```

#### SpeciesName + Lat Lon + Year


```{.r .klippy}
dataDuplic3 <- cc_dupl (data, lon = "decimalLongitude", 
                        lat = "decimalLatitude", 
                        species = "species", 
                        additions = 'year')
```

```
## Testing duplicates
```

```
## Removed 136 records.
```

#### SpeciesName + Lat Lon


```{.r .klippy}
dataDuplic4 <- cc_dupl (data, lon = "decimalLongitude", 
                        lat = "decimalLatitude", 
                        species = "species")
```

```
## Testing duplicates
```

```
## Removed 147 records.
```

#### SpeciesName + Lat Lon with buffer or rounded coordinates


```{.r .klippy}
data$lon_round <- round(data$decimalLongitude, 2) 
data$lat_round <- round(data$decimalLatitude, 2) 
dataDuplic5 <- cc_dupl (data, lon = "lon_round", 
                        lat = "lat_round", species = "species")
```

```
## Testing duplicates
```

```
## Removed 156 records.
```

#### SpeciesName + cell


```{.r .klippy}
datbiogeo_b <- keepmainfields(data, 
                              ID='gbifID',
                              Species='species',
                              x='decimalLongitude', y='decimalLatitude') 
dataDuplic6 <- duplicatesexclude(datbiogeo_b, 10) # spatial resolution in minutes
dataDuplic6 <- merge(data, dataDuplic6, by.x='gbifID', by.y='ID', all.x=TRUE) 
dataDuplic6 <- dataDuplic6 %>% filter(Exclude == 0)
```

------------------------------------------------------------------------

#### SAVE THE CLEANED DATASET

#### Here SpeciesName + Lat Lon with rounded coordinates


```{.r .klippy}
write.table(dataDuplic5, 'final_data.csv', sep="\t", row.names = FALSE)
```
