################ Setup ########################



### Install and load necessary libraries ####
install.packages("sf")
install.packages("spdep")
install.packages("spatialreg")
install.packages("spatstat")
install.packages("webshot")
install.packages("tmap")
install.packages("recipes")
install.packages("MASS")
install.packages("pscl")
install.packages("bestNormalize")


### Calling libraries ####

library(readr) # Better for French encoding system
library(sf)
library(ggplot2)
library(reshape2)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(dplyr)
library(spdep)
library(sp)
library(spatialreg)
library(spatstat)
library(raster)
library(htmlwidgets)
library(webshot)
library(tidyr)
library(RColorBrewer)
library(tidyverse)
library(recipes)
library(caret)   # For normalization
library(MASS)  # For Box-Cox transformations
library(car) # For powerTransform function
library(bestNormalize) # For Yeo-Johnson transformation
library(pscl) # For the Zero-inflated model (ZIP)
library(lmtest) # For the Ramesy RESET Test

############################################
# Creating a bounding box for later spatial wrangling 

bbox <- st_as_sfc(st_bbox(c(xmin = -5.45, ymin = 41.33, xmax = 8.23, ymax = 51.09), crs = 4326))

# Setting the directory
setwd("C:/Users/mohammed.younes/Desktop/DB/LVMT_DB")

################# Data Importation ################################################



# 1. Importing the main indicators data
main_indicators <- read.csv("data/analysis/main_sheet.csv", encoding = "UTF-8")


# 2. Importing the communes shapefile and setting the CRS
communes <- st_read("data/shp/france/com_aav/com_aav2020_2024.shp")
communes <- st_set_crs(communes, 4326)


# 3. Importing the AAV shapefile and setting the CRS
aav <- st_read("data/shp/france/AAV/aav2020_2024.shp")
aav <- st_set_crs(aav, 4326)


# 4. Clipping the communes and AAV to include only mainland France (excluding Corsica)
bbox <- st_bbox(c(xmin = -5.5, xmax = 10, ymin = 41, ymax = 51), crs = st_crs(4326))
communes <- st_intersection(communes, bbox)
aav <- st_intersection(aav, bbox)

# 5. Correcting the names of the code columns and converting them to numeric
main_indicators$commune_code <- as.numeric(main_indicators$CODGEO)
communes$commune_code <- as.numeric(communes$codgeo)

# 6. Joining the communes with the indicators data
communes_indicators <- communes %>%
  inner_join(main_indicators, by = c("commune_code" = "commune_code"))

# 7. Importing the unemployment rate data for AAV
unemployment_aav <- read_csv2("data/unemployment_AAV.csv")
unemployment_aav <- as.data.frame(unemployment_aav)
unemployment_aav$Code <- as.numeric(unemployment_aav$Code)

# 8. Joining the unemployment rate with the AAV indicators data
aav_indicators_unemployment_clean <- aav_indicators %>%
  inner_join(unemployment_aav, by = c("aav2020" = "Code")) %>%
  na.omit()

# 9. Importing additional variables (e.g., Utilised Agricultural Area, Vacant Dwellings) and joining with existing data
UtilisedAgroAreaEvo_ChangeNumberofVacantDwellings <- read_csv2("C:/Users/mohammed.younes/Desktop/DATA/INDICATORS/data_raw/UtilisedAgroAreaEvo_ChangeNumberofVacantDwellings.csv")
UtilisedAgroAreaEvo_ChangeNumberofVacantDwellings$aav2020 <- as.numeric(UtilisedAgroAreaEvo_ChangeNumberofVacantDwellings$aav2020)
UtilisedAgroAreaEvo_ChangeNumberofVacantDwellings <- na.omit(UtilisedAgroAreaEvo_ChangeNumberofVacantDwellings)
aav_indicators_unemployment_clean <- aav_indicators_unemployment_clean %>%
  left_join(UtilisedAgroAreaEvo_ChangeNumberofVacantDwellings, by = "aav2020")

GGH_Poverty_BusinessCreation <- read_csv2("C:/Users/mohammed.younes/Desktop/DATA/INDICATORS/data_raw/GGH_Poverty_BusinessCreation.csv")
GGH_Poverty_BusinessCreation$aav2020 <- as.numeric(GGH_Poverty_BusinessCreation$aav2020)
GGH_Poverty_BusinessCreation <- na.omit(GGH_Poverty_BusinessCreation)
aav_indicators_unemployment_clean <- aav_indicators_unemployment_clean %>%
  left_join(GGH_Poverty_BusinessCreation, by = "aav2020")

# Cleaning some duplicate columns and renaming
aav_indicators_unemployment_clean <- aav_indicators_unemployment_clean %>%
  select(-AAV.x, -AAV.y, -prg_2021.x, -prg_2021.y, -poverty_rate_2018.x, -poverty_rate_2018.y, -buisness_creation.x, -buisness_creation.y, -nb_log_vac_14_20.y, -nb_log_vac_14_20.x, -sau_10_20.x, -sau_10_20.y)
colnames(aav_indicators_unemployment_clean) <- make.names(colnames(aav_indicators_unemployment_clean))

# 10. Final cleaning and export
indicatros_without_geo <- st_set_geometry(aav_indicators_unemployment_clean, NULL)
indicatros_without_geo <- indicatros_without_geo %>%
  mutate_if(is.numeric, function(x) as.numeric(gsub(",", ".", as.character(x)))) %>%
  as.data.frame()

write.csv2(indicatros_without_geo, "indicatros_without_geo.csv", row.names = FALSE)


#########################




data <- read.csv2("data/WHData/ecommerce/EcommerceManuallyCollected.csv")
data$longitude <- as.numeric(data$longitude)
data$latitude <- as.numeric(data$latitude)
data <- data %>%
  rename(country = Ã..country)

# data <- data %>% mutate(across(c(opened, closed), as.Date))

# Converting the data to sf object
data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)

# Converting siren field is treated as character to avoid issues with large numbers

data_sf$siren <- as.character(data_sf$siren)


plot(data_sf)

# Write the sf object to a shapefile
st_write(data_sf, "data/shp/manually_collected/ecommerceWH_1.shp", layer_options = "ENCODING=UTF-8")



