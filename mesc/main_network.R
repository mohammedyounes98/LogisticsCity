library(sf)
library(tidyverse)

windowsFonts(georg = windowsFont('Georgia'))

# Download the Europe OSM file
u <- "https://download.geofabrik.de/europe-latest.osm.pbf"
download.file(u, basename(u), mode="wb")





# Download the roads network for countries surrounding France
countries <- c("belgium", "france", "luxembourg", "germany", "switzerland", "italy", "spain", "andorra", "monaco")

for (country in countries) {
  u <- paste0("https://download.geofabrik.de/europe/", country, "-latest.osm.pbf")
  download.file(u, basename(u), mode="wb")
}


# Query all roads
q <- "SELECT highway 
      FROM lines 
      WHERE highway IN('motorway', 'motorway_link', 'trunk', 'trunk_link', 
                      'primary', 'primary_link', 'secondary', 'secondary_link', 
                      'tertiary', 'tertiary_link', 'unclassified', 'residential', 
                      'service', 'living_street', 'pedestrian', 'track', 
                      'busway', 'cycleway', 'footway', 'path', 'steps', 
                      'bridleway', 'construction')"

# Read the OSM files and filter roads
roads <- list()
for (country in countries) {
  file <- paste0(country, "-latest.osm.pbf")
  roads[[country]] <- st_read(file, query = q) %>% 
    st_transform(4326) %>% 
    st_as_sf()
}

# Combine the roads data for all countries
roads_combined <- do.call(rbind, roads)

# Perform network analysis using the roads data
# ... (insert your network analysis code here)







# Load the osmread package
library(osmread)

# Set the path to the folder containing the PBF files
pbf_folder <- "C:\\Users\\mohammed.younes\\Desktop\\DATA\\OSM\\PBF"

# Get a list of all PBF files in the folder
pbf_files <- dir(pbf_folder, pattern = "*.osm.pbf")

# Initialize an empty list to store the airports data
airports_list <- list()

# Loop through each PBF file

for (pbf_file in pbf_files) {
  # Read the PBF file
  osm_data <- read_osm(file.path(pbf_folder, pbf_file))
  
  # Extract the nodes and ways with the "aeroway" and "aerodrome" tags
  airports <- osm_data$nodes[osm_data$nodes-tags$aeroway == "aerodrome" & osm_data$nodes-tags$international == "yes", ]
  airports <- rbind(airports, osm_data$ways[osm_data$ways-tags$aeroway == "aerodrome" & osm_data$ways-tags$international == "yes", ])
  
  # Extract the relevant columns (e.g., name, lat, lon)
  airports <- airports[, c("name", "lat", "lon")]
  
  # Add the country name to the airports data
  airports$country <- gsub(".osm.pbf", "", pbf_file)
  
  # Add the airports data to the list
  airports_list <- c(airports_list, list(airports))
}

# Combine the airports data for all countries
airports_combined <- do.call(rbind, airports_list)

# View the extracted airports data
head(airports_combined)