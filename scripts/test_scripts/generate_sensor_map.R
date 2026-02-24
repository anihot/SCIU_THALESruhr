library(leaflet)
library(htmlwidgets)
library(dplyr)
library(readr)
library(fs)

# Config
metadata_file <- "data/sensor_metadata.csv"
output_file <- "data/sensor_map.html"

cat("Generating interactive sensor map...\n")

if (!file_exists(metadata_file)) {
    stop("Metadata file not found: ", metadata_file)
}

# 1. Load coordinates
sensors <- read_csv(metadata_file, show_col_types = FALSE)

# 2. Create Leaflet map
m <- leaflet(sensors) %>%
    addTiles() %>% # Add default OpenStreetMap map tiles
    addProviderTiles(providers$CartoDB.Positron) %>% # Add a clean, light base map
    addMarkers(
        lng = ~lon,
        lat = ~lat,
        popup = ~ paste0("<b>Station: ", station, "</b><br>", label),
        label = ~station
    ) %>%
    # Add a mini map in the corner
    addMiniMap(toggleDisplay = TRUE)

# 3. Save as HTML
# We set selfcontained = FALSE to avoid Pandoc dependency issues in some environments.
# This will create a 'sensor_map_files' folder alongside the HTML.
saveWidget(m, file = output_file, selfcontained = FALSE)

cat("SUCCESS: Interactive map saved to", output_file, "\n")
