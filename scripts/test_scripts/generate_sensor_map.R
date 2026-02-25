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
# We set selfcontained = TRUE to ensure all dependencies (JS/CSS) are bundled into the HTML.
# This avoids issues with external folders in different environments.
saveWidget(m, file = output_file, selfcontained = TRUE)

cat("SUCCESS: Interactive map saved to", output_file, "\n")
