library(ggplot2)
library(sf)
library(dplyr)
library(readr)
library(ggspatial)

# Config
metadata_file <- "data/sensor_metadata.csv"
output_file <- "data/sensor_static_map.png"

cat("Generating static sensor map with background tiles...\n")

if (!file.exists(metadata_file)) {
    stop("Metadata file not found: ", metadata_file)
}

# 1. Load coordinates
sensors <- read_csv(metadata_file, show_col_types = FALSE)

# Convert to sf object (WGS84)
sensors_sf <- st_as_sf(sensors, coords = c("lon", "lat"), crs = 4326)

# 2. Create Static Map using ggplot2 and ggspatial
p <- ggplot(sensors_sf) +
    # Add OpenStreetMap tiles as background
    # zoom = 13 is usually good for city level, but annotation_map_tile can auto-calculate
    annotation_map_tile(type = "osm", zoomin = 0) +
    # Add the points (using a bright color to stand out against the map)
    geom_sf(color = "red", size = 4, alpha = 0.9) +
    # Add labels with a white background for readability over the map
    geom_label(aes(label = station, geometry = geometry),
        stat = "sf_coordinates",
        nudge_y = 0.003,
        size = 3.5,
        fontface = "bold",
        color = "black",
        fill = alpha("white", 0.7),
        label.padding = unit(0.15, "lines")
    ) +
    # Fix the coordinate system to ensure tiles are not distorted
    coord_sf(crs = 4326) +
    # Aesthetics
    labs(
        title = "📍 SCIU THALESruhr Sensor-Netzwerk",
        subtitle = "Standorte der automatisierten Messstationen (Hintergrund: OpenStreetMap)",
        x = "Längengrad",
        y = "Breitengrad",
        caption = paste0("Stand: ", Sys.time())
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
        panel.background = element_rect(fill = "#f8f9fa", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = alpha("gray", 0.3)),
        panel.grid.minor = element_blank()
    )

# 3. Save as PNG
ggsave(output_file, plot = p, width = 10, height = 7, dpi = 150)

cat("SUCCESS: Static map with background saved to", output_file, "\n")
