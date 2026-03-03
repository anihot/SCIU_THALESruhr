library(ggplot2)
library(sf)
library(dplyr)
library(readr)
library(ggspatial)

# Config
metadata_file <- "data/metadata/sensor_metadata.csv"
output_file <- "data/output/sensor_static_map.png"

cat("Generating static sensor map with background tiles...\n")

if (!file.exists(metadata_file)) {
    stop("Metadata file not found: ", metadata_file)
}

# 1. Load coordinates
sensors <- read_csv(metadata_file, show_col_types = FALSE)

# Convert to sf object (WGS84)
sensors_sf <- st_as_sf(sensors, coords = c("lon", "lat"), crs = 4326)

# Check for required packages for tiles
required_packages <- c("prettymapr", "rosm")
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages)) {
    stop(
        "Missing packages for map tiles: ", paste(missing_packages, collapse = ", "),
        ". Please run: install.packages(c('prettymapr', 'rosm'))"
    )
}

# 2. Create Static Map using ggplot2 and ggspatial
# Add some padding to make it more square and less stretched
bbox <- st_bbox(sensors_sf)
lon_range <- bbox["xmax"] - bbox["xmin"]
lat_range <- bbox["ymax"] - bbox["ymin"]
# Increase lon range to match lat range (roughly) for 1:1 aspect
target_lon_range <- lat_range * 1.5 # at 51 deg N, lon degrees are wider
padding_lon <- (target_lon_range - lon_range) / 2

p <- ggplot(sensors_sf) +
    # Add OpenStreetMap tiles as background
    annotation_map_tile(type = "osm", zoomin = -1) +
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
    # Fix the coordinate system and set limits with padding
    coord_sf(
        xlim = c(bbox["xmin"] - padding_lon - 0.01, bbox["xmax"] + padding_lon + 0.01),
        ylim = c(bbox["ymin"] - 0.01, bbox["ymax"] + 0.01),
        crs = 4326, expand = FALSE
    ) +
    # Aesthetics
    labs(
        title = "📍 SCIU THALESruhr Sensor-Netzwerk",
        subtitle = "Standorte der automatisierten Messstationen (Hintergrund: OpenStreetMap)",
        x = "Längengrad",
        y = "Breitengrad",
        caption = paste0("Stand: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
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

# 3. Save as PNG (using a more square format 8x8)
ggsave(output_file, plot = p, width = 8, height = 8, dpi = 150)

cat("SUCCESS: Static map with background saved to", output_file, "\n")
