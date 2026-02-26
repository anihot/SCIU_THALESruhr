library(ggplot2)
library(sf)
library(dplyr)
library(readr)

# Config
metadata_file <- "data/sensor_metadata.csv"
output_file <- "data/sensor_static_map.png"

cat("Generating static sensor map...\n")

if (!file.exists(metadata_file)) {
    stop("Metadata file not found: ", metadata_file)
}

# 1. Load coordinates
sensors <- read_csv(metadata_file, show_col_types = FALSE)

# Convert to sf object
sensors_sf <- st_as_sf(sensors, coords = c("lon", "lat"), crs = 4326)

# 2. Create Static Map using ggplot2
# We use a clean coordinate system and add labels
p <- ggplot(sensors_sf) +
    # Add a simple grid and clean theme
    theme_minimal() +
    # Add the points
    geom_sf(color = "#0077b6", size = 4, alpha = 0.8) +
    # Add labels with ggrepel-like logic using geom_label
    # We use nudge to move labels slightly so they don't overlap with points
    geom_text(aes(label = station, geometry = geometry),
        stat = "sf_coordinates",
        nudge_y = 0.005,
        size = 4,
        fontface = "bold",
        color = "#023e8a"
    ) +
    # Aesthetics
    labs(
        title = "📍 SCIU THALESruhr Sensor-Netzwerk",
        subtitle = "Standorte der automatisierten Messstationen",
        x = "Längengrad",
        y = "Breitengrad",
        caption = paste0("Stand: ", Sys.time())
    ) +
    theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
        panel.background = element_rect(fill = "#f8f9fa", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank()
    )

# 3. Save as PNG
ggsave(output_file, plot = p, width = 10, height = 7, dpi = 150)

cat("SUCCESS: Static map saved to", output_file, "\n")
