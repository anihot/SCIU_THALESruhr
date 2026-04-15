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

# 1. Load coordinates (Herzogstraße in Metadaten für fetch_radolan, aber auf Karten ausgeblendet)
sensors <- read_csv(metadata_file, show_col_types = FALSE) %>%
    filter(station != "Herzogstraße")

# Manual label offsets to avoid overlapping labels
# Wasserstraße and Königsallee are very close together
label_offsets <- tribble(
    ~station,            ~nudge_x, ~nudge_y,
    "Herzogstraße",       0.000,    0.005,
    "An der Kost",        0.000,    0.005,
    "Wasserstraße",      -0.020,   -0.005,
    "Königsallee",        0.000,    0.005,
    "Wasserbaulabor 2",   0.000,    0.005
)
sensors <- sensors %>% left_join(label_offsets, by = "station")

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

# 2. Create Static Map
bbox <- st_bbox(sensors_sf)
lon_range <- bbox["xmax"] - bbox["xmin"]
lat_range <- bbox["ymax"] - bbox["ymin"]
target_lon_range <- lat_range * 1.5
padding_lon <- (target_lon_range - lon_range) / 2

p <- ggplot(sensors_sf) +
    # Light, muted basemap so markers pop
    annotation_map_tile(type = "cartolight", zoomin = -1) +
    # White-outlined dots
    geom_sf(color = "white", size = 5.5, shape = 16) +
    geom_sf(color = "#0072B2", size = 4, shape = 16) +
    # Clean labels with per-station offsets
    geom_label(aes(label = label, geometry = geometry),
        stat = "sf_coordinates",
        nudge_x = sensors_sf$nudge_x,
        nudge_y = sensors_sf$nudge_y,
        size = 3,
        family = "sans",
        fontface = "bold",
        color = "#333333",
        fill = alpha("white", 0.85),
        label.padding = unit(0.2, "lines"),
        label.r = unit(0.15, "lines"),
        label.size = 0.2
    ) +
    coord_sf(
        xlim = c(bbox["xmin"] - padding_lon - 0.005, bbox["xmax"] + padding_lon + 0.005),
        ylim = c(bbox["ymin"] - 0.008, bbox["ymax"] + 0.008),
        crs = 4326, expand = FALSE
    ) +
    labs(x = NULL, y = NULL) +
    theme_void() +
    theme(
        plot.margin = margin(0, 0, 0, 0),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "white", color = NA)
    )

# 3. Save as PNG
ggsave(output_file, plot = p, width = 8, height = 7, dpi = 200)

cat("SUCCESS: Static map saved to", output_file, "\n")
