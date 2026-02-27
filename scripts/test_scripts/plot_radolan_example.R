library(rdwd)
library(terra)
library(ggplot2)
library(readr)
library(dplyr)
library(sf)

# Config
metadata_file <- "data/sensor_metadata.csv"
output_plot <- "data/plots/radolan_example.png"

cat("🎨 Generating RADOLAN Example Plot...\n")

# 1. Load sensor coordinates
sensors <- read_csv(metadata_file, show_col_types = FALSE) %>%
    select(station, lat, lon)

# Convert sensors to sf object (WGS84)
sensors_sf <- st_as_sf(sensors, coords = c("lon", "lat"), crs = 4326)

# 2. Get latest RADOLAN RW file
urls <- selectDWD(res = "recent", var = "radolan/rw", per = "hr")
latest_url <- urls[length(urls)]
cat("Downloading latest file:", basename(latest_url), "\n")

file <- dataDWD(latest_url, dir = tempdir(), read = FALSE, quiet = TRUE)

# 3. Read and project
# readDWD converts the RADOLAN binary to a SpatRaster and sets the CRS
grid <- readDWD(file)

# Project sensors to RADOLAN CRS (Stereographic)
sensors_proj <- st_transform(sensors_sf, crs(grid))

# 4. Crop to the relevant area (NRW area around sensors)
# Sensors are around 51.5N, 7E.
# Let's crop to a 100km window
extent_padding <- 50000 # 50km
sensor_bounds <- st_bbox(sensors_proj)
crop_extent <- ext(
    sensor_bounds["xmin"] - extent_padding,
    sensor_bounds["xmax"] + extent_padding,
    sensor_bounds["ymin"] - extent_padding,
    sensor_bounds["ymax"] + extent_padding
)
grid_cropped <- crop(grid, crop_extent)

# 5. Plot
# Convert SpatRaster to data.frame for ggplot
grid_df <- as.data.frame(grid_cropped, xy = TRUE)
names(grid_df)[3] <- "precip"

p <- ggplot() +
    geom_raster(data = grid_df, aes(x = x, y = y, fill = precip)) +
    scale_fill_viridis_c(name = "Niederschlag\n(mm/h)", na.value = "transparent") +
    geom_sf(data = sensors_proj, color = "red", size = 3) +
    geom_sf_text(
        data = sensors_proj, aes(label = station),
        color = "white", vjust = -1, size = 3, fontface = "bold"
    ) +
    theme_minimal() +
    labs(
        title = paste("RADOLAN RW Example:", format(as.POSIXct(gsub(".*RW_([0-9]{10}).*", "\\1", basename(file)), format = "%y%m%d%H%M"), "%Y-%m-%d %H:%M")),
        subtitle = "Standorte der THALES Sensoren (rot markiert)",
        x = "Ostwert", y = "Nordwert"
    ) +
    coord_sf()

# Save
if (!dir.exists("data/plots")) dir.create("data/plots", recursive = TRUE)
ggsave(output_plot, p, width = 10, height = 8, dpi = 300)

cat("✅ Example plot saved to:", output_plot, "\n")
