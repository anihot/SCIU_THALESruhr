library(leaflet)
library(htmlwidgets)
library(dplyr)
library(readr)
library(fs)

# Config
metadata_file <- "data/metadata/sensor_metadata.csv"
output_file <- "data/output/sensor_map.html"

cat("Generating interactive sensor map...\n")

if (!file_exists(metadata_file)) {
    stop("Metadata file not found: ", metadata_file)
}

# Ensure output directory exists
if (!dir_exists(path_dir(output_file))) {
    dir_create(path_dir(output_file))
}

# 1. Load coordinates
sensors <- read_csv(metadata_file, show_col_types = FALSE)

# 2. Add latest data to popups
cat("Fetching latest data for popups...\n")
cleaned_dir <- "data/processed/cleaned_analysis"

sensors <- sensors %>%
    rowwise() %>%
    mutate(
        # Attempt to find the cleaned data file for this station
        safe_name = gsub("[^[:alnum:]]", "_", station),
        file_glob = paste0(cleaned_dir, "/", safe_name, "*_cleaned.csv"),
        files = list(Sys.glob(file_glob)),
        latest_level = NA_real_,
        latest_time = NA_character_,
        
        # If file exists, read the last line
        has_data = length(files) > 0,
        temp_data = if(has_data) {
            tryCatch({
                d <- read_csv(files[[1]], show_col_types = FALSE, n_max = 50, skip = max(0, count_fields(files[[1]], tokenizer_csv()) - 50))
                tail(d, 1)
            }, error = function(e) NULL)
        } else { NULL }
    ) %>%
    mutate(
        latest_level = if(!is.null(temp_data)) temp_data$level else NA_real_,
        latest_time = if(!is.null(temp_data)) format(temp_data$Zeit_Datum, "%Y-%m-%d %H:%M") else "No recent data"
    ) %>%
    ungroup()

# Construct Popup Content
sensors <- sensors %>%
    mutate(
        popup_content = paste0(
            "<div style='font-family: Arial, sans-serif;'>",
            "<b>Station: </b>", station, "<br>",
            "<b>Label: </b>", label, "<br><hr>",
            "<b>Aktueller Stand: </b>", ifelse(is.na(latest_level), "N/A", paste0(latest_level, " m")), "<br>",
            "<b>Zeitpunkt: </b>", latest_time,
            "</div>"
        )
    )

# 3. Create Leaflet map
m <- leaflet(sensors) %>%
    addTiles() %>% 
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addMarkers(
        lng = ~lon,
        lat = ~lat,
        popup = ~popup_content,
        label = ~station
    ) %>%
    addMiniMap(toggleDisplay = TRUE)

# 3. Save as HTML
# We set selfcontained = TRUE to ensure all dependencies (JS/CSS) are bundled into the HTML.
# This avoids issues with external folders in different environments.
saveWidget(m, file = output_file, selfcontained = TRUE)

cat("✅ SUCCESS: Interactive map saved to", output_file, "\n")
