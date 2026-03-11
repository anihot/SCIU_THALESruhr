library(leaflet)
library(htmlwidgets)
library(dplyr)
library(readr)
library(fs)
library(plotly)
library(leafpop)
library(lubridate)

# Config
metadata_file <- "data/metadata/sensor_metadata.csv"
output_file <- "data/output/sensor_map.html"
cleaned_dir <- "data/processed/cleaned_analysis"

cat("Generating interactive sensor map with Plotly popups...\n")

if (!file_exists(metadata_file)) {
    stop("Metadata file not found: ", metadata_file)
}

# Ensure output directory exists
if (!dir_exists(path_dir(output_file))) {
    dir_create(path_dir(output_file))
}

# 1. Load coordinates
sensors <- read_csv(metadata_file, show_col_types = FALSE)

# 2. Prepare plots for each sensor
cat("Generating 24h plots for popups...\n")
plot_list <- list()

for (i in seq_len(nrow(sensors))) {
    station_name <- sensors$station[i]
    station_label <- sensors$label[i]
    
    # Find matching data file
    safe_name <- gsub("[^[:alnum:]]", "_", station_name)
    file_glob <- paste0(cleaned_dir, "/", safe_name, "*_cleaned.csv")
    matching_files <- Sys.glob(file_glob)
    
    p <- NULL
    
    if (length(matching_files) > 0) {
        # Load data
        df <- read_csv(matching_files[1], show_col_types = FALSE)
        
        if (nrow(df) > 0) {
            # Filter for last 24 hours
            current_time <- now(tzone = tz(df$Zeit_Datum))
            start_time <- current_time - hours(24)
            df_24h <- df %>% filter(Zeit_Datum >= start_time)
            
            if (nrow(df_24h) > 0) {
                # Create interactive plotly chart
                p <- plot_ly(df_24h, x = ~Zeit_Datum, y = ~level, type = 'scatter', mode = 'lines',
                             line = list(color = '#0072B2', width = 2),
                             fill = 'tozeroy', fillcolor = 'rgba(0, 114, 178, 0.2)') %>%
                    layout(
                        title = list(text = paste("24h Trend:", station_label), font = list(size = 12)),
                        xaxis = list(title = "", gridcolor = "#f0f0f0"),
                        yaxis = list(title = "Level (m)", gridcolor = "#f0f0f0"),
                        margin = list(l = 40, r = 10, t = 30, b = 30),
                        showlegend = FALSE,
                        hovermode = "x unified"
                    ) %>%
                    config(displayModeBar = FALSE)
            }
        }
    }
    
    if (is.null(p)) {
        # Fallback if no data found
        p <- plotly_empty() %>%
            layout(title = list(text = paste("No 24h data for", station_label), font = list(size = 10)))
    }
    
    plot_list[[i]] <- p
}

# 3. Create Leaflet map
m <- leaflet(sensors) %>%
    addTiles() %>% 
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addMarkers(
        lng = ~lon,
        lat = ~lat,
        label = ~label,
        group = "sensors"
    )

# 4. Add Plotly popups
# leafpop::addPopupGraphs integrates the plotly objects into the markers
m <- m %>%
    addPopupGraphs(plot_list, group = "sensors", width = 350, height = 250) %>%
    addMiniMap(toggleDisplay = TRUE)

# 5. Save as HTML
# selfcontained = TRUE is critical for the map to work as a standalone file in the browser.
saveWidget(m, file = output_file, selfcontained = TRUE)

cat("✅ SUCCESS: Interactive map with Plotly popups saved to", output_file, "\n")
