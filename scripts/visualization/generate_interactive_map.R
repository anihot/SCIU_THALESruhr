library(leaflet)
library(htmlwidgets)
library(htmltools)
library(plotly)
library(dplyr)
library(readr)
library(lubridate)
library(fs)

# Config
metadata_file  <- "data/metadata/sensor_metadata.csv"
cleaned_dir    <- "data/processed/cleaned_analysis"
precip_file    <- "data/processed/precipitation_at_sensors.csv"
output_file    <- "data/output/sensor_map.html"

cat("🌐 Generating interactive sensor map with plotly popups (forced update)...\n")

if (!file.exists(metadata_file)) stop("Metadata file not found: ", metadata_file)

# 1. Load sensor metadata
sensors <- read_csv(metadata_file, show_col_types = FALSE)

# 2. Load precipitation data (may be empty)
if (file.exists(precip_file)) {
    precip_data <- read_csv(precip_file, show_col_types = FALSE) %>%
        mutate(timestamp = as.POSIXct(timestamp, tz = "UTC"))
} else {
    precip_data <- NULL
}

# 3. Build a plotly popup HTML for each sensor
now_utc <- now(tzone = "UTC")
cutoff  <- now_utc - hours(24)

popup_htmls  <- character(nrow(sensors))
sensor_lats  <- sensors$lat
sensor_lons  <- sensors$lon
sensor_names <- sensors$station

# We'll use this to capture the plotly dependency
sample_p <- NULL

for (i in seq_len(nrow(sensors))) {
    station_name <- sensor_names[i]
    cat("Building popup for:", station_name, "\n")

    sensor_file <- file.path(cleaned_dir, paste0(station_name, "_merged_export_cleaned.csv"))

    if (!file.exists(sensor_file)) {
        popup_htmls[i] <- paste0("<div style='width:500px;'><h3>📍 ", station_name, "</h3><p>Keine Daten.</p></div>")
        next
    }

    df <- read_csv(sensor_file, show_col_types = FALSE) %>%
        mutate(Zeit_Datum = as.POSIXct(Zeit_Datum, tz = "UTC")) %>%
        filter(Zeit_Datum >= cutoff, !is.na(level))

    if (nrow(df) == 0) {
        popup_htmls[i] <- paste0("<div style='width:500px;'><h3>📍 ", station_name, "</h3><p>Keine Daten in 24h.</p></div>")
        next
    }

    precip_24h <- NULL
    if (!is.null(precip_data)) {
        precip_24h <- precip_data %>% filter(station == station_name, timestamp >= cutoff, precipitation_mm > 0)
    }

    p <- plot_ly()
    if (!is.null(precip_24h) && nrow(precip_24h) > 0) {
        p <- add_bars(p, data=precip_24h, x=~timestamp, y=~precipitation_mm, name="Regen (mm)", 
                      marker=list(color="rgba(100,181,246,0.5)"), yaxis="y2")
    }
    p <- add_lines(p, data=df, x=~Zeit_Datum, y=~round(level * 100, 2), name="Level (cm)", 
                   line=list(color="#1565C0", width=2))
    
    p <- layout(p, height=250, margin=list(l=50, r=50, t=10, b=40),
                xaxis=list(title="", tickformat="%H:%M"),
                yaxis=list(title="Level (cm)", titlefont=list(color="#1565C0"), tickfont=list(color="#1565C0"), rangemode="tozero"),
                yaxis2=list(title="Regen (mm)", titlefont=list(color="#42A5F5"), tickfont=list(color="#42A5F5"), 
                            overlaying="y", side="right", rangemode="tozero", showgrid=FALSE),
                legend=list(orientation="h", x=0, y=-0.2),
                paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)")
    p <- config(p, displayModeBar=FALSE, responsive=TRUE)

    if (is.null(sample_p)) sample_p <- p
    
    # Critical: Convert to HTML but we MUST ensure dependencies are in the final map
    chart_html <- as.character(tags$div(style="width:100%; height:250px;", p))

    popup_htmls[i] <- paste0(
        "<div style='width:520px;font-family:sans-serif;'>",
        "<h3 style='margin:4px 0; color:#1a3a5c;'>📍 ", station_name, "</h3>",
        chart_html,
        "</div>"
    )
}

# 4. Build Leaflet map
cat("Building Leaflet map...\n")
m <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    fitBounds(lng1=min(sensor_lons), lat1=min(sensor_lats), lng2=max(sensor_lons), lat2=max(sensor_lats))

for (i in seq_len(nrow(sensors))) {
    m <- addCircleMarkers(m, lng=sensor_lons[i], lat=sensor_lats[i], 
                          popup=popup_htmls[i], popupOptions=popupOptions(maxWidth=550, minWidth=540),
                          label=sensor_names[i], radius=10, color="#1a3a5c", fillColor="#2196F3", 
                          fillOpacity=0.85, weight=2)
}

# 5. ATTACH PLOTLY DEPENDENCIES
if (!is.null(sample_p)) {
    # Extract dependencies from a plotly object
    pl_deps <- htmlwidgets::getDependency("plotly", "plotly")
    # Also need crosstalk if used, but let's start with plotly
    m$dependencies <- c(m$dependencies, pl_deps)
}

# 6. Save map
output_abs <- normalizePath(output_file, mustWork = FALSE)
if (!dir_exists(path_dir(output_abs))) dir_create(path_dir(output_abs))

saveWidget(m, file = output_abs, selfcontained = FALSE)

cat("✅ SUCCESS: Interactive map with plotly popups saved to", output_file, "\n")
