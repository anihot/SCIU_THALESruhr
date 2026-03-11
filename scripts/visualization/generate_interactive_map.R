library(leaflet)
library(htmlwidgets)
library(htmltools)
library(dplyr)
library(readr)
library(fs)
library(plotly)
library(lubridate)

# Config
metadata_file <- "data/metadata/sensor_metadata.csv"
output_file   <- "data/output/sensor_map.html"
cleaned_dir   <- "data/processed/cleaned_analysis"

cat("Generating interactive sensor map with inline Plotly popups...\n")

if (!file_exists(metadata_file)) stop("Metadata file not found: ", metadata_file)
if (!dir_exists(path_dir(output_file))) dir_create(path_dir(output_file))

# 1. Load sensor coordinates
sensors <- read_csv(metadata_file, show_col_types = FALSE)

# 2. Build one Plotly chart per sensor and serialise to HTML string
cat("Building 24h plots...\n")

make_popup_html <- function(station_name, station_label) {
    # Robust glob: replace non-alphanumeric chars (e.g. Umlauts, spaces) with wildcard
    clean_search  <- gsub("[^a-zA-Z0-9]+", "*", station_name)
    file_glob     <- paste0(cleaned_dir, "/*", clean_search, "*_cleaned.csv")
    matching_files <- Sys.glob(file_glob)

    if (length(matching_files) == 0) {
        cat("  [WARN] No data file found for:", station_name, "-> glob:", file_glob, "\n")
        return(paste0("<b>", station_label, "</b><br>Keine Daten gefunden."))
    }

    df <- read_csv(matching_files[1], show_col_types = FALSE)
    cat("  [OK]  Loaded", nrow(df), "rows for:", station_name, "\n")

    if (nrow(df) == 0) return(paste0("<b>", station_label, "</b><br>Datei leer."))

    # Latest reading for subtitle
    latest_row  <- tail(df, 1)
    meas_level  <- ifelse(is.na(latest_row$level), "N/A", paste0(round(latest_row$level, 3), " m"))
    meas_time   <- format(latest_row$Zeit_Datum, "%d.%m. %H:%M")

    # Filter last 24 h
    tz_data    <- tz(df$Zeit_Datum)
    start_time <- now(tzone = tz_data) - hours(24)
    df_24h     <- df %>% filter(Zeit_Datum >= start_time)

    if (nrow(df_24h) == 0) {
        cat("  [WARN] No data in last 24h for:", station_name, "\n")
        return(paste0("<b>", station_label, "</b><br>Aktuell: ", meas_level, " (", meas_time, ")<br>Keine 24h-Daten."))
    }

    cat("  [OK]  24h rows:", nrow(df_24h), "\n")

    # Build Plotly object
    p <- plot_ly(df_24h, x = ~Zeit_Datum, y = ~level,
                 type = "scatter", mode = "lines",
                 line = list(color = "#0072B2", width = 2),
                 fill = "tozeroy", fillcolor = "rgba(0, 114, 178, 0.2)",
                 hovertemplate = "%{x|%H:%M}<br>%{y:.3f} m<extra></extra>") %>%
        layout(
            title       = list(text = paste0("<b>", station_label, "</b>  <sup>", meas_level, " (", meas_time, ")</sup>"),
                               font = list(size = 12), x = 0),
            xaxis       = list(title = "", gridcolor = "#eeeeee"),
            yaxis       = list(title = "Level (m)", gridcolor = "#eeeeee"),
            margin      = list(l = 45, r = 10, t = 35, b = 30),
            showlegend  = FALSE,
            hovermode   = "x unified",
            plot_bgcolor  = "white",
            paper_bgcolor = "white"
        ) %>%
        config(displayModeBar = FALSE, scrollZoom = FALSE)

    # Serialise to self-contained HTML snippet (no outer <html>/<body>)
    html_str <- as.character(as_widget(p))
    return(html_str)
}

# Generate popup HTML for each sensor
popup_htmls <- mapply(
    make_popup_html,
    station_name  = sensors$station,
    station_label = sensors$label,
    SIMPLIFY      = FALSE
)

# 3. Build Leaflet map with embedded popup HTML
m <- leaflet(sensors) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addMarkers(
        lng   = ~lon,
        lat   = ~lat,
        label = ~label,
        popup = lapply(popup_htmls, htmltools::HTML)   # embed raw HTML
    ) %>%
    addMiniMap(toggleDisplay = TRUE)

# 4. Save standalone HTML (selfcontained bundles all JS/CSS)
saveWidget(m, file = output_file, selfcontained = TRUE)

cat("✅ SUCCESS: Map saved to", output_file, "\n")
