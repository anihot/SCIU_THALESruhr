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

cat("🌐 Generating interactive sensor map with plotly popups...\n")

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

# 3. Build a plotly popup HTML for each sensor (plain loop to avoid rowwise issues)
now_utc <- now(tzone = "UTC")
cutoff  <- now_utc - hours(24)

popup_htmls  <- character(nrow(sensors))
sensor_lats  <- sensors$lat
sensor_lons  <- sensors$lon
sensor_names <- sensors$station

for (i in seq_len(nrow(sensors))) {
    station_name <- sensor_names[i]
    cat("Building popup for:", station_name, "\n")

    sensor_file <- file.path(cleaned_dir, paste0(station_name, "_merged_export_cleaned.csv"))

    # Fallback if no data file
    if (!file.exists(sensor_file)) {
        popup_htmls[i] <- paste0(
            "<div style='width:500px;'>",
            "<h3 style='color:#1a3a5c;'>📍 ", station_name, "</h3>",
            "<p style='color:gray;'><i>Keine Datendatei gefunden.</i></p>",
            "</div>"
        )
        next
    }

    # Load and filter to last 24h
    df <- read_csv(sensor_file, show_col_types = FALSE) %>%
        mutate(Zeit_Datum = as.POSIXct(Zeit_Datum, tz = "UTC")) %>%
        filter(Zeit_Datum >= cutoff, !is.na(level))

    if (nrow(df) == 0) {
        popup_htmls[i] <- paste0(
            "<div style='width:500px;'>",
            "<h3 style='color:#1a3a5c;'>📍 ", station_name, "</h3>",
            "<p style='color:gray;'><i>Keine Daten in den letzten 24h.</i></p>",
            "</div>"
        )
        next
    }

    # Precipitation for this station
    precip_24h <- NULL
    if (!is.null(precip_data)) {
        precip_24h <- precip_data %>%
            filter(station == station_name, timestamp >= cutoff, precipitation_mm > 0)
    }

    # Build plotly chart
    p <- plot_ly()

    # Precipitation bars (secondary y-axis)
    if (!is.null(precip_24h) && nrow(precip_24h) > 0) {
        p <- add_bars(p,
            data   = precip_24h,
            x      = ~timestamp,
            y      = ~precipitation_mm,
            name   = "Niederschlag (mm)",
            marker = list(color = "rgba(100,181,246,0.55)", line = list(width = 0)),
            yaxis  = "y2"
        )
    }

    # Water level line (primary y-axis, m -> cm)
    p <- add_lines(p,
        data = df,
        x    = ~Zeit_Datum,
        y    = ~round(level * 100, 2),
        name = "Wasserstand (cm)",
        line = list(color = "#1565C0", width = 2)
    )

    # Layout
    p <- layout(p,
        title  = list(text = FALSE),
        height = 260,
        margin = list(l = 55, r = 60, t = 8, b = 45),
        xaxis  = list(
            title      = "",
            tickformat = "%d.%m %H:%M"
        ),
        yaxis  = list(
            title     = "Wasserstand (cm)",
            titlefont = list(color = "#1565C0", size = 11),
            tickfont  = list(color = "#1565C0"),
            rangemode = "tozero"
        ),
        yaxis2 = list(
            title      = "Niederschlag (mm)",
            titlefont  = list(color = "#42A5F5", size = 11),
            tickfont   = list(color = "#42A5F5"),
            overlaying = "y",
            side       = "right",
            rangemode  = "tozero",
            showgrid   = FALSE
        ),
        legend        = list(orientation = "h", x = 0, y = -0.22, font = list(size = 10)),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
    )
    p <- config(p,
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
        responsive = TRUE,
        displaylogo = FALSE
    )

    # Render to HTML string
    chart_html <- as.character(as_widget(p))

    popup_htmls[i] <- paste0(
        "<div style='width:510px;font-family:sans-serif;'>",
        "<h3 style='margin:4px 0 2px 0;color:#1a3a5c;'>📍 ", station_name, "</h3>",
        "<p style='margin:0 0 2px 0;color:#888;font-size:0.8em;'>",
        "Letzte 24h &mdash; ",
        "<span style='color:#1565C0;font-weight:bold;'>Wasserstand (cm)</span>",
        " &amp; <span style='color:#42A5F5;font-weight:bold;'>Niederschlag (mm)</span>",
        "</p>",
        chart_html,
        "</div>"
    )
}

# 4. Build Leaflet map
cat("Building Leaflet map...\n")
m <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    fitBounds(
        lng1 = min(sensor_lons), lat1 = min(sensor_lats),
        lng2 = max(sensor_lons), lat2 = max(sensor_lats)
    )

for (i in seq_len(nrow(sensors))) {
    m <- addCircleMarkers(m,
        lng          = sensor_lons[i],
        lat          = sensor_lats[i],
        popup        = popup_htmls[i],
        popupOptions = popupOptions(maxWidth = 550, minWidth = 540),
        label        = sensor_names[i],
        radius       = 10,
        color        = "#1a3a5c",
        fillColor    = "#2196F3",
        fillOpacity  = 0.85,
        weight       = 2,
        stroke       = TRUE
    )
}

# 5. Save map
output_abs <- normalizePath(output_file, mustWork = FALSE)
if (!dir_exists(path_dir(output_abs))) dir_create(path_dir(output_abs))

saveWidget(m, file = output_abs, selfcontained = FALSE)

cat("✅ SUCCESS: Interactive map with plotly popups saved to", output_file, "\n")
