library(leaflet)
library(htmlwidgets)
library(htmltools)
library(plotly)
library(dplyr)
library(readr)
library(lubridate)
library(fs)
library(leafpop)

# Config
metadata_file  <- "data/metadata/sensor_metadata.csv"
cleaned_dir    <- "data/processed/cleaned_analysis"
precip_file    <- "data/processed/precipitation_at_sensors.csv"
output_file    <- "data/output/sensor_map.html"

cat("🌐 Generating interactive sensor map with plotly popups via leafpop...\n")

if (!file.exists(metadata_file)) stop("Metadata file not found: ", metadata_file)

sensors <- read_csv(metadata_file, show_col_types = FALSE)

if (file.exists(precip_file)) {
    precip_data <- read_csv(precip_file, show_col_types = FALSE) %>%
        mutate(timestamp = as.POSIXct(timestamp, tz = "UTC"))
} else {
    precip_data <- NULL
}

now_utc <- now(tzone = "UTC")
cutoff  <- now_utc - hours(24)

# Create a list to hold the plots
plots_list <- list()

for (i in seq_len(nrow(sensors))) {
    station_name <- sensors$station[i]
    cat("Building plotly chart for:", station_name, "\n")

    sensor_file <- file.path(cleaned_dir, paste0(station_name, "_merged_export_cleaned.csv"))
    
    if (!file.exists(sensor_file)) {
        # Create a simple plotly chart with a message if no data
        p <- plot_ly() %>% 
             layout(title=list(text="Keine Daten vorhanden", font=list(size=14)), 
                    xaxis=list(visible=FALSE), yaxis=list(visible=FALSE))
        plots_list[[i]] <- p
        next
    }

    df <- read_csv(sensor_file, show_col_types = FALSE) %>%
        mutate(Zeit_Datum = as.POSIXct(Zeit_Datum, tz = "UTC")) %>%
        filter(Zeit_Datum >= cutoff, !is.na(level))

    if (nrow(df) == 0) {
        # Create a simple plotly chart with a message if no recent data
        p <- plot_ly() %>% 
             layout(title=list(text="Keine Daten in den letzten 24h", font=list(size=14)), 
                    xaxis=list(visible=FALSE), yaxis=list(visible=FALSE))
        plots_list[[i]] <- p
        next
    }

    precip_24h <- NULL
    if (!is.null(precip_data)) {
        precip_24h <- precip_data %>% filter(station == station_name, timestamp >= cutoff, precipitation_mm > 0)
    }

    p <- plot_ly()
    if (!is.null(precip_24h) && nrow(precip_24h) > 0) {
        p <- add_bars(p, data=precip_24h, x=~timestamp, y=~precipitation_mm, name="Regen (mm)", 
                      marker=list(color="rgba(100,181,246,0.6)"), yaxis="y2")
    }
    p <- add_lines(p, data=df, x=~Zeit_Datum, y=~round(level * 100, 2), name="Level (cm)", 
                   line=list(color="#1565C0", width=2))
    
    p <- layout(p,
        title  = list(text=paste("Letzte 24h:", station_name), font=list(size=14, color="#1a3a5c")),
        height = 300, 
        margin = list(l=50, r=50, t=40, b=40),
        xaxis  = list(title="", tickformat="%H:%M"),
        yaxis  = list(title="Level (cm)", titlefont=list(color="#1565C0"), tickfont=list(color="#1565C0"), rangemode="tozero"),
        yaxis2 = list(title="Regen (mm)", titlefont=list(color="#42A5F5"), tickfont=list(color="#42A5F5"), 
                      overlaying="y", side="right", rangemode="tozero", showgrid=FALSE),
        legend = list(orientation="h", x=0, y=-0.2),
        paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)"
    )
    p <- config(p, displayModeBar=FALSE, responsive=TRUE)
    
    plots_list[[i]] <- p
}

names(plots_list) <- sensors$station

cat("Building Leaflet map with leafpop popups...\n")
m <- leaflet(sensors) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat)) %>%
    addCircleMarkers(
        lng          = ~lon, 
        lat          = ~lat,
        label        = ~station,
        radius       = 10,
        color        = "#1a3a5c",
        fillColor    = "#2196F3",
        fillOpacity  = 0.85,
        weight       = 2,
        stroke       = TRUE,
        group        = "sensors"
    )

# Use leafpop to bind the plotly htmlwidgets as popups!
# type="html" ensures it understands they are htmlwidgets, not static images.
m <- addPopupGraphs(m, plots_list, group = "sensors", width = 500, height = 300)

output_abs <- normalizePath(output_file, mustWork = FALSE)
if (!dir_exists(path_dir(output_abs))) dir_create(path_dir(output_abs))

saveWidget(m, file = output_abs, selfcontained = FALSE)

cat("✅ SUCCESS: Interactive map with leafpop-plotly popups saved to", output_file, "\n")
