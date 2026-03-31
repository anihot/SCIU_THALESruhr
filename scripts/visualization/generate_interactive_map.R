library(leaflet)
library(htmlwidgets)
library(dplyr)
library(readr)
library(fs)
library(plotly)
library(lubridate)

# Config
metadata_file <- "data/metadata/sensor_metadata.csv"
output_file   <- "data/output/sensor_map.html"
cleaned_dir   <- "data/processed/cleaned_analysis"
graphs_dir    <- "data/output/graphs"

cat("Generating interactive sensor map with external iframes...\n")
if (!file_exists(metadata_file)) stop("Metadata file not found: ", metadata_file)

# Load Open-Meteo forecast as fallback precipitation source
forecast_file    <- "data/processed/weather_forecast.csv"
precip_fallback  <- NULL
if (file_exists(forecast_file)) {
    precip_fallback <- read_csv(forecast_file, show_col_types = FALSE) %>%
        mutate(timestamp = as.POSIXct(timestamp, tz = "Europe/Berlin")) %>%
        select(timestamp, precipitation_mm)
}

# Ensure output directories exist
if (!dir_exists(path_dir(output_file))) dir_create(path_dir(output_file))
if (!dir_exists(graphs_dir)) dir_create(graphs_dir)

sensors <- read_csv(metadata_file, show_col_types = FALSE)

# We will store the popup iframe strings here
popup_vec <- character(nrow(sensors))

# ---------------------------------------------------------------------------
# Build popup HTML for each sensor.
# We save each Plotly plot as a standalone HTML in `data/output/graphs/`
# and link to it inside the Leaflet popup via an <iframe>.
# This prevents Leaflet from breaking the scripts and avoids giant files.
# ---------------------------------------------------------------------------

for (i in seq_len(nrow(sensors))) {
  station_name  <- sensors$station[i]
  station_label <- sensors$label[i]
  
  # Safe ID for filename
  safe_id <- gsub("[^a-zA-Z0-9]", "_", station_name)
  graph_html_name <- paste0(safe_id, ".html")
  graph_html_path <- file.path(graphs_dir, graph_html_name)
  
  # Robust data matching
  clean_search   <- gsub("[^a-zA-Z0-9]+", "*", station_name)
  file_glob      <- paste0(cleaned_dir, "/*", clean_search, "*_cleaned.csv")
  matching_files <- Sys.glob(file_glob)
  
  cat("  Station:", station_name, "-> files:", length(matching_files), "\n")
  
  p <- NULL
  if (length(matching_files) == 0) {
    p <- plotly_empty() %>% layout(title = list(text = paste(station_label, "<br>Keine Datei gefunden"), font=list(size=11)))
  } else {
    df <- read_csv(matching_files[1], show_col_types = FALSE)
    if (nrow(df) == 0) {
      p <- plotly_empty() %>% layout(title = list(text = paste(station_label, "<br>Datei leer"), font=list(size=11)))
    } else {
      # Data extraction
      latest_row <- tail(df, 1)
      meas_level <- ifelse(is.na(latest_row$level), "N/A", paste0(round(latest_row$level * 100, 1), " cm"))
      meas_time  <- format(latest_row$Zeit_Datum, "%d.%m. %H:%M")
      
      start_time <- now(tzone = tz(df$Zeit_Datum)) - hours(24)
      df_24h <- df %>% filter(Zeit_Datum >= start_time)
      
      # Prepare precipitation: RADOLAN (gemessen) + Open-Meteo (Vorhersage) kombiniert
      # RADOLAN zeigt den tatsächlich gefallenen Regen, Open-Meteo ergänzt ab Ende der Messdaten
      precip_file     <- "data/processed/precipitation_at_sensors.csv"
      radolan_precip  <- data.frame(timestamp = as.POSIXct(character()), precipitation_mm = numeric())
      forecast_precip <- data.frame(timestamp = as.POSIXct(character()), precipitation_mm = numeric())

      # 1) RADOLAN laden (gemessener Niederschlag)
      if (file_exists(precip_file)) {
          precip_df <- read_csv(precip_file, show_col_types = FALSE) %>%
              mutate(timestamp = as.POSIXct(timestamp, tz = "UTC"))
          if (nrow(precip_df) > 0) {
              radolan_station <- precip_df %>%
                  filter(station == station_name, timestamp >= start_time)
              if (nrow(radolan_station) > 0) {
                  radolan_precip <- radolan_station
              }
          }
      }

      # 2) Open-Meteo Vorhersage: nur Zeitpunkte NACH dem letzten RADOLAN-Wert
      if (!is.null(precip_fallback)) {
          if (nrow(radolan_precip) > 0) {
              radolan_end     <- max(radolan_precip$timestamp, na.rm = TRUE)
              forecast_precip <- precip_fallback %>%
                  filter(timestamp > radolan_end)
          } else {
              # Kein RADOLAN vorhanden → gesamte Vorhersage nutzen
              forecast_precip <- precip_fallback %>%
                  filter(timestamp >= start_time)
          }
      }
      
      if (nrow(df_24h) == 0) {
        title_text <- paste0("<b>", station_label, "</b><br>Aktuell: ", meas_level, " (", meas_time, ")<br>Keine 24h-Daten.")
        p <- plotly_empty() %>% layout(title = list(text = title_text, font=list(size=11)))
      } else {
        title_text <- paste0("<b>", station_label, "</b><br>Aktuell: ", meas_level, " (", meas_time, ")")
        
        # Build base trace (Water Level)
        p <- plot_ly() %>%
          add_trace(data = df_24h, x = ~Zeit_Datum, y = ~(level * 100),
                    type = "scatter", mode = "lines", name = "Pegel",
                    line = list(color = "#0072B2", width = 2),
                    fill = "tozeroy", fillcolor = "rgba(0, 114, 178, 0.2)",
                    hovertemplate = "Pegel: %{y:.1f} cm<extra></extra>")
                    
        # Add precipitation traces: RADOLAN (gemessen) + Vorhersage (getrennt)
        has_radolan  <- nrow(radolan_precip) > 0
        has_forecast <- nrow(forecast_precip) > 0
        has_precip   <- has_radolan || has_forecast

        if (has_radolan) {
            p <- p %>% add_trace(data = radolan_precip, x = ~timestamp, y = ~precipitation_mm,
                                 type = "bar", name = "Regen (RADOLAN)",
                                 yaxis = "y2",
                                 marker = list(color = "rgba(0, 158, 115, 0.6)"),
                                 hovertemplate = "Regen: %{y:.2f} mm/5min (RADOLAN)<extra></extra>")
        }

        if (has_forecast) {
            p <- p %>% add_trace(data = forecast_precip, x = ~timestamp, y = ~precipitation_mm,
                                 type = "bar", name = "Regen (Vorhersage)",
                                 yaxis = "y2",
                                 marker = list(color = "rgba(230, 159, 0, 0.5)"),
                                 hovertemplate = "Regen: %{y:.2f} mm/h (Vorhersage)<extra></extra>")
        }

        # Determine layout params based on precipitation
        margin_r <- if(has_precip) 40 else 10
                    
        p <- p %>% layout(
            title      = list(text = title_text, font = list(size = 11), x = 0),
            xaxis      = list(title = "", gridcolor = "#eeeeee"),
            yaxis      = list(title = "Pegel (cm)", gridcolor = "#eeeeee", side = "left"),
            yaxis2     = list(title = "Regen (mm)", overlaying = "y", side = "right",
                              showgrid = FALSE, zeroline = FALSE, rangemode = "tozero"),
            margin     = list(l = 40, r = margin_r, t = 40, b = 30),
            showlegend = has_precip,
            legend     = list(orientation = "h", x = 0, y = -0.15, font = list(size = 9)),
            hovermode  = "x unified",
            plot_bgcolor  = "white",
            paper_bgcolor = "white"
          ) %>%
          config(displayModeBar = FALSE, scrollZoom = TRUE)
      }
    }
  }
  
  # Save the individual widget
  # Note: selfcontained = TRUE makes the iframe completely independent
  saveWidget(p, file = graph_html_path, selfcontained = TRUE)
  
  # Create the iframe tag to link relatively (data/output/graphs/... is just graphs/... from data/output/)
  # We use width and height ensuring it fits well in the popup
  popup_vec[i] <- paste0('<iframe src="graphs/', graph_html_name, '" width="360" height="260" frameborder="0" scrolling="no"></iframe>')
}

# ---------------------------------------------------------------------------
# Build Leaflet map
# ---------------------------------------------------------------------------
m <- leaflet(sensors) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(
    lng     = ~lon,
    lat     = ~lat,
    label   = ~label,
    popup   = popup_vec,
    options = markerOptions(interactive = TRUE)
  ) %>%
  addMiniMap(toggleDisplay = TRUE)

# Save the main map
saveWidget(m, file = output_file, selfcontained = TRUE)
cat("✅ Map saved to", output_file, "\n")
