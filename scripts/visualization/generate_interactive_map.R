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

# Load Open-Meteo forecast as fallback precipitation source (per-sensor)
forecast_file    <- "data/processed/weather_forecast.csv"
precip_fallback  <- NULL
if (file_exists(forecast_file)) {
    precip_fallback <- read_csv(forecast_file, show_col_types = FALSE) %>%
        mutate(timestamp = with_tz(timestamp, tzone = "Europe/Berlin"))
    # Abwärtskompatibel: falls keine station-Spalte, für alle Sensoren nutzen
    if (!"station" %in% names(precip_fallback)) {
        precip_fallback <- precip_fallback %>% select(timestamp, precipitation_mm)
    }
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
      # Bimodal-Sensor-Erkennung und Normalisierung:
      # Manche Sensoren (z.B. Brücken-/Kanalsensoren) liefern den API-level als
      # absolute Höhe (mehrere Meter), nicht als Pegelanstieg über Null.
      # Wenn max(level) > 1 m, normieren wir auf 0–10 cm:
      #   0 cm = trocken  |  10 cm = Sensor auf Mindestabstand (Kanal voll)
      # So bleibt der Plot neben den anderen Stationen lesbar.
      max_level_all <- max(df$level, na.rm = TRUE)
      is_bimodal    <- !is.na(max_level_all) && max_level_all > 1.0
      if (is_bimodal) {
          bimodal_scale <- 0.10 / max_level_all
          df <- df %>% mutate(level = round(pmax(0, level * bimodal_scale), 4))
          if ("level_raw" %in% names(df)) {
              df <- df %>% mutate(level_raw = round(pmax(0, level_raw * bimodal_scale), 4))
          }
      }

      # Data extraction
      latest_row <- tail(df, 1)
      meas_level <- ifelse(
          is.na(latest_row$level), "N/A",
          paste0(round(latest_row$level * 100, 1),
                 if (is_bimodal) " cm (0–10 cm = trocken–voll)" else " cm")
      )
      meas_time  <- format(latest_row$Zeit_Datum, "%d.%m. %H:%M")

      now_time   <- now(tzone = tz(df$Zeit_Datum))
      start_time <- now_time - days(7)
      df_48h <- df %>% filter(Zeit_Datum >= start_time)

      # Prepare precipitation: RADOLAN (gemessen) + Open-Meteo (Vorhersage) kombiniert
      # Wichtig: Der gesamte 48h-Zeitraum wird abgedeckt, fehlende Werte = 0 mm
      precip_file     <- "data/processed/precipitation_at_sensors.csv"
      radolan_precip  <- data.frame(timestamp = as.POSIXct(character()), precipitation_mm = numeric())
      forecast_precip <- data.frame(timestamp = as.POSIXct(character()), precipitation_mm = numeric())

      # 1) RADOLAN laden (gemessener Niederschlag)
      if (file_exists(precip_file)) {
          precip_df <- read_csv(precip_file, show_col_types = FALSE) %>%
              mutate(timestamp = as.POSIXct(timestamp, tz = "UTC"))
          if (nrow(precip_df) > 0) {
              radolan_station <- precip_df %>%
                  filter(station == station_name, timestamp >= start_time) %>%
                  select(timestamp, precipitation_mm)
              if (nrow(radolan_station) > 0) {
                  radolan_precip <- radolan_station
              }
          }
      }

      # 2) Open-Meteo Vorhersage: nur ZUKÜNFTIGE Zeitpunkte (ab jetzt)
      #    RADOLAN = gemessener Regen (Vergangenheit)
      #    Forecast = vorhergesagter Regen (Zukunft, nächste 24h)
      if (!is.null(precip_fallback)) {
          # Per-Sensor filtern falls station-Spalte vorhanden
          if ("station" %in% names(precip_fallback)) {
              station_forecast <- precip_fallback %>%
                  filter(station == station_name) %>%
                  select(timestamp, precipitation_mm)
          } else {
              station_forecast <- precip_fallback
          }

          # Forecast immer ab jetzt in die Zukunft (nächste 24h)
          forecast_precip <- station_forecast %>%
              filter(timestamp > now_time, timestamp <= (now_time + hours(24)))
      }

      # 3) RADOLAN auf Stundensummen aggregieren (mm/h)
      #    5-min-Balken über 7 Tage sind zu dünn/niedrig → stündliche Summen besser sichtbar
      if (nrow(radolan_precip) > 0) {
          radolan_precip <- radolan_precip %>%
              mutate(hour = floor_date(timestamp, "hour")) %>%
              group_by(timestamp = hour) %>%
              summarise(precipitation_mm = sum(precipitation_mm, na.rm = TRUE), .groups = "drop")

          # Vollständiges Stundenraster auffüllen (Lücken = 0 mm)
          radolan_end <- max(radolan_precip$timestamp, na.rm = TRUE)
          full_grid <- data.frame(
              timestamp = seq(
                  from = floor_date(as.POSIXct(start_time, tz = "UTC"), "hour"),
                  to   = as.POSIXct(radolan_end, tz = "UTC"),
                  by   = "1 hour"
              )
          )
          radolan_precip <- full_grid %>%
              left_join(radolan_precip, by = "timestamp") %>%
              mutate(precipitation_mm = ifelse(is.na(precipitation_mm), 0, precipitation_mm))
      } else {
          # Kein RADOLAN vorhanden → stündliche Nullwerte für die Vergangenheit
          radolan_precip <- data.frame(
              timestamp = seq(
                  from = floor_date(as.POSIXct(start_time, tz = "UTC"), "hour"),
                  to   = floor_date(as.POSIXct(now_time, tz = "UTC"), "hour"),
                  by   = "1 hour"
              ),
              precipitation_mm = 0
          )
      }
      
      if (nrow(df_48h) == 0) {
        title_text <- paste0("<b>", station_label, "</b><br>Aktuell: ", meas_level, " (", meas_time, ")<br>Keine 7-Tage-Daten.")
        p <- plotly_empty() %>% layout(title = list(text = title_text, font=list(size=11)))
      } else {
        title_text <- paste0("<b>", station_label, "</b><br>Aktuell: ", meas_level, " (", meas_time, ")")
        
        # Build base trace (Water Level)
        has_raw_level <- "level_raw" %in% names(df_48h)

        p <- plot_ly()

        # Für Stationen mit Shift-Korrektur (Königsallee): erst Rohpegel als
        # graue gestrichelte Linie, dann korrigierten Pegel darüber.
        # So sind Baseline-Drift und Sensor-Shifts direkt erkennbar.
        if (has_raw_level) {
          p <- p %>%
            add_trace(data = df_48h, x = ~Zeit_Datum, y = ~(level_raw * 100),
                      type = "scatter", mode = "lines",
                      name = "Rohpegel (inkl. Sensordrift)",
                      line = list(color = "rgba(160,160,160,0.7)", width = 1, dash = "dot"),
                      hovertemplate = "Rohpegel: %{y:.1f} cm<extra></extra>")
        }

        p <- p %>%
          add_trace(data = df_48h, x = ~Zeit_Datum, y = ~(level * 100),
                    type = "scatter", mode = "lines",
                    name = if (is_bimodal) "Füllstand (0–10 cm = trocken–voll)"
                           else if (has_raw_level) "Pegel (korrigiert)" else "Pegel",
                    line = list(color = "#0072B2", width = 2),
                    fill = "tozeroy", fillcolor = "rgba(0, 114, 178, 0.2)",
                    hovertemplate = if (is_bimodal) "Füllstand: %{y:.1f} cm<extra></extra>"
                                    else "Pegel: %{y:.1f} cm<extra></extra>")
                    
        # Add precipitation traces: RADOLAN (gemessen) + Vorhersage (getrennt)
        has_radolan  <- nrow(radolan_precip) > 0
        has_forecast <- nrow(forecast_precip) > 0
        has_precip   <- has_radolan || has_forecast

        # Max. Regen-Wert für feste y2-Range (damit Achse auch bei 0 mm sichtbar bleibt)
        max_precip <- max(
            c(radolan_precip$precipitation_mm, forecast_precip$precipitation_mm, 1),
            na.rm = TRUE
        )

        if (has_radolan) {
            p <- p %>% add_trace(data = radolan_precip, x = ~timestamp, y = ~precipitation_mm,
                                 type = "bar", name = "Regen (RADOLAN)",
                                 yaxis = "y2",
                                 marker = list(color = "rgba(0, 158, 115, 0.6)"),
                                 hovertemplate = "Regen: %{y:.2f} mm/h (RADOLAN)<extra></extra>")
        }

        if (has_forecast) {
            p <- p %>% add_trace(data = forecast_precip, x = ~timestamp, y = ~precipitation_mm,
                                 type = "bar", name = "Regen (Vorhersage)",
                                 yaxis = "y2",
                                 marker = list(color = "rgba(230, 159, 0, 0.5)"),
                                 hovertemplate = "Regen: %{y:.2f} mm/h (Vorhersage)<extra></extra>")
        }

        # "Jetzt"-Linie um Vergangenheit (RADOLAN) und Zukunft (Vorhersage) zu trennen
        now_shapes <- list(
            list(type = "line",
                 x0 = as.character(now_time), x1 = as.character(now_time),
                 y0 = 0, y1 = 1, yref = "paper",
                 line = list(color = "rgba(100,100,100,0.5)", width = 1, dash = "dash"))
        )

        # Determine layout params based on precipitation
        margin_r <- if(has_precip) 40 else 10
                    
        p <- p %>% layout(
            title      = list(text = title_text, font = list(size = 11), x = 0),
            xaxis      = list(title = "", gridcolor = "#eeeeee",
                              range = c(as.character(start_time), as.character(now_time + hours(24)))),
            yaxis      = list(title = if (is_bimodal) "Füllstand (cm)" else "Pegel (cm)",
                              gridcolor = "#eeeeee", side = "left",
                              range = c(0, if (is_bimodal) 12
                                           else max(max(df_48h$level * 100, na.rm = TRUE) * 1.15, 20))),
            yaxis2     = list(title = "Regen (mm/h)", overlaying = "y", side = "right",
                              showgrid = FALSE, zeroline = TRUE,
                              range = c(0, max_precip * 1.2)),
            shapes     = now_shapes,
            margin     = list(l = 40, r = margin_r, t = 40, b = 30),
            showlegend = TRUE,
            legend     = list(orientation = "h", x = 0, y = -0.15, font = list(size = 9)),
            hovermode  = "x unified",
            plot_bgcolor  = "white",
            paper_bgcolor = "white"
          ) %>%
          config(displayModeBar = TRUE, scrollZoom = TRUE)
      }
    }
  }
  
  # Save the individual widget
  # Note: selfcontained = TRUE makes the iframe completely independent
  saveWidget(p, file = graph_html_path, selfcontained = TRUE)
  
  # Create the iframe tag to link relatively (data/output/graphs/... is just graphs/... from data/output/)
  # We use width and height ensuring it fits well in the popup
  popup_vec[i] <- paste0('<iframe src="graphs/', graph_html_name, '" width="450" height="320" frameborder="0" style="border:none;overflow:hidden;"></iframe>')
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
    popupOptions = popupOptions(maxWidth = 480, minWidth = 460),
    options = markerOptions(interactive = TRUE)
  ) %>%
  addMiniMap(toggleDisplay = TRUE)

# Save the main map — unchanged, exactly as before
saveWidget(m, file = output_file, selfcontained = TRUE)
cat("Map saved to", output_file, "\n")
