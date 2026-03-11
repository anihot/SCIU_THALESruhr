library(leaflet)
library(htmlwidgets)
library(htmltools)
library(dplyr)
library(readr)
library(fs)
library(plotly)
library(lubridate)
library(jsonlite)

# Config
metadata_file <- "data/metadata/sensor_metadata.csv"
output_file   <- "data/output/sensor_map.html"
cleaned_dir   <- "data/processed/cleaned_analysis"

cat("Generating interactive sensor map...\n")
if (!file_exists(metadata_file)) stop("Metadata file not found: ", metadata_file)
if (!dir_exists(path_dir(output_file))) dir_create(path_dir(output_file))

sensors <- read_csv(metadata_file, show_col_types = FALSE)

# ---------------------------------------------------------------------------
# Build popup HTML for each sensor.
# Each popup contains:
#   - A <div id="sensor_plot_N"> as the render target
#   - An inline <script> calling Plotly.newPlot() with the data JSON embedded
# Plotly.js itself is attached as a widget dependency below (bundled once).
# ---------------------------------------------------------------------------
make_popup <- function(station_name, station_label, idx) {
  div_id <- paste0("sensor_plot_", idx)

  # Robust file matching: replace non-ASCII / non-alphanum chars with wildcard
  clean_search  <- gsub("[^a-zA-Z0-9]+", "*", station_name)
  file_glob     <- paste0(cleaned_dir, "/*", clean_search, "*_cleaned.csv")
  matching_files <- Sys.glob(file_glob)

  cat("  Station:", station_name, "-> files found:", length(matching_files), "\n")

  if (length(matching_files) == 0) {
    return(paste0("<b>", station_label, "</b><br>Keine Datei gefunden."))
  }

  df <- read_csv(matching_files[1], show_col_types = FALSE)
  if (nrow(df) == 0) return(paste0("<b>", station_label, "</b><br>Datei leer."))

  # Latest reading for subtitle
  latest_row <- tail(df, 1)
  meas_level <- ifelse(is.na(latest_row$level), "N/A",
                       paste0(round(latest_row$level, 3), " m"))
  meas_time  <- format(latest_row$Zeit_Datum, "%d.%m. %H:%M")

  # Last 24 h
  start_time <- now(tzone = tz(df$Zeit_Datum)) - hours(24)
  df_24h <- df %>% filter(Zeit_Datum >= start_time)
  cat("  24h rows:", nrow(df_24h), "\n")

  if (nrow(df_24h) == 0) {
    return(paste0("<b>", station_label, "</b><br>Aktuell: ", meas_level,
                  " – keine 24h-Daten."))
  }

  # Build plotly object and export as JSON (data + layout only, no JS lib)
  p <- plot_ly(df_24h, x = ~Zeit_Datum, y = ~level,
               type    = "scatter", mode = "lines",
               line    = list(color = "#0072B2", width = 2),
               fill    = "tozeroy",
               fillcolor = "rgba(0, 114, 178, 0.2)",
               hovertemplate = "%{x|%d.%m. %H:%M}<br>%{y:.3f} m<extra></extra>") %>%
    layout(
      title      = list(
        text = paste0(station_label,
                      "  <sup>Aktuell: ", meas_level, " (", meas_time, ")</sup>"),
        font = list(size = 11), x = 0),
      xaxis      = list(title = "", gridcolor = "#eeeeee"),
      yaxis      = list(title = "Level (m)", gridcolor = "#eeeeee"),
      margin     = list(l = 50, r = 8, t = 38, b = 28),
      showlegend = FALSE,
      hovermode  = "x unified",
      plot_bgcolor  = "white",
      paper_bgcolor = "white"
    ) %>%
    config(displayModeBar = FALSE, responsive = TRUE)

  pb          <- plotly_build(p)
  data_json   <- toJSON(pb$x$data,   auto_unbox = TRUE, null = "null")
  layout_json <- toJSON(pb$x$layout, auto_unbox = TRUE, null = "null")

  # Inline HTML: div + Plotly.newPlot() call (setTimeout lets popup DOM settle)
  paste0(
    '<div id="', div_id, '" style="width:340px;height:240px;"></div>',
    '<script>',
      'setTimeout(function(){',
        'Plotly.newPlot("', div_id, '",',
          data_json, ',', layout_json,
          ',{displayModeBar:false,responsive:true});',
      '},150);',
    '</script>'
  )
}

# Generate popup HTML strings (character vector, not html-object list)
popup_vec <- vapply(
  seq_len(nrow(sensors)),
  function(i) make_popup(sensors$station[i], sensors$label[i], i),
  character(1)
)

# Build a dummy plotly to extract plotly.js dependency
dummy_widget <- as_widget(plot_ly(data.frame(x = 1, y = 1), x = ~x, y = ~y,
                                  type = "scatter"))
plotly_deps <- resolveDependencies(htmlDependencies(dummy_widget))

# Build Leaflet map
m <- leaflet(sensors) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(
    lng     = ~lon,
    lat     = ~lat,
    label   = ~label,
    popup   = popup_vec,          # plain character vector → markers are clickable
    options = markerOptions(interactive = TRUE)
  ) %>%
  addMiniMap(toggleDisplay = TRUE)

# Attach plotly.js so it is bundled once in the combined output
m$dependencies <- c(m$dependencies, plotly_deps)

# Save: selfcontained bundles all JS / CSS into one file
saveWidget(m, file = output_file, selfcontained = TRUE)
cat("✅ Map saved to", output_file, "\n")
