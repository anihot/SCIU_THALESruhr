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
      meas_level <- ifelse(is.na(latest_row$level), "N/A", paste0(round(latest_row$level, 3), " m"))
      meas_time  <- format(latest_row$Zeit_Datum, "%d.%m. %H:%M")
      
      start_time <- now(tzone = tz(df$Zeit_Datum)) - hours(24)
      df_24h <- df %>% filter(Zeit_Datum >= start_time)
      
      if (nrow(df_24h) == 0) {
        title_text <- paste0("<b>", station_label, "</b><br>Aktuell: ", meas_level, " (", meas_time, ")<br>Keine 24h-Daten.")
        p <- plotly_empty() %>% layout(title = list(text = title_text, font=list(size=11)))
      } else {
        title_text <- paste0("<b>", station_label, "</b><br>Aktuell: ", meas_level, " (", meas_time, ")")
        
        p <- plot_ly(df_24h, x = ~Zeit_Datum, y = ~level,
                     type = "scatter", mode = "lines",
                     line = list(color = "#0072B2", width = 2),
                     fill = "tozeroy", fillcolor = "rgba(0, 114, 178, 0.2)",
                     hovertemplate = "%{x|%d.%m. %H:%M}<br>%{y:.3f} m<extra></extra>") %>%
          layout(
            title      = list(text = title_text, font = list(size = 11), x = 0),
            xaxis      = list(title = "", gridcolor = "#eeeeee"),
            yaxis      = list(title = "Level (m)", gridcolor = "#eeeeee"),
            margin     = list(l = 40, r = 10, t = 40, b = 30),
            showlegend = FALSE,
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
