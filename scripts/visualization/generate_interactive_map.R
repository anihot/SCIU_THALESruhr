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
output_file   <- "data/output/sensor_map.html"
cleaned_dir   <- "data/processed/cleaned_analysis"

cat("Generating interactive sensor map with Plotly & leafpop...\n")

if (!file_exists(metadata_file)) stop("Metadata file not found: ", metadata_file)
if (!dir_exists(path_dir(output_file))) dir_create(path_dir(output_file))

# 1. Load sensor coordinates
sensors <- read_csv(metadata_file, show_col_types = FALSE)

# 2. Build one Plotly chart per sensor
plot_list <- list()

for (i in seq_len(nrow(sensors))) {
  station_name  <- sensors$station[i]
  station_label <- sensors$label[i]
  
  clean_search   <- gsub("[^a-zA-Z0-9]+", "*", station_name)
  file_glob      <- paste0(cleaned_dir, "/*", clean_search, "*_cleaned.csv")
  matching_files <- Sys.glob(file_glob)
  
  if (length(matching_files) == 0) {
    p <- plotly_empty() %>% layout(title = list(text = paste(station_label, "<br>Keine Daten"), font = list(size = 11)))
  } else {
    df <- read_csv(matching_files[1], show_col_types = FALSE)
    if (nrow(df) == 0) {
      p <- plotly_empty() %>% layout(title = list(text = paste(station_label, "<br>Datei leer"), font = list(size = 11)))
    } else {
      latest_row <- tail(df, 1)
      meas_level <- ifelse(is.na(latest_row$level), "N/A", paste0(round(latest_row$level, 3), " m"))
      meas_time  <- format(latest_row$Zeit_Datum, "%d.%m. %H:%M")
      
      start_time <- now(tzone = tz(df$Zeit_Datum)) - hours(24)
      df_24h <- df %>% filter(Zeit_Datum >= start_time)
      
      if (nrow(df_24h) == 0) {
        title_text <- paste0("<b>", station_label, "</b><br>Aktuell: ", meas_level, " (", meas_time, ")<br>Keine 24h-Daten")
        p <- plotly_empty() %>% layout(title = list(text = title_text, font = list(size = 11)))
      } else {
        title_text <- paste0("<b>", station_label, "</b><br>Aktuell: ", meas_level, " (", meas_time, ")")
        
        p <- plot_ly(df_24h, x = ~Zeit_Datum, y = ~level, type = "scatter", mode = "lines",
                     line = list(color = "#0072B2", width = 2),
                     fill = "tozeroy", fillcolor = "rgba(0, 114, 178, 0.2)",
                     hovertemplate = "%{x|%d.%m. %H:%M}<br>%{y:.3f} m<extra></extra>") %>%
          layout(title = list(text = title_text, font = list(size = 11), x = 0),
                 xaxis = list(title = "", gridcolor = "#eeeeee"),
                 yaxis = list(title = "Level (m)", gridcolor = "#eeeeee"),
                 margin = list(l = 50, r = 10, t = 40, b = 30),
                 showlegend = FALSE, hovermode = "x unified") %>%
          config(displayModeBar = FALSE, scrollZoom = FALSE)
      }
    }
  }
  
  plot_list[[i]] <- p
}

# The names of the list MUST match the layer IDs if we use group/layer binding
# Leafpop works best if we name the list items identically to the marker row indices or layerIds
names(plot_list) <- as.character(seq_len(nrow(sensors)))

# 3. Build Leaflet map
m <- leaflet(sensors) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(
    lng     = ~lon,
    lat     = ~lat,
    label   = ~label,
    layerId = ~as.character(seq_len(nrow(sensors))),  # Explicitly bind layer IDs
    group   = "sensors"
  ) %>%
  addMiniMap(toggleDisplay = TRUE)

# 4. Attach leafpop graphs
# Using group = "sensors" with named plot_list matches the layerIds automatically
m <- m %>% addPopupGraphs(plot_list, group = "sensors", width = 360, height = 250)

# 5. Save HTML
# Note: Pandoc MUST be available (setup in fetch_data.yml) for selfcontained Plotly+Leaflet combos.
saveWidget(m, file = output_file, selfcontained = TRUE)
cat("✅ Map saved to", output_file, "\n")
