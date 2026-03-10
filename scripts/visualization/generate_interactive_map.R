library(dplyr)
library(readr)
library(lubridate)
library(jsonlite)
library(fs)

# Config
metadata_file <- "data/metadata/sensor_metadata.csv"
cleaned_dir   <- "data/processed/cleaned_analysis"
precip_file   <- "data/processed/precipitation_at_sensors.csv"
output_file   <- "data/output/sensor_map.html"

cat("Generating interactive sensor map with hover time series...\n")

if (!file.exists(metadata_file)) stop("Metadata file not found: ", metadata_file)
sensors <- read_csv(metadata_file, show_col_types = FALSE)

# Load precipitation data (if available and non-empty)
precip_data <- NULL
if (file.exists(precip_file)) {
    pd <- read_csv(precip_file, show_col_types = FALSE)
    if (nrow(pd) > 0) {
        precip_data <- pd %>% mutate(timestamp = as.POSIXct(timestamp, tz = "UTC"))
        cat("Loaded precipitation data:", nrow(precip_data), "rows\n")
    } else {
        cat("Precipitation file is empty, skipping.\n")
    }
}

now_utc <- now(tzone = "UTC")
cutoff  <- now_utc - hours(24)
cat("Time window: last 24h (from", format(cutoff, "%Y-%m-%d %H:%M UTC"), ")\n")

# Helper: find cleaned CSV file for a station (robust name matching)
find_sensor_file <- function(station_name, dir) {
    # Try exact match
    f <- file.path(dir, paste0(station_name, "_merged_export_cleaned.csv"))
    if (file.exists(f)) return(f)
    # Try with spaces replaced by underscores
    f <- file.path(dir, paste0(gsub(" ", "_", station_name), "_merged_export_cleaned.csv"))
    if (file.exists(f)) return(f)
    # Try glob: station prefix + anything + _merged_export_cleaned.csv
    pattern <- file.path(dir, paste0(gsub(" ", "_", station_name), "*_merged_export_cleaned.csv"))
    matches <- Sys.glob(pattern)
    if (length(matches) > 0) return(matches[1])
    return(NULL)
}

# Build sensor data for JavaScript
sensor_list <- list()

for (i in seq_len(nrow(sensors))) {
    stn <- sensors$station[i]
    lbl <- if ("label" %in% names(sensors)) sensors$label[i] else stn

    level_times   <- character(0)
    level_values  <- numeric(0)
    precip_times  <- character(0)
    precip_values <- numeric(0)

    sf <- find_sensor_file(stn, cleaned_dir)
    if (!is.null(sf)) {
        cat("Reading:", basename(sf), "\n")
        df <- read_csv(sf, show_col_types = FALSE) %>%
            mutate(Zeit_Datum = as.POSIXct(Zeit_Datum, tz = "UTC")) %>%
            filter(Zeit_Datum >= cutoff, !is.na(level))
        if (nrow(df) > 0) {
            level_times  <- format(df$Zeit_Datum, "%Y-%m-%dT%H:%M:%SZ")
            level_values <- round(df$level * 100, 2)  # m -> cm
            cat("  ->", nrow(df), "data points in last 24h\n")
        } else {
            cat("  -> No data in last 24h\n")
        }
    } else {
        cat("WARNING: No data file found for station:", stn, "\n")
    }

    if (!is.null(precip_data)) {
        p24 <- precip_data %>%
            filter(station == stn, timestamp >= cutoff, precipitation_mm > 0)
        if (nrow(p24) > 0) {
            precip_times  <- format(p24$timestamp, "%Y-%m-%dT%H:%M:%SZ")
            precip_values <- round(p24$precipitation_mm, 2)
        }
    }

    sensor_list[[stn]] <- list(
        station       = stn,
        label         = lbl,
        lat           = sensors$lat[i],
        lon           = sensors$lon[i],
        level_times   = level_times,
        level_values  = level_values,
        precip_times  = precip_times,
        precip_values = precip_values
    )
}

json_data  <- toJSON(sensor_list, auto_unbox = TRUE, pretty = FALSE)
center_lat <- mean(sensors$lat)
center_lon <- mean(sensors$lon)

# Build self-contained HTML
html_out <- paste0(
'<!DOCTYPE html>
<html lang="de">
<head>
  <meta charset="UTF-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0" />
  <title>Sensor Map - THALESruhr</title>
  <link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css" />
  <script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
  <script src="https://cdn.plot.ly/plotly-2.32.0.min.js"></script>
  <style>
    * { margin: 0; padding: 0; box-sizing: border-box; }
    html, body, #map { width: 100%; height: 100vh; }
    #sensor-tooltip {
      position: fixed;
      z-index: 9999;
      background: white;
      border: 1px solid #d0d7e2;
      border-radius: 10px;
      padding: 6px;
      box-shadow: 4px 4px 16px rgba(0,0,0,0.22);
      display: none;
      pointer-events: none;
    }
  </style>
</head>
<body>
  <div id="map"></div>
  <div id="sensor-tooltip">
    <div id="plotly-chart" style="width: 460px; height: 290px;"></div>
  </div>

  <script>
  var SENSOR_DATA = ', json_data, ';

  var map = L.map("map").setView([', center_lat, ', ', center_lon, '], 13);

  L.tileLayer("https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png", {
    attribution: "&copy; <a href=\'https://www.openstreetmap.org/copyright\'>OpenStreetMap</a> contributors &copy; <a href=\'https://carto.com/\'>CARTO</a>",
    subdomains: "abcd",
    maxZoom: 20
  }).addTo(map);

  var tooltip  = document.getElementById("sensor-tooltip");
  var chartDiv = document.getElementById("plotly-chart");

  function updateTooltipPosition(x, y) {
    var vpW = window.innerWidth;
    var vpH = window.innerHeight;
    var w = 480, h = 320;
    var left = x + 20;
    var top  = y - 20;
    if (left + w > vpW) left = x - w - 10;
    if (top  + h > vpH) top  = vpH - h - 10;
    if (top  < 0)       top  = 10;
    tooltip.style.left = left + "px";
    tooltip.style.top  = top  + "px";
  }

  function showChart(sensor, x, y) {
    tooltip.style.display = "block";
    updateTooltipPosition(x, y);

    var traces = [];

    if (sensor.level_times && sensor.level_times.length > 0) {
      traces.push({
        x:    sensor.level_times,
        y:    sensor.level_values,
        name: "Pegel (cm)",
        type: "scatter",
        mode: "lines",
        line: { color: "#1565C0", width: 2 },
        yaxis: "y"
      });
    }

    if (sensor.precip_times && sensor.precip_times.length > 0) {
      traces.push({
        x:      sensor.precip_times,
        y:      sensor.precip_values,
        name:   "Niederschlag (mm)",
        type:   "bar",
        marker: { color: "rgba(66, 165, 245, 0.65)" },
        yaxis:  "y2"
      });
    }

    if (traces.length === 0) {
      chartDiv.innerHTML =
        \'<div style="padding:40px;text-align:center;font-family:sans-serif;\' +
        \'color:#888;font-size:14px;">Keine Daten in den letzten 24h</div>\';
      return;
    }

    var layout = {
      title: {
        text: "Letzte 24h \u2014 " + (sensor.label || sensor.station),
        font: { size: 13, color: "#1a3a5c" }
      },
      height:  290,
      margin:  { l: 58, r: 58, t: 40, b: 50 },
      xaxis:   {
        tickformat: "%H:%M",
        showgrid:   true,
        gridcolor:  "#f0f0f0"
      },
      yaxis: {
        title:      "Pegel (cm)",
        titlefont:  { color: "#1565C0", size: 11 },
        tickfont:   { color: "#1565C0" },
        rangemode:  "tozero",
        gridcolor:  "#f0f0f0"
      },
      paper_bgcolor: "white",
      plot_bgcolor:  "white",
      legend: { orientation: "h", x: 0, y: -0.22, font: { size: 11 } },
      showlegend: traces.length > 1
    };

    if (traces.some(function(t) { return t.yaxis === "y2"; })) {
      layout.yaxis2 = {
        title:     "Regen (mm)",
        titlefont: { color: "#42A5F5", size: 11 },
        tickfont:  { color: "#42A5F5" },
        overlaying: "y",
        side:       "right",
        rangemode:  "tozero",
        showgrid:   false
      };
    }

    Plotly.react(chartDiv, traces, layout, { displayModeBar: false, responsive: false });
  }

  // Create a marker for each sensor
  Object.values(SENSOR_DATA).forEach(function(sensor) {
    var marker = L.circleMarker([sensor.lat, sensor.lon], {
      radius:      10,
      color:       "#1a3a5c",
      fillColor:   "#2196F3",
      fillOpacity: 0.85,
      weight:      2,
      stroke:      true
    });

    marker.on("mouseover", function(e) {
      showChart(sensor, e.originalEvent.clientX, e.originalEvent.clientY);
    });

    marker.on("mousemove", function(e) {
      if (tooltip.style.display !== "none") {
        updateTooltipPosition(e.originalEvent.clientX, e.originalEvent.clientY);
      }
    });

    marker.on("mouseout", function() {
      tooltip.style.display = "none";
    });

    // Small label on hover showing station name
    marker.bindTooltip(sensor.label || sensor.station, {
      permanent:  false,
      direction:  "top",
      offset:     [0, -12],
      className:  "sensor-label"
    });

    marker.addTo(map);
  });
  </script>
</body>
</html>')

# Save
output_abs <- normalizePath(output_file, mustWork = FALSE)
if (!dir_exists(path_dir(output_abs))) dir_create(path_dir(output_abs))
writeLines(html_out, output_abs, useBytes = FALSE)

cat("SUCCESS: Interactive map with hover time series saved to", output_file, "\n")
