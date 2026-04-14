library(plotly)
library(htmlwidgets)
library(readr)
library(dplyr)
library(lubridate)
library(fs)

# Config
events_file <- "data/processed/detected_events.csv"
cleaned_dir <- "data/processed/cleaned_analysis"
precip_file <- "data/processed/precipitation_at_sensors.csv"
output_dir  <- "data/output/event_plots"

BUFFER_BEFORE <- 3   # Stunden vor Event-Start
BUFFER_AFTER  <- 1   # Stunden nach Event-Ende

cat("Generating event plots...\n")

# 1. Events laden
if (!file_exists(events_file)) stop("Events file not found: ", events_file)
events <- read_csv(events_file, show_col_types = FALSE) %>%
    mutate(
        start_time = as.POSIXct(start_time),
        end_time   = as.POSIXct(end_time),
        peak_time  = as.POSIXct(peak_time)
    )

cat("Loaded", nrow(events), "events.\n")
if (nrow(events) == 0) {
    cat("No events to plot.\n")
    quit(save = "no")
}

# 2. RADOLAN-Daten laden
precip <- NULL
if (file_exists(precip_file)) {
    precip <- read_csv(precip_file, show_col_types = FALSE) %>%
        mutate(timestamp = as.POSIXct(timestamp, tz = "UTC"))
    cat("RADOLAN data loaded:", nrow(precip), "records.\n")
}

# 3. Sensor-Daten vorladen (alle Stationen)
sensor_data <- list()
cleaned_files <- dir_ls(cleaned_dir, glob = "*.csv")
for (f in cleaned_files) {
    df <- read_csv(f, show_col_types = FALSE)
    # Stationsname aus Dateiname ableiten (z.B. "An_der_Kost_merged_export_cleaned.csv")
    station_key <- gsub("_merged_export_cleaned\\.csv$", "", basename(f))
    sensor_data[[station_key]] <- df
}
cat("Loaded sensor data for", length(sensor_data), "stations.\n")

# 4. Output-Verzeichnis
if (!dir_exists(output_dir)) dir_create(output_dir)

# 5. Helper: Stationsname → Sensor-Daten-Key matchen
match_station <- function(event_station) {
    # Event-Station: "An_der_Kost", "Wasserstraße_Springorum", etc.
    # Sensor-Key:    "An_der_Kost", "Wasserstraße_Springorum", etc.
    key <- gsub("[^a-zA-Z0-9äöüÄÖÜß]", "_", event_station)
    # Direkte Suche
    if (key %in% names(sensor_data)) return(key)
    # Fuzzy: Teilstring-Match
    matches <- grep(gsub("_", ".*", key), names(sensor_data), value = TRUE, ignore.case = TRUE)
    if (length(matches) > 0) return(matches[1])
    return(NULL)
}

# 6. Plots generieren
cat("\nGenerating", nrow(events), "event plots...\n")
ok <- 0L

for (i in seq_len(nrow(events))) {
    ev <- events[i, ]

    # Zeitfenster: Buffer vor und nach Event
    t_start <- ev$start_time - hours(BUFFER_BEFORE)
    t_end   <- ev$end_time + hours(BUFFER_AFTER)

    # Sensor-Daten finden
    station_key <- match_station(ev$station)
    if (is.null(station_key)) {
        cat("  [SKIP] No sensor data for:", ev$station, "\n")
        next
    }

    df <- sensor_data[[station_key]] %>%
        filter(Zeit_Datum >= t_start, Zeit_Datum <= t_end)

    if (nrow(df) == 0) next

    # RADOLAN-Daten für diese Station im Zeitfenster
    # Stationsnamen-Mapping: Events haben z.B. "Wasserstraße_Springorum", RADOLAN hat "Wasserstraße"
    radolan <- NULL
    if (!is.null(precip)) {
        # Fuzzy match: Event-Station enthält RADOLAN-Station als Teilstring
        event_station_clean <- gsub("_", " ", ev$station)
        precip_stations <- unique(precip$station)
        matched_precip_station <- precip_stations[
            sapply(precip_stations, function(ps) grepl(ps, event_station_clean, fixed = TRUE))
        ]
        if (length(matched_precip_station) == 0) {
            # Fallback: erster gemeinsamer Wort-Match
            matched_precip_station <- precip_stations[
                sapply(precip_stations, function(ps) {
                    any(strsplit(ps, " ")[[1]] %in% strsplit(event_station_clean, " ")[[1]])
                })
            ]
        }
        precip_station_name <- if (length(matched_precip_station) > 0) matched_precip_station[1] else ev$station

        radolan <- precip %>%
            filter(station == precip_station_name, timestamp >= t_start, timestamp <= t_end)

        # Auf Stundensummen aggregieren (konsistent mit Kartenplots)
        if (nrow(radolan) > 0) {
            radolan <- radolan %>%
                mutate(hour = floor_date(timestamp, "hour")) %>%
                group_by(timestamp = hour) %>%
                summarise(precipitation_mm = sum(precipitation_mm, na.rm = TRUE), .groups = "drop")
        }
    }

    has_rain <- !is.null(radolan) && nrow(radolan) > 0

    # Peak-Wert für y-Range
    max_level <- max(df$level * 100, na.rm = TRUE)
    y_max <- max(max_level * 1.3, 2)  # Mindestens 2 cm

    # Titel
    event_date <- format(ev$start_time, "%d.%m.%Y %H:%M")
    title_text <- paste0(
        "<b>", ev$station, "</b> — ", event_date,
        "<br>Peak: ", ev$peak_level_cm, " cm | ",
        ev$duration_min, " min | ", ev$event_type
    )

    # Plot erstellen
    p <- plot_ly() %>%
        add_trace(
            data = df, x = ~Zeit_Datum, y = ~(level * 100),
            type = "scatter", mode = "lines", name = "Pegel",
            line = list(color = "#0072B2", width = 2),
            fill = "tozeroy", fillcolor = "rgba(0, 114, 178, 0.2)",
            hovertemplate = "Pegel: %{y:.2f} cm<extra></extra>"
        )

    # RADOLAN-Balken
    max_precip <- 1
    if (has_rain) {
        max_precip <- max(c(radolan$precipitation_mm, 1), na.rm = TRUE)
        p <- p %>% add_trace(
            data = radolan, x = ~timestamp, y = ~precipitation_mm,
            type = "bar", name = "Regen (RADOLAN)",
            yaxis = "y2",
            marker = list(color = "rgba(0, 158, 115, 0.6)"),
            hovertemplate = "Regen: %{y:.2f} mm/h<extra></extra>"
        )
    }

    # Event-Zeitfenster markieren (halbtransparentes Rechteck)
    event_shapes <- list(
        list(
            type = "rect",
            x0 = as.character(ev$start_time), x1 = as.character(ev$end_time),
            y0 = 0, y1 = 1, yref = "paper",
            fillcolor = "rgba(255, 0, 0, 0.08)",
            line = list(color = "rgba(255, 0, 0, 0.3)", width = 1)
        )
    )

    margin_r <- if (has_rain) 45 else 10

    p <- p %>% layout(
        title  = list(text = title_text, font = list(size = 11), x = 0),
        xaxis  = list(title = "", gridcolor = "#eeeeee"),
        yaxis  = list(title = "Pegel (cm)", gridcolor = "#eeeeee", side = "left",
                      range = c(0, y_max)),
        yaxis2 = list(title = "Regen (mm/h)", overlaying = "y", side = "right",
                      showgrid = FALSE, zeroline = TRUE,
                      range = c(0, max_precip * 1.3)),
        shapes     = event_shapes,
        margin     = list(l = 45, r = margin_r, t = 50, b = 30),
        showlegend = TRUE,
        legend     = list(orientation = "h", x = 0, y = -0.15, font = list(size = 9)),
        hovermode  = "x unified",
        plot_bgcolor  = "white",
        paper_bgcolor = "white"
    ) %>%
        config(displayModeBar = TRUE, scrollZoom = TRUE)

    # Dateiname: Station_YYYYMMDD_HHMM.html
    safe_station <- gsub("[^a-zA-Z0-9]", "_", ev$station)
    time_str     <- format(ev$start_time, "%Y%m%d_%H%M")
    filename     <- paste0(safe_station, "_", time_str, ".html")
    filepath     <- file.path(output_dir, filename)

    saveWidget(p, file = filepath, selfcontained = TRUE)
    ok <- ok + 1L
}

cat("\nDone:", ok, "/", nrow(events), "event plots saved to", output_dir, "\n")
