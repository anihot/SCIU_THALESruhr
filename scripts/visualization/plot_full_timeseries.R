library(plotly)
library(htmlwidgets)
library(htmltools)
library(dplyr)
library(readr)
library(lubridate)
library(fs)
source("scripts/utils/nahecho.R")

cleaned_dir <- "data/processed/cleaned_analysis"
output_dir  <- "data/output/graphs"
docs_dir    <- "docs/graphs"
precip_file <- "data/processed/precipitation_at_sensors.csv"
precip_hourly_file <- "data/processed/precipitation_hourly_at_sensors.csv"

if (!dir_exists(output_dir)) dir_create(output_dir)
if (!dir_exists(docs_dir))   dir_create(docs_dir)

stations <- list(
  list(name = "An der Kost",      file = "An_der_Kost_merged_export_cleaned.csv",              safe = "An_der_Kost"),
  list(name = "Wasserstraße",     file = "Wasserstraße_Springorum_merged_export_cleaned.csv",   safe = "Wasserstra_e"),
  list(name = "Königsallee",      file = "Königsallee_Springorum_merged_export_cleaned.csv",    safe = "K_nigsallee"),
  list(name = "Wasserbaulabor 2", file = "Wasserbaulabor_2_merged_export_cleaned.csv",          safe = "Wasserbaulabor_2"),
  list(name = "Herzogstraße",     file = "Herzogstraße_merged_export_cleaned.csv",              safe = "Herzogstra_e")
)

# Load RADOLAN precipitation once.
# Bevorzugt die stündliche Datei (gesamte Historie), sonst die 5-min-Datei
# (nur die letzten Tage). Beide werden unten auf Stundensummen aggregiert.
precip_all <- NULL
for (pf in c(precip_hourly_file, precip_file)) {
  if (is.null(precip_all) && file_exists(pf)) {
    precip_all <- read_csv(pf, show_col_types = FALSE) %>%
      mutate(timestamp = with_tz(as.POSIXct(timestamp, tz = "UTC"), "Europe/Berlin"))
    if (nrow(precip_all) == 0) precip_all <- NULL else cat("Precipitation source:", pf, "\n")
  }
}

cat("Generating full timeseries plots...\n")

for (st in stations) {
  csv_path <- file.path(cleaned_dir, st$file)
  if (!file_exists(csv_path)) {
    cat("  SKIP:", st$name, "- file not found\n")
    next
  }

  df <- read_csv(csv_path, show_col_types = FALSE)
  if (nrow(df) == 0 || !"Zeit_Datum" %in% names(df) || !"level" %in% names(df)) {
    cat("  SKIP:", st$name, "- missing required columns or empty\n")
    next
  }

  df <- df %>%
    mutate(Zeit_Datum = with_tz(as.POSIXct(Zeit_Datum, tz = "UTC"), "Europe/Berlin")) %>%
    filter(!is.na(Zeit_Datum))

  has_raw     <- "level_raw" %in% names(df)
  has_nahecho <- "nahecho" %in% names(df) && any(df$nahecho, na.rm = TRUE)
  if (!"nahecho" %in% names(df)) df$nahecho <- FALSE
  # Rohpegel im Nahecho ist ein Messartefakt (~6 m) -> nicht darstellen
  if (has_raw && has_nahecho) df <- df %>% mutate(level_raw = ifelse(nahecho, NA_real_, level_raw))

  na_mean <- function(v) if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE)
  df_hourly <- df %>%
    mutate(bin = floor_date(Zeit_Datum, "10 minutes")) %>%
    group_by(bin) %>%
    summarise(
      level     = na_mean(level),
      level_raw = if (has_raw) na_mean(level_raw) else NA_real_,
      nahecho   = any(nahecho, na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    rename(Zeit_Datum = bin) %>%
    # Nahecho-Bins bleiben als NA erhalten, damit die Linie dort unterbrochen wird
    filter(!is.na(level) | nahecho)

  if (nrow(df_hourly) == 0) next

  max_level  <- max(df_hourly$level, na.rm = TRUE)
  is_bimodal <- !is.na(max_level) && max_level > 1.0
  y_unit     <- if (is_bimodal) "m" else "cm"
  y_scale    <- if (is_bimodal) 1 else 100
  y_label    <- if (is_bimodal) "Wasserstand (m)" else "Pegel (cm)"

  # Precipitation: aggregate RADOLAN to hourly sums for this station
  precip_hourly <- NULL
  has_precip    <- FALSE
  if (!is.null(precip_all)) {
    station_precip <- precip_all %>% filter(station == st$name)
    if (nrow(station_precip) > 0) {
      precip_hourly <- station_precip %>%
        mutate(hour = floor_date(timestamp, "hour")) %>%
        group_by(timestamp = hour) %>%
        summarise(precipitation_mm = sum(precipitation_mm, na.rm = TRUE), .groups = "drop") %>%
        filter(precipitation_mm > 0)
      has_precip <- nrow(precip_hourly) > 0
    }
  }

  # Nahecho-Phasen und indirekte Ereignisse (RADOLAN-Regen bei nassem Sensor)
  nahecho_shapes <- list()
  ev_nh <- NULL
  if (has_nahecho) {
    eps <- nahecho_episodes(df$Zeit_Datum, df$nahecho)
    nahecho_shapes <- lapply(seq_len(nrow(eps)), function(k) list(
      type = "rect", xref = "x", yref = "paper",
      x0 = as.character(eps$start[k]), x1 = as.character(eps$end[k]),
      y0 = 0, y1 = 1, layer = "below", line = list(width = 0),
      fillcolor = "rgba(110, 110, 160, 0.18)"))
    if (!is.null(precip_all)) {
      ph <- precip_all %>% filter(station == st$name) %>%
        mutate(timestamp = floor_date(timestamp, "hour")) %>%
        group_by(timestamp) %>%
        summarise(precipitation_mm = sum(precipitation_mm, na.rm = TRUE), .groups = "drop")
      ev_nh <- nahecho_events(eps, ph)
    }
    cat("  ", st$name, ": ", nrow(eps), " Nahecho-Phasen, ",
        if (is.null(ev_nh)) 0 else nrow(ev_nh), " indirekte Ereignisse\n", sep = "")
  }

  p <- plot_ly()

  if (has_raw && any(!is.na(df_hourly$level_raw))) {
    p <- p %>% add_trace(
      data = df_hourly, x = ~Zeit_Datum, y = ~(level_raw * y_scale),
      type = "scatter", mode = "lines",
      name = "Rohpegel",
      line = list(color = "rgba(0,0,0,0.6)", width = 1),
      hovertemplate = paste0("Rohpegel: %{y:.2f} ", y_unit, "<extra></extra>")
    )
  }

  # Fläche und Linie getrennt: Plotly füllt "tozeroy" sonst über Lücken (NA)
  # hinweg. Die Fläche nutzt 0 statt NA, die Linie bleibt in Lücken unterbrochen.
  p <- p %>% add_trace(
    data = df_hourly %>% mutate(level_fill = ifelse(is.na(level), 0, level)),
    x = ~Zeit_Datum, y = ~(level_fill * y_scale),
    type = "scatter", mode = "none", fill = "tozeroy",
    fillcolor = "rgba(0, 114, 178, 0.2)", showlegend = FALSE, hoverinfo = "skip"
  ) %>% add_trace(
    data = df_hourly, x = ~Zeit_Datum, y = ~(level * y_scale),
    type = "scatter", mode = "lines", connectgaps = FALSE,
    name = if (has_raw) "Pegel (bereinigt)" else "Pegel",
    line = list(color = "#0072B2", width = 1.5),
    hovertemplate = paste0("Pegel: %{y:.2f} ", y_unit, "<extra></extra>")
  )

  if (has_precip) {
    max_precip <- max(precip_hourly$precipitation_mm, na.rm = TRUE)
    p <- p %>% add_trace(
      data = precip_hourly, x = ~timestamp, y = ~precipitation_mm,
      type = "bar", name = "Niederschlag (RADOLAN)",
      yaxis = "y2",
      marker = list(color = "rgba(255, 78, 0, 0.93)"),
      hovertemplate = "Regen: %{y:.2f} mm/h<extra></extra>"
    )
  }

  y_max <- suppressWarnings(max(df_hourly$level, na.rm = TRUE)) * y_scale * 1.1
  if (!is.finite(y_max) || y_max <= 0) y_max <- 1

  if (has_nahecho) {
    p <- p %>% add_trace(
      # Legenden-Dummy außerhalb des sichtbaren y-Bereichs
      x = min(df_hourly$Zeit_Datum), y = -1000, type = "scatter", mode = "markers",
      name = "Sensor nass (Pegel nicht messbar)",
      marker = list(symbol = "square", size = 11, color = "rgba(110, 110, 160, 0.45)"),
      hoverinfo = "skip")
  }
  if (!is.null(ev_nh) && nrow(ev_nh) > 0) {
    ev_nh <- ev_nh %>% mutate(
      y = y_max * 0.93,
      txt = paste0("<b>Ereignis (indirekt)</b><br>",
                   format(rain_start, "%d.%m.%Y %H:%M"), "–", format(rain_end, "%H:%M"),
                   "<br>Regen: ", format(total_precip_mm, decimal.mark = ","), " mm",
                   " (max. ", format(max_intensity_mm_h, decimal.mark = ","), " mm/h)",
                   "<br>Sensor nass, Pegel nicht messbar"))
    p <- p %>% add_trace(
      data = ev_nh, x = ~rain_start, y = ~y, type = "scatter", mode = "markers",
      name = "Ereignis (indirekt, Nahecho)",
      marker = list(symbol = "triangle-down", size = 10, color = "#B2182B",
                    line = list(color = "white", width = 1)),
      text = ~txt, hovertemplate = "%{text}<extra></extra>")
  }

  date_range <- paste0(
    format(min(df_hourly$Zeit_Datum), "%d.%m.%Y"),
    " – ",
    format(max(df_hourly$Zeit_Datum), "%d.%m.%Y")
  )

  margin_r <- if (has_precip) 50 else 20
  max_precip_range <- if (has_precip) max(precip_hourly$precipitation_mm, na.rm = TRUE) * 1.3 else 10

  p <- p %>% layout(
    title = list(
      text = paste0("<b>", st$name, "</b> – Vollständige Zeitreihe (bereinigt)<br>",
                    "<span style='font-size:11px;color:#888;'>", date_range,
                    " (10-min-Mittel, ", format(nrow(df_hourly), big.mark = " "), " Punkte)</span>"),
      font = list(size = 14), x = 0, y = 0.98
    ),
    xaxis = list(
      title = "", gridcolor = "#eeeeee",
      rangeselector = list(
        x = 0, y = 1.13,
        buttons = list(
          list(count = 7,  label = "7T",  step = "day",   stepmode = "backward"),
          list(count = 1,  label = "1M",  step = "month", stepmode = "backward"),
          list(count = 3,  label = "3M",  step = "month", stepmode = "backward"),
          list(count = 6,  label = "6M",  step = "month", stepmode = "backward"),
          list(step = "all", label = "Alle")
        )
      ),
      rangeslider = list(visible = TRUE, thickness = 0.06)
    ),
    yaxis  = list(title = y_label, gridcolor = "#eeeeee",
                  range = c(0, y_max)),
    yaxis2 = list(title = "Niederschlag (mm/h)", overlaying = "y", side = "right",
                  showgrid = FALSE, zeroline = TRUE,
                  range = c(0, max_precip_range)),
    shapes = nahecho_shapes,
    margin = list(l = 50, r = margin_r, t = 90, b = 40),
    showlegend = TRUE,
    legend = list(orientation = "h", x = 0, y = -0.25, font = list(size = 10)),
    hovermode = "x unified",
    plot_bgcolor = "white",
    paper_bgcolor = "white"
  ) %>% config(displayModeBar = TRUE, scrollZoom = TRUE)

  out_name <- paste0(st$safe, "_full.html")
  out_path <- file.path(output_dir, out_name)

  nav_js <- tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function() {
      setTimeout(function() {
        var gd = document.querySelector('.plotly.html-widget');
        if (!gd) return;
        var btnStyle = 'padding:6px 16px;margin:0 4px;cursor:pointer;border:1px solid #bbb;' +
                       'border-radius:5px;background:#f4f4f4;font-size:14px;color:#333;';
        var container = document.createElement('div');
        container.style.cssText = 'text-align:center;margin:8px 0 4px 0;';
        var btnPrev = document.createElement('button');
        btnPrev.innerHTML = '\\u25C0 Zur\\u00FCck';
        btnPrev.style.cssText = btnStyle;
        var btnNext = document.createElement('button');
        btnNext.innerHTML = 'Vor \\u25B6';
        btnNext.style.cssText = btnStyle;
        function shiftRange(dir) {
          var r = gd._fullLayout.xaxis.range;
          var t0 = new Date(r[0]).getTime(), t1 = new Date(r[1]).getTime();
          var span = t1 - t0;
          Plotly.relayout(gd, {'xaxis.range': [
            new Date(t0 + dir * span).toISOString(),
            new Date(t1 + dir * span).toISOString()
          ]});
        }
        btnPrev.onclick = function() { shiftRange(-1); };
        btnNext.onclick = function() { shiftRange(1); };
        container.appendChild(btnPrev);
        container.appendChild(btnNext);
        gd.parentNode.insertBefore(container, gd.nextSibling);
      }, 500);
    });
  "))
  widget_with_nav <- tagList(p, nav_js)

  tryCatch(
    save_html(widget_with_nav, file = out_path),
    error = function(e) {
      cat("  save_html failed, falling back to saveWidget:", conditionMessage(e), "\n")
      saveWidget(p, file = out_path, selfcontained = FALSE)
    }
  )
  file_copy(out_path, file.path(docs_dir, out_name), overwrite = TRUE)

  # save_html() puts dependencies in lib/ (not _files/), sync to docs
  src_lib <- file.path(output_dir, "lib")
  if (dir_exists(src_lib)) {
    docs_lib <- file.path(docs_dir, "lib")
    if (!dir_exists(docs_lib)) dir_create(docs_lib)
    for (subdir in dir_ls(src_lib, type = "directory")) {
      dest <- file.path(docs_lib, basename(subdir))
      if (dir_exists(dest)) dir_delete(dest)
      dir_copy(subdir, dest)
    }
  }

  # fallback: saveWidget uses _files/ convention
  files_dir <- paste0(tools::file_path_sans_ext(out_path), "_files")
  if (dir_exists(files_dir)) {
    docs_files <- file.path(docs_dir, basename(files_dir))
    if (dir_exists(docs_files)) dir_delete(docs_files)
    dir_copy(files_dir, docs_files)
  }
  cat("  ", st$name, "->", out_name, "(", nrow(df_hourly), "lvl,",
      if (has_precip) nrow(precip_hourly) else 0, "precip points)\n")
}

cat("Full timeseries plots complete.\n")
