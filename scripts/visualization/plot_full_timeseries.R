library(plotly)
library(htmlwidgets)
library(htmltools)
library(dplyr)
library(readr)
library(lubridate)
library(fs)

cleaned_dir <- "data/processed/cleaned_analysis"
output_dir  <- "data/output/graphs"
docs_dir    <- "docs/graphs"
precip_file <- "data/processed/precipitation_at_sensors.csv"

if (!dir_exists(output_dir)) dir_create(output_dir)
if (!dir_exists(docs_dir))   dir_create(docs_dir)

stations <- list(
  list(name = "An der Kost",      file = "An_der_Kost_merged_export_cleaned.csv",              safe = "An_der_Kost"),
  list(name = "Wasserstraße",     file = "Wasserstraße_Springorum_merged_export_cleaned.csv",   safe = "Wasserstra_e"),
  list(name = "Königsallee",      file = "Königsallee_Springorum_merged_export_cleaned.csv",    safe = "K_nigsallee"),
  list(name = "Wasserbaulabor 2", file = "Wasserbaulabor_2_merged_export_cleaned.csv",          safe = "Wasserbaulabor_2"),
  list(name = "Herzogstraße",     file = "Herzogstraße_merged_export_cleaned.csv",              safe = "Herzogstra_e")
)

# Load RADOLAN precipitation once
precip_all <- NULL
if (file_exists(precip_file)) {
  precip_all <- read_csv(precip_file, show_col_types = FALSE) %>%
    mutate(timestamp = as.POSIXct(timestamp, tz = "UTC"))
}

cat("Generating full timeseries plots...\n")

for (st in stations) {
  csv_path <- file.path(cleaned_dir, st$file)
  if (!file_exists(csv_path)) {
    cat("  SKIP:", st$name, "- file not found\n")
    next
  }

  df <- read_csv(csv_path, show_col_types = FALSE)
  if (nrow(df) == 0) next

  df <- df %>%
    mutate(Zeit_Datum = as.POSIXct(Zeit_Datum, tz = "UTC")) %>%
    filter(!is.na(Zeit_Datum))

  has_raw <- "level_raw" %in% names(df)

  df_hourly <- df %>%
    mutate(hour = floor_date(Zeit_Datum, "hour")) %>%
    group_by(hour) %>%
    summarise(
      level     = mean(level, na.rm = TRUE),
      level_raw = if (has_raw) mean(level_raw, na.rm = TRUE) else NA_real_,
      .groups   = "drop"
    ) %>%
    rename(Zeit_Datum = hour) %>%
    filter(!is.na(level))

  if (nrow(df_hourly) == 0) next

  max_level  <- max(df_hourly$level, na.rm = TRUE)
  max_raw    <- if (has_raw) max(df_hourly$level_raw, na.rm = TRUE) else max_level
  max_any    <- max(max_level, max_raw, na.rm = TRUE)
  is_bimodal <- !is.na(max_any) && max_any > 1.0
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

  p <- plot_ly()

  if (has_raw && any(!is.na(df_hourly$level_raw))) {
    p <- p %>% add_trace(
      data = df_hourly, x = ~Zeit_Datum, y = ~(level_raw * y_scale),
      type = "scatter", mode = "lines",
      name = "Rohpegel",
      line = list(color = "rgba(160,160,160,0.6)", width = 1, dash = "dot"),
      hovertemplate = paste0("Rohpegel: %{y:.2f} ", y_unit, "<extra></extra>")
    )
  }

  p <- p %>% add_trace(
    data = df_hourly, x = ~Zeit_Datum, y = ~(level * y_scale),
    type = "scatter", mode = "lines",
    name = if (has_raw) "Pegel (bereinigt)" else "Pegel",
    line = list(color = "#0072B2", width = 1.5),
    fill = "tozeroy", fillcolor = "rgba(0, 114, 178, 0.2)",
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
                    " (Stundenmittel, ", format(nrow(df_hourly), big.mark = " "), " Punkte)</span>"),
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
    yaxis  = list(title = y_label, gridcolor = "#eeeeee"),
    yaxis2 = list(title = "Niederschlag (mm/h)", overlaying = "y", side = "right",
                  showgrid = FALSE, zeroline = TRUE,
                  range = c(0, max_precip_range)),
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
