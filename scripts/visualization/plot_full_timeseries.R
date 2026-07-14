library(plotly)
library(htmlwidgets)
library(dplyr)
library(readr)
library(lubridate)
library(fs)

cleaned_dir <- "data/processed/cleaned_analysis"
output_dir  <- "data/output/graphs"
docs_dir    <- "docs/graphs"

if (!dir_exists(output_dir)) dir_create(output_dir)
if (!dir_exists(docs_dir))   dir_create(docs_dir)

stations <- list(
  list(name = "An der Kost",    file = "An_der_Kost_merged_export_cleaned.csv",                safe = "An_der_Kost"),
  list(name = "Wasserstraße",   file = "Wasserstraße_Springorum_merged_export_cleaned.csv",    safe = "Wasserstra_e"),
  list(name = "Königsallee",    file = "Königsallee_Springorum_merged_export_cleaned.csv",     safe = "K_nigsallee"),
  list(name = "Wasserbaulabor 2", file = "Wasserbaulabor_2_merged_export_cleaned.csv",         safe = "Wasserbaulabor_2"),
  list(name = "Herzogstraße",   file = "Herzogstraße_merged_export_cleaned.csv",               safe = "Herzogstra_e")
)

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

  # Downsample to hourly means
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

  # Bimodal detection (same logic as generate_interactive_map.R)
  max_level <- max(df_hourly$level, na.rm = TRUE)
  is_bimodal <- !is.na(max_level) && max_level > 1.0
  if (is_bimodal) {
    scale_f <- 0.10 / max_level
    df_hourly <- df_hourly %>% mutate(
      level     = round(pmax(0, level * scale_f), 4),
      level_raw = if (has_raw) round(pmax(0, level_raw * scale_f), 4) else NA_real_
    )
  }

  y_label <- if (is_bimodal) "Füllstand (cm)" else "Pegel (cm)"

  p <- plot_ly()

  if (has_raw && any(!is.na(df_hourly$level_raw))) {
    p <- p %>% add_trace(
      data = df_hourly, x = ~Zeit_Datum, y = ~(level_raw * 100),
      type = "scatter", mode = "lines",
      name = "Rohpegel",
      line = list(color = "rgba(160,160,160,0.6)", width = 1, dash = "dot"),
      hovertemplate = "Rohpegel: %{y:.1f} cm<extra></extra>"
    )
  }

  p <- p %>% add_trace(
    data = df_hourly, x = ~Zeit_Datum, y = ~(level * 100),
    type = "scatter", mode = "lines",
    name = if (is_bimodal) "Füllstand" else "Pegel",
    line = list(color = "#0072B2", width = 1.5),
    fill = "tozeroy", fillcolor = "rgba(0, 114, 178, 0.15)",
    hovertemplate = paste0(if (is_bimodal) "Füllstand" else "Pegel", ": %{y:.1f} cm<extra></extra>")
  )

  date_range <- paste0(
    format(min(df_hourly$Zeit_Datum), "%d.%m.%Y"),
    " – ",
    format(max(df_hourly$Zeit_Datum), "%d.%m.%Y")
  )

  p <- p %>% layout(
    title = list(
      text = paste0("<b>", st$name, "</b> – Vollständige Zeitreihe<br>",
                    "<span style='font-size:11px;color:#888;'>", date_range,
                    " (Stundenmittel, ", format(nrow(df_hourly), big.mark = " "), " Punkte)</span>"),
      font = list(size = 14), x = 0
    ),
    xaxis = list(
      title = "", gridcolor = "#eeeeee",
      rangeselector = list(buttons = list(
        list(count = 7,  label = "7T",  step = "day",   stepmode = "backward"),
        list(count = 1,  label = "1M",  step = "month", stepmode = "backward"),
        list(count = 3,  label = "3M",  step = "month", stepmode = "backward"),
        list(count = 6,  label = "6M",  step = "month", stepmode = "backward"),
        list(step = "all", label = "Alle")
      )),
      rangeslider = list(visible = TRUE, thickness = 0.06)
    ),
    yaxis = list(title = y_label, gridcolor = "#eeeeee"),
    margin = list(l = 50, r = 20, t = 60, b = 40),
    showlegend = has_raw,
    legend = list(orientation = "h", x = 0, y = -0.25, font = list(size = 10)),
    hovermode = "x unified",
    plot_bgcolor = "white",
    paper_bgcolor = "white"
  ) %>% config(displayModeBar = TRUE, scrollZoom = TRUE)

  out_name <- paste0(st$safe, "_full.html")
  out_path <- file.path(output_dir, out_name)
  tryCatch(
    saveWidget(p, file = out_path, selfcontained = TRUE),
    error = function(e) saveWidget(p, file = out_path, selfcontained = FALSE)
  )
  file_copy(out_path, file.path(docs_dir, out_name), overwrite = TRUE)
  lib_dir <- paste0(tools::file_path_sans_ext(out_path), "_files")
  if (dir_exists(lib_dir)) {
    docs_lib <- file.path(docs_dir, basename(lib_dir))
    if (dir_exists(docs_lib)) dir_delete(docs_lib)
    dir_copy(lib_dir, docs_lib)
  }
  cat("  ", st$name, "->", out_name, "(", nrow(df_hourly), "points)\n")
}

cat("Full timeseries plots complete.\n")
