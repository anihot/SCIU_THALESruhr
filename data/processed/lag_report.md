# Lag-Analyse: Reaktionszeiten der Sensoren
*Erstellt: 2026-03-18 08:17*

## Methodik
- **Onset-Lag**: Zeit zwischen erstem Stundenwert ≥ 0,5 mm/h und erstem Schwellenübertritt am Sensor (1,5 cm)
- **Peak-Lag**: Zeit zwischen Niederschlagsmaximum und Sensor-Peakwert
- Niederschlagsquelle: Open-Meteo Archive (stündlich) – Messungenauigkeit ±30 min
- Lags außerhalb [−60 min, 360 min] werden als nicht korreliert verworfen

## Gesamtergebnis
- Ereignisse analysiert: **147**
- Median Onset-Lag (alle Stationen): **142 min**
- Mittlerer Onset-Lag: **125 min**

## Zusammenhang Lag ~ Regenintensität
Lineare Regression: Onset-Lag ~ max. Intensität (mm/h)
- Koeffizient: **19.37 min pro mm/h** (negativ = höherer Regen → kürzerer Lag)
- R² = 0.061

## Nach Ereignistyp

|event_type                              |   n| Median-Lag (min)| Mittel-Lag (min)| Min (min)| Max (min)|
|:---------------------------------------|---:|----------------:|----------------:|---------:|---------:|
|Leichter Regen / Unterhalb DWD-Schwelle |   7|              157|              159|       139|       178|
|Regenereignis / Natürlich               |  15|              142|              113|       -43|       180|
|Sturzflut-Ereignis                      | 125|              140|              125|       -46|       179|

## Nach Station

|station                 |  n| Median-Lag (min)| Mittel-Lag (min)| Min (min)| Max (min)|
|:-----------------------|--:|----------------:|----------------:|---------:|---------:|
|An_der_Kost             |  1|               90|             90.0|        90|        90|
|Königsallee_Springorum  |  1|              126|            126.0|       126|       126|
|Wasserstraße_Springorum | 47|              139|            131.8|       -37|       179|
|Herzogstraße            | 98|              143|            122.4|       -46|       180|

## Plots
- `data/output/plots/lag_onset_by_station.png` – Boxplot Onset-Lag je Station
- `data/output/plots/lag_onset_by_type.png` – Boxplot nach Ereignistyp
- `data/output/plots/lag_vs_intensity.png` – Scatter Lag vs. Regenintensität
- `data/output/plots/lag_peak_distribution.png` – Histogramm Peak-Lag je Station
