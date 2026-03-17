# Lag-Analyse: Reaktionszeiten der Sensoren
*Erstellt: 2026-03-17 14:48*

## Methodik
- **Onset-Lag**: Zeit zwischen erstem Stundenwert ≥ 0,5 mm/h und erstem Schwellenübertritt am Sensor (1,5 cm)
- **Peak-Lag**: Zeit zwischen Niederschlagsmaximum und Sensor-Peakwert
- Niederschlagsquelle: Open-Meteo Archive (stündlich) – Messungenauigkeit ±30 min
- Lags außerhalb [−60 min, 360 min] werden als nicht korreliert verworfen

## Gesamtergebnis
- Ereignisse analysiert: **409**
- Median Onset-Lag (alle Stationen): **139 min**
- Mittlerer Onset-Lag: **121 min**

## Zusammenhang Lag ~ Regenintensität
Lineare Regression: Onset-Lag ~ max. Intensität (mm/h)
- Koeffizient: **9.96 min pro mm/h** (negativ = höherer Regen → kürzerer Lag)
- R² = 0.024

## Nach Ereignistyp

|event_type                              |   n| Median-Lag (min)| Mittel-Lag (min)| Min (min)| Max (min)|
|:---------------------------------------|---:|----------------:|----------------:|---------:|---------:|
|Leichter Regen / Unterhalb DWD-Schwelle |   8|              155|              157|       139|       178|
|Regenereignis / Natürlich               |  76|              140|              124|       -43|       180|
|Sturzflut-Ereignis                      | 325|              137|              120|       -46|       180|

## Nach Station

|station                 |   n| Median-Lag (min)| Mittel-Lag (min)| Min (min)| Max (min)|
|:-----------------------|---:|----------------:|----------------:|---------:|---------:|
|An_der_Kost             |   2|             93.0|             93.0|        90|        96|
|Königsallee_Springorum  |   7|            126.0|            118.4|        57|       174|
|Wasserstraße_Springorum | 112|            139.0|            122.8|       -37|       179|
|Herzogstraße            | 288|            139.5|            120.6|       -46|       180|

## Plots
- `data/output/plots/lag_onset_by_station.png` – Boxplot Onset-Lag je Station
- `data/output/plots/lag_onset_by_type.png` – Boxplot nach Ereignistyp
- `data/output/plots/lag_vs_intensity.png` – Scatter Lag vs. Regenintensität
- `data/output/plots/lag_peak_distribution.png` – Histogramm Peak-Lag je Station
