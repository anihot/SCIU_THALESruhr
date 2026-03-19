# Lag-Analyse: Reaktionszeiten der Sensoren
*Erstellt: 2026-03-19 09:41*

## Methodik
- **Onset-Lag**: Zeit zwischen erstem Stundenwert ≥ 0,5 mm/h und erstem Schwellenübertritt am Sensor (1,5 cm)
- **Peak-Lag**: Zeit zwischen Niederschlagsmaximum und Sensor-Peakwert
- Niederschlagsquelle: Open-Meteo Archive (stündlich) – Messungenauigkeit ±30 min
- Lags außerhalb [−60 min, 360 min] werden als nicht korreliert verworfen

## Gesamtergebnis
- Ereignisse analysiert: **3**
- Median Onset-Lag (alle Stationen): **106 min**
- Mittlerer Onset-Lag: **100 min**

## Nach Ereignistyp

|event_type                |  n| Median-Lag (min)| Mittel-Lag (min)| Min (min)| Max (min)|
|:-------------------------|--:|----------------:|----------------:|---------:|---------:|
|Regenereignis / Natürlich |  3|              106|              100|        37|       156|

## Nach Station

|station      |  n| Median-Lag (min)| Mittel-Lag (min)| Min (min)| Max (min)|
|:------------|--:|----------------:|----------------:|---------:|---------:|
|Herzogstraße |  3|              106|             99.7|        37|       156|

## Plots
- `data/output/plots/lag_onset_by_station.png` – Boxplot Onset-Lag je Station
- `data/output/plots/lag_onset_by_type.png` – Boxplot nach Ereignistyp
- `data/output/plots/lag_vs_intensity.png` – Scatter Lag vs. Regenintensität
- `data/output/plots/lag_peak_distribution.png` – Histogramm Peak-Lag je Station
