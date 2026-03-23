# Lag-Analyse: Reaktionszeiten der Sensoren
*Erstellt: 2026-03-23 09:51*

## Methodik
- **Onset-Lag**: Zeit zwischen erstem Stundenwert ≥ 0,5 mm/h und erstem Schwellenübertritt am Sensor (1,5 cm)
- **Peak-Lag**: Zeit zwischen Niederschlagsmaximum und Sensor-Peakwert
- Niederschlagsquelle: Open-Meteo Archive (stündlich) – Messungenauigkeit ±30 min
- Lags außerhalb [−60 min, 360 min] werden als nicht korreliert verworfen

## Gesamtergebnis
- Ereignisse analysiert: **16**
- Median Onset-Lag (alle Stationen): **148 min**
- Mittlerer Onset-Lag: **131 min**

## Zusammenhang Lag ~ Regenintensität
Lineare Regression: Onset-Lag ~ max. Intensität (mm/h)
- Koeffizient: **20.2 min pro mm/h** (negativ = höherer Regen → kürzerer Lag)
- R² = 0.078

## Nach Ereignistyp

|event_type                |  n| Median-Lag (min)| Mittel-Lag (min)| Min (min)| Max (min)|
|:-------------------------|--:|----------------:|----------------:|---------:|---------:|
|Regenereignis / Natürlich |  3|              106|              100|        37|       156|
|Sturzflut-Ereignis        | 13|              151|              138|        17|       173|

## Nach Station

|station                 |  n| Median-Lag (min)| Mittel-Lag (min)| Min (min)| Max (min)|
|:-----------------------|--:|----------------:|----------------:|---------:|---------:|
|Wasserstraße_Springorum |  1|              132|            132.0|       132|       132|
|Herzogstraße            | 15|              151|            130.8|        17|       173|

## Plots
- `data/output/plots/lag_onset_by_station.png` – Boxplot Onset-Lag je Station
- `data/output/plots/lag_onset_by_type.png` – Boxplot nach Ereignistyp
- `data/output/plots/lag_vs_intensity.png` – Scatter Lag vs. Regenintensität
- `data/output/plots/lag_peak_distribution.png` – Histogramm Peak-Lag je Station
