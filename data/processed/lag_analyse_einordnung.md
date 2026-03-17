# Lag-Analyse: Wissenschaftliche Einordnung der Sensor-Reaktionszeiten
**Projekt:** SCIU THALESruhr – Wasserstandsmonitoring Bochum/Hattingen
**Analysezeitraum:** September 2025 – März 2026
**Stand:** März 2026

---

## 1. Hintergrund und Fragestellung

In der urbanen Hydrologie bezeichnet der **Lag** (auch: *Response Time* oder *Time of Concentration*) die zeitliche Verzögerung zwischen dem Einsetzen eines Niederschlagsereignisses und der messbaren Reaktion im Gewässer- oder Kanalsystem. Diese Kenngröße ist zentral für die Bemessung von Frühwarnsystemen: Je kürzer der Lag, desto weniger Zeit bleibt für eine Reaktion – etwa Sperrungen, Evakuierungen oder den Einsatz mobiler Schutzmaßnahmen.

Für urbane Einzugsgebiete mit hohem Versiegelungsgrad (wie die hier untersuchten Standorte im Ruhrgebiet) ist bekannt, dass die Reaktionszeiten erheblich kürzer ausfallen als in naturnahen Einzugsgebieten. Versiegelte Flächen verhindern die Versickerung, sodass Niederschlagswasser schnell in das Kanalnetz und auf Straßenoberflächen abfließt. Gleichzeitig sorgen begrenzte Kanalkapazitäten für schnelle Rückstauphänomene an Tiefpunkten.

---

## 2. Methodik

### 2.1 Onset-Lag

Der **Onset-Lag** beschreibt den Zeitraum zwischen dem ersten detektierten Niederschlag oberhalb einer Mindestschwelle (hier: ≥ 0,5 mm/h, entspricht DWD-Warnstufe 1) und dem ersten Überschreiten des Sensor-Schwellenwerts von 1,5 cm Wasserstand. Er wird berechnet als:

```
Onset-Lag [min] = t_sensor_onset − t_precip_onset
```

Ein positiver Wert bedeutet: Der Sensor reagiert nach dem Regen (erwartetes Verhalten).
Ein negativer Wert bedeutet: Der Sensor registriert Wasser bevor der Niederschlag im Messnetz erfasst wird – häufig ein Hinweis auf räumlich sehr kleinräumige Ereignisse oder auf Abfluss aus angrenzenden Einzugsgebieten.

### 2.2 Peak-Lag

Der **Peak-Lag** misst den Abstand zwischen dem stündlichen Niederschlagsmaximum und dem Wasserstandsmaximum am Sensor:

```
Peak-Lag [min] = t_sensor_peak − t_precip_peak
```

### 2.3 Datengrundlage und Einschränkungen

| Quelle | Auflösung | Einschränkung |
|--------|-----------|---------------|
| Sensor-Wasserstand (Nivus) | ~1 Minute | Hohe zeitliche Auflösung, gut geeignet |
| Niederschlag – historisch (Open-Meteo Archive) | 1 Stunde | Messungenauigkeit ±30 Minuten (Altdaten) |
| Niederschlag – aktuell (RADOLAN RY, DWD) | **5 Minuten** | Messungenauigkeit ±2,5 Minuten (ab Systemumstellung) |
| Räumliche Abdeckung Niederschlag | 1 km Raster, sensorspezifisch | Lokale Starkregenzellen < 1 km räumlich nicht auflösbar |

Für historische Ereignisse (vor Umstellung auf RADOLAN RY) gilt eine Messungenauigkeit von ±30 Minuten aus der stündlichen Open-Meteo-Quelle. **Zukünftige Ereignisse** werden mit RADOLAN RY (5-Minuten-Produkt des DWD) mit einer Genauigkeit von **±2,5 Minuten** erfasst – ausreichend für die Charakterisierung auch kurzer Sturzflut-Lags.

---

## 3. Ergebnisse und Interpretation

### 3.1 Zusammenfassung nach Station

| Station | n | Median-Lag (min) | Mittel-Lag (min) | Min (min) | Max (min) |
|---------|---|-----------------|-----------------|-----------|-----------|
| Königsallee Springorum | 5 | 92 | 92 | −3 | 174 |
| An der Kost | 1 | 96 | 96 | 96 | 96 |
| Wasserstraße Springorum | 33 | 101 | 88 | −44 | 178 |
| Herzogstraße | 143 | 113 | 93 | −60 | 180 |

### 3.2 Einordnung der Absolutwerte

Die gemessenen Median-Lags von **92–113 Minuten** liegen im unteren bis mittleren Bereich für urbane Einzugsgebiete. Zum Vergleich:

- **Naturnahe Einzugsgebiete** (Wälder, Grünland): typische Lags von 3–24 Stunden
- **Urbane Einzugsgebiete** (Mittelstädte, gemischte Nutzung): 30 Minuten bis 3 Stunden
- **Hochversiegelte Innenstadtlagen** (> 80 % Versiegelung): teils < 20 Minuten
- **Tiefpunkte mit direktem Straßenabfluss** (wie hier): 15–90 Minuten erwartet

Die beobachteten Werte legen nahe, dass die Sensoren überwiegend **Sammelpunkte mit mittelgroßen Einzugsgebieten** erfassen, in denen der Abfluss aus mehreren Teilbereichen zusammenfließt bevor er den Tiefpunkt erreicht. Ein rein lokaler Direktabfluss würde deutlich kürzere Lags erzeugen.

### 3.3 Stationsvergleich

**Herzogstraße** zeigt mit 143 analysierbaren Ereignissen die größte Stichprobe und den höchsten Median-Lag (113 min). Die breite Streuung (−60 bis 180 min) deutet auf ein heterogenes Einzugsgebiet hin, in dem Ereignisse aus unterschiedlichen Richtungen und Entfernungen eintreffen können. Die negativen Werte (Sensor reagiert vor dem Regen) weisen auf abflussrelevante Voreignisse oder Zuflüsse hin, die durch das Niederschlagspunkt-Messnetz räumlich nicht abgedeckt werden.

**Wasserstraße Springorum** (33 Ereignisse, Median 101 min) reagiert im Mittel etwas schneller. Die negativen Minimalwerte (bis −44 min) sind hier besonders auffällig und könnten auf Abfluss aus dem angrenzenden Springorum-Grünzug hinweisen, wo lokale Starkregenzellen durch das Messnetz nicht erfasst werden.

**Königsallee Springorum** und **An der Kost** haben zu geringe Fallzahlen (5 bzw. 1) für belastbare Aussagen. Die räumliche Nähe beider Stationen zum Springorum-Bereich lässt ähnliche hydrologische Charakteristika wie bei der Wasserstraße vermuten.

### 3.4 Negative Lags

In 12–15 % der Fälle sind negative Lags zu beobachten (Sensor vor Regen). Mögliche Erklärungen:

1. **Räumliche Verschiebung der Niederschlagszelle**: Der Regen fällt zunächst im Einzugsgebiet des Sensors, wird aber am zentralen Messpunkt (Bochum-Zentrum) erst später oder schwächer gemessen.
2. **Abfluss aus Nachbareinzugsgebieten**: Wasser aus bereits gesättigten Bereichen fließt zu, bevor lokaler Regen einsetzt.
3. **Messlatenz**: Bei sehr schnellen Ereignissen (< 15 min) kann die stündliche Niederschlagsauflösung zu scheinbar negativen Lags führen.

---

## 4. Implikationen für das Frühwarnsystem

Auf Basis der beobachteten Lags lassen sich folgende **Vorlaufzeiten für Warnungen** ableiten:

| Szenario | Verbleibende Vorlaufzeit nach Regendetekttion |
|----------|----------------------------------------------|
| Konservativ (Median-Lag ~90 min) | ~60 min (nach Abzug Kommunikationszeit) |
| Ungünstig (unteres Quartil ~30 min) | ~0–15 min – kaum reaktionsfähig |
| Sturzflut-Ereignisse | Potenziell < 15 min – Nowcasting erforderlich |

Das System ist für **Warnstufe 2-Ereignisse** (DWD ≥ 15 mm/h) bei medianen Lags gut geeignet, um automatische Warnmeldungen mit sinnvoller Vorlaufzeit auszulösen. Für Sturzfluten (Stufe 3–4, Lag < 30 min) wäre eine direkte Kopplung an hochaufgelöste Radar-Nowcasting-Produkte des DWD (z. B. RADVOR-RQ, 5-min-Auflösung) notwendig.

---

## 5. Empfehlungen

1. **Niederschlagsmessung verbessern**: Stationsnahe Regenmesser oder vollständige RADOLAN-Punktextraktion (5-min RW-Produkt) würden die Lag-Unsicherheit von ±30 auf ±2,5 Minuten reduzieren.
2. **Stichprobengröße erhöhen**: Königsallee und An der Kost haben zu wenige Events für statistisch belastbare Aussagen – weiteres Monitoring erforderlich.
3. **Ereignistyp-spezifische Schwellenwerte**: Sturzfluten und Regenereignisse sollten mit unterschiedlichen Warnschwellen und Vorlaufzeiten behandelt werden.
4. **Einzugsgebietsanalyse**: Eine GIS-basierte Abgrenzung der hydrologischen Einzugsgebiete je Sensor würde die Interpretation der Lags und der negativen Ausreißer erheblich verbessern.

---

*Analyse: SCIU THALESruhr Monitoring-System | Niederschlagsquelle: Open-Meteo Archive (DWD-basiert) | Sensorik: Nivus DataKiosk*
