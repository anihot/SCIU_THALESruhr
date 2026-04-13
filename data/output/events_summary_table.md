# Sensor Event Analysis Summary

## Station Overview

|station                 | Ereignisse| Ø_Peak_cm| Max_Peak_cm| Sturzflut_Anzahl| Regen_Anzahl| Verdächtig_Anzahl|
|:-----------------------|----------:|---------:|-----------:|----------------:|------------:|-----------------:|
|Königsallee_Springorum  |         28|       0.7|         1.2|                0|            0|                 0|
|Herzogstraße            |         25|       4.9|        34.9|               10|            3|                 0|
|Wasserstraße_Springorum |         14|       1.8|         5.0|                3|            1|                 0|
|An_der_Kost             |         11|       0.6|         0.7|                0|            0|                 0|
|Wasserbaulabor_2        |          1|      19.6|        19.6|                1|            0|                 0|

## Top 20 Most Intense Events

|station                 |start_time          | duration_min| peak_level_cm| avg_gradient_cm_min|event_type                              | total_precip_mm| max_intensity_mm_h|dwd_risk_level                         |
|:-----------------------|:-------------------|------------:|-------------:|-------------------:|:---------------------------------------|---------------:|------------------:|:--------------------------------------|
|Herzogstraße            |2025-12-19 14:32:00 |           43|          34.9|               1.390|Sturzflut-Ereignis                      |             0.6|                0.3|Level 0 – Kein nennenswerter Regen     |
|Herzogstraße            |2026-01-16 17:31:00 |           42|          23.7|               0.762|Sturzflut-Ereignis                      |             0.6|                0.2|Level 0 – Kein nennenswerter Regen     |
|Wasserbaulabor_2        |2025-02-22 08:30:00 |           45|          19.6|               0.651|Sturzflut-Ereignis                      |             0.0|                0.0|Level 0 – Kein nennenswerter Regen     |
|Herzogstraße            |2026-04-02 13:20:00 |           42|          17.3|               0.479|Sturzflut-Ereignis                      |             0.1|                0.1|Level 0 – Kein nennenswerter Regen     |
|Wasserstraße_Springorum |2025-10-12 06:57:00 |           39|           5.0|               0.226|Sturzflut-Ereignis                      |             0.1|                0.1|Level 0 – Kein nennenswerter Regen     |
|Herzogstraße            |2026-02-19 09:39:00 |           11|           4.7|               0.580|Sturzflut-Ereignis                      |             1.5|                0.7|Level 1 – Leichter Regen (0.5–15 mm/h) |
|Wasserstraße_Springorum |2025-10-18 03:59:00 |           59|           4.4|               0.257|Regenereignis / Natürlich               |             0.1|                0.1|Level 0 – Kein nennenswerter Regen     |
|Herzogstraße            |2026-02-22 21:44:00 |           26|           4.2|               0.347|Sturzflut-Ereignis                      |             1.6|                0.8|Level 1 – Leichter Regen (0.5–15 mm/h) |
|Wasserstraße_Springorum |2025-11-24 14:12:00 |           12|           4.2|               0.592|Sturzflut-Ereignis                      |             0.8|                0.3|Level 0 – Kein nennenswerter Regen     |
|Herzogstraße            |2026-02-17 15:32:00 |           11|           3.7|               0.725|Sturzflut-Ereignis                      |             0.2|                0.1|Level 0 – Kein nennenswerter Regen     |
|Herzogstraße            |2026-02-12 13:15:00 |           36|           3.4|               0.141|Sturzflut-Ereignis                      |             1.9|                1.0|Level 1 – Leichter Regen (0.5–15 mm/h) |
|Herzogstraße            |2026-02-16 10:46:00 |           50|           3.3|               0.094|Regenereignis / Natürlich               |             0.8|                0.6|Level 1 – Leichter Regen (0.5–15 mm/h) |
|Herzogstraße            |2026-02-19 12:13:00 |           38|           3.1|               0.147|Sturzflut-Ereignis                      |             3.4|                1.5|Level 1 – Leichter Regen (0.5–15 mm/h) |
|Wasserstraße_Springorum |2025-09-30 05:18:00 |           44|           3.0|               0.124|Sturzflut-Ereignis                      |             0.1|                0.1|Level 0 – Kein nennenswerter Regen     |
|Herzogstraße            |2025-10-23 04:14:00 |           37|           2.7|               0.141|Sturzflut-Ereignis                      |             1.9|                1.4|Level 1 – Leichter Regen (0.5–15 mm/h) |
|Herzogstraße            |2026-01-14 10:42:00 |           60|           2.1|               0.412|Regenereignis / Natürlich               |             3.0|                1.5|Level 1 – Leichter Regen (0.5–15 mm/h) |
|Herzogstraße            |2025-10-23 14:07:00 |           15|           2.1|               0.512|Sturzflut-Ereignis                      |             0.6|                0.5|Level 1 – Leichter Regen (0.5–15 mm/h) |
|Herzogstraße            |2026-01-04 06:19:00 |           20|           2.0|               0.488|Regenereignis / Natürlich               |             0.7|                0.5|Level 1 – Leichter Regen (0.5–15 mm/h) |
|Herzogstraße            |2025-10-26 13:50:00 |           21|           1.9|               0.118|Leichter Regen / Unterhalb DWD-Schwelle |             0.2|                0.1|Level 0 – Kein nennenswerter Regen     |
|Herzogstraße            |2025-10-23 17:07:00 |           21|           1.6|               0.122|Leichter Regen / Unterhalb DWD-Schwelle |             2.5|                1.3|Level 1 – Leichter Regen (0.5–15 mm/h) |

*Full log available in data/processed/detected_events.csv — 79 rain-verified events total.*
