import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from datetime import datetime

df = pd.read_csv("data/raw/sensor_exports/An_der_Kost_merged_export.csv",
                  names=["Timestamp", "level_m", "distance_m"], skiprows=1)
df["Timestamp"] = pd.to_datetime(df["Timestamp"], format="mixed", utc=True)

# Convert level and distance from meters to cm for readability
df["level_cm"] = df["level_m"] * 100
df["distance_cm"] = df["distance_m"] * 100

# Filter May 5 00:00 to May 6 06:00
mask = (df["Timestamp"] >= "2026-05-05") & (df["Timestamp"] <= "2026-05-06 06:00")
d = df[mask].copy()

# Spike threshold: level > 40 cm (obstructions, per CLAUDE.md)
spikes = d[d["level_cm"] > 40]
clean = d[d["level_cm"] <= 40]

fig, axes = plt.subplots(3, 1, figsize=(16, 12), sharex=True)
fig.suptitle("An der Kost — Rohdaten 5.–6. Mai 2026", fontsize=16, fontweight="bold")

# 1) Distance (raw, in meters)
ax1 = axes[0]
ax1.plot(d["Timestamp"], d["distance_m"], color="#2196F3", linewidth=0.5, alpha=0.8)
ax1.scatter(spikes["Timestamp"], spikes["distance_m"], color="red", s=30, zorder=5, label="Spike/Obstruktion (>40 cm)")
ax1.set_ylabel("Distance (m)", fontsize=12)
ax1.set_title("Sensor-Distanz (Rohwerte in Metern)", fontsize=13)
ax1.legend(loc="lower left")
ax1.grid(True, alpha=0.3)
ax1.invert_yaxis()

# 2) Level (raw in cm, full scale to show spikes)
ax2 = axes[1]
ax2.plot(d["Timestamp"], d["level_cm"], color="#4CAF50", linewidth=0.5, alpha=0.8)
ax2.scatter(spikes["Timestamp"], spikes["level_cm"], color="red", s=30, zorder=5, label="Spike/Obstruktion (>40 cm)")
ax2.set_ylabel("Level (cm)", fontsize=12)
ax2.set_title("Wasserstand — volle Skala (mit Spikes)", fontsize=13)
ax2.legend(loc="upper left")
ax2.grid(True, alpha=0.3)

# 3) Level zoomed (without spikes, to see actual signal in cm)
ax3 = axes[2]
ax3.plot(clean["Timestamp"], clean["level_cm"], color="#FF9800", linewidth=0.8)
ax3.axhline(y=1.5, color="red", linestyle="--", alpha=0.7, linewidth=1.5, label="Event-Schwelle (1,5 cm)")
ax3.set_ylabel("Level (cm)", fontsize=12)
ax3.set_title("Wasserstand — gezoomt (ohne Spikes)", fontsize=13)
ax3.legend(loc="upper left")
ax3.grid(True, alpha=0.3)
ax3.set_xlabel("Zeit (UTC)", fontsize=12)

# Event-Zeitraum und Regenperiode markieren
for ax in axes:
    ax.axvspan(datetime(2026, 5, 6, 0, 6), datetime(2026, 5, 6, 0, 34, 30),
               alpha=0.15, color="red", label="Erkanntes Event" if ax == axes[0] else None)
    ax.axvspan(datetime(2026, 5, 5, 14, 0), datetime(2026, 5, 5, 21, 0),
               alpha=0.08, color="blue", label="Starkregen (RADOLAN)" if ax == axes[0] else None)

axes[0].legend(loc="lower left")

ax3.xaxis.set_major_formatter(mdates.DateFormatter("%d.%m\n%H:%M"))
ax3.xaxis.set_major_locator(mdates.HourLocator(interval=2))
plt.xticks(rotation=0)

plt.tight_layout()
plt.savefig("data/output/plots/An_der_Kost_event_May5_6_raw.png", dpi=150, bbox_inches="tight")
print("Plot saved to data/output/plots/An_der_Kost_event_May5_6_raw.png")
plt.close()
