# vibrio_forecasting

Forecasting Vibrio vulnificus presence in the Baltic Sea & Warnow estuary using environmental (Copernicus + in-situ) and microbial community data (16S/18S).
We compare Random Forest (baseline), Random Forest with time-lagged features, and LSTM sequence models.
Evaluation emphasizes AUPRC (class imbalance) alongside ROC-AUC.

Data summary

Scope: ~15 locations, ~1 year/location, ≈100 time points per location

Target: presence/absence of V. vulnificus (highly seasonal, mainly summer)

Features:

Environmental/Copernicus (e.g., SST, salinity, chlorophyll, discharge, etc.)

Microbiome: 16S & 18S relative abundances (0–1)

Splits: spatial leave-one-location-out for validation, or 80:20 validation; held-out test sites commonly {7, 11, 13}
