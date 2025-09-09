


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

This repository contains all analysis scripts used for modeling and visualization in the study of Vibrio vulnificus occurrence across Baltic Sea and Warnow estuary locations. The workflow combines deep learning (LSTM) and machine learning (Random Forest) approaches with microbial (16S and 18S sequencing), Copernicus satellite, and environmental data. Each script is dedicated to a specific part of the analysis: training and validating models, generating location-based cross-validation results, and producing final figures for the manuscript.

Script descriptions

graphs_paper.R
Produces all figures and visualizations included in the manuscript. This includes performance plots for LSTM and Random Forest models, confusion matrices, and weekly prediction summaries.

lstm_all_models_val_all_timesteps_best_test.R
Trains and validates LSTM models across time steps 1–10 using leave-one-location-out validation. Selects the best-performing time step based on average validation AUPRC and evaluates all models from that step on the held-out test set.

v_vul_16s_all_rf_time_lag_loop_val_loc.R
Implements Random Forest models with time-lagged predictors using the extended 16S microbial dataset across all locations. Validation is performed using spatial leave-one-location-out cross-validation.

v_vul_16s_rf_time_lag_loop_val_loc.R
Runs Random Forest models with time-lagged predictors using the primary 16S microbial dataset. Validation is based on spatial leave-one-location-out cross-validation.

v_vul_18s_all_rf_time_lag_loop_val_loc.R
Implements Random Forest models with time-lagged predictors using the extended 18S microbial dataset across all locations. Spatial leave-one-location-out cross-validation ensures robust model evaluation.

v_vul_18s_rf_time_lag_loop_val_loc.R
Runs Random Forest models with time-lagged predictors using the primary 18S microbial dataset. Validation follows the spatial leave-one-location-out framework.

v_vul_all_rf_models_loc.R
Summarizes and compares Random Forest performance across multiple datasets (16S, 18S, Copernicus, and environmental data) under consistent location-based validation.

v_vul_copernicus_rf_time_lag_loop.R
Applies Random Forest models with time-lagged predictors to Copernicus satellite and environmental datasets. Uses spatial leave-one-location-out cross-validation for model validation and evaluation.
