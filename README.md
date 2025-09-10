


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
Generates all figures used in the manuscript. This includes:

Performance curves (AUPRC, ROC-AUC) for LSTM and Random Forest models.

Weekly summaries of predictions compared to observations.

Confusion matrices broken down by time (e.g., true positives/negatives per week).

Final polished plots formatted for publication.

lstm_all_models_val_all_timesteps_best_test.R:

Trains LSTM models to predict V. vulnificus presence from microbial or environmental predictors. The script:

Evaluates time steps ranging from 1 to 10 days of input history.

Uses leave-one-location-out validation across training locations to identify the best-performing time step based on average AUPRC.

Selects the top fold within that time step and evaluates all models on the independent test set (locations 7, 11, and 13).

Outputs validation and test AUPRC/ROC-AUC tables plus confusion matrices for interpretation.


v_vul_16s_all_rf_time_lag_loop_val_loc.R:

Builds Random Forest models using 16S microbial community data from all sampling locations. This script:

Incorporates time-lagged microbial features (e.g., conditions in previous days).

Performs spatial leave-one-location-out cross-validation.

Records validation AUPRC and ROC-AUC across folds.

Outputs model performance tables for later comparison across datasets.


v_vul_16s_rf_time_lag_loop_val_loc.R:

Similar setup as above but restricted to the core 16S dataset (excluding extended “all locations” sequences). This provides a direct comparison between core and extended 16S microbial datasets.


v_vul_18s_all_rf_time_lag_loop_val_loc.R:

Implements Random Forest models with 18S microbial eukaryotic community data from all sampling locations. Key features:

Time-lagged predictors to capture short-term microbial dynamics.

Spatial leave-one-location-out validation.

Outputs fold-wise AUPRC/ROC-AUC results for comparison with other data types.


v_vul_18s_rf_time_lag_loop_val_loc.R
Runs Random Forest models with primary 18S data only (without extended locations). Outputs are parallel to the “all” version, allowing performance comparison between primary and extended datasets.


v_vul_all_rf_models_loc.R:
Provides a cross-dataset Random Forest comparison. This script:

Collects results from 16S, 18S, Copernicus, and environmental RF models.

Summarizes performance metrics across datasets under consistent location-based validation.

Produces tables and summary statistics used to compare predictive value of different data sources.


v_vul_copernicus_rf_time_lag_loop.R:

Runs Random Forest models using Copernicus satellite and environmental variables. This script:

Builds time-lagged predictors from daily measurements (e.g., SST, salinity).

Validates with spatial leave-one-location-out cross-validation.

Outputs performance metrics and tables directly comparable to microbial-based models.
