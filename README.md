


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



Here is an update of some scripts:

packages.R

Sets up the R environment for the project. Installs and loads all required packages for data handling, visualization, Random Forest, and LSTM models (via reticulate, tensorflow, and keras3). Also contains helper functions for creating or refreshing the r-conda-tf TensorFlow environment.


bootstrap_1000x.R

Performs 1000 bootstrap resampling iterations of model performance metrics. Generates uncertainty estimates (e.g., distributions of AUPRC and ROC-AUC) for both Random Forest and LSTM models to quantify variability and confidence in model performance.


target_scramble_1000x_2.R

Runs a target-scrambling (permutation) experiment with 1000 iterations. The presence/absence labels are randomly shuffled for each iteration, models are re-trained, and a null distribution of performance metrics is produced. This is used to verify that observed model performance is significantly above chance.


graphs_paper_2.R

Creates updated, publication-ready figures for the manuscript. Integrates outputs from the bootstrap and target-scramble analyses, as well as refined weekly summaries. Produces AUPRC/ROC curves, bootstrap confidence distributions, null vs. observed comparisons, and weekly confusion-based barplots.


lstm_conf_graph_2.R

Processes LSTM prediction outputs to:

determine decision thresholds (e.g., thresholds guaranteeing ≥ X% recall),

classify predictions into TP/TN/FP/FN,

generate weekly confusion summaries, and

prepare tidy datasets for the confusion matrices and stacked weekly performance barplots.


rf_time_lag_conf_graph_2.R

Processes Random Forest time-lag model outputs to:

compute probability thresholds for each dataset that achieve ≥ 50% recall,

classify predictions into TP/TN/FP/FN,

produce weekly summaries grouped by week_start, and

generate confusion matrices and weekly stacked barplots for Copernicus, 16S, 16S_all, 18S, and 18S_all Random Forest models.


Here is a description of all datasets used:

join_all_current_discharge_hplc_temp_weather_full_year.csv

A processed data file containing merged environmental and discharge variables used in Random Forest and LSTM analyses.

copernicus_data.csv	
Environmental and oceanographic variables derived from the Copernicus Marine Service. Includes satellite or reanalysis variables (e.g., sea surface temperature, salinity, currents). Used as input features in the Copernicus-based Random Forest and LSTM models.

join_all_current_discharge_hplc_temp_weather_full_year.csv	
Fully merged environmental dataset containing current measurements, river discharge, nutrient concentrations (HPLC), water temperature, and meteorological variables for the entire sampling year. This is the main environmental feature table used in model training and testing.

otus_16s_filtered.csv	
Filtered 16S rRNA ASV/OTU abundance table containing prokaryotic microbial community profiles after quality control and feature filtering. Used in microbial-community Random Forest and LSTM modelling.

otus_18S_filtered_no1.csv	
Filtered 18S rRNA ASV/OTU abundance table representing eukaryotic microbial community structure. Used in 18S-based predictive modelling and ecological comparisons.

taxonomy_16s.csv	
Taxonomic assignments for all 16S ASVs/OTUs, including hierarchical taxonomy from kingdom to species (where available). Used to map ASVs to taxonomic labels for ecological interpretation and feature importance summaries.

v_vul_otu.csv	
Derived microbial dataset focusing on OTUs relevant to Vibrio vulnificus. Used in exploratory analysis and to link microbial composition with pathogen presence/absence.
