# Name: Conor Glackin
# Date: 17 June 2025
# Description: lstm with all model results for validation, test etc. this is for best test result auprc


setwd("C:/Users/glack/Documents/vibrio_second_paper/")

############################## trying with just weather and copernicus

join_all_current_discharge_hplc_temp_weather <-
  read_csv("data_all/join_all_current_discharge_hplc_temp_weather_full_year.csv")


join_all_current_discharge_hplc_temp_weather <-
  join_all_current_discharge_hplc_temp_weather %>%
  select(
    c(
      "sample",
      "quality_windspeed",
      "mean_windspeed",
      "precipitation_height",
      "precipitation_form",
      "sunshine_duration",
      "snow_depth",
      "mean_cloud_cover",
      "mean_pressure",
      "mean_relative_humidity",
      "downward_radiation",
      "diffuse_solar_radiation",
      "sunshine_duration_solar",
      "v_vul_total_abundance"
    )
  )


# Select the columns to modify
#columns_to_modify <- c("PO4", "NH4", "NO2")

# Loop through each column and replace negative values with 0
#for (col in columns_to_modify) {
#  join_all_current_discharge_hplc_temp_weather[[col]][join_all_current_discharge_hplc_temp_weather[[col]] < 0] <- 0
#}

copernicus_data <- read_csv("data_all/copernicus_data.csv")

copernicus_data <- copernicus_data %>%
  select((
    !c(
      "ssh_15_m",  "pot_temp_sea_floor_1m", "mixed_layer_thickness_1m","salinity_1m",
      "salinity_sea_floor_1m", "temp_potent_1m" , "velocity_east", "velocity_north",        
      "velocity_south_1m", "velocity_north_1m", "velocity_upward_1m", "ssh_untided_1d",
      "pot_temp_sea_floor_1d", "pot_temp_sea_floor_1d", "mixed_layer_thickness_1d", "salinity_1d",
      "salinity_sea_floor_1d", "temp_potent_1d", "velocity_south_1d", "velocity_north_1d",
      "velocity_upward_1d", "velocity_east_detided", "pot_temp_sea_floor_1h","velocity_north_detided"
    )
  ))


copernicus_data_ddpcr <-
  left_join(copernicus_data,
            join_all_current_discharge_hplc_temp_weather,
            by = "sample")



copernicus_data_ddpcr <- na.omit(copernicus_data_ddpcr)

# remove columns of all same value

logistic_reg <-
  copernicus_data_ddpcr[vapply(copernicus_data_ddpcr, function(x)
    length(unique(x)) > 1, logical(1L))]


#logistic_reg <- subset(logistic_reg, location %in%
#                         c("Heiligendamm", "Borgerende", "Nienhagen", "Warnemuende"))
#logistic_reg <-join_all_current_discharge_hplc_temp_weather

# Add a new column 'v_vul_presence' to the 'logistic_reg' data frame
logistic_reg <- logistic_reg %>%
  mutate(v_vul_presence = ifelse(v_vul_total_abundance == 0, 0, 1))

logistic_reg$v_vul_presence <-
  as.factor(logistic_reg$v_vul_presence)



logistic_reg <- logistic_reg %>%
  select((
    !c(
      "lon",
      "lat",
      "...1",
      "time",
      "date_time",
      "v_vul_total_abundance"
    )
  ))


# Create a named vector for the replacements
replacementS <- c(
  "Heiligendamm"    = "1",
  "Jemnitzschleuse" = "2",
  "Borgerende"      = "3",
  "Nienhagen"       = "4",
  "Elmenhorst"      = "5",
  "Diedrichshagen"  = "6",
  "Warnemuende"     = "7"
)

logistic_reg <- logistic_reg %>%
  mutate(location = recode(location,!!!replacementS))


logistic_reg$location <-  as.numeric(logistic_reg$location)

set.seed(7896)

# Remove NA values
otu_16s_filtered_rel_rf <- na.omit(logistic_reg)

convert_to_numeric <- function(data) {
  data[] <- lapply(names(data), function(col) {
    if (col %in% c("sample", "date")) {
      return(data[[col]])
    } else if (is.factor(data[[col]]) || is.character(data[[col]])) {
      return(as.numeric(as.character(data[[col]])))
    } else {
      return(data[[col]])
    }
  })
  return(data)
}

create_sequences <- function(features, target, time_steps, sample_ids) {
  X <- list()
  y <- list()
  ids <- list()
  
  for (i in seq_len(nrow(features) - time_steps)) {
    sequence_matrix <- as.matrix(features[i:(i + time_steps - 1), ])
    X[[i]] <- sequence_matrix
    y[[i]] <- target[i + time_steps]
    ids[[i]] <- sample_ids[i + time_steps]
  }
  
  if (length(X) > 0) {
    n_samples <- length(X)
    time_steps <- nrow(X[[1]])
    n_features <- ncol(X[[1]])
    X_array <- array(NA, dim = c(n_samples, time_steps, n_features))
    for (i in seq_along(X)) {
      X_array[i,,] <- X[[i]]
    }
    return(list(X = X_array, y = unlist(y), ids = unlist(ids)))
  } else {
    return(NULL)
  }
}

create_sequences_location <- function(data, time_steps) {
  features <- data %>% select(-v_vul_presence, -location, -date, -sample)
  target <- data$v_vul_presence
  sample_ids <- data$sample
  dates <- data$date
  if (nrow(features) > time_steps) {
    seq_data <- create_sequences(features, target, time_steps, sample_ids)
    seq_data$dates <- dates[(time_steps + 1):length(dates)]
    return(seq_data)
  } else {
    return(NULL)
  }
}

set.seed(7896)
time_step_range <- 1:10
validation_results_copernicus <- data.frame()
test_results_copernicus <- data.frame()
fold_models_by_time <- list()

for (time_steps in time_step_range) {
  cat(sprintf("\n--- Time Step %d ---\n", time_steps))
  fold_metrics <- list()
  fold_models <- list()
  
  for (val_loc in 1:6) {
    train_locs <- setdiff(1:6, val_loc)
    test_loc <- 7
    
    train_data <- convert_to_numeric(otu_16s_filtered_rel_rf %>% filter(location %in% train_locs))
    val_data <- convert_to_numeric(otu_16s_filtered_rel_rf %>% filter(location == val_loc))
    
    train_sequences <- lapply(unique(train_data$location), function(loc) {
      create_sequences_location(train_data %>% filter(location == loc), time_steps)
    })
    val_sequences <- create_sequences_location(val_data, time_steps)
    
    if (is.null(val_sequences)) next
    
    X_train <- do.call(abind::abind, c(lapply(train_sequences, function(seq) seq$X), list(along = 1)))
    y_train <- unlist(lapply(train_sequences, function(seq) seq$y))
    X_val <- val_sequences$X
    y_val <- val_sequences$y
    
    tensorflow::set_random_seed(7896)
    model <- keras_model_sequential() %>%
      layer_lstm(units = 128, input_shape = c(time_steps, dim(X_train)[3]), return_sequences = TRUE) %>%
      layer_lstm(units = 64, return_sequences = FALSE) %>%
      layer_dropout(rate = 0.4) %>%
      layer_dense(units = 1, activation = 'sigmoid')
    
    model %>% compile(
      loss = 'binary_crossentropy',
      optimizer = optimizer_adam(learning_rate = 0.001),
      metrics = c('accuracy')
    )
    
    model %>% fit(
      x = X_train, y = y_train,
      validation_data = list(X_val, y_val),
      epochs = 50, batch_size = 32,
      class_weight = list('0' = 1, '1' = length(y_train) / sum(y_train == 1)),
      callbacks = list(callback_early_stopping(patience = 10, restore_best_weights = TRUE)),
      verbose = 0
    )
    
    val_probs <- as.numeric(model %>% predict(X_val))
    
    if (length(unique(y_val)) >= 2) {
      pr <- pr.curve(scores.class0 = val_probs, weights.class0 = y_val, curve = FALSE)
      roc <- roc(y_val, val_probs)
      
      fold_metrics[[val_loc]] <- data.frame(
        TimeStep = time_steps,
        Fold = val_loc,
        AUPRC = pr$auc.integral,
        ROC_AUC = as.numeric(auc(roc))
      )
      fold_models[[val_loc]] <- model
    }
  }
  
  fold_df <- do.call(rbind, fold_metrics)
  validation_results_copernicus <- rbind(validation_results_copernicus, fold_df)
  
  test_results_copernicus <- rbind(test_results_copernicus, data.frame(
    TimeStep = time_steps,
    Avg_Validation_AUPRC = mean(fold_df$AUPRC, na.rm = TRUE),
    Avg_Validation_ROC = mean(fold_df$ROC_AUC, na.rm = TRUE)
  ))
  
  fold_models_by_time[[as.character(time_steps)]] <- fold_models
}

avg_summary_copernicus <- test_results_copernicus %>%
  filter(!is.na(Avg_Validation_AUPRC)) %>%
  arrange(desc(Avg_Validation_AUPRC))

best_time_step <- avg_summary_copernicus$TimeStep[1]
cat(sprintf("\n✅ Best Time Step based on average validation AUPRC: %d\n", best_time_step))

best_fold <- validation_results_copernicus %>%
  filter(TimeStep == best_time_step) %>%
  arrange(desc(AUPRC)) %>%
  slice(1) %>%
  pull(Fold)

cat(sprintf("🏆 Best fold in that time step: %d\n", best_fold))

# Prepare test set
test_loc <- 7
test_data <- convert_to_numeric(otu_16s_filtered_rel_rf %>% filter(location == test_loc))
test_sequences <- create_sequences_location(test_data, best_time_step)

# Evaluate all 6 models from best timestep on the test set
test_results_all_models <- data.frame()
fold_models_best_time <- fold_models_by_time[[as.character(best_time_step)]]

for (fold_num in 1:6) {
  model <- fold_models_best_time[[fold_num]]
  
  if (!is.null(model)) {
    probs <- model %>% predict(test_sequences$X)
    
    pr <- pr.curve(scores.class0 = probs, weights.class0 = test_sequences$y, curve = FALSE)
    roc_obj <- roc(test_sequences$y, probs)
    
    test_results_all_models <- rbind(test_results_all_models, data.frame(
      TimeStep = best_time_step,
      Fold = fold_num,
      Test_AUPRC = pr$auc.integral,
      Test_ROC_AUC = as.numeric(auc(roc_obj))
    ))
  }
}

# Average performance across all 6 models
test_summary_copernicus <- test_results_all_models %>%
  summarise(
    TimeStep = best_time_step,
    Avg_Test_AUPRC = mean(Test_AUPRC, na.rm = TRUE),
    Avg_Test_ROC_AUC = mean(Test_ROC_AUC, na.rm = TRUE)
  )

cat("\n📊 Average test performance from 6 folds at best timestep:\n")
print(test_summary_copernicus)

# Use best model from best fold for manual inspection
model <- fold_models_best_time[[best_fold]]

# Predict on test set
predicted_probabilities <- model %>% predict(test_sequences$X)
results_manual_copernicus <- data.frame(
  Sample_ID = test_sequences$ids,
  Date = test_sequences$dates,
  Actual = test_sequences$y,
  Predicted_Probability = predicted_probabilities
)

# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_copernicus$Actual == 1)
thresholds <- seq(1, 0.01, -0.01)
best_threshold <- 0.01

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_copernicus$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_copernicus$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.7) {
    best_threshold <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold selected (≥70%% recall): %.2f\n", best_threshold))

# Step: Convert Date column to Date class and add week_start column
results_manual_copernicus <- results_manual_copernicus %>%
  mutate(
    date = as.Date(Date),
    week_start = lubridate::floor_date(date, unit = "week", week_start = 1)
  )

# Apply threshold and create results table
results_manual_copernicus <- results_manual_copernicus %>%
  mutate(
    Predicted_Class = ifelse(Predicted_Probability > best_threshold, 1, 0),
    Category = case_when(
      Actual == 1 & Predicted_Class == 1 ~ "True Positive",
      Actual == 0 & Predicted_Class == 0 ~ "True Negative",
      Actual == 0 & Predicted_Class == 1 ~ "False Positive",
      Actual == 1 & Predicted_Class == 0 ~ "False Negative"
    )
  )

# Confusion Matrix
confusion_matrix_copernicus <- table(
  Predicted = results_manual_copernicus$Predicted_Class,
  Actual = results_manual_copernicus$Actual
)

cat("\n📌 Confusion Matrix (from best fold in best timestep):\n")
print(confusion_matrix_copernicus)

# Step: Weekly summary
weekly_summary_copernicus <- results_manual_copernicus %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")

# Create and expand tracking for test performance across all time steps
test_results_all_models_copernicus <- data.frame()
test_summary_all_timesteps_copernicus <- data.frame()

for (time_steps in time_step_range) {
  cat(sprintf("\n🔍 Evaluating all models from Time Step %d on Test Set\n", time_steps))
  
  fold_models <- fold_models_by_time[[as.character(time_steps)]]
  
  for (fold_num in 1:6) {
    model <- fold_models[[fold_num]]
    
    if (!is.null(model)) {
      probs <- model %>% predict(test_sequences$X)
      pr <- pr.curve(scores.class0 = probs, weights.class0 = test_sequences$y, curve = FALSE)
      roc_obj <- roc(test_sequences$y, probs)
      
      test_results_all_models_copernicus <- rbind(test_results_all_models_copernicus, data.frame(
        TimeStep = time_steps,
        Fold = fold_num,
        Test_AUPRC = pr$auc.integral,
        Test_ROC_AUC = as.numeric(auc(roc_obj))
      ))
    }
  }
  
  # Summary stats for this timestep
  timestep_results <- test_results_all_models_copernicus %>%
    filter(TimeStep == time_steps)
  
  test_summary_all_timesteps_copernicus <- rbind(test_summary_all_timesteps_copernicus, data.frame(
    TimeStep = time_steps,
    Avg_Test_AUPRC = mean(timestep_results$Test_AUPRC, na.rm = TRUE),
    Avg_Test_ROC_AUC = mean(timestep_results$Test_ROC_AUC, na.rm = TRUE),
    Best_Test_AUPRC = max(timestep_results$Test_AUPRC, na.rm = TRUE),
    Best_Test_ROC_AUC = max(timestep_results$Test_ROC_AUC, na.rm = TRUE)
  ))
}

# Output summaries
cat("\n✅ Test Results for Every Model:\n")
print(test_results_all_models_copernicus)

cat("\n📈 Summary of Test Results Across All Timesteps:\n")
print(test_summary_all_timesteps_copernicus)


####################### 16s

set.seed(7896)
# Load data
otu_16s_filtered <- read_csv("data_all/otus_16S_filtered.csv")

colnames(otu_16s_filtered)[which(names(otu_16s_filtered) == "...1")] <- "sample"

otu_16s_filtered$sample <- toupper(otu_16s_filtered$sample)

otu_16s_filtered <- t(otu_16s_filtered)

colnames(otu_16s_filtered) <- otu_16s_filtered[1,]

# Remove the first row
otu_16s_filtered <- otu_16s_filtered[-1, ]

otu_16s_filtered <- as.data.frame(otu_16s_filtered)

otu_16s_filtered <- tibble::rownames_to_column(otu_16s_filtered, "otu")

taxV3_V4 <- read_csv("data_all/taxonomy_16S.csv")  

taxV3_V4 <- taxV3_V4[, c("zotu", "biodomain", "phylum", "class", "bioorder","family", 
                         "genus","species")]


colnames(taxV3_V4)[which(names(taxV3_V4) == "zotu")] <- "otu"

otu_16s_filtered <- left_join(otu_16s_filtered, taxV3_V4, by = "otu")

otu_16s_filtered <- as.data.table(otu_16s_filtered)

otu_16s_filtered <- otu_16s_filtered[!(genus == "Vibrio")]

otu_16s_filtered <- otu_16s_filtered %>% 
  select((!c("biodomain", "phylum", "class", "bioorder","family", 
             "genus","species")))

otu_16s_filtered <- t(otu_16s_filtered)

colnames(otu_16s_filtered) <- otu_16s_filtered[1,]

# Remove the first row
otu_16s_filtered <- otu_16s_filtered[-1, ]

otu_16s_filtered <- as.matrix(otu_16s_filtered)

otu_16s_filtered <- otu_16s_filtered[, !colnames(otu_16s_filtered) %in% c("sample")]

class(otu_16s_filtered) <- "numeric"

otu_16s_filtered_1 <- make_relative(otu_16s_filtered)

otu_16s_filtered_1 <- as.data.frame(otu_16s_filtered_1)

otu_16s_filtered_1 <- tibble::rownames_to_column(otu_16s_filtered_1, "sample")

otu_18s_filtered <- read_csv("data_all/otus_18S_filtered_no1.csv")

colnames(otu_18s_filtered)[which(names(otu_18s_filtered) == "...1")] <- "sample"

otu_18s_filtered$sample <- toupper(otu_18s_filtered$sample)

otu_18s_filtered <- as.matrix(otu_18s_filtered)

rownames(otu_18s_filtered) <- otu_18s_filtered[,1]

otu_18s_filtered <- otu_18s_filtered[, !colnames(otu_18s_filtered) %in% c("sample")]

class(otu_18s_filtered) <- "numeric"

otu_18s_filtered <- make_relative(otu_18s_filtered)

otu_18s_filtered <- as.data.frame(otu_18s_filtered)

otu_18s_filtered <- tibble::rownames_to_column(otu_18s_filtered, "sample")

######

v_vul_16s <- read_csv("data_all/v_vul_otu.csv")

v_vul_16s <- v_vul_16s %>% 
  select((!c("...1")))

v_vul_16s <- v_vul_16s[!duplicated(v_vul_16s$sample),]

v_vul_16s$sample <- toupper(v_vul_16s$sample)

# Extract the last two digits from the 'sample' column
v_vul_16s$year <- as.numeric(substr(v_vul_16s$sample, nchar(v_vul_16s$sample) - 1, nchar(v_vul_16s$sample)))

v_vul_16s$week <- as.numeric(substr(v_vul_16s$sample, 5, 6))

v_vul_16s$location <- as.numeric(substr(v_vul_16s$sample, 2, 3))

v_vul_16s$day <- substr(v_vul_16s$sample, 4, 4)


# Assuming your dataframe is called v_vul_16s
# Convert year and week to proper format
v_vul_16s$Year <- as.numeric(paste0("20", v_vul_16s$year))  # Adding century to year
v_vul_16s$Week <- as.numeric(v_vul_16s$week)

# Create a new column for the Monday of the corresponding week
v_vul_16s$Monday_Date <- ISOweek2date(paste0(v_vul_16s$Year, "-W", sprintf("%02d", v_vul_16s$Week), "-1"))

# Adjust the Monday date based on the day column
v_vul_16s$Day_Shift <- dplyr::case_when(
  v_vul_16s$day == "M" ~ 0,  # Monday, no shift
  v_vul_16s$day == "X" ~ 1,  # Tuesday, add 1 day
  v_vul_16s$day == "T" ~ 3   # Thursday, add 3 days
)

# Create final date column by shifting Monday's date
v_vul_16s$date <- v_vul_16s$Monday_Date + v_vul_16s$Day_Shift

# View the updated dataframe with the new date column
head(v_vul_16s)


# Convert day labels to numeric values
v_vul_16s$day[v_vul_16s$day == "M"] <- 1
v_vul_16s$day[v_vul_16s$day == "T"] <- 2
v_vul_16s$day[v_vul_16s$day == "X"] <- 3

split_data <- split(v_vul_16s, v_vul_16s$location)

#################################### beginning of time lag

# Function to order by year and week, then shift the sample column up 
process_group <- function(df) {
  # Order the data frame by year and week
  df <- df[order(df$year, df$week), ]
  
  # Shift the sample column up by two rows
  df$sample_shifted <- c(rep(NA, 0), df$sample[1:(nrow(df) - 0)])
  
  return(df)
}

# Apply the process_group function to each location group
processed_data <- lapply(split_data, process_group)

# Combine the processed data back into a single data frame
v_vul_16s_processed <- do.call(rbind, processed_data)

# Reset the rownames to be sequential
rownames(v_vul_16s_processed) <- NULL

# Display the first few rows of the result
head(v_vul_16s_processed)


v_vul_16s_processed <- v_vul_16s_processed %>% 
  select((!c("sample" )))

colnames(v_vul_16s_processed)[which(names(v_vul_16s_processed) == "sample_shifted")] <- "sample"


v_vul_16s_processed <- na.omit(v_vul_16s_processed)

#otu_16s_filtered_rel <- left_join( otu_18s_filtered, v_vul_16s_processed, by = "sample")

otu_16s_filtered_rel <- left_join(  otu_16s_filtered_1,v_vul_16s_processed, by = "sample")


# Add a new column 'v_vul_presence' to the 'logistic_reg' data frame
otu_16s_filtered_rel <- otu_16s_filtered_rel %>%
  mutate(v_vul_presence = ifelse(v_vul_total_abundance == 0, 0, 1))

otu_16s_filtered_rel$v_vul_presence <- as.factor(otu_16s_filtered_rel$v_vul_presence)

otu_16s_filtered_rel_rf <- otu_16s_filtered_rel %>% 
  select((!c( "OTU_16S_25709","OTU_16S_25947", "OTU_16S_33238", "v_vul_total_abundance" )))

otu_16s_filtered_rel_rf <- na.omit(otu_16s_filtered_rel_rf)

otu_16s_filtered_rel_rf$day_of_year <- yday(otu_16s_filtered_rel_rf$date)
otu_16s_filtered_rel_rf$week_of_year <- week(otu_16s_filtered_rel_rf$date)

# Normalize day_of_year
#otu_16s_filtered_rel_rf$day_of_year_norm <- otu_16s_filtered_rel_rf$day_of_year / 365

# Normalize week_of_year
#otu_16s_filtered_rel_rf$week_of_year_norm <- otu_16s_filtered_rel_rf$week_of_year / 52

# Prepare the dataset for LSTM, keeping the 'sample' column
otu_16s_filtered_rel_rf <- otu_16s_filtered_rel %>% 
  select( -OTU_16S_25709, -OTU_16S_25947, -OTU_16S_33238, -v_vul_total_abundance,
          -year, -week, -day, -Year, -Week, -Monday_Date, -Day_Shift)
otu_16s_filtered_rel_rf <- na.omit(otu_16s_filtered_rel_rf)



# otu_16s_filtered_rel_rf <- otu_16s_filtered_rel_rf %>%
#   group_by(location) %>%
#   mutate(v_vul_presence = sample(v_vul_presence)) %>%
#   ungroup()




#ltsm_temp <- 
#  join_all_current_discharge_hplc_temp_weather_full_year[, c("sample", "temperature.y", "v_vul_total_abundance")]


#otu_16s_filtered_rel_rf <- left_join(otu_16s_filtered_rel_rf,
#                       ltsm_temp, by = "sample")


#otu_16s_filtered_rel_rf <- left_join(otu_16s_filtered_rel_rf,
#                                     otu_18s_filtered, by = "sample")




# Step 2: Split dataset into training and testing
#train_data <- otu_16s_filtered_rel_rf %>% filter(!location %in% c(6, 12))
#test_data  <- otu_16s_filtered_rel_rf %>% filter(location %in% c(6, 12))
otu_16s_filtered_rel_rf <- subset(otu_16s_filtered_rel_rf, location %in%  
                                    c("1", "2", "3", "4", "5", "6", "7"))#, "8", "9", "10", "11", "12", "13", "14", "15"))

convert_to_numeric <- function(data) {
  data[] <- lapply(names(data), function(col) {
    if (col %in% c("sample", "date")) {
      return(data[[col]])
    } else if (is.factor(data[[col]]) || is.character(data[[col]])) {
      return(as.numeric(as.character(data[[col]])))
    } else {
      return(data[[col]])
    }
  })
  return(data)
}

# Step 4: Function to create sequences ensuring sample IDs are included
create_sequences <- function(features, target, time_steps, sample_ids) {
  X <- list()
  y <- list()
  ids <- list()
  
  for (i in seq_len(nrow(features) - time_steps)) {
    sequence_matrix <- as.matrix(features[i:(i + time_steps - 1), ])
    
    # Make sure dimensions are time_steps x n_features (already OK)
    X[[i]] <- sequence_matrix
    y[[i]] <- target[i + time_steps]
    ids[[i]] <- sample_ids[i + time_steps]
  }
  
  if (length(X) > 0) {
    # Convert list of matrices to 3D array: [n_samples, time_steps, n_features]
    n_samples <- length(X)
    time_steps <- nrow(X[[1]])
    n_features <- ncol(X[[1]])
    
    # Stack the matrices correctly
    X_array <- array(NA, dim = c(n_samples, time_steps, n_features))
    for (i in seq_along(X)) {
      X_array[i,,] <- X[[i]]
    }
    
    return(list(X = X_array, y = unlist(y), ids = unlist(ids)))
  } else {
    return(NULL)
  }
}


# Step 5: Adapted generate sequences function for each location that includes sample_ids
create_sequences_location <- function(data, time_steps) {
  features <- data %>% select(-v_vul_presence, -location, -date, -sample)
  target <- data$v_vul_presence
  sample_ids <- data$sample
  dates <- data$date  # Keep track of dates
  
  if (nrow(features) > time_steps) {
    seq_data <- create_sequences(features, target, time_steps, sample_ids)
    seq_data$dates <- dates[(time_steps + 1):length(dates)]  # Align dates with target
    return(seq_data)
  } else {
    return(NULL)
  }
}

set.seed(7896)

# (Keep your entire data preprocessing section here unchanged)
set.seed(7896)

time_step_range <- 1:10
validation_results_16s <- data.frame()
test_results_16s <- data.frame()
fold_models_by_time_16s <- list()

for (time_steps in time_step_range) {
  cat(sprintf("\n--- Time Step %d ---\n", time_steps))
  fold_metrics <- list()
  fold_models <- list()
  
  for (val_loc in 1:6) {
    train_locs <- setdiff(1:6, val_loc)
    test_loc <- 7
    
    train_data <- convert_to_numeric(otu_16s_filtered_rel_rf %>% filter(location %in% train_locs))
    val_data <- convert_to_numeric(otu_16s_filtered_rel_rf %>% filter(location == val_loc))
    
    train_sequences <- lapply(unique(train_data$location), function(loc) {
      create_sequences_location(train_data %>% filter(location == loc), time_steps)
    })
    val_sequences <- create_sequences_location(val_data, time_steps)
    
    if (is.null(val_sequences)) next
    
    X_train <- do.call(abind::abind, c(lapply(train_sequences, function(seq) seq$X), list(along = 1)))
    y_train <- unlist(lapply(train_sequences, function(seq) seq$y))
    X_val <- val_sequences$X
    y_val <- val_sequences$y
    
    tensorflow::set_random_seed(7896)
    model <- keras_model_sequential() %>%
      layer_lstm(units = 128, input_shape = c(time_steps, dim(X_train)[3]), return_sequences = TRUE) %>%
      layer_lstm(units = 64, return_sequences = FALSE) %>%
      layer_dropout(rate = 0.4) %>%
      layer_dense(units = 1, activation = 'sigmoid')
    
    model %>% compile(
      loss = 'binary_crossentropy',
      optimizer = optimizer_adam(learning_rate = 0.001),
      metrics = c('accuracy')
    )
    
    model %>% fit(
      x = X_train, y = y_train,
      validation_data = list(X_val, y_val),
      epochs = 50, batch_size = 32,
      class_weight = list('0' = 1, '1' = length(y_train) / sum(y_train == 1)),
      callbacks = list(callback_early_stopping(patience = 10, restore_best_weights = TRUE)),
      verbose = 0
    )
    
    val_probs <- as.numeric(model %>% predict(X_val))
    
    if (length(unique(y_val)) >= 2) {
      pr <- pr.curve(scores.class0 = val_probs, weights.class0 = y_val, curve = FALSE)
      roc <- roc(y_val, val_probs)
      
      fold_metrics[[val_loc]] <- data.frame(
        TimeStep = time_steps,
        Fold = val_loc,
        AUPRC = pr$auc.integral,
        ROC_AUC = as.numeric(auc(roc))
      )
      fold_models[[val_loc]] <- model
    }
  }
  
  fold_df <- do.call(rbind, fold_metrics)
  validation_results_16s <- rbind(validation_results_16s, fold_df)
  
  test_results_16s <- rbind(test_results_16s, data.frame(
    TimeStep = time_steps,
    Avg_Validation_AUPRC = mean(fold_df$AUPRC, na.rm = TRUE),
    Avg_Validation_ROC = mean(fold_df$ROC_AUC, na.rm = TRUE)
  ))
  
  fold_models_by_time_16s[[as.character(time_steps)]] <- fold_models
}

# The rest of the script—evaluating on test set, summarizing per time step, confusion matrix, etc.—can follow identically,
# just be sure to continue replacing `_copernicus` with `_16s`.


avg_summary_16s <- test_results_16s %>%
  filter(!is.na(Avg_Validation_AUPRC)) %>%
  arrange(desc(Avg_Validation_AUPRC))

best_time_step <- avg_summary_16s$TimeStep[1]
cat(sprintf("\n✅ Best Time Step based on average validation AUPRC: %d\n", best_time_step))

best_fold <- validation_results_16s %>%
  filter(TimeStep == best_time_step) %>%
  arrange(desc(AUPRC)) %>%
  slice(1) %>%
  pull(Fold)

cat(sprintf("🏆 Best fold in that time step: %d\n", best_fold))

# Prepare test set
# Prepare test set
test_loc <- 7
test_data <- convert_to_numeric(otu_16s_filtered_rel_rf %>% filter(location == test_loc))

# Evaluate all 6 models from best timestep on the test set
test_results_all_models_16s <- data.frame()
fold_models_best_time_16s <- fold_models_by_time_16s[[as.character(best_time_step)]]

for (fold_num in 1:6) {
  model <- fold_models_best_time_16s[[fold_num]]
  
  if (!is.null(model)) {
    # Regenerate test sequences with the same best_time_step each time
    test_sequences <- create_sequences_location(test_data, best_time_step)
    
    if (!is.null(test_sequences)) {
      probs <- model %>% predict(test_sequences$X)
      
      pr <- pr.curve(scores.class0 = probs, weights.class0 = test_sequences$y, curve = FALSE)
      roc_obj <- roc(test_sequences$y, as.numeric(probs))
      
      test_results_all_models_16s <- rbind(test_results_all_models_16s, data.frame(
        TimeStep = best_time_step,
        Fold = fold_num,
        Test_AUPRC = pr$auc.integral,
        Test_ROC_AUC = as.numeric(auc(roc_obj))
      ))
    }
  }
}

# Average performance across all 6 models
test_summary_16s <- test_results_all_models_16s %>%
  summarise(
    TimeStep = best_time_step,
    Avg_Test_AUPRC = mean(Test_AUPRC, na.rm = TRUE),
    Avg_Test_ROC_AUC = mean(Test_ROC_AUC, na.rm = TRUE)
  )

cat("\n📊 Average test performance from 6 folds at best timestep:\n")
print(test_summary_16s)

# Use best model from best fold for manual inspection
model <- fold_models_best_time_16s[[best_fold]]

# Predict on test set
predicted_probabilities <- model %>% predict(test_sequences$X)
results_manual_16s <- data.frame(
  Sample_ID = test_sequences$ids,
  Date = test_sequences$dates,
  Actual = test_sequences$y,
  Predicted_Probability = predicted_probabilities
)

# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_16s$Actual == 1)
thresholds <- seq(1, 0.01, -0.01)
best_threshold <- 0.01

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_16s$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_16s$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.7) {
    best_threshold <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold selected (≥70%% recall): %.2f\n", best_threshold))

# Step: Convert Date column to Date class and add week_start column
results_manual_16s <- results_manual_16s %>%
  mutate(
    date = as.Date(Date),
    week_start = lubridate::floor_date(date, unit = "week", week_start = 1)
  )

# Apply threshold and create results table
results_manual_16s <- results_manual_16s %>%
  mutate(
    Predicted_Class = ifelse(Predicted_Probability > best_threshold, 1, 0),
    Category = case_when(
      Actual == 1 & Predicted_Class == 1 ~ "True Positive",
      Actual == 0 & Predicted_Class == 0 ~ "True Negative",
      Actual == 0 & Predicted_Class == 1 ~ "False Positive",
      Actual == 1 & Predicted_Class == 0 ~ "False Negative"
    )
  )

# Confusion Matrix
confusion_matrix_16s <- table(
  Predicted = results_manual_16s$Predicted_Class,
  Actual = results_manual_16s$Actual
)

cat("\n📌 Confusion Matrix (from best fold in best timestep):\n")
print(confusion_matrix_16s)

# Step: Weekly summary
weekly_summary_16s <- results_manual_16s %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")

# Create and expand tracking for test performance across all time steps
test_results_all_models_16s <- data.frame()
test_summary_all_timesteps_16s <- data.frame()

for (time_steps in time_step_range) {
  cat(sprintf("\n🔍 Evaluating all models from Time Step %d on Test Set\n", time_steps))
  
  fold_models <- fold_models_by_time_16s[[as.character(time_steps)]]
  test_sequences <- create_sequences_location(test_data, time_steps)  # FIXED: regenerate per timestep
  
  if (is.null(test_sequences)) next  # skip if not enough data
  
  for (fold_num in 1:6) {
    model <- fold_models[[fold_num]]
    
    if (!is.null(model)) {
      probs <- model %>% predict(test_sequences$X)
      pr <- pr.curve(scores.class0 = probs, weights.class0 = test_sequences$y, curve = FALSE)
      roc_obj <- roc(test_sequences$y, as.numeric(probs))
      
      test_results_all_models_16s <- rbind(test_results_all_models_16s, data.frame(
        TimeStep = time_steps,
        Fold = fold_num,
        Test_AUPRC = pr$auc.integral,
        Test_ROC_AUC = as.numeric(auc(roc_obj))
      ))
    }
  }
  
  # Summary stats for this timestep
  timestep_results <- test_results_all_models_16s %>%
    filter(TimeStep == time_steps)
  
  test_summary_all_timesteps_16s <- rbind(test_summary_all_timesteps_16s, data.frame(
    TimeStep = time_steps,
    Avg_Test_AUPRC = mean(timestep_results$Test_AUPRC, na.rm = TRUE),
    Avg_Test_ROC_AUC = mean(timestep_results$Test_ROC_AUC, na.rm = TRUE),
    Best_Test_AUPRC = max(timestep_results$Test_AUPRC, na.rm = TRUE),
    Best_Test_ROC_AUC = max(timestep_results$Test_ROC_AUC, na.rm = TRUE)
  ))
}

# Output summaries
cat("\n✅ Test Results for Every Model:\n")
print(test_results_all_models_16s)

cat("\n📈 Summary of Test Results Across All Timesteps:\n")
print(test_summary_all_timesteps_16s)



############## 18s 


otu_18s_filtered <- read_csv("data_all/otus_18S_filtered_no1.csv")

colnames(otu_18s_filtered)[which(names(otu_18s_filtered) == "...1")] <- "sample"

otu_18s_filtered$sample <- toupper(otu_18s_filtered$sample)

otu_18s_filtered <- as.matrix(otu_18s_filtered)

rownames(otu_18s_filtered) <- otu_18s_filtered[,1]

otu_18s_filtered <- otu_18s_filtered[, !colnames(otu_18s_filtered) %in% c("sample")]

class(otu_18s_filtered) <- "numeric"

otu_18s_filtered <- make_relative(otu_18s_filtered)

otu_18s_filtered <- as.data.frame(otu_18s_filtered)

otu_18s_filtered <- tibble::rownames_to_column(otu_18s_filtered, "sample")

######

v_vul_16s <- read_csv("data_all/v_vul_otu.csv")

v_vul_16s <- v_vul_16s %>% 
  select((!c("...1")))

v_vul_16s <- v_vul_16s[!duplicated(v_vul_16s$sample),]

v_vul_16s$sample <- toupper(v_vul_16s$sample)

# Extract the last two digits from the 'sample' column
v_vul_16s$year <- as.numeric(substr(v_vul_16s$sample, nchar(v_vul_16s$sample) - 1, nchar(v_vul_16s$sample)))

v_vul_16s$week <- as.numeric(substr(v_vul_16s$sample, 5, 6))

v_vul_16s$location <- as.numeric(substr(v_vul_16s$sample, 2, 3))

v_vul_16s$day <- substr(v_vul_16s$sample, 4, 4)


# Assuming your dataframe is called v_vul_16s
# Convert year and week to proper format
v_vul_16s$Year <- as.numeric(paste0("20", v_vul_16s$year))  # Adding century to year
v_vul_16s$Week <- as.numeric(v_vul_16s$week)

# Create a new column for the Monday of the corresponding week
v_vul_16s$Monday_Date <- ISOweek2date(paste0(v_vul_16s$Year, "-W", sprintf("%02d", v_vul_16s$Week), "-1"))

# Adjust the Monday date based on the day column
v_vul_16s$Day_Shift <- dplyr::case_when(
  v_vul_16s$day == "M" ~ 0,  # Monday, no shift
  v_vul_16s$day == "X" ~ 1,  # Tuesday, add 1 day
  v_vul_16s$day == "T" ~ 3   # Thursday, add 3 days
)

# Create final date column by shifting Monday's date
v_vul_16s$date <- v_vul_16s$Monday_Date + v_vul_16s$Day_Shift

# View the updated dataframe with the new date column
head(v_vul_16s)


# Convert day labels to numeric values
v_vul_16s$day[v_vul_16s$day == "M"] <- 1
v_vul_16s$day[v_vul_16s$day == "T"] <- 2
v_vul_16s$day[v_vul_16s$day == "X"] <- 3

split_data <- split(v_vul_16s, v_vul_16s$location)

#################################### beginning of time lag

# Function to order by year and week, then shift the sample column up 
process_group <- function(df) {
  # Order the data frame by year and week
  df <- df[order(df$year, df$week), ]
  
  # Shift the sample column up by two rows
  df$sample_shifted <- c(rep(NA, 0), df$sample[1:(nrow(df) - 0)])
  
  return(df)
}

# Apply the process_group function to each location group
processed_data <- lapply(split_data, process_group)

# Combine the processed data back into a single data frame
v_vul_16s_processed <- do.call(rbind, processed_data)

# Reset the rownames to be sequential
rownames(v_vul_16s_processed) <- NULL

# Display the first few rows of the result
head(v_vul_16s_processed)


v_vul_16s_processed <- v_vul_16s_processed %>% 
  select((!c("sample" )))

colnames(v_vul_16s_processed)[which(names(v_vul_16s_processed) == "sample_shifted")] <- "sample"


v_vul_16s_processed <- na.omit(v_vul_16s_processed)

otu_16s_filtered_rel <- left_join( otu_18s_filtered, v_vul_16s_processed, by = "sample")

# Add a new column 'v_vul_presence' to the 'logistic_reg' data frame
otu_16s_filtered_rel <- otu_16s_filtered_rel %>%
  mutate(v_vul_presence = ifelse(v_vul_total_abundance == 0, 0, 1))

otu_16s_filtered_rel$v_vul_presence <- as.factor(otu_16s_filtered_rel$v_vul_presence)

otu_16s_filtered_rel_rf <- otu_16s_filtered_rel %>% 
  select((!c( "OTU_16S_25709","OTU_16S_25947", "OTU_16S_33238", "v_vul_total_abundance" )))

# Prepare the dataset for LSTM, keeping the 'sample' column
otu_16s_filtered_rel_rf <- otu_16s_filtered_rel %>% 
  select( -OTU_16S_25709, -OTU_16S_25947, -OTU_16S_33238, -v_vul_total_abundance,
          -year, -week, -day, -Year, -Week, -Monday_Date, -Day_Shift)


otu_16s_filtered_rel_rf <- na.omit(otu_16s_filtered_rel_rf)






# Step 2: Split dataset into training and testing
#train_data <- otu_16s_filtered_rel_rf %>% filter(!location %in% c(6, 12))
#test_data  <- otu_16s_filtered_rel_rf %>% filter(location %in% c(6, 12))
otu_16s_filtered_rel_rf <- subset(otu_16s_filtered_rel_rf, location %in%  
                                    c("1", "2", "3", "4", "5", "6", "7"))#,"8", "9", "10", "11", "12", "13", "14", "15"))

set.seed(7896)

convert_to_numeric <- function(data) {
  data[] <- lapply(names(data), function(col) {
    if (col %in% c("sample", "date")) {
      return(data[[col]])
    } else if (is.factor(data[[col]]) || is.character(data[[col]])) {
      return(as.numeric(as.character(data[[col]])))
    } else {
      return(data[[col]])
    }
  })
  return(data)
}

# Step 4: Function to create sequences ensuring sample IDs are included
create_sequences <- function(features, target, time_steps, sample_ids) {
  X <- list()
  y <- list()
  ids <- list()
  
  for (i in seq_len(nrow(features) - time_steps)) {
    sequence_matrix <- as.matrix(features[i:(i + time_steps - 1), ])
    
    # Make sure dimensions are time_steps x n_features (already OK)
    X[[i]] <- sequence_matrix
    y[[i]] <- target[i + time_steps]
    ids[[i]] <- sample_ids[i + time_steps]
  }
  
  if (length(X) > 0) {
    # Convert list of matrices to 3D array: [n_samples, time_steps, n_features]
    n_samples <- length(X)
    time_steps <- nrow(X[[1]])
    n_features <- ncol(X[[1]])
    
    # Stack the matrices correctly
    X_array <- array(NA, dim = c(n_samples, time_steps, n_features))
    for (i in seq_along(X)) {
      X_array[i,,] <- X[[i]]
    }
    
    return(list(X = X_array, y = unlist(y), ids = unlist(ids)))
  } else {
    return(NULL)
  }
}


# Step 5: Adapted generate sequences function for each location that includes sample_ids
create_sequences_location <- function(data, time_steps) {
  features <- data %>% select(-v_vul_presence, -location, -date, -sample)
  target <- data$v_vul_presence
  sample_ids <- data$sample
  dates <- data$date  # Keep track of dates
  
  if (nrow(features) > time_steps) {
    seq_data <- create_sequences(features, target, time_steps, sample_ids)
    seq_data$dates <- dates[(time_steps + 1):length(dates)]  # Align dates with target
    return(seq_data)
  } else {
    return(NULL)
  }
}

set.seed(7896)

time_step_range <- 1:10
validation_results_18s <- data.frame()
test_results_18s <- data.frame()
fold_models_by_time_18s <- list()

for (time_steps in time_step_range) {
  cat(sprintf("\n--- Time Step %d ---\n", time_steps))
  fold_metrics <- list()
  fold_models <- list()
  
  for (val_loc in 1:6) {
    train_locs <- setdiff(1:6, val_loc)
    test_loc <- 7
    
    train_data <- convert_to_numeric(otu_16s_filtered_rel_rf %>% filter(location %in% train_locs))
    val_data <- convert_to_numeric(otu_16s_filtered_rel_rf %>% filter(location == val_loc))
    
    train_sequences <- lapply(unique(train_data$location), function(loc) {
      create_sequences_location(train_data %>% filter(location == loc), time_steps)
    })
    val_sequences <- create_sequences_location(val_data, time_steps)
    
    if (is.null(val_sequences)) next
    
    X_train <- do.call(abind::abind, c(lapply(train_sequences, function(seq) seq$X), list(along = 1)))
    y_train <- unlist(lapply(train_sequences, function(seq) seq$y))
    X_val <- val_sequences$X
    y_val <- val_sequences$y
    
    tensorflow::set_random_seed(7896)
    model <- keras_model_sequential() %>%
      layer_lstm(units = 128, input_shape = c(time_steps, dim(X_train)[3]), return_sequences = TRUE) %>%
      layer_lstm(units = 64, return_sequences = FALSE) %>%
      layer_dropout(rate = 0.4) %>%
      layer_dense(units = 1, activation = 'sigmoid')
    
    model %>% compile(
      loss = 'binary_crossentropy',
      optimizer = optimizer_adam(learning_rate = 0.001),
      metrics = c('accuracy')
    )
    
    model %>% fit(
      x = X_train, y = y_train,
      validation_data = list(X_val, y_val),
      epochs = 50, batch_size = 32,
      class_weight = list('0' = 1, '1' = length(y_train) / sum(y_train == 1)),
      callbacks = list(callback_early_stopping(patience = 10, restore_best_weights = TRUE)),
      verbose = 0
    )
    
    val_probs <- as.numeric(model %>% predict(X_val))
    
    if (length(unique(y_val)) >= 2) {
      pr <- pr.curve(scores.class0 = val_probs, weights.class0 = y_val, curve = FALSE)
      roc <- roc(y_val, val_probs)
      
      fold_metrics[[val_loc]] <- data.frame(
        TimeStep = time_steps,
        Fold = val_loc,
        AUPRC = pr$auc.integral,
        ROC_AUC = as.numeric(auc(roc))
      )
      fold_models[[val_loc]] <- model
    }
  }
  
  fold_df <- do.call(rbind, fold_metrics)
  validation_results_18s <- rbind(validation_results_18s, fold_df)
  
  test_results_18s <- rbind(test_results_18s, data.frame(
    TimeStep = time_steps,
    Avg_Validation_AUPRC = mean(fold_df$AUPRC, na.rm = TRUE),
    Avg_Validation_ROC = mean(fold_df$ROC_AUC, na.rm = TRUE)
  ))
  
  fold_models_by_time_18s[[as.character(time_steps)]] <- fold_models
}

avg_summary_18s <- test_results_18s %>%
  filter(!is.na(Avg_Validation_AUPRC)) %>%
  arrange(desc(Avg_Validation_AUPRC))

best_time_step <- avg_summary_18s$TimeStep[1]
cat(sprintf("\n✅ Best Time Step based on average validation AUPRC: %d\n", best_time_step))

best_fold <- validation_results_18s %>%
  filter(TimeStep == best_time_step) %>%
  arrange(desc(AUPRC)) %>%
  slice(1) %>%
  pull(Fold)

cat(sprintf("🏆 Best fold in that time step: %d\n", best_fold))

# Prepare test set
test_loc <- 7
test_data <- convert_to_numeric(otu_16s_filtered_rel_rf %>% filter(location == test_loc))

test_results_all_models_18s <- data.frame()
fold_models_best_time_18s <- fold_models_by_time_18s[[as.character(best_time_step)]]

for (fold_num in 1:6) {
  model <- fold_models_best_time_18s[[fold_num]]
  
  if (!is.null(model)) {
    test_sequences <- create_sequences_location(test_data, best_time_step)
    
    if (!is.null(test_sequences)) {
      probs <- model %>% predict(test_sequences$X)
      pr <- pr.curve(scores.class0 = probs, weights.class0 = test_sequences$y, curve = FALSE)
      roc_obj <- roc(test_sequences$y, as.numeric(probs))
      
      test_results_all_models_18s <- rbind(test_results_all_models_18s, data.frame(
        TimeStep = best_time_step,
        Fold = fold_num,
        Test_AUPRC = pr$auc.integral,
        Test_ROC_AUC = as.numeric(auc(roc_obj))
      ))
    }
  }
}

test_summary_18s <- test_results_all_models_18s %>%
  summarise(
    TimeStep = best_time_step,
    Avg_Test_AUPRC = mean(Test_AUPRC, na.rm = TRUE),
    Avg_Test_ROC_AUC = mean(Test_ROC_AUC, na.rm = TRUE)
  )

cat("\n📊 Average test performance from 6 folds at best timestep:\n")
print(test_summary_18s)

model <- fold_models_best_time_18s[[best_fold]]
predicted_probabilities <- model %>% predict(test_sequences$X)

results_manual_18s <- data.frame(
  Sample_ID = test_sequences$ids,
  Date = test_sequences$dates,
  Actual = test_sequences$y,
  Predicted_Probability = predicted_probabilities
)

actual_positives <- sum(results_manual_18s$Actual == 1)
thresholds <- seq(1, 0.001, -0.001)
best_threshold <- 0.001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_18s$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_18s$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.7) {
    best_threshold <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold selected (≥70%% recall): %.2f\n", best_threshold))


# Step: Convert Date column to Date class and add week_start column
results_manual_18s <- results_manual_18s %>%
  mutate(
    date = as.Date(Date),
    week_start = lubridate::floor_date(date, unit = "week", week_start = 1)
  )

results_manual_18s <- results_manual_18s %>%
  mutate(
    Predicted_Class = ifelse(Predicted_Probability > best_threshold, 1, 0),
    Category = case_when(
      Actual == 1 & Predicted_Class == 1 ~ "True Positive",
      Actual == 0 & Predicted_Class == 0 ~ "True Negative",
      Actual == 0 & Predicted_Class == 1 ~ "False Positive",
      Actual == 1 & Predicted_Class == 0 ~ "False Negative"
    )
  )

confusion_matrix_18s <- table(
  Predicted = results_manual_18s$Predicted_Class,
  Actual = results_manual_18s$Actual
)

cat("\n📌 Confusion Matrix (from best fold in best timestep):\n")
print(confusion_matrix_18s)

# Step: Weekly summary
weekly_summary_18s <- results_manual_18s %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")

# Evaluate across all timesteps
test_results_all_models_18s <- data.frame()
test_summary_all_timesteps_18s <- data.frame()

for (time_steps in time_step_range) {
  cat(sprintf("\n🔍 Evaluating all models from Time Step %d on Test Set\n", time_steps))
  
  fold_models <- fold_models_by_time_18s[[as.character(time_steps)]]
  test_sequences <- create_sequences_location(test_data, time_steps)
  
  if (is.null(test_sequences)) next
  
  for (fold_num in 1:6) {
    model <- fold_models[[fold_num]]
    
    if (!is.null(model)) {
      probs <- model %>% predict(test_sequences$X)
      pr <- pr.curve(scores.class0 = probs, weights.class0 = test_sequences$y, curve = FALSE)
      roc_obj <- roc(test_sequences$y, as.numeric(probs))
      
      test_results_all_models_18s <- rbind(test_results_all_models_18s, data.frame(
        TimeStep = time_steps,
        Fold = fold_num,
        Test_AUPRC = pr$auc.integral,
        Test_ROC_AUC = as.numeric(auc(roc_obj))
      ))
    }
  }
  
  timestep_results <- test_results_all_models_18s %>%
    filter(TimeStep == time_steps)
  
  test_summary_all_timesteps_18s <- rbind(test_summary_all_timesteps_18s, data.frame(
    TimeStep = time_steps,
    Avg_Test_AUPRC = mean(timestep_results$Test_AUPRC, na.rm = TRUE),
    Avg_Test_ROC_AUC = mean(timestep_results$Test_ROC_AUC, na.rm = TRUE),
    Best_Test_AUPRC = max(timestep_results$Test_AUPRC, na.rm = TRUE),
    Best_Test_ROC_AUC = max(timestep_results$Test_ROC_AUC, na.rm = TRUE)
  ))
}

cat("\n✅ Test Results for Every Model:\n")
print(test_results_all_models_18s)

cat("\n📈 Summary of Test Results Across All Timesteps:\n")
print(test_summary_all_timesteps_18s)


####################### 16s all


set.seed(7896)
# Load data
otu_16s_filtered <- read_csv("data_all/otus_16S_filtered.csv")

colnames(otu_16s_filtered)[which(names(otu_16s_filtered) == "...1")] <- "sample"

otu_16s_filtered$sample <- toupper(otu_16s_filtered$sample)

otu_16s_filtered <- t(otu_16s_filtered)

colnames(otu_16s_filtered) <- otu_16s_filtered[1,]

# Remove the first row
otu_16s_filtered <- otu_16s_filtered[-1, ]

otu_16s_filtered <- as.data.frame(otu_16s_filtered)

otu_16s_filtered <- tibble::rownames_to_column(otu_16s_filtered, "otu")

taxV3_V4 <- read_csv("data_all/taxonomy_16S.csv")  

taxV3_V4 <- taxV3_V4[, c("zotu", "biodomain", "phylum", "class", "bioorder","family", 
                         "genus","species")]


colnames(taxV3_V4)[which(names(taxV3_V4) == "zotu")] <- "otu"

otu_16s_filtered <- left_join(otu_16s_filtered, taxV3_V4, by = "otu")

otu_16s_filtered <- as.data.table(otu_16s_filtered)

otu_16s_filtered <- otu_16s_filtered[!(genus == "Vibrio")]

otu_16s_filtered <- otu_16s_filtered %>% 
  select((!c("biodomain", "phylum", "class", "bioorder","family", 
             "genus","species")))

otu_16s_filtered <- t(otu_16s_filtered)

colnames(otu_16s_filtered) <- otu_16s_filtered[1,]

# Remove the first row
otu_16s_filtered <- otu_16s_filtered[-1, ]

otu_16s_filtered <- as.matrix(otu_16s_filtered)

otu_16s_filtered <- otu_16s_filtered[, !colnames(otu_16s_filtered) %in% c("sample")]

class(otu_16s_filtered) <- "numeric"

otu_16s_filtered_1 <- make_relative(otu_16s_filtered)

otu_16s_filtered_1 <- as.data.frame(otu_16s_filtered_1)

otu_16s_filtered_1 <- tibble::rownames_to_column(otu_16s_filtered_1, "sample")

otu_18s_filtered <- read_csv("data_all/otus_18S_filtered_no1.csv")

colnames(otu_18s_filtered)[which(names(otu_18s_filtered) == "...1")] <- "sample"

otu_18s_filtered$sample <- toupper(otu_18s_filtered$sample)

otu_18s_filtered <- as.matrix(otu_18s_filtered)

rownames(otu_18s_filtered) <- otu_18s_filtered[,1]

otu_18s_filtered <- otu_18s_filtered[, !colnames(otu_18s_filtered) %in% c("sample")]

class(otu_18s_filtered) <- "numeric"

otu_18s_filtered <- make_relative(otu_18s_filtered)

otu_18s_filtered <- as.data.frame(otu_18s_filtered)

otu_18s_filtered <- tibble::rownames_to_column(otu_18s_filtered, "sample")

######

v_vul_16s <- read_csv("data_all/v_vul_otu.csv")

v_vul_16s <- v_vul_16s %>% 
  select((!c("...1")))

v_vul_16s <- v_vul_16s[!duplicated(v_vul_16s$sample),]

v_vul_16s$sample <- toupper(v_vul_16s$sample)

# Extract the last two digits from the 'sample' column
v_vul_16s$year <- as.numeric(substr(v_vul_16s$sample, nchar(v_vul_16s$sample) - 1, nchar(v_vul_16s$sample)))

v_vul_16s$week <- as.numeric(substr(v_vul_16s$sample, 5, 6))

v_vul_16s$location <- as.numeric(substr(v_vul_16s$sample, 2, 3))

v_vul_16s$day <- substr(v_vul_16s$sample, 4, 4)


# Assuming your dataframe is called v_vul_16s
# Convert year and week to proper format
v_vul_16s$Year <- as.numeric(paste0("20", v_vul_16s$year))  # Adding century to year
v_vul_16s$Week <- as.numeric(v_vul_16s$week)

# Create a new column for the Monday of the corresponding week
v_vul_16s$Monday_Date <- ISOweek2date(paste0(v_vul_16s$Year, "-W", sprintf("%02d", v_vul_16s$Week), "-1"))

# Adjust the Monday date based on the day column
v_vul_16s$Day_Shift <- dplyr::case_when(
  v_vul_16s$day == "M" ~ 0,  # Monday, no shift
  v_vul_16s$day == "X" ~ 1,  # Tuesday, add 1 day
  v_vul_16s$day == "T" ~ 3   # Thursday, add 3 days
)

# Create final date column by shifting Monday's date
v_vul_16s$date <- v_vul_16s$Monday_Date + v_vul_16s$Day_Shift

# View the updated dataframe with the new date column
head(v_vul_16s)


# Convert day labels to numeric values
v_vul_16s$day[v_vul_16s$day == "M"] <- 1
v_vul_16s$day[v_vul_16s$day == "T"] <- 2
v_vul_16s$day[v_vul_16s$day == "X"] <- 3

split_data <- split(v_vul_16s, v_vul_16s$location)

#################################### beginning of time lag

# Function to order by year and week, then shift the sample column up 
process_group <- function(df) {
  # Order the data frame by year and week
  df <- df[order(df$year, df$week), ]
  
  # Shift the sample column up by two rows
  df$sample_shifted <- c(rep(NA, 0), df$sample[1:(nrow(df) - 0)])
  
  return(df)
}

# Apply the process_group function to each location group
processed_data <- lapply(split_data, process_group)

# Combine the processed data back into a single data frame
v_vul_16s_processed <- do.call(rbind, processed_data)

# Reset the rownames to be sequential
rownames(v_vul_16s_processed) <- NULL

# Display the first few rows of the result
head(v_vul_16s_processed)


v_vul_16s_processed <- v_vul_16s_processed %>% 
  select((!c("sample" )))

colnames(v_vul_16s_processed)[which(names(v_vul_16s_processed) == "sample_shifted")] <- "sample"


v_vul_16s_processed <- na.omit(v_vul_16s_processed)

#otu_16s_filtered_rel <- left_join( otu_18s_filtered, v_vul_16s_processed, by = "sample")

otu_16s_filtered_rel <- left_join(  otu_16s_filtered_1,v_vul_16s_processed, by = "sample")


# Add a new column 'v_vul_presence' to the 'logistic_reg' data frame
otu_16s_filtered_rel <- otu_16s_filtered_rel %>%
  mutate(v_vul_presence = ifelse(v_vul_total_abundance == 0, 0, 1))

otu_16s_filtered_rel$v_vul_presence <- as.factor(otu_16s_filtered_rel$v_vul_presence)

otu_16s_filtered_rel_rf <- otu_16s_filtered_rel %>% 
  select((!c( "OTU_16S_25709","OTU_16S_25947", "OTU_16S_33238", "v_vul_total_abundance" )))

otu_16s_filtered_rel_rf <- na.omit(otu_16s_filtered_rel_rf)

otu_16s_filtered_rel_rf$day_of_year <- yday(otu_16s_filtered_rel_rf$date)
otu_16s_filtered_rel_rf$week_of_year <- week(otu_16s_filtered_rel_rf$date)

# Normalize day_of_year
#otu_16s_filtered_rel_rf$day_of_year_norm <- otu_16s_filtered_rel_rf$day_of_year / 365

# Normalize week_of_year
#otu_16s_filtered_rel_rf$week_of_year_norm <- otu_16s_filtered_rel_rf$week_of_year / 52

# Prepare the dataset for LSTM, keeping the 'sample' column
otu_16s_filtered_rel_rf <- otu_16s_filtered_rel %>% 
  select( -OTU_16S_25709, -OTU_16S_25947, -OTU_16S_33238, -v_vul_total_abundance,
          -year, -week, -day, -Year, -Week, -Monday_Date, -Day_Shift)
otu_16s_filtered_rel_rf <- na.omit(otu_16s_filtered_rel_rf)



# otu_16s_filtered_rel_rf <- otu_16s_filtered_rel_rf %>%
#   group_by(location) %>%
#   mutate(v_vul_presence = sample(v_vul_presence)) %>%
#   ungroup()




#ltsm_temp <- 
#  join_all_current_discharge_hplc_temp_weather_full_year[, c("sample", "temperature.y", "v_vul_total_abundance")]


#otu_16s_filtered_rel_rf <- left_join(otu_16s_filtered_rel_rf,
#                       ltsm_temp, by = "sample")


#otu_16s_filtered_rel_rf <- left_join(otu_16s_filtered_rel_rf,
#                                     otu_18s_filtered, by = "sample")




# Step 2: Split dataset into training and testing
#train_data <- otu_16s_filtered_rel_rf %>% filter(!location %in% c(6, 12))
#test_data  <- otu_16s_filtered_rel_rf %>% filter(location %in% c(6, 12))
otu_16s_filtered_rel_rf <- subset(otu_16s_filtered_rel_rf, location %in%  
                                    c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15"))

convert_to_numeric <- function(data) {
  data[] <- lapply(names(data), function(col) {
    if (col %in% c("sample", "date")) {
      return(data[[col]])
    } else if (is.factor(data[[col]]) || is.character(data[[col]])) {
      return(as.numeric(as.character(data[[col]])))
    } else {
      return(data[[col]])
    }
  })
  return(data)
}

# Step 4: Function to create sequences ensuring sample IDs are included
create_sequences <- function(features, target, time_steps, sample_ids) {
  X <- list()
  y <- list()
  ids <- list()
  
  for (i in seq_len(nrow(features) - time_steps)) {
    sequence_matrix <- as.matrix(features[i:(i + time_steps - 1), ])
    
    # Make sure dimensions are time_steps x n_features (already OK)
    X[[i]] <- sequence_matrix
    y[[i]] <- target[i + time_steps]
    ids[[i]] <- sample_ids[i + time_steps]
  }
  
  if (length(X) > 0) {
    # Convert list of matrices to 3D array: [n_samples, time_steps, n_features]
    n_samples <- length(X)
    time_steps <- nrow(X[[1]])
    n_features <- ncol(X[[1]])
    
    # Stack the matrices correctly
    X_array <- array(NA, dim = c(n_samples, time_steps, n_features))
    for (i in seq_along(X)) {
      X_array[i,,] <- X[[i]]
    }
    
    return(list(X = X_array, y = unlist(y), ids = unlist(ids)))
  } else {
    return(NULL)
  }
}


# Step 5: Adapted generate sequences function for each location that includes sample_ids
create_sequences_location <- function(data, time_steps) {
  features <- data %>% select(-v_vul_presence, -location, -date, -sample)
  target <- data$v_vul_presence
  sample_ids <- data$sample
  dates <- data$date  # Keep track of dates
  
  if (nrow(features) > time_steps) {
    seq_data <- create_sequences(features, target, time_steps, sample_ids)
    seq_data$dates <- dates[(time_steps + 1):length(dates)]  # Align dates with target
    return(seq_data)
  } else {
    return(NULL)
  }
}

set.seed(7896)


time_step_range <- 1:10
validation_results_16s_all <- data.frame()
test_results_16s_all <- data.frame()
fold_models_by_time_16s_all <- list()

# Validation fold pairs
val_pairs <- list(c(1, 10), c(2, 14), c(3, 15), c(4, 12), c(5, 9), c(6, 8))
test_locs <- c(7, 11, 13)

for (time_steps in time_step_range) {
  cat(sprintf("\n--- Time Step %d ---\n", time_steps))
  fold_metrics <- list()
  fold_models <- list()
  
  for (fold_index in seq_along(val_pairs)) {
    val_locs <- val_pairs[[fold_index]]
    train_locs <- setdiff(1:15, c(val_locs, test_locs))
    
    train_data <- convert_to_numeric(otu_16s_filtered_rel_rf %>% filter(location %in% train_locs))
    val_data <- convert_to_numeric(otu_16s_filtered_rel_rf %>% filter(location %in% val_locs))
    
    train_sequences <- lapply(unique(train_data$location), function(loc) {
      create_sequences_location(train_data %>% filter(location == loc), time_steps)
    })
    val_sequences <- create_sequences_location(val_data, time_steps)
    
    if (is.null(val_sequences)) next
    
    X_train <- do.call(abind::abind, c(lapply(train_sequences, function(seq) seq$X), list(along = 1)))
    y_train <- unlist(lapply(train_sequences, function(seq) seq$y))
    X_val <- val_sequences$X
    y_val <- val_sequences$y
    
    tensorflow::set_random_seed(7896)
    model <- keras_model_sequential() %>%
      layer_lstm(units = 128, input_shape = c(time_steps, dim(X_train)[3]), return_sequences = TRUE) %>%
      layer_lstm(units = 64, return_sequences = FALSE) %>%
      layer_dropout(rate = 0.4) %>%
      layer_dense(units = 1, activation = 'sigmoid')
    
    model %>% compile(
      loss = 'binary_crossentropy',
      optimizer = optimizer_adam(learning_rate = 0.001),
      metrics = c('accuracy')
    )
    
    model %>% fit(
      x = X_train, y = y_train,
      validation_data = list(X_val, y_val),
      epochs = 50, batch_size = 32,
      class_weight = list('0' = 1, '1' = length(y_train) / sum(y_train == 1)),
      callbacks = list(callback_early_stopping(patience = 10, restore_best_weights = TRUE)),
      verbose = 0
    )
    
    val_probs <- as.numeric(model %>% predict(X_val))
    
    if (length(unique(y_val)) >= 2) {
      pr <- pr.curve(scores.class0 = val_probs, weights.class0 = y_val, curve = FALSE)
      roc <- roc(y_val, val_probs)
      
      fold_metrics[[fold_index]] <- data.frame(
        TimeStep = time_steps,
        Fold = fold_index,
        AUPRC = pr$auc.integral,
        ROC_AUC = as.numeric(auc(roc))
      )
      fold_models[[fold_index]] <- model
    }
  }
  
  fold_df <- do.call(rbind, fold_metrics)
  validation_results_16s_all <- rbind(validation_results_16s_all, fold_df)
  
  test_results_16s_all <- rbind(test_results_16s_all, data.frame(
    TimeStep = time_steps,
    Avg_Validation_AUPRC = mean(fold_df$AUPRC, na.rm = TRUE),
    Avg_Validation_ROC = mean(fold_df$ROC_AUC, na.rm = TRUE)
  ))
  
  fold_models_by_time_16s_all[[as.character(time_steps)]] <- fold_models
}

avg_summary_16s_all <- test_results_16s_all %>%
  filter(!is.na(Avg_Validation_AUPRC)) %>%
  arrange(desc(Avg_Validation_AUPRC))

best_time_step <- avg_summary_16s_all$TimeStep[1]
cat(sprintf("\n✅ Best Time Step based on average validation AUPRC: %d\n", best_time_step))

best_fold <- validation_results_16s_all %>%
  filter(TimeStep == best_time_step) %>%
  arrange(desc(AUPRC)) %>%
  slice(1) %>%
  pull(Fold)

cat(sprintf("🏆 Best fold in that time step: %d\n", best_fold))

# ---- Evaluate on test set (locations 7,11,13) ----
test_data_16s_all <- convert_to_numeric(otu_16s_filtered_rel_rf %>% filter(location %in% test_locs))
test_sequences_16s_all <- create_sequences_location(test_data_16s_all, best_time_step)

test_results_all_models_16s_all <- data.frame()
fold_models_best_time_16s_all <- fold_models_by_time_16s_all[[as.character(best_time_step)]]

for (fold_num in 1:6) {
  model <- fold_models_best_time_16s_all[[fold_num]]
  
  if (!is.null(model) && !is.null(test_sequences_16s_all)) {
    probs <- model %>% predict(test_sequences_16s_all$X)
    pr <- pr.curve(scores.class0 = probs, weights.class0 = test_sequences_16s_all$y, curve = FALSE)
    roc_obj <- roc(test_sequences_16s_all$y, as.numeric(probs))
    
    test_results_all_models_16s_all <- rbind(test_results_all_models_16s_all, data.frame(
      TimeStep = best_time_step,
      Fold = fold_num,
      Test_AUPRC = pr$auc.integral,
      Test_ROC_AUC = as.numeric(auc(roc_obj))
    ))
  }
}

# ---- Manual threshold and confusion matrix ----
model <- fold_models_best_time_16s_all[[best_fold]]
predicted_probabilities <- model %>% predict(test_sequences_16s_all$X)

results_manual_16s_all <- data.frame(
  Sample_ID = test_sequences_16s_all$ids,
  Date = test_sequences_16s_all$dates,
  Actual = test_sequences_16s_all$y,
  Predicted_Probability = predicted_probabilities
)

actual_positives <- sum(results_manual_16s_all$Actual == 1)
thresholds <- seq(1, 0.01, -0.01)
best_threshold <- 0.01

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_16s_all$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_16s_all$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.7) {
    best_threshold <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold selected (≥70%% recall): %.2f\n", best_threshold))

# Step: Convert Date column to Date class and add week_start column
results_manual_16s_all <- results_manual_16s_all %>%
  mutate(
    date = as.Date(Date),
    week_start = lubridate::floor_date(date, unit = "week", week_start = 1)
  )

results_manual_16s_all <- results_manual_16s_all %>%
  mutate(
    Predicted_Class = ifelse(Predicted_Probability > best_threshold, 1, 0),
    Category = case_when(
      Actual == 1 & Predicted_Class == 1 ~ "True Positive",
      Actual == 0 & Predicted_Class == 0 ~ "True Negative",
      Actual == 0 & Predicted_Class == 1 ~ "False Positive",
      Actual == 1 & Predicted_Class == 0 ~ "False Negative"
    )
  )

confusion_matrix_16s_all <- table(
  Predicted = results_manual_16s_all$Predicted_Class,
  Actual = results_manual_16s_all$Actual
)

cat("\n📌 Confusion Matrix (from best fold in best timestep):\n")
print(confusion_matrix_16s_all)

# Step: Weekly summary
weekly_summary_16s_all <- results_manual_16s_all %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")


# ---- Evaluate on test set across all time steps ----
test_results_all_models_16s_all <- data.frame()
test_summary_all_timesteps_16s_all <- data.frame()

for (time_steps in time_step_range) {
  cat(sprintf("\n🔍 Evaluating all models from Time Step %d on Test Set\n", time_steps))
  
  fold_models <- fold_models_by_time_16s_all[[as.character(time_steps)]]
  test_sequences <- create_sequences_location(test_data_16s_all, time_steps)
  
  if (is.null(test_sequences)) next
  
  for (fold_num in 1:6) {
    model <- fold_models[[fold_num]]
    
    if (!is.null(model)) {
      probs <- model %>% predict(test_sequences$X)
      pr <- pr.curve(scores.class0 = probs, weights.class0 = test_sequences$y, curve = FALSE)
      roc_obj <- roc(test_sequences$y, as.numeric(probs))
      
      test_results_all_models_16s_all <- rbind(test_results_all_models_16s_all, data.frame(
        TimeStep = time_steps,
        Fold = fold_num,
        Test_AUPRC = pr$auc.integral,
        Test_ROC_AUC = as.numeric(auc(roc_obj))
      ))
    }
  }
  
  timestep_results <- test_results_all_models_16s_all %>%
    filter(TimeStep == time_steps)
  
  test_summary_all_timesteps_16s_all <- rbind(test_summary_all_timesteps_16s_all, data.frame(
    TimeStep = time_steps,
    Avg_Test_AUPRC = mean(timestep_results$Test_AUPRC, na.rm = TRUE),
    Avg_Test_ROC_AUC = mean(timestep_results$Test_ROC_AUC, na.rm = TRUE),
    Best_Test_AUPRC = max(timestep_results$Test_AUPRC, na.rm = TRUE),
    Best_Test_ROC_AUC = max(timestep_results$Test_ROC_AUC, na.rm = TRUE)
  ))
}

cat("\n✅ Test Results for Every Model:\n")
print(test_results_all_models_16s_all)

cat("\n📈 Summary of Test Results Across All Timesteps:\n")
print(test_summary_all_timesteps_16s_all)



############## 18s all

otu_18s_filtered <- read_csv("data_all/otus_18S_filtered_no1.csv")

colnames(otu_18s_filtered)[which(names(otu_18s_filtered) == "...1")] <- "sample"

otu_18s_filtered$sample <- toupper(otu_18s_filtered$sample)

otu_18s_filtered <- as.matrix(otu_18s_filtered)

rownames(otu_18s_filtered) <- otu_18s_filtered[,1]

otu_18s_filtered <- otu_18s_filtered[, !colnames(otu_18s_filtered) %in% c("sample")]

class(otu_18s_filtered) <- "numeric"

otu_18s_filtered <- make_relative(otu_18s_filtered)

otu_18s_filtered <- as.data.frame(otu_18s_filtered)

otu_18s_filtered <- tibble::rownames_to_column(otu_18s_filtered, "sample")

######

v_vul_16s <- read_csv("data_all/v_vul_otu.csv")

v_vul_16s <- v_vul_16s %>% 
  select((!c("...1")))

v_vul_16s <- v_vul_16s[!duplicated(v_vul_16s$sample),]

v_vul_16s$sample <- toupper(v_vul_16s$sample)

# Extract the last two digits from the 'sample' column
v_vul_16s$year <- as.numeric(substr(v_vul_16s$sample, nchar(v_vul_16s$sample) - 1, nchar(v_vul_16s$sample)))

v_vul_16s$week <- as.numeric(substr(v_vul_16s$sample, 5, 6))

v_vul_16s$location <- as.numeric(substr(v_vul_16s$sample, 2, 3))

v_vul_16s$day <- substr(v_vul_16s$sample, 4, 4)


# Assuming your dataframe is called v_vul_16s
# Convert year and week to proper format
v_vul_16s$Year <- as.numeric(paste0("20", v_vul_16s$year))  # Adding century to year
v_vul_16s$Week <- as.numeric(v_vul_16s$week)

# Create a new column for the Monday of the corresponding week
v_vul_16s$Monday_Date <- ISOweek2date(paste0(v_vul_16s$Year, "-W", sprintf("%02d", v_vul_16s$Week), "-1"))

# Adjust the Monday date based on the day column
v_vul_16s$Day_Shift <- dplyr::case_when(
  v_vul_16s$day == "M" ~ 0,  # Monday, no shift
  v_vul_16s$day == "X" ~ 1,  # Tuesday, add 1 day
  v_vul_16s$day == "T" ~ 3   # Thursday, add 3 days
)

# Create final date column by shifting Monday's date
v_vul_16s$date <- v_vul_16s$Monday_Date + v_vul_16s$Day_Shift

# View the updated dataframe with the new date column
head(v_vul_16s)


# Convert day labels to numeric values
v_vul_16s$day[v_vul_16s$day == "M"] <- 1
v_vul_16s$day[v_vul_16s$day == "T"] <- 2
v_vul_16s$day[v_vul_16s$day == "X"] <- 3

split_data <- split(v_vul_16s, v_vul_16s$location)

#################################### beginning of time lag

# Function to order by year and week, then shift the sample column up 
process_group <- function(df) {
  # Order the data frame by year and week
  df <- df[order(df$year, df$week), ]
  
  # Shift the sample column up by two rows
  df$sample_shifted <- c(rep(NA, 0), df$sample[1:(nrow(df) - 0)])
  
  return(df)
}

# Apply the process_group function to each location group
processed_data <- lapply(split_data, process_group)

# Combine the processed data back into a single data frame
v_vul_16s_processed <- do.call(rbind, processed_data)

# Reset the rownames to be sequential
rownames(v_vul_16s_processed) <- NULL

# Display the first few rows of the result
head(v_vul_16s_processed)


v_vul_16s_processed <- v_vul_16s_processed %>% 
  select((!c("sample" )))

colnames(v_vul_16s_processed)[which(names(v_vul_16s_processed) == "sample_shifted")] <- "sample"


v_vul_16s_processed <- na.omit(v_vul_16s_processed)

otu_16s_filtered_rel <- left_join( otu_18s_filtered, v_vul_16s_processed, by = "sample")

# Add a new column 'v_vul_presence' to the 'logistic_reg' data frame
otu_16s_filtered_rel <- otu_16s_filtered_rel %>%
  mutate(v_vul_presence = ifelse(v_vul_total_abundance == 0, 0, 1))

otu_16s_filtered_rel$v_vul_presence <- as.factor(otu_16s_filtered_rel$v_vul_presence)

otu_16s_filtered_rel_rf <- otu_16s_filtered_rel %>% 
  select((!c( "OTU_16S_25709","OTU_16S_25947", "OTU_16S_33238", "v_vul_total_abundance" )))

# Prepare the dataset for LSTM, keeping the 'sample' column
otu_16s_filtered_rel_rf <- otu_16s_filtered_rel %>% 
  select( -OTU_16S_25709, -OTU_16S_25947, -OTU_16S_33238, -v_vul_total_abundance,
          -year, -week, -day, -Year, -Week, -Monday_Date, -Day_Shift)


otu_16s_filtered_rel_rf <- na.omit(otu_16s_filtered_rel_rf)

# Step 2: Split dataset into training and testing
#train_data <- otu_16s_filtered_rel_rf %>% filter(!location %in% c(6, 12))
#test_data  <- otu_16s_filtered_rel_rf %>% filter(location %in% c(6, 12))
otu_16s_filtered_rel_rf <- subset(otu_16s_filtered_rel_rf, location %in%  
                                    c("1", "2", "3", "4", "5", "6", "7","8", "9", "10", "11", "12", "13", "14", "15"))

set.seed(7896)

convert_to_numeric <- function(data) {
  data[] <- lapply(names(data), function(col) {
    if (col %in% c("sample", "date")) {
      return(data[[col]])
    } else if (is.factor(data[[col]]) || is.character(data[[col]])) {
      return(as.numeric(as.character(data[[col]])))
    } else {
      return(data[[col]])
    }
  })
  return(data)
}

# Step 4: Function to create sequences ensuring sample IDs are included
create_sequences <- function(features, target, time_steps, sample_ids) {
  X <- list()
  y <- list()
  ids <- list()
  
  for (i in seq_len(nrow(features) - time_steps)) {
    sequence_matrix <- as.matrix(features[i:(i + time_steps - 1), ])
    
    # Make sure dimensions are time_steps x n_features (already OK)
    X[[i]] <- sequence_matrix
    y[[i]] <- target[i + time_steps]
    ids[[i]] <- sample_ids[i + time_steps]
  }
  
  if (length(X) > 0) {
    # Convert list of matrices to 3D array: [n_samples, time_steps, n_features]
    n_samples <- length(X)
    time_steps <- nrow(X[[1]])
    n_features <- ncol(X[[1]])
    
    # Stack the matrices correctly
    X_array <- array(NA, dim = c(n_samples, time_steps, n_features))
    for (i in seq_along(X)) {
      X_array[i,,] <- X[[i]]
    }
    
    return(list(X = X_array, y = unlist(y), ids = unlist(ids)))
  } else {
    return(NULL)
  }
}


# Step 5: Adapted generate sequences function for each location that includes sample_ids
create_sequences_location <- function(data, time_steps) {
  features <- data %>% select(-v_vul_presence, -location, -date, -sample)
  target <- data$v_vul_presence
  sample_ids <- data$sample
  dates <- data$date  # Keep track of dates
  
  if (nrow(features) > time_steps) {
    seq_data <- create_sequences(features, target, time_steps, sample_ids)
    seq_data$dates <- dates[(time_steps + 1):length(dates)]  # Align dates with target
    return(seq_data)
  } else {
    return(NULL)
  }
}

set.seed(7896)


time_step_range <- 1:10
validation_results_18s_all <- data.frame()
test_results_18s_all <- data.frame()
fold_models_by_time_18s_all <- list()

# Validation fold pairs
val_pairs <- list(c(1, 10), c(2, 14), c(3, 15), c(4, 12), c(5, 9), c(6, 8))
test_locs <- c(7, 11, 13)

for (time_steps in time_step_range) {
  cat(sprintf("\n--- Time Step %d ---\n", time_steps))
  fold_metrics <- list()
  fold_models <- list()
  
  for (fold_index in seq_along(val_pairs)) {
    val_locs <- val_pairs[[fold_index]]
    train_locs <- setdiff(1:15, c(val_locs, test_locs))
    
    train_data <- convert_to_numeric(otu_16s_filtered_rel_rf %>% filter(location %in% train_locs))
    val_data <- convert_to_numeric(otu_16s_filtered_rel_rf %>% filter(location %in% val_locs))
    
    train_sequences <- lapply(unique(train_data$location), function(loc) {
      create_sequences_location(train_data %>% filter(location == loc), time_steps)
    })
    val_sequences <- create_sequences_location(val_data, time_steps)
    
    if (is.null(val_sequences)) next
    
    X_train <- do.call(abind::abind, c(lapply(train_sequences, function(seq) seq$X), list(along = 1)))
    y_train <- unlist(lapply(train_sequences, function(seq) seq$y))
    X_val <- val_sequences$X
    y_val <- val_sequences$y
    
    tensorflow::set_random_seed(7896)
    model <- keras_model_sequential() %>%
      layer_lstm(units = 128, input_shape = c(time_steps, dim(X_train)[3]), return_sequences = TRUE) %>%
      layer_lstm(units = 64, return_sequences = FALSE) %>%
      layer_dropout(rate = 0.4) %>%
      layer_dense(units = 1, activation = 'sigmoid')
    
    model %>% compile(
      loss = 'binary_crossentropy',
      optimizer = optimizer_adam(learning_rate = 0.001),
      metrics = c('accuracy')
    )
    
    model %>% fit(
      x = X_train, y = y_train,
      validation_data = list(X_val, y_val),
      epochs = 50, batch_size = 32,
      class_weight = list('0' = 1, '1' = length(y_train) / sum(y_train == 1)),
      callbacks = list(callback_early_stopping(patience = 10, restore_best_weights = TRUE)),
      verbose = 0
    )
    
    val_probs <- as.numeric(model %>% predict(X_val))
    
    if (length(unique(y_val)) >= 2) {
      pr <- pr.curve(scores.class0 = val_probs, weights.class0 = y_val, curve = FALSE)
      roc <- roc(y_val, val_probs)
      
      fold_metrics[[fold_index]] <- data.frame(
        TimeStep = time_steps,
        Fold = fold_index,
        AUPRC = pr$auc.integral,
        ROC_AUC = as.numeric(auc(roc))
      )
      fold_models[[fold_index]] <- model
    }
  }
  
  fold_df <- do.call(rbind, fold_metrics)
  validation_results_18s_all <- rbind(validation_results_18s_all, fold_df)
  
  test_results_18s_all <- rbind(test_results_18s_all, data.frame(
    TimeStep = time_steps,
    Avg_Validation_AUPRC = mean(fold_df$AUPRC, na.rm = TRUE),
    Avg_Validation_ROC = mean(fold_df$ROC_AUC, na.rm = TRUE)
  ))
  
  fold_models_by_time_18s_all[[as.character(time_steps)]] <- fold_models
}

avg_summary_18s_all <- test_results_18s_all %>%
  filter(!is.na(Avg_Validation_AUPRC)) %>%
  arrange(desc(Avg_Validation_AUPRC))

best_time_step <- avg_summary_18s_all$TimeStep[1]
cat(sprintf("\n✅ Best Time Step based on average validation AUPRC: %d\n", best_time_step))

best_fold <- validation_results_18s_all %>%
  filter(TimeStep == best_time_step) %>%
  arrange(desc(AUPRC)) %>%
  slice(1) %>%
  pull(Fold)

cat(sprintf("🏆 Best fold in that time step: %d\n", best_fold))

# ---- Evaluate on test set (locations 7,11,13) ----
test_data_18s_all <- convert_to_numeric(otu_16s_filtered_rel_rf %>% filter(location %in% test_locs))
test_sequences_18s_all <- create_sequences_location(test_data_18s_all, best_time_step)

test_results_all_models_18s_all <- data.frame()
fold_models_best_time_18s_all <- fold_models_by_time_18s_all[[as.character(best_time_step)]]

for (fold_num in 1:6) {
  model <- fold_models_best_time_18s_all[[fold_num]]
  
  if (!is.null(model) && !is.null(test_sequences_18s_all)) {
    probs <- model %>% predict(test_sequences_18s_all$X)
    pr <- pr.curve(scores.class0 = probs, weights.class0 = test_sequences_18s_all$y, curve = FALSE)
    roc_obj <- roc(test_sequences_18s_all$y, as.numeric(probs))
    
    test_results_all_models_18s_all <- rbind(test_results_all_models_18s_all, data.frame(
      TimeStep = best_time_step,
      Fold = fold_num,
      Test_AUPRC = pr$auc.integral,
      Test_ROC_AUC = as.numeric(auc(roc_obj))
    ))
  }
}

# ---- Manual threshold and confusion matrix ----
model <- fold_models_best_time_18s_all[[best_fold]]
predicted_probabilities <- model %>% predict(test_sequences_18s_all$X)

results_manual_18s_all <- data.frame(
  Sample_ID = test_sequences_18s_all$ids,
  Date = test_sequences_18s_all$dates,
  Actual = test_sequences_18s_all$y,
  Predicted_Probability = predicted_probabilities
)

actual_positives <- sum(results_manual_18s_all$Actual == 1)
thresholds <- seq(1, 0.01, -0.01)
best_threshold <- 0.01

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_18s_all$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_18s_all$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.7) {
    best_threshold <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold selected (≥70%% recall): %.2f\n", best_threshold))

# Step: Convert Date column to Date class and add week_start column
results_manual_18s_all <- results_manual_18s_all %>%
  mutate(
    date = as.Date(Date),
    week_start = lubridate::floor_date(date, unit = "week", week_start = 1)
  )

results_manual_18s_all <- results_manual_18s_all %>%
  mutate(
    Predicted_Class = ifelse(Predicted_Probability > best_threshold, 1, 0),
    Category = case_when(
      Actual == 1 & Predicted_Class == 1 ~ "True Positive",
      Actual == 0 & Predicted_Class == 0 ~ "True Negative",
      Actual == 0 & Predicted_Class == 1 ~ "False Positive",
      Actual == 1 & Predicted_Class == 0 ~ "False Negative"
    )
  )

confusion_matrix_18s_all <- table(
  Predicted = results_manual_18s_all$Predicted_Class,
  Actual = results_manual_18s_all$Actual
)

cat("\n📌 Confusion Matrix (from best fold in best timestep):\n")
print(confusion_matrix_18s_all)

# Step: Weekly summary
weekly_summary_18s_all <- results_manual_18s_all %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")

# ---- Evaluate on test set across all time steps ----
test_results_all_models_18s_all <- data.frame()
test_summary_all_timesteps_18s_all <- data.frame()

for (time_steps in time_step_range) {
  cat(sprintf("\n🔍 Evaluating all models from Time Step %d on Test Set\n", time_steps))
  
  fold_models <- fold_models_by_time_18s_all[[as.character(time_steps)]]
  test_sequences <- create_sequences_location(test_data_18s_all, time_steps)
  
  if (is.null(test_sequences)) next
  
  for (fold_num in 1:6) {
    model <- fold_models[[fold_num]]
    
    if (!is.null(model)) {
      probs <- model %>% predict(test_sequences$X)
      pr <- pr.curve(scores.class0 = probs, weights.class0 = test_sequences$y, curve = FALSE)
      roc_obj <- roc(test_sequences$y, as.numeric(probs))
      
      test_results_all_models_18s_all <- rbind(test_results_all_models_18s_all, data.frame(
        TimeStep = time_steps,
        Fold = fold_num,
        Test_AUPRC = pr$auc.integral,
        Test_ROC_AUC = as.numeric(auc(roc_obj))
      ))
    }
  }
  
  timestep_results <- test_results_all_models_18s_all %>%
    filter(TimeStep == time_steps)
  
  test_summary_all_timesteps_18s_all <- rbind(test_summary_all_timesteps_18s_all, data.frame(
    TimeStep = time_steps,
    Avg_Test_AUPRC = mean(timestep_results$Test_AUPRC, na.rm = TRUE),
    Avg_Test_ROC_AUC = mean(timestep_results$Test_ROC_AUC, na.rm = TRUE),
    Best_Test_AUPRC = max(timestep_results$Test_AUPRC, na.rm = TRUE),
    Best_Test_ROC_AUC = max(timestep_results$Test_ROC_AUC, na.rm = TRUE)
  ))
}

cat("\n✅ Test Results for Every Model:\n")
print(test_results_all_models_18s_all)

cat("\n📈 Summary of Test Results Across All Timesteps:\n")
print(test_summary_all_timesteps_18s_all)


tables <- list(
  copernicus = validation_results_copernicus,
  `16s` = validation_results_16s,
  `16s_all` = validation_results_16s_all,
  `18s` = validation_results_18s,
  `18s_all` = validation_results_18s_all
)

# Process each table: average AUPRC and ROC_AUC by TimeStep
avg_results <- lapply(names(tables), function(name) {
  tables[[name]] %>%
    group_by(TimeStep) %>%
    dplyr::summarise(
      AUPRC = mean(AUPRC, na.rm = TRUE),
      ROC_AUC = mean(ROC_AUC, na.rm = TRUE)
    ) %>%
    mutate(Source = name)
})

# Combine all into a single dataframe
final_avg_results <- bind_rows(avg_results) %>%
  select(Source, TimeStep, AUPRC, ROC_AUC)

# Save to CSV
write.csv(final_avg_results, "lstm_validation_avg.csv", row.names = FALSE)
