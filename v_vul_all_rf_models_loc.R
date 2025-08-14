# Name: Conor Glackin
# Date: 18 Feb 2025
# Description: rf models for all datasets split by location in test set


setwd("C:/Users/glack/Documents/vibrio_second_paper/")

#### firstly I will look at 16S v vul vs copernicus

join_all_current_discharge_hplc_temp_weather <- 
  read_csv("data_all/join_all_current_discharge_hplc_temp_weather_full_year.csv")


join_all_current_discharge_hplc_temp_weather <- join_all_current_discharge_hplc_temp_weather %>% 
  select(c(
    "sample", "discharge_flow", "PO4", "NH4", "NO2", "Chl_a", "quality_windspeed", 
    "mean_windspeed", "precipitation_height", "precipitation_form", 
    "sunshine_duration", "snow_depth", "mean_cloud_cover",
    "mean_pressure", "mean_relative_humidity", 
    "downward_radiation", "diffuse_solar_radiation", 
    "sunshine_duration_solar", "temperature.y",
    "total_cell_counts" #, "siconc",  "ssh_1m",
     # "mixed_layer_thickness_1h", "ssh_1_h","salinity_1h","salinity_sea_floor_1h","temp_potent_1h",          
   # "velocity_south_1h","velocity_north_1h", "velocity_upward_1h" 
  ))



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


# 18s
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
v_vul_16s$year <- as.numeric(substr(v_vul_16s$sample, 
                                    nchar(v_vul_16s$sample) - 1, nchar(v_vul_16s$sample)))

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


otu_16s_filtered_rel <- left_join( join_all_current_discharge_hplc_temp_weather
                                   , v_vul_16s_processed, by = "sample")

otu_16s_filtered_rel <- left_join( otu_16s_filtered_rel, otu_18s_filtered, by = "sample")

otu_16s_filtered_rel <- left_join(  otu_16s_filtered_rel,otu_16s_filtered_1 ,by = "sample")

#otu_16s_filtered_rel <- left_join(  otu_16s_filtered_rel, v_vul_16s_processed ,by = "sample")

#otu_16s_filtered_rel <- left_join( otu_16s_filtered_rel,join_all_current_discharge_hplc_temp_weather
#                                   , by = "sample")


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
  select(-OTU_16S_25709, -OTU_16S_25947, -OTU_16S_33238,
         -year, -week, -day, -Year, -Week, -Monday_Date, -Day_Shift, -v_vul_total_abundance) %>%
  na.omit()



# Prepare the data
set.seed(7896)  # For reproducibility
X <- otu_16s_filtered_rel_rf

# Ensure the response variable is a factor with valid level names
y <- factor(otu_16s_filtered_rel_rf$v_vul_presence, levels = c(0, 1), labels = c("Absent", "Present"))

# Combine the predictors and response into one data frame for caret
data <- cbind(X, v_vul_presence = y)

# Remove missing values
data <- na.omit(data)

# Remove duplicated columns
data <- data %>% 
  select(-c("v_vul_presence", "sample"))

# Check the class distribution
table(y)

# Step 2: Split dataset into training and testing

data <- subset(data, location %in%  
                                    c("1", "2", "3", "4", "5", "6", "7"))#, "8", "9", "10", "11", "12", "13", "14", "15"))

# Step 2: Split dataset into training and testing

train_data <- data %>% filter(!location %in% c(7, 11, 13))
test_data  <- data %>% filter(location %in% c(7, 11, 13))

#set.seed(999)  # Ensures the scrambling can be reproduced
#train_data$v_vul_presence <- sample(train_data$v_vul_presence)

test_data_with_date <- test_data
  
# Remove duplicated columns
train_data <- train_data %>% 
  select(-c("location", "date"))

test_data <- test_data %>% 
  select(-c("location", "date"))

# Apply SMOTE to the training data
set.seed(7896)
train_data_smote <- SMOTE(v_vul_presence ~ ., data = train_data, perc.over = 200, perc.under = 100)

# Define the train control for k-fold cross-validation
train_control <- trainControl(method = "cv",
                              number = 10,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary,
                              savePredictions = "final")

# Tuning grid for Random Forest
tune_grid <- expand.grid(mtry = c(2, 3))

# Train the Random Forest model with cross-validation on the SMOTE balanced data
set.seed(7896)
rf_model_cv <- caret::train(v_vul_presence ~ ., data = train_data_smote,
                            method = "rf",
                            metric = "ROC",
                            trControl = train_control,
                            tuneGrid = tune_grid,
                            ntree = 500)

rf_model_cv$bestTune

# Evaluate the model's performance on the test data
predicted_class <- predict(rf_model_cv, test_data)
conf_matrix <- confusionMatrix(predicted_class, test_data$v_vul_presence)
print(conf_matrix)

# Plot the ROC curve and calculate AUC for the test data
predicted_prob <- predict(rf_model_cv, test_data, type = "prob")[, "Present"]
roc_curve <- roc(test_data$v_vul_presence, predicted_prob)
plot(roc_curve, col = "blue", main = "ROC Curve")
auc_value_all <- auc(roc_curve)
print(auc_value_all)  # Area Under the Curve (AUC)

# Adjust the threhold o fix false positives
threshold <- 0.64
predicted_class_threshold <- ifelse(predicted_prob > threshold, "Present", "Absent")
conf_matrix_all <- confusionMatrix(as.factor(predicted_class_threshold), test_data$v_vul_presence)
print(conf_matrix_all)



# Create results_manual dataframe with actual and predicted data
results_manual_rf_all <- data.frame(
  Date = test_data_with_date$date,
  Week = week(test_data_with_date$date),
  Actual = test_data$v_vul_presence,
  Predicted_Class = predicted_class_threshold,
  Predicted_probability = predicted_prob
) %>%
  mutate(
    Category = case_when(
      Actual == "Present" & Predicted_Class == "Present" ~ "True Positive",
      Actual == "Absent" & Predicted_Class == "Absent" ~ "True Negative",
      Actual == "Absent" & Predicted_Class == "Present" ~ "False Positive",
      Actual == "Present" & Predicted_Class == "Absent" ~ "False Negative"
    )
  ) %>%
  arrange(Date)  # Sort by Date to ensure proper alignment

# Print the results_manual to verify
print(results_manual_rf_all)


# Step 3: Group by Week and Category to count occurrences
weekly_summary <- results_manual_rf_all %>%
  group_by(Week, Category) %>%
  dplyr::summarise(Count = n()) %>%
  ungroup()

# Step 4: Create stacked bar chart with ggplot
ggplot(weekly_summary, aes(x = Week, y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Prediction Performance by Week", x = "Week", y = "Count") +
  scale_fill_manual(values = c("True Positive" = "green", "True Negative" = "blue",
                               "False Positive" = "red", "False Negative" = "orange")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the importance of predictors
importance_values_reg <- varImp(rf_model_cv, scale = FALSE)

print("Variable Importance for Regression Model:")
print(importance_values_reg)

# Visualize the top 10 important variables for the Regression Model
importance_reg_df <- as.data.frame(importance_values_reg$importance)
importance_reg_df$Variables <- rownames(importance_reg_df)
importance_reg_df <- importance_reg_df %>% arrange(desc(Overall))

# Top 10 important variables for Regression Model
top_10_all <- ggplot(importance_reg_df[1:10, ], aes(x = reorder(Variables, Overall), y = Overall)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Important Variables (Regression Model)", x = "Variables", y = "Importance") +
  theme_minimal()


subset <- importance_reg_df[1:10, ]

taxV3_V4_16s <- read_csv("data_all/taxonomy_16S.csv")
taxV3_V4_18s <- read_csv("data_all/taxonomy_18S.csv")

colnames(taxV3_V4_16s)[which(names(taxV3_V4_16s) == "name...1")] <- "zotu"
colnames(taxV3_V4_18s)[which(names(taxV3_V4_18s) == "name...1")] <- "zotu"

taxV3_V4_16s <- taxV3_V4_16s[, c("zotu", "biodomain", "phylum", "class", "bioorder","family", 
                                 "genus","species")]

taxV3_V4_18s <- taxV3_V4_18s[, c("zotu", "biodomain", "phylum", "class", "bioorder","family", 
                                 "genus","species")]

# Concatenate the two taxonomy data frames
taxV3_V4 <- bind_rows(taxV3_V4_16s, taxV3_V4_18s)

colnames(subset)[which(names(subset) == "Variables")] <- "zotu"

top_10_otus_all <- left_join(subset, taxV3_V4, by = "zotu")

# Create a nested label
top_10_otus_all <- top_10_otus_all %>%
  mutate(nested_label = paste(biodomain, genus, sep = " - "))

# Plot using the nested label
top_10_all <- ggplot(top_10_otus_all, aes(x = reorder(nested_label, Overall), y = Overall)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "All data: Baltic Sea", x = "Variable", y = "Importance") +
  theme_minimal()

print(top_10_all)

test <- test_data

test$v_vul_presence <- ifelse(test_data$v_vul_presence == "Present", 1, 0)

# Get predicted probabilities
predicted_prob <- predict(rf_model_cv, test, type = "prob")[, "Present"]

# Compute Precision-Recall Curve
pr_curve <- pr.curve(scores.class0 = predicted_prob, weights.class0 = test$v_vul_presence, curve = TRUE)

# Extract curve data
pr_data <- data.frame(
  Recall = pr_curve$curve[, 1],   # Recall
  Precision = pr_curve$curve[, 2] # Precision
)

# Create a ggplot object for the PR curve
pr_plot_all <- ggplot(pr_data, aes(x = Recall, y = Precision)) +
  geom_line(color = "blue", linewidth =2) +
  ggtitle(paste("Precision-Recall Curve (AUPR =", round(pr_curve$auc.integral, 3), ")")) +
  xlab("Recall") +
  ylab("Precision") +
  theme_minimal()

# Print the plot (optional)
print(pr_plot_all)

pr_value_all <-pr_curve$auc.integral

############### all model with all locations

#### firstly I will look at 16S v vul vs copernicus

join_all_current_discharge_hplc_temp_weather <- 
  read_csv("data_all/join_all_current_discharge_hplc_temp_weather_full_year.csv")


join_all_current_discharge_hplc_temp_weather <- join_all_current_discharge_hplc_temp_weather %>% 
  select(c(
    "sample", "discharge_flow", "PO4", "NH4", "NO2", "Chl_a", "quality_windspeed", 
    "mean_windspeed", "precipitation_height", "precipitation_form", 
    "sunshine_duration", "snow_depth", "mean_cloud_cover",
    "mean_pressure", "mean_relative_humidity", 
    "downward_radiation", "diffuse_solar_radiation", 
    "sunshine_duration_solar", "temperature.y",
    "total_cell_counts" #, "siconc",  "ssh_1m",
    # "mixed_layer_thickness_1h", "ssh_1_h","salinity_1h","salinity_sea_floor_1h","temp_potent_1h",          
    # "velocity_south_1h","velocity_north_1h", "velocity_upward_1h" 
  ))



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


# 18s
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
v_vul_16s$year <- as.numeric(substr(v_vul_16s$sample, 
                                    nchar(v_vul_16s$sample) - 1, nchar(v_vul_16s$sample)))

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


otu_16s_filtered_rel <- left_join( join_all_current_discharge_hplc_temp_weather
                                   , v_vul_16s_processed, by = "sample")

otu_16s_filtered_rel <- left_join( otu_16s_filtered_rel, otu_18s_filtered, by = "sample")

otu_16s_filtered_rel <- left_join(  otu_16s_filtered_rel,otu_16s_filtered_1 ,by = "sample")

#otu_16s_filtered_rel <- left_join(  otu_16s_filtered_rel, v_vul_16s_processed ,by = "sample")

#otu_16s_filtered_rel <- left_join( otu_16s_filtered_rel,join_all_current_discharge_hplc_temp_weather
#                                   , by = "sample")


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
  select(-OTU_16S_25709, -OTU_16S_25947, -OTU_16S_33238,
         -year, -week, -day, -Year, -Week, -Monday_Date, -Day_Shift, -v_vul_total_abundance) %>%
  na.omit()



# Prepare the data
set.seed(7896)  # For reproducibility
X <- otu_16s_filtered_rel_rf

# Ensure the response variable is a factor with valid level names
y <- factor(otu_16s_filtered_rel_rf$v_vul_presence, levels = c(0, 1), labels = c("Absent", "Present"))

# Combine the predictors and response into one data frame for caret
data <- cbind(X, v_vul_presence = y)

# Remove missing values
data <- na.omit(data)

# Remove duplicated columns
data <- data %>% 
  select(-c("v_vul_presence", "sample"))

# Check the class distribution
table(y)

# Step 2: Split dataset into training and testing

data <- subset(data, location %in%  
                 c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15"))

# Step 2: Split dataset into training and testing

train_data <- data %>% filter(!location %in% c(7, 11, 13))
test_data  <- data %>% filter(location %in% c(7, 11, 13))

#set.seed(999)  # Ensures the scrambling can be reproduced
#train_data$v_vul_presence <- sample(train_data$v_vul_presence)

test_data_with_date <- test_data

# Remove duplicated columns
train_data <- train_data %>% 
  select(-c("location", "date"))

test_data <- test_data %>% 
  select(-c("location", "date"))

# Apply SMOTE to the training data
set.seed(7896)
train_data_smote <- SMOTE(v_vul_presence ~ ., data = train_data, perc.over = 200, perc.under = 100)

# Define the train control for k-fold cross-validation
train_control <- trainControl(method = "cv",
                              number = 10,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary,
                              savePredictions = "final")

# Tuning grid for Random Forest
tune_grid <- expand.grid(mtry = c(2, 3))

# Train the Random Forest model with cross-validation on the SMOTE balanced data
set.seed(7896)
rf_model_cv <- caret::train(v_vul_presence ~ ., data = train_data_smote,
                            method = "rf",
                            metric = "ROC",
                            trControl = train_control,
                            tuneGrid = tune_grid,
                            ntree = 500)

rf_model_cv$bestTune

# Evaluate the model's performance on the test data
predicted_class <- predict(rf_model_cv, test_data)
conf_matrix <- confusionMatrix(predicted_class, test_data$v_vul_presence)
print(conf_matrix)

# Plot the ROC curve and calculate AUC for the test data
predicted_prob <- predict(rf_model_cv, test_data, type = "prob")[, "Present"]
roc_curve <- roc(test_data$v_vul_presence, predicted_prob)
plot(roc_curve, col = "blue", main = "ROC Curve")
auc_value_all_all <- auc(roc_curve)
print(auc_value_all_all)  # Area Under the Curve (AUC)

# Adjust the threhold o fix false positives
threshold <- 0.64
predicted_class_threshold <- ifelse(predicted_prob > threshold, "Present", "Absent")
conf_matrix_all <- confusionMatrix(as.factor(predicted_class_threshold), test_data$v_vul_presence)
print(conf_matrix_all)



# Create results_manual dataframe with actual and predicted data
results_manual_rf_all_all_loc <- data.frame(
  Date = test_data_with_date$date,
  Week = week(test_data_with_date$date),
  Actual = test_data$v_vul_presence,
  Predicted_Class = predicted_class_threshold,
  Predicted_probability = predicted_prob
) %>%
  mutate(
    Category = case_when(
      Actual == "Present" & Predicted_Class == "Present" ~ "True Positive",
      Actual == "Absent" & Predicted_Class == "Absent" ~ "True Negative",
      Actual == "Absent" & Predicted_Class == "Present" ~ "False Positive",
      Actual == "Present" & Predicted_Class == "Absent" ~ "False Negative"
    )
  ) %>%
  arrange(Date)  # Sort by Date to ensure proper alignment

# Print the results_manual to verify
print(results_manual_rf_all_all_loc)


# Step 3: Group by Week and Category to count occurrences
weekly_summary <- results_manual_rf_all_all_loc %>%
  group_by(Week, Category) %>%
  dplyr::summarise(Count = n()) %>%
  ungroup()

# Step 4: Create stacked bar chart with ggplot
ggplot(weekly_summary, aes(x = Week, y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Prediction Performance by Week", x = "Week", y = "Count") +
  scale_fill_manual(values = c("True Positive" = "green", "True Negative" = "blue",
                               "False Positive" = "red", "False Negative" = "orange")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the importance of predictors
importance_values_reg <- varImp(rf_model_cv, scale = FALSE)

print("Variable Importance for Regression Model:")
print(importance_values_reg)

# Visualize the top 10 important variables for the Regression Model
importance_reg_df <- as.data.frame(importance_values_reg$importance)
importance_reg_df$Variables <- rownames(importance_reg_df)
importance_reg_df <- importance_reg_df %>% arrange(desc(Overall))

# Top 10 important variables for Regression Model
top_10_all_all_loc <- ggplot(importance_reg_df[1:10, ], aes(x = reorder(Variables, Overall), y = Overall)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "All data: Baltic Sea and Warnow Estuary", x = "Variables", y = "Importance") +
  theme_minimal()


subset <- importance_reg_df[1:10, ]

taxV3_V4_16s <- read_csv("data_all/taxonomy_16S.csv")
taxV3_V4_18s <- read_csv("data_all/taxonomy_18S.csv")

colnames(taxV3_V4_16s)[which(names(taxV3_V4_16s) == "name...1")] <- "zotu"
colnames(taxV3_V4_18s)[which(names(taxV3_V4_18s) == "name...1")] <- "zotu"

taxV3_V4_16s <- taxV3_V4_16s[, c("zotu", "biodomain", "phylum", "class", "bioorder","family", 
                                 "genus","species")]

taxV3_V4_18s <- taxV3_V4_18s[, c("zotu", "biodomain", "phylum", "class", "bioorder","family", 
                                 "genus","species")]

# Concatenate the two taxonomy data frames
taxV3_V4 <- bind_rows(taxV3_V4_16s, taxV3_V4_18s)

colnames(subset)[which(names(subset) == "Variables")] <- "zotu"

top_10_otus_all_all_loc <- left_join(subset, taxV3_V4, by = "zotu")

# Create a nested label
top_10_otus_all_all_loc <- top_10_otus_all_all_loc %>%
  mutate(nested_label = paste(biodomain, genus, sep = " - "))

# Plot using the nested label
top_10_all_all_loc <- ggplot(top_10_otus_all_all_loc, aes(x = reorder(nested_label, Overall), y = Overall)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "All data: Baltic Sea and Warnow Estuary", x = "Variable", y = "Importance") +
  theme_minimal()

print(top_10_all_all_loc)

test <- test_data

test$v_vul_presence <- ifelse(test_data$v_vul_presence == "Present", 1, 0)

# Get predicted probabilities
predicted_prob <- predict(rf_model_cv, test, type = "prob")[, "Present"]

# Compute Precision-Recall Curve
pr_curve <- pr.curve(scores.class0 = predicted_prob, weights.class0 = test$v_vul_presence, curve = TRUE)

# Extract curve data
pr_data <- data.frame(
  Recall = pr_curve$curve[, 1],   # Recall
  Precision = pr_curve$curve[, 2] # Precision
)

# Create a ggplot object for the PR curve
pr_plot_all_all_loc <- ggplot(pr_data, aes(x = Recall, y = Precision)) +
  geom_line(color = "blue", linewidth =2) +
  ggtitle(paste("Precision-Recall Curve (AUPR =", round(pr_curve$auc.integral, 3), ")")) +
  xlab("Recall") +
  ylab("Precision") +
  theme_minimal()

# Print the plot (optional)
print(pr_plot_all_all_loc)

pr_value_all_all_loc <-pr_curve$auc.integral



############### 16s model

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

# Subset the dataset where week is between 25 and 36
#v_vul_16s <- v_vul_16s[v_vul_16s$week >= 25 & v_vul_16s$week <= 36, ]


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

otu_16s_filtered_rel <- left_join( otu_16s_filtered_1, v_vul_16s_processed, by = "sample")

# Add a new column 'v_vul_presence' to the 'logistic_reg' data frame
otu_16s_filtered_rel <- otu_16s_filtered_rel %>%
  mutate(v_vul_presence = ifelse(v_vul_total_abundance == 0, 0, 1))

otu_16s_filtered_rel$v_vul_presence <- as.factor(otu_16s_filtered_rel$v_vul_presence)

otu_16s_filtered_rel_rf <- otu_16s_filtered_rel %>% 
  select((!c( "OTU_16S_25709","OTU_16S_25947", "OTU_16S_33238", "v_vul_total_abundance",
  )))

otu_16s_filtered_rel_rf <- na.omit(otu_16s_filtered_rel_rf)

#### trying cross validation

# Assuming otu_16s_filtered_rel_rf is your data frame
#otu_16s_filtered_rel_rf$v_vul_presence <- sample(otu_16s_filtered_rel_rf$v_vul_presence)

# Check the updated data frame
head(otu_16s_filtered_rel_rf)


# Prepare the data
set.seed(7896)  # For reproducibility

# Prepare the data
X <- otu_16s_filtered_rel_rf

# Ensure the response variable is a factor with valid level names
y <- factor(otu_16s_filtered_rel_rf$v_vul_presence, levels = c(0, 1), labels = c("Absent", "Present"))

# Combine the predictors and response into one data frame for caret
data <- cbind(X, v_vul_presence = y)

# Remove missing values
data <- na.omit(data)
# Remove duplicated columns
data <- data %>% 
  select((!c("v_vul_presence","sample", "year", "week", "day",
             "Year", "Week", "Monday_Date", "Day_Shift")))
# Remove 'sample' from the predictors for training
#data_for_model <- data %>% select(-sample)
# Remove duplicated columns


# Check the class distribution
table(y)

# Step 2: Split dataset into training and testing
#train_data <- otu_16s_filtered_rel_rf %>% filter(!location %in% c(6, 12))
#test_data  <- otu_16s_filtered_rel_rf %>% filter(location %in% c(6, 12))
# Step 2: Split dataset into training and testing

data <- subset(data, location %in%  
                 c("1", "2", "3", "4", "5", "6", "7"))#, "8", "9", "10", "11", "12", "13", "14", "15"))

train_data <- data %>% filter(!location %in% c(7, 11, 13))
test_data  <- data %>% filter(location %in% c(7, 11, 13))

test_data_with_date <- test_data

# Remove duplicated columns
train_data <- train_data %>% 
  select(-c("location", "date"))

test_data <- test_data %>% 
  select(-c("location", "date"))

# Apply SMOTE to the training data
set.seed(7896)
train_data_smote <- SMOTE(v_vul_presence ~ ., data = train_data, perc.over = 200, perc.under = 100)

# Check the new class distribution after SMOTE
table(train_data_smote$v_vul_presence)

# Define the train control for k-fold cross-validation
train_control <- trainControl(method = "cv",        # Cross-validation method
                              number = 10,          # Number of folds
                              classProbs = TRUE,    # To compute class probabilities
                              summaryFunction = twoClassSummary,  # For ROC and other metrics
                              savePredictions = "final")  # Save predictions

# Tuning grid for Random Forest
tune_grid <- expand.grid(mtry = c(2, 3))

# Train the Random Forest model with cross-validation on the SMOTE balanced data
set.seed(7896)
rf_model_cv <- caret::train(v_vul_presence ~ ., data = train_data_smote,
                            method = "rf",
                            metric = "ROC",  # Optimize for ROC
                            trControl = train_control,
                            tuneGrid = tune_grid,
                            ntree = 500)  # Increase number of trees

# Print the cross-validated model results
print(rf_model_cv)

rf_model_cv$bestTune

# Evaluate the model's performance on the test data
predicted_class <- predict(rf_model_cv, test_data)
conf_matrix <- confusionMatrix(predicted_class, test_data$v_vul_presence)
print(conf_matrix)

# Plot the ROC curve and calculate AUC for the test data
predicted_prob <- predict(rf_model_cv, test_data, type = "prob")[, "Present"]
roc_curve <- roc(test_data$v_vul_presence, predicted_prob)
plot(roc_curve, col = "blue", main = "ROC Curve")
auc_value_16s <- auc(roc_curve)
print(auc_value_16s)  # Area Under the Curve (AUC)

#### Trying to fix false positives

# Adjust the threshold
threshold <- 0.664# Adjust this value to see how it impacts the results
predicted_class_threshold <- ifelse(predicted_prob > threshold, "Present", "Absent")

# Confusion matrix with the new threshold
conf_matrix_16s <- confusionMatrix(as.factor(predicted_class), test_data$v_vul_presence)
print(conf_matrix_16s)




# Create results_manual dataframe with actual and predicted data
results_manual_rf_16s <- data.frame(
  Date = test_data_with_date$date,
  Week = week(test_data_with_date$date),
  Actual = test_data$v_vul_presence,
  Predicted_Class = predicted_class_threshold,
  Predicted_probability = predicted_prob
) %>%
  mutate(
    Category = case_when(
      Actual == "Present" & Predicted_Class == "Present" ~ "True Positive",
      Actual == "Absent" & Predicted_Class == "Absent" ~ "True Negative",
      Actual == "Absent" & Predicted_Class == "Present" ~ "False Positive",
      Actual == "Present" & Predicted_Class == "Absent" ~ "False Negative"
    )
  ) %>%
  arrange(Date)  # Sort by Date to ensure proper alignment

# Print the results_manual to verify
print(results_manual_rf_16s)



# Step 3: Group by Week and Category to count occurrences
weekly_summary <- results_manual_rf_16s %>%
  group_by(Week, Category) %>%
  dplyr::summarise(Count = n()) %>%
  ungroup()

# Step 4: Create stacked bar chart with ggplot
ggplot(weekly_summary, aes(x = Week, y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Prediction Performance by Week", x = "Week", y = "Count") +
  scale_fill_manual(values = c("True Positive" = "green", "True Negative" = "blue",
                               "False Positive" = "red", "False Negative" = "orange")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the importance of predictors
importance_values_reg <- varImp(rf_model_cv, scale = FALSE)

print("Variable Importance for Regression Model:")
print(importance_values_reg)

# Visualize the top 10 important variables for the Regression Model
importance_reg_df <- as.data.frame(importance_values_reg$importance)
importance_reg_df$Variables <- rownames(importance_reg_df)
importance_reg_df <- importance_reg_df %>% arrange(desc(Overall))

# Top 10 important variables for Regression Model
top_10_16s <- ggplot(importance_reg_df[1:10, ], aes(x = reorder(Variables, Overall), y = Overall)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Important Variables (Regression Model)", x = "Variables", y = "Importance") +
  theme_minimal()


subset <- importance_reg_df[1:10, ]

taxV3_V4 <- read_csv("data_all/taxonomy_16S.csv")  

colnames(taxV3_V4)[which(names(taxV3_V4) == "name...1")] <- "zotu"

taxV3_V4 <- taxV3_V4[, c("zotu", "biodomain", "phylum", "class", "bioorder","family", 
                         "genus","species")]



colnames(subset)[which(names(subset) == "Variables")] <- "zotu"

top_10_otus_16s <- left_join(subset, taxV3_V4, by = "zotu")

# Top 10 important variables for Regression Model
top_10_16s <- ggplot(top_10_otus_16s, aes(x = reorder(genus, Overall), y = Overall)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "16S rRNA rel. abundance: Baltic Sea", x = "Genus", y = "Importance") +
  theme_minimal()


test <- test_data

test$v_vul_presence <- ifelse(test_data$v_vul_presence == "Present", 1, 0)

# Get predicted probabilities
predicted_prob <- predict(rf_model_cv, test, type = "prob")[, "Present"]

# Compute Precision-Recall Curve
pr_curve <- pr.curve(scores.class0 = predicted_prob, weights.class0 = test$v_vul_presence, curve = TRUE)

# Extract curve data
pr_data <- data.frame(
  Recall = pr_curve$curve[, 1],   # Recall
  Precision = pr_curve$curve[, 2] # Precision
)

# Create a ggplot object for the PR curve
pr_plot_16s <- ggplot(pr_data, aes(x = Recall, y = Precision)) +
  geom_line(color = "blue", linewidth =2) +
  ggtitle(paste("Precision-Recall Curve (AUPR =", round(pr_curve$auc.integral, 3), ")")) +
  xlab("Recall") +
  ylab("Precision") +
  theme_minimal()

# Print the plot (optional)
print(pr_plot_16s)

pr_value_16s <-pr_curve$auc.integral


############### 16s model all

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

# Subset the dataset where week is between 25 and 36
#v_vul_16s <- v_vul_16s[v_vul_16s$week >= 25 & v_vul_16s$week <= 36, ]


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

otu_16s_filtered_rel <- left_join( otu_16s_filtered_1, v_vul_16s_processed, by = "sample")

# Add a new column 'v_vul_presence' to the 'logistic_reg' data frame
otu_16s_filtered_rel <- otu_16s_filtered_rel %>%
  mutate(v_vul_presence = ifelse(v_vul_total_abundance == 0, 0, 1))

otu_16s_filtered_rel$v_vul_presence <- as.factor(otu_16s_filtered_rel$v_vul_presence)

otu_16s_filtered_rel_rf <- otu_16s_filtered_rel %>% 
  select((!c( "OTU_16S_25709","OTU_16S_25947", "OTU_16S_33238", "v_vul_total_abundance",
  )))

otu_16s_filtered_rel_rf <- na.omit(otu_16s_filtered_rel_rf)

#### trying cross validation

# Assuming otu_16s_filtered_rel_rf is your data frame
#otu_16s_filtered_rel_rf$v_vul_presence <- sample(otu_16s_filtered_rel_rf$v_vul_presence)

# Check the updated data frame
head(otu_16s_filtered_rel_rf)


# Prepare the data
set.seed(7896)  # For reproducibility

# Prepare the data
X <- otu_16s_filtered_rel_rf

# Ensure the response variable is a factor with valid level names
y <- factor(otu_16s_filtered_rel_rf$v_vul_presence, levels = c(0, 1), labels = c("Absent", "Present"))

# Combine the predictors and response into one data frame for caret
data <- cbind(X, v_vul_presence = y)

# Remove missing values
data <- na.omit(data)
# Remove duplicated columns
data <- data %>% 
  select((!c("v_vul_presence","sample", "year", "week", "day",
             "Year", "Week", "Monday_Date", "Day_Shift")))
# Remove 'sample' from the predictors for training
#data_for_model <- data %>% select(-sample)
# Remove duplicated columns


# Check the class distribution
table(y)

# Step 2: Split dataset into training and testing
#train_data <- otu_16s_filtered_rel_rf %>% filter(!location %in% c(6, 12))
#test_data  <- otu_16s_filtered_rel_rf %>% filter(location %in% c(6, 12))
# Step 2: Split dataset into training and testing

data <- subset(data, location %in%  
                 c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15"))

train_data <- data %>% filter(!location %in% c(7, 11, 13))
test_data  <- data %>% filter(location %in% c(7, 11, 13))

test_data_with_date <- test_data

# Remove duplicated columns
train_data <- train_data %>% 
  select(-c("location", "date"))

test_data <- test_data %>% 
  select(-c("location", "date"))

# Apply SMOTE to the training data
set.seed(7896)
train_data_smote <- SMOTE(v_vul_presence ~ ., data = train_data, perc.over = 200, perc.under = 100)

# Check the new class distribution after SMOTE
table(train_data_smote$v_vul_presence)

# Define the train control for k-fold cross-validation
train_control <- trainControl(method = "cv",        # Cross-validation method
                              number = 10,          # Number of folds
                              classProbs = TRUE,    # To compute class probabilities
                              summaryFunction = twoClassSummary,  # For ROC and other metrics
                              savePredictions = "final")  # Save predictions

# Tuning grid for Random Forest
tune_grid <- expand.grid(mtry = c(2, 3))

# Train the Random Forest model with cross-validation on the SMOTE balanced data
set.seed(7896)
rf_model_cv <- caret::train(v_vul_presence ~ ., data = train_data_smote,
                            method = "rf",
                            metric = "ROC",  # Optimize for ROC
                            trControl = train_control,
                            tuneGrid = tune_grid,
                            ntree = 500)  # Increase number of trees

# Print the cross-validated model results
print(rf_model_cv)

rf_model_cv$bestTune

# Evaluate the model's performance on the test data
predicted_class <- predict(rf_model_cv, test_data)
conf_matrix <- confusionMatrix(predicted_class, test_data$v_vul_presence)
print(conf_matrix)

# Plot the ROC curve and calculate AUC for the test data
predicted_prob <- predict(rf_model_cv, test_data, type = "prob")[, "Present"]
roc_curve <- roc(test_data$v_vul_presence, predicted_prob)
plot(roc_curve, col = "blue", main = "ROC Curve")
auc_value_16s_all <- auc(roc_curve)
print(auc_value_16s_all)  # Area Under the Curve (AUC)

#### Trying to fix false positives

# Adjust the threshold
threshold <- 0.664# Adjust this value to see how it impacts the results
predicted_class_threshold <- ifelse(predicted_prob > threshold, "Present", "Absent")

# Confusion matrix with the new threshold
conf_matrix_16s <- confusionMatrix(as.factor(predicted_class), test_data$v_vul_presence)
print(conf_matrix_16s)




# Create results_manual dataframe with actual and predicted data
results_manual_rf_16s_all <- data.frame(
  Date = test_data_with_date$date,
  Week = week(test_data_with_date$date),
  Actual = test_data$v_vul_presence,
  Predicted_Class = predicted_class_threshold,
  Predicted_probability = predicted_prob
) %>%
  mutate(
    Category = case_when(
      Actual == "Present" & Predicted_Class == "Present" ~ "True Positive",
      Actual == "Absent" & Predicted_Class == "Absent" ~ "True Negative",
      Actual == "Absent" & Predicted_Class == "Present" ~ "False Positive",
      Actual == "Present" & Predicted_Class == "Absent" ~ "False Negative"
    )
  ) %>%
  arrange(Date)  # Sort by Date to ensure proper alignment

# Print the results_manual to verify
print(results_manual_rf_16s_all)



# Step 3: Group by Week and Category to count occurrences
weekly_summary <- results_manual_rf_16s_all %>%
  group_by(Week, Category) %>%
  dplyr::summarise(Count = n()) %>%
  ungroup()

# Step 4: Create stacked bar chart with ggplot
ggplot(weekly_summary, aes(x = Week, y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Prediction Performance by Week", x = "Week", y = "Count") +
  scale_fill_manual(values = c("True Positive" = "green", "True Negative" = "blue",
                               "False Positive" = "red", "False Negative" = "orange")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the importance of predictors
importance_values_reg <- varImp(rf_model_cv, scale = FALSE)

print("Variable Importance for Regression Model:")
print(importance_values_reg)

# Visualize the top 10 important variables for the Regression Model
importance_reg_df <- as.data.frame(importance_values_reg$importance)
importance_reg_df$Variables <- rownames(importance_reg_df)
importance_reg_df <- importance_reg_df %>% arrange(desc(Overall))

# Top 10 important variables for Regression Model
top_10_all_all_loc <- ggplot(importance_reg_df[1:10, ], aes(x = reorder(Variables, Overall), y = Overall)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Important Variables (Regression Model)", x = "Variables", y = "Importance") +
  theme_minimal()


subset <- importance_reg_df[1:10, ]

taxV3_V4 <- read_csv("data_all/taxonomy_16S.csv")  

colnames(taxV3_V4)[which(names(taxV3_V4) == "name...1")] <- "zotu"

taxV3_V4 <- taxV3_V4[, c("zotu", "biodomain", "phylum", "class", "bioorder","family", 
                         "genus","species")]



colnames(subset)[which(names(subset) == "Variables")] <- "zotu"

top_10_otus_16s_all <- left_join(subset, taxV3_V4, by = "zotu")



top_10_otus_16s_all$Overall <- as.numeric(top_10_otus_16s_all$Overall)

# Aggregate Overall importance by genus
genus_importance <- aggregate(Overall ~ genus, data = top_10_otus_16s_all, FUN = sum, na.rm = TRUE)

# Sort by Overall importance in decreasing order
genus_importance <- genus_importance[order(genus_importance$Overall, decreasing = TRUE), ]

top10_genus <- head(genus_importance, 10)

# Top 10 important variables for Regression Model
top_10_16s_all <- ggplot(top10_genus, aes(x = reorder(genus, Overall), y = Overall)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "18S rRNA rel. abundance: Baltic Sea and Warnow Estuary", x = "Genus", y = "Importance") +
  theme_minimal()



test <- test_data

test$v_vul_presence <- ifelse(test_data$v_vul_presence == "Present", 1, 0)

# Get predicted probabilities
predicted_prob <- predict(rf_model_cv, test, type = "prob")[, "Present"]

# Compute Precision-Recall Curve
pr_curve <- pr.curve(scores.class0 = predicted_prob, weights.class0 = test$v_vul_presence, curve = TRUE)

# Extract curve data
pr_data <- data.frame(
  Recall = pr_curve$curve[, 1],   # Recall
  Precision = pr_curve$curve[, 2] # Precision
)

# Create a ggplot object for the PR curve
pr_plot_16s_all_loc <- ggplot(pr_data, aes(x = Recall, y = Precision)) +
  geom_line(color = "blue", linewidth =2) +
  ggtitle(paste("Precision-Recall Curve (AUPR =", round(pr_curve$auc.integral, 3), ")")) +
  xlab("Recall") +
  ylab("Precision") +
  theme_minimal()

# Print the plot (optional)
print(pr_plot_16s)

pr_value_16s_all_loc <-pr_curve$auc.integral

############### 18s model

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

otu_16s_filtered_rel_rf <- na.omit(otu_16s_filtered_rel_rf)

#### trying cross validation

# Assuming otu_16s_filtered_rel_rf is your data frame
#otu_16s_filtered_rel_rf$v_vul_presence <- sample(otu_16s_filtered_rel_rf$v_vul_presence)
# Prepare the dataset for LSTM, keeping the 'sample' column
otu_16s_filtered_rel_rf <- otu_16s_filtered_rel %>% 
  select(-OTU_16S_25709, -OTU_16S_25947, -OTU_16S_33238,
         -year, -week, -day, -Year, -Week, -Monday_Date, -Day_Shift, -v_vul_total_abundance) %>%
  na.omit()

# Check the updated data frame
head(otu_16s_filtered_rel_rf)




# Prepare the data
set.seed(7896)  # For reproducibility
X <- otu_16s_filtered_rel_rf

# Ensure the response variable is a factor with valid level names
y <- factor(otu_16s_filtered_rel_rf$v_vul_presence, levels = c(0, 1), labels = c("Absent", "Present"))

# Combine the predictors and response into one data frame for caret
data <- cbind(X, v_vul_presence = y)

# Remove missing values
data <- na.omit(data)

# Remove duplicated columns

data <- data %>% 
  select(-c("v_vul_presence", "sample"))

# Check the class distribution
table(y)

# Step 2: Split dataset into training and testing
#train_data <- otu_16s_filtered_rel_rf %>% filter(!location %in% c(6, 12))
#test_data  <- otu_16s_filtered_rel_rf %>% filter(location %in% c(6, 12))
# Step 2: Split dataset into training and testing

data <- subset(data, location %in%  
                 c("1", "2", "3", "4", "5", "6", "7"))#, "8", "9", "10", "11", "12", "13", "14", "15"))

train_data <- data %>% filter(!location %in% c(7, 11, 13))
test_data  <- data %>% filter(location %in% c(7, 11, 13))

test_data_with_date <- test_data

# Remove duplicated columns
train_data <- train_data %>% 
  select(-c("location", "date"))

test_data <- test_data %>% 
  select(-c("location", "date"))
# Check the class distribution
table(y)

# Apply SMOTE to the training data
set.seed(7896)
train_data_smote <- SMOTE(v_vul_presence ~ ., data = train_data, perc.over = 200, perc.under = 100)

# Define the train control for k-fold cross-validation
train_control <- trainControl(method = "cv",
                              number = 10,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary,
                              savePredictions = "final")

# Tuning grid for Random Forest
tune_grid <- expand.grid(mtry = c(2, 3))

# Train the Random Forest model with cross-validation on the SMOTE balanced data
set.seed(7896)
rf_model_cv <- caret::train(v_vul_presence ~ ., data = train_data_smote,
                            method = "rf",
                            metric = "ROC",
                            trControl = train_control,
                            tuneGrid = tune_grid,
                            ntree = 500)
rf_model_cv$bestTune

# Evaluate the model's performance on the test data
predicted_class <- predict(rf_model_cv, test_data)
conf_matrix <- confusionMatrix(predicted_class, test_data$v_vul_presence)
print(conf_matrix)

# Plot the ROC curve and calculate AUC for the test data
predicted_prob <- predict(rf_model_cv, test_data, type = "prob")[, "Present"]
roc_curve <- roc(test_data$v_vul_presence, predicted_prob)
plot(roc_curve, col = "blue", main = "ROC Curve")
auc_value_18s <- auc(roc_curve)
print(auc_value_18s)  # Area Under the Curve (AUC)

# Adjust the threshold to fix false positives
threshold <- 0.651
predicted_class_threshold <- ifelse(predicted_prob > threshold, "Present", "Absent")
conf_matrix_18s <- confusionMatrix(as.factor(predicted_class_threshold), test_data$v_vul_presence)
print(conf_matrix_18s)

# Create results_manual dataframe with actual and predicted data
results_manual_rf_18s <- data.frame(
  Date = test_data_with_date$date,
  Week = week(test_data_with_date$date),
  Actual = test_data$v_vul_presence,
  Predicted_Class = predicted_class_threshold,
  Predicted_probability = predicted_prob
) %>%
  mutate(
    Category = case_when(
      Actual == "Present" & Predicted_Class == "Present" ~ "True Positive",
      Actual == "Absent" & Predicted_Class == "Absent" ~ "True Negative",
      Actual == "Absent" & Predicted_Class == "Present" ~ "False Positive",
      Actual == "Present" & Predicted_Class == "Absent" ~ "False Negative"
    )
  ) %>%
  arrange(Date)  # Sort by Date to ensure proper alignment

# Print the results_manual to verify
print(results_manual_rf_18s)

# Step 3: Group by Week and Category to count occurrences
weekly_summary <- results_manual_rf_18s %>%
  group_by(Week, Category) %>%
  dplyr::summarise(Count = n()) %>%
  ungroup()

# Step 4: Create stacked bar chart with ggplot
ggplot(weekly_summary, aes(x = Week, y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Prediction Performance by Week", x = "Week", y = "Count") +
  scale_fill_manual(values = c("True Positive" = "green", "True Negative" = "blue",
                               "False Positive" = "red", "False Negative" = "orange")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the importance of predictors
importance_values_reg <- varImp(rf_model_cv, scale = FALSE)

print("Variable Importance for Regression Model:")
print(importance_values_reg)

# Visualize the top 10 important variables for the Regression Model
importance_reg_df <- as.data.frame(importance_values_reg$importance)
importance_reg_df$Variables <- rownames(importance_reg_df)
importance_reg_df <- importance_reg_df %>% arrange(desc(Overall))

# Top 10 important variables for Regression Model
top_10_18s <- ggplot(importance_reg_df[1:10, ], aes(x = reorder(Variables, Overall), y = Overall)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Important Variables (Regression Model)", x = "Variables", y = "Importance") +
  theme_minimal()

subset <- importance_reg_df[1:10, ]

taxV3_V4 <- read_csv("data_all/taxonomy_18S.csv")  

colnames(taxV3_V4)[which(names(taxV3_V4) == "name...1")] <- "zotu"

taxV3_V4 <- taxV3_V4[, c("zotu", "biodomain", "phylum", "class", "bioorder","family", 
                         "genus","species")]

colnames(subset)[which(names(subset) == "Variables")] <- "zotu"

top_10_otus_18s <- left_join(subset, taxV3_V4, by = "zotu")


# Top 10 important variables for Regression Model
top_10_18s <- ggplot(top_10_otus_18s, aes(x = reorder(genus, Overall), y = Overall)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "18S rRNA rel. abundance: Baltic Sea", x = "Genus", y = "Importance") +
  theme_minimal()


test <- test_data

test$v_vul_presence <- ifelse(test_data$v_vul_presence == "Present", 1, 0)

# Get predicted probabilities
predicted_prob <- predict(rf_model_cv, test, type = "prob")[, "Present"]

# Compute Precision-Recall Curve
pr_curve <- pr.curve(scores.class0 = predicted_prob, weights.class0 = test$v_vul_presence, curve = TRUE)

# Extract curve data
pr_data <- data.frame(
  Recall = pr_curve$curve[, 1],   # Recall
  Precision = pr_curve$curve[, 2] # Precision
)

# Create a ggplot object for the PR curve
pr_plot_18s <- ggplot(pr_data, aes(x = Recall, y = Precision)) +
  geom_line(color = "blue", linewidth =2) +
  ggtitle(paste("Precision-Recall Curve (AUPR =", round(pr_curve$auc.integral, 3), ")")) +
  xlab("Recall") +
  ylab("Precision") +
  theme_minimal()

# Print the plot (optional)
print(pr_plot_18s)

pr_value_18s <-pr_curve$auc.integral



######## 18s all locations

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

otu_16s_filtered_rel_rf <- na.omit(otu_16s_filtered_rel_rf)

#### trying cross validation

# Assuming otu_16s_filtered_rel_rf is your data frame
#otu_16s_filtered_rel_rf$v_vul_presence <- sample(otu_16s_filtered_rel_rf$v_vul_presence)
# Prepare the dataset for LSTM, keeping the 'sample' column
otu_16s_filtered_rel_rf <- otu_16s_filtered_rel %>% 
  select(-OTU_16S_25709, -OTU_16S_25947, -OTU_16S_33238,
         -year, -week, -day, -Year, -Week, -Monday_Date, -Day_Shift, -v_vul_total_abundance) %>%
  na.omit()

# Check the updated data frame
head(otu_16s_filtered_rel_rf)




# Prepare the data
set.seed(7896)  # For reproducibility
X <- otu_16s_filtered_rel_rf

# Ensure the response variable is a factor with valid level names
y <- factor(otu_16s_filtered_rel_rf$v_vul_presence, levels = c(0, 1), labels = c("Absent", "Present"))

# Combine the predictors and response into one data frame for caret
data <- cbind(X, v_vul_presence = y)

# Remove missing values
data <- na.omit(data)

# Remove duplicated columns

data <- data %>% 
  select(-c("v_vul_presence", "sample"))

# Check the class distribution
table(y)

# Step 2: Split dataset into training and testing
#train_data <- otu_16s_filtered_rel_rf %>% filter(!location %in% c(6, 12))
#test_data  <- otu_16s_filtered_rel_rf %>% filter(location %in% c(6, 12))
# Step 2: Split dataset into training and testing

data <- subset(data, location %in%  
                 c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15"))

train_data <- data %>% filter(!location %in% c(7, 11, 13))
test_data  <- data %>% filter(location %in% c(7, 11, 13))

test_data_with_date <- test_data

# Remove duplicated columns
train_data <- train_data %>% 
  select(-c("location", "date"))

test_data <- test_data %>% 
  select(-c("location", "date"))
# Check the class distribution
table(y)

# Apply SMOTE to the training data
set.seed(7896)
train_data_smote <- SMOTE(v_vul_presence ~ ., data = train_data, perc.over = 200, perc.under = 100)

# Define the train control for k-fold cross-validation
train_control <- trainControl(method = "cv",
                              number = 10,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary,
                              savePredictions = "final")

# Tuning grid for Random Forest
tune_grid <- expand.grid(mtry = c(2, 3))

# Train the Random Forest model with cross-validation on the SMOTE balanced data
set.seed(7896)
rf_model_cv <- caret::train(v_vul_presence ~ ., data = train_data_smote,
                            method = "rf",
                            metric = "ROC",
                            trControl = train_control,
                            tuneGrid = tune_grid,
                            ntree = 500)
rf_model_cv$bestTune

# Evaluate the model's performance on the test data
predicted_class <- predict(rf_model_cv, test_data)
conf_matrix <- confusionMatrix(predicted_class, test_data$v_vul_presence)
print(conf_matrix)

# Plot the ROC curve and calculate AUC for the test data
predicted_prob <- predict(rf_model_cv, test_data, type = "prob")[, "Present"]
roc_curve <- roc(test_data$v_vul_presence, predicted_prob)
plot(roc_curve, col = "blue", main = "ROC Curve")
auc_value_18s_all <- auc(roc_curve)
print(auc_value_18s)  # Area Under the Curve (AUC)

# Adjust the threshold to fix false positives
threshold <- 0.651
predicted_class_threshold <- ifelse(predicted_prob > threshold, "Present", "Absent")
conf_matrix_18s <- confusionMatrix(as.factor(predicted_class_threshold), test_data$v_vul_presence)
print(conf_matrix_18s)

# Create results_manual dataframe with actual and predicted data
results_manual_rf_18s_all <- data.frame(
  Date = test_data_with_date$date,
  Week = week(test_data_with_date$date),
  Actual = test_data$v_vul_presence,
  Predicted_Class = predicted_class_threshold,
  Predicted_probability = predicted_prob
) %>%
  mutate(
    Category = case_when(
      Actual == "Present" & Predicted_Class == "Present" ~ "True Positive",
      Actual == "Absent" & Predicted_Class == "Absent" ~ "True Negative",
      Actual == "Absent" & Predicted_Class == "Present" ~ "False Positive",
      Actual == "Present" & Predicted_Class == "Absent" ~ "False Negative"
    )
  ) %>%
  arrange(Date)  # Sort by Date to ensure proper alignment

# Print the results_manual to verify
print(results_manual_rf_18s_all)

# Step 3: Group by Week and Category to count occurrences
weekly_summary <- results_manual_rf_18s_all %>%
  group_by(Week, Category) %>%
  dplyr::summarise(Count = n()) %>%
  ungroup()

# Step 4: Create stacked bar chart with ggplot
ggplot(weekly_summary, aes(x = Week, y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Prediction Performance by Week", x = "Week", y = "Count") +
  scale_fill_manual(values = c("True Positive" = "green", "True Negative" = "blue",
                               "False Positive" = "red", "False Negative" = "orange")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the importance of predictors
importance_values_reg <- varImp(rf_model_cv, scale = FALSE)

print("Variable Importance for Regression Model:")
print(importance_values_reg)

# Visualize the top 10 important variables for the Regression Model
importance_reg_df <- as.data.frame(importance_values_reg$importance)
importance_reg_df$Variables <- rownames(importance_reg_df)
importance_reg_df <- importance_reg_df %>% arrange(desc(Overall))

# Top 10 important variables for Regression Model
top_10_18s_all <- ggplot(importance_reg_df[1:10, ], aes(x = reorder(Variables, Overall), y = Overall)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Important Variables (Regression Model)", x = "Variables", y = "Importance") +
  theme_minimal()




subset <- importance_reg_df[1:10, ]

taxV3_V4 <- read_csv("data_all/taxonomy_18S.csv")  

colnames(taxV3_V4)[which(names(taxV3_V4) == "name...1")] <- "zotu"

taxV3_V4 <- taxV3_V4[, c("zotu", "biodomain", "phylum", "class", "bioorder","family", 
                         "genus","species")]

colnames(subset)[which(names(subset) == "Variables")] <- "zotu"

top_10_otus_18s_all <- left_join(subset, taxV3_V4, by = "zotu")



top_10_otus_18s_all$Overall <- as.numeric(top_10_otus_18s_all$Overall)

# Aggregate Overall importance by genus
genus_importance <- aggregate(Overall ~ genus, data = top_10_otus_18s_all, FUN = sum, na.rm = TRUE)

# Sort by Overall importance in decreasing order
genus_importance <- genus_importance[order(genus_importance$Overall, decreasing = TRUE), ]

top10_genus <- head(genus_importance, 10)

# Top 10 important variables for Regression Model
top_10_18s_all <- ggplot(top10_genus, aes(x = reorder(genus, Overall), y = Overall)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "18S rRNA rel. abundance: Baltic Sea and Warnow Estuary", x = "Genus", y = "Importance") +
  theme_minimal()

test <- test_data

test$v_vul_presence <- ifelse(test_data$v_vul_presence == "Present", 1, 0)

# Get predicted probabilities
predicted_prob <- predict(rf_model_cv, test, type = "prob")[, "Present"]

# Compute Precision-Recall Curve
pr_curve <- pr.curve(scores.class0 = predicted_prob, weights.class0 = test$v_vul_presence, curve = TRUE)

# Extract curve data
pr_data <- data.frame(
  Recall = pr_curve$curve[, 1],   # Recall
  Precision = pr_curve$curve[, 2] # Precision
)

# Create a ggplot object for the PR curve
pr_plot_18s_all_loc <- ggplot(pr_data, aes(x = Recall, y = Precision)) +
  geom_line(color = "blue", linewidth =2) +
  ggtitle(paste("Precision-Recall Curve (AUPR =", round(pr_curve$auc.integral, 3), ")")) +
  xlab("Recall") +
  ylab("Precision") +
  theme_minimal()

# Print the plot (optional)
print(pr_plot_18s_all_loc)

pr_value_18s_all_loc <-pr_curve$auc.integral

########### otc data

setwd("C:/Users/glack/Documents/vibrio_second_paper/")

#### firstly I will look at 16S v vul vs copernicus

join_all_current_discharge_hplc_temp_weather <- 
  read_csv("data_all/join_all_current_discharge_hplc_temp_weather_full_year.csv")


join_all_current_discharge_hplc_temp_weather <- join_all_current_discharge_hplc_temp_weather %>% 
  select(c(
    "sample", "date.x", "discharge_flow", "PO4", "NH4", "NO2", "Chl_a", "quality_windspeed", 
    "mean_windspeed", "precipitation_height", "precipitation_form", 
    "sunshine_duration", "snow_depth", "mean_cloud_cover",
    "mean_pressure", "mean_relative_humidity", 
    "downward_radiation", "diffuse_solar_radiation", 
    "sunshine_duration_solar", "temperature.y",
    "total_cell_counts", "v_vul_total_abundance"
  ))

colnames(join_all_current_discharge_hplc_temp_weather)[which(names(
  join_all_current_discharge_hplc_temp_weather) ==  "date.x")] <- "date"


join_all_current_discharge_hplc_temp_weather$location <- as.numeric(
  substr(join_all_current_discharge_hplc_temp_weather$sample, 2, 3))

# Select the columns to modify
columns_to_modify <- c("PO4", "NH4", "NO2")

# Loop through each column and replace negative values with 0
for (col in columns_to_modify) {
  join_all_current_discharge_hplc_temp_weather[[col]][join_all_current_discharge_hplc_temp_weather[[col]] < 0] <- 0
}

copernicus_data <- read_csv("data_all/copernicus_data.csv")

copernicus_data <- copernicus_data %>% 
  select((!c("pot_temp_sea_floor_1m", "mixed_layer_thickness_1m", "ssh_1m",
             "salinity_1m", "salinity_sea_floor_1m", "temp_potent_1m" ,         
             "velocity_south_1m", "velocity_north_1m", "velocity_upward_1m",
             "pot_temp_sea_floor_1h", "mixed_layer_thickness_1h", "ssh_1_h", "salinity_1h",
             "salinity_sea_floor_1h", "temp_potent_1h", "velocity_south_1h", "velocity_north_1h",
             "velocity_upward_1h", "ssh_15_m", "velocity_east", "velocity_north")))


copernicus_data_ddpcr <- left_join(copernicus_data, join_all_current_discharge_hplc_temp_weather, by = "sample")

copernicus_data_ddpcr <- na.omit(copernicus_data_ddpcr)

# remove columns of all same value

logistic_reg <- copernicus_data_ddpcr[vapply(copernicus_data_ddpcr, 
                                             function(x) length(unique(x)) > 1, logical(1L))]


#logistic_reg <- subset(logistic_reg, location %in%  
#                         c("Heiligendamm", "Borgerende", "Nienhagen", "Warnemuende"))
logistic_reg <-join_all_current_discharge_hplc_temp_weather

# Add a new column 'v_vul_presence' to the 'logistic_reg' data frame
logistic_reg <- logistic_reg %>%
  mutate(v_vul_presence = ifelse(v_vul_total_abundance == 0, 0, 1))

logistic_reg$v_vul_presence <- as.factor(logistic_reg$v_vul_presence)

#logistic_reg <- logistic_reg %>% 
#  select((!c("lon", "lat","date", "location", 
#             "date_time", "sample", "...1", "time", "ddpcr" )))


logistic_reg <- logistic_reg %>% 
  select((!c( "sample", "v_vul_total_abundance" )))

# Remove missing values
logistic_reg <- na.omit(logistic_reg)


# Prepare the data
set.seed(7896)  # For reproducibility
X <- logistic_reg

# Ensure the response variable is a factor with valid level names
y <- factor(logistic_reg$v_vul_presence, levels = c(0, 1), labels = c("Absent", "Present"))

# Combine the predictors and response into one data frame for caret
data <- cbind(X, v_vul_presence = y)

# Remove missing values
data <- na.omit(data)

# Remove duplicated columns
# Remove duplicated columns
data <- data %>% 
  select(-c("v_vul_presence"))

# Check the class distribution
table(y)

# Step 2: Split dataset into training and testing
#train_data <- otu_16s_filtered_rel_rf %>% filter(!location %in% c(6, 12))
#test_data  <- otu_16s_filtered_rel_rf %>% filter(location %in% c(6, 12))
# Step 2: Split dataset into training and testing
data <- subset(data, location %in%  
                 c("1", "2", "3", "4", "5", "6", "7"))#, "8", "9", "10", "11", "12", "13", "14", "15"))

train_data <- data %>% filter(!location %in% c(7, 11, 13))
test_data  <- data %>% filter(location %in% c(7, 11, 13))

test_data_with_date <- test_data

# Remove duplicated columns
train_data <- train_data %>% 
  select(-c("location", "date"))

test_data <- test_data %>% 
  select(-c("location", "date"))
# Apply SMOTE to the training data
set.seed(7896)
train_data_smote <- SMOTE(v_vul_presence ~ ., data = train_data, perc.over = 200, perc.under = 100)

# Define the train control for k-fold cross-validation
train_control <- trainControl(method = "cv",
                              number = 10,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary,
                              savePredictions = "final")

# Tuning grid for Random Forest
tune_grid <- expand.grid(mtry = c(2, 3))

# Train the Random Forest model with cross-validation on the SMOTE balanced data
set.seed(7896)
rf_model_cv <- caret::train(v_vul_presence ~ ., data = train_data_smote,
                            method = "rf",
                            metric = "ROC",
                            trControl = train_control,
                            tuneGrid = tune_grid,
                            ntree = 500)

rf_model_cv$bestTune

# Evaluate the model's performance on the test data
predicted_class <- predict(rf_model_cv, test_data)
conf_matrix <- confusionMatrix(predicted_class, test_data$v_vul_presence)
print(conf_matrix)

# Plot the ROC curve and calculate AUC for the test data
predicted_prob <- predict(rf_model_cv, test_data, type = "prob")[, "Present"]
roc_curve <- roc(test_data$v_vul_presence, predicted_prob)
plot(roc_curve, col = "blue", main = "ROC Curve")
auc_value_otc <- auc(roc_curve)
print(auc_value_otc)  # Area Under the Curve (AUC)

# Adjust the threshold to fix false positives
threshold <- 0.7355
predicted_class_threshold <- ifelse(predicted_prob > threshold, "Present", "Absent")
conf_matrix_otc <- confusionMatrix(as.factor(predicted_class_threshold), test_data$v_vul_presence)
print(conf_matrix_otc)

# Create results_manual dataframe with actual and predicted data
results_manual_rf_otc <- data.frame(
  Date = test_data_with_date$date,
  Week = week(test_data_with_date$date),
  Actual = test_data$v_vul_presence,
  Predicted_Class = predicted_class_threshold,
  Predicted_probability = predicted_prob
) %>%
  mutate(
    Category = case_when(
      Actual == "Present" & Predicted_Class == "Present" ~ "True Positive",
      Actual == "Absent" & Predicted_Class == "Absent" ~ "True Negative",
      Actual == "Absent" & Predicted_Class == "Present" ~ "False Positive",
      Actual == "Present" & Predicted_Class == "Absent" ~ "False Negative"
    )
  ) %>%
  arrange(Date)  # Sort by Date to ensure proper alignment

# Print the results_manual to verify
print(results_manual_rf_otc)


# Step 3: Group by Week and Category to count occurrences
weekly_summary <- results_manual_rf_otc %>%
  group_by(Week, Category) %>%
  dplyr::summarise(Count = n()) %>%
  ungroup()

# Step 4: Create stacked bar chart with ggplot
ggplot(weekly_summary, aes(x = Week, y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Prediction Performance by Week", x = "Week", y = "Count") +
  scale_fill_manual(values = c("True Positive" = "green", "True Negative" = "blue",
                               "False Positive" = "red", "False Negative" = "orange")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the importance of predictors
importance_values_reg <- varImp(rf_model_cv, scale = FALSE)

print("Variable Importance for Regression Model:")
print(importance_values_reg)

# Visualize the top 10 important variables for the Regression Model
importance_reg_df <- as.data.frame(importance_values_reg$importance)
importance_reg_df$Variables <- rownames(importance_reg_df)
importance_reg_df <- importance_reg_df %>% arrange(desc(Overall))

# Top 10 important variables for Regression Model
top_10_otc_plot <- ggplot(importance_reg_df[1:10, ], aes(x = reorder(Variables, Overall), y = Overall)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Biological/physical data: Baltic Sea", x = "Variables", y = "Importance") +
  theme_minimal()

top_10_otc <- importance_reg_df[1:10, ]

test <- test_data

test$v_vul_presence <- ifelse(test_data$v_vul_presence == "Present", 1, 0)

# Get predicted probabilities
predicted_prob <- predict(rf_model_cv, test, type = "prob")[, "Present"]

# Compute Precision-Recall Curve
pr_curve <- pr.curve(scores.class0 = predicted_prob, weights.class0 = test$v_vul_presence, curve = TRUE)

# Extract curve data
pr_data <- data.frame(
  Recall = pr_curve$curve[, 1],   # Recall
  Precision = pr_curve$curve[, 2] # Precision
)

# Create a ggplot object for the PR curve
pr_plot_otc <- ggplot(pr_data, aes(x = Recall, y = Precision)) +
  geom_line(color = "blue", linewidth =2) +
  ggtitle(paste("Precision-Recall Curve (AUPR =", round(pr_curve$auc.integral, 3), ")")) +
  xlab("Recall") +
  ylab("Precision") +
  theme_minimal()

# Print the plot (optional)
print(pr_plot_otc)

pr_value_otc <-pr_curve$auc.integral


####### otc all

#### firstly I will look at 16S v vul vs copernicus

join_all_current_discharge_hplc_temp_weather <- 
  read_csv("data_all/join_all_current_discharge_hplc_temp_weather_full_year.csv")


join_all_current_discharge_hplc_temp_weather <- join_all_current_discharge_hplc_temp_weather %>% 
  select(c(
    "sample", "date.x", "discharge_flow", "PO4", "NH4", "NO2", "Chl_a", "quality_windspeed", 
    "mean_windspeed", "precipitation_height", "precipitation_form", 
    "sunshine_duration", "snow_depth", "mean_cloud_cover",
    "mean_pressure", "mean_relative_humidity", 
    "downward_radiation", "diffuse_solar_radiation", 
    "sunshine_duration_solar", "temperature.y",
    "total_cell_counts", "v_vul_total_abundance"
  ))

colnames(join_all_current_discharge_hplc_temp_weather)[which(names(
  join_all_current_discharge_hplc_temp_weather) ==  "date.x")] <- "date"


join_all_current_discharge_hplc_temp_weather$location <- as.numeric(
  substr(join_all_current_discharge_hplc_temp_weather$sample, 2, 3))

# Select the columns to modify
columns_to_modify <- c("PO4", "NH4", "NO2")

# Loop through each column and replace negative values with 0
for (col in columns_to_modify) {
  join_all_current_discharge_hplc_temp_weather[[col]][join_all_current_discharge_hplc_temp_weather[[col]] < 0] <- 0
}

copernicus_data <- read_csv("data_all/copernicus_data.csv")

copernicus_data <- copernicus_data %>% 
  select((!c("pot_temp_sea_floor_1m", "mixed_layer_thickness_1m", "ssh_1m",
             "salinity_1m", "salinity_sea_floor_1m", "temp_potent_1m" ,         
             "velocity_south_1m", "velocity_north_1m", "velocity_upward_1m",
             "pot_temp_sea_floor_1h", "mixed_layer_thickness_1h", "ssh_1_h", "salinity_1h",
             "salinity_sea_floor_1h", "temp_potent_1h", "velocity_south_1h", "velocity_north_1h",
             "velocity_upward_1h", "ssh_15_m", "velocity_east", "velocity_north")))


copernicus_data_ddpcr <- left_join(copernicus_data, join_all_current_discharge_hplc_temp_weather, by = "sample")

copernicus_data_ddpcr <- na.omit(copernicus_data_ddpcr)

# remove columns of all same value

logistic_reg <- copernicus_data_ddpcr[vapply(copernicus_data_ddpcr, 
                                             function(x) length(unique(x)) > 1, logical(1L))]


#logistic_reg <- subset(logistic_reg, location %in%  
#                         c("Heiligendamm", "Borgerende", "Nienhagen", "Warnemuende"))
logistic_reg <-join_all_current_discharge_hplc_temp_weather

# Add a new column 'v_vul_presence' to the 'logistic_reg' data frame
logistic_reg <- logistic_reg %>%
  mutate(v_vul_presence = ifelse(v_vul_total_abundance == 0, 0, 1))

logistic_reg$v_vul_presence <- as.factor(logistic_reg$v_vul_presence)

#logistic_reg <- logistic_reg %>% 
#  select((!c("lon", "lat","date", "location", 
#             "date_time", "sample", "...1", "time", "ddpcr" )))


logistic_reg <- logistic_reg %>% 
  select((!c( "sample", "v_vul_total_abundance" )))

# Remove missing values
logistic_reg <- na.omit(logistic_reg)


# Prepare the data
set.seed(7896)  # For reproducibility
X <- logistic_reg

# Ensure the response variable is a factor with valid level names
y <- factor(logistic_reg$v_vul_presence, levels = c(0, 1), labels = c("Absent", "Present"))

# Combine the predictors and response into one data frame for caret
data <- cbind(X, v_vul_presence = y)

# Remove missing values
data <- na.omit(data)

# Remove duplicated columns
# Remove duplicated columns
data <- data %>% 
  select(-c("v_vul_presence"))

# Check the class distribution
table(y)

# Step 2: Split dataset into training and testing
#train_data <- otu_16s_filtered_rel_rf %>% filter(!location %in% c(6, 12))
#test_data  <- otu_16s_filtered_rel_rf %>% filter(location %in% c(6, 12))
# Step 2: Split dataset into training and testing
data <- subset(data, location %in%  
                 c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15"))

train_data <- data %>% filter(!location %in% c(7, 11, 13))
test_data  <- data %>% filter(location %in% c(7, 11, 13))

test_data_with_date <- test_data

# Remove duplicated columns
train_data <- train_data %>% 
  select(-c("location", "date"))

test_data <- test_data %>% 
  select(-c("location", "date"))
# Apply SMOTE to the training data
set.seed(7896)
train_data_smote <- SMOTE(v_vul_presence ~ ., data = train_data, perc.over = 200, perc.under = 100)

# Define the train control for k-fold cross-validation
train_control <- trainControl(method = "cv",
                              number = 10,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary,
                              savePredictions = "final")

# Tuning grid for Random Forest
tune_grid <- expand.grid(mtry = c(2, 3))

# Train the Random Forest model with cross-validation on the SMOTE balanced data
set.seed(7896)
rf_model_cv <- caret::train(v_vul_presence ~ ., data = train_data_smote,
                            method = "rf",
                            metric = "ROC",
                            trControl = train_control,
                            tuneGrid = tune_grid,
                            ntree = 500)

rf_model_cv$bestTune

# Evaluate the model's performance on the test data
predicted_class <- predict(rf_model_cv, test_data)
conf_matrix <- confusionMatrix(predicted_class, test_data$v_vul_presence)
print(conf_matrix)

# Plot the ROC curve and calculate AUC for the test data
predicted_prob <- predict(rf_model_cv, test_data, type = "prob")[, "Present"]
roc_curve <- roc(test_data$v_vul_presence, predicted_prob)
plot(roc_curve, col = "blue", main = "ROC Curve")
auc_value_otc_all <- auc(roc_curve)
print(auc_value_otc)  # Area Under the Curve (AUC)

# Adjust the threshold to fix false positives
threshold <- 0.7355
predicted_class_threshold <- ifelse(predicted_prob > threshold, "Present", "Absent")
conf_matrix_otc <- confusionMatrix(as.factor(predicted_class_threshold), test_data$v_vul_presence)
print(conf_matrix_otc)

# Create results_manual dataframe with actual and predicted data
results_manual_rf_otc_all <- data.frame(
  Date = test_data_with_date$date,
  Week = week(test_data_with_date$date),
  Actual = test_data$v_vul_presence,
  Predicted_Class = predicted_class_threshold,
  Predicted_probability = predicted_prob
) %>%
  mutate(
    Category = case_when(
      Actual == "Present" & Predicted_Class == "Present" ~ "True Positive",
      Actual == "Absent" & Predicted_Class == "Absent" ~ "True Negative",
      Actual == "Absent" & Predicted_Class == "Present" ~ "False Positive",
      Actual == "Present" & Predicted_Class == "Absent" ~ "False Negative"
    )
  ) %>%
  arrange(Date)  # Sort by Date to ensure proper alignment

# Print the results_manual to verify
print(results_manual_rf_otc_all)


# Step 3: Group by Week and Category to count occurrences
weekly_summary <- results_manual_rf_otc_all %>%
  group_by(Week, Category) %>%
  dplyr::summarise(Count = n()) %>%
  ungroup()

# Step 4: Create stacked bar chart with ggplot
ggplot(weekly_summary, aes(x = Week, y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Prediction Performance by Week", x = "Week", y = "Count") +
  scale_fill_manual(values = c("True Positive" = "green", "True Negative" = "blue",
                               "False Positive" = "red", "False Negative" = "orange")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the importance of predictors
importance_values_reg <- varImp(rf_model_cv, scale = FALSE)

print("Variable Importance for Regression Model:")
print(importance_values_reg)

# Visualize the top 10 important variables for the Regression Model
importance_reg_df <- as.data.frame(importance_values_reg$importance)
importance_reg_df$Variables <- rownames(importance_reg_df)
importance_reg_df <- importance_reg_df %>% arrange(desc(Overall))

# Top 10 important variables for Regression Model
top_10_otc_all_plot <- ggplot(importance_reg_df[1:10, ], aes(x = reorder(Variables, Overall), y = Overall)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Biological/physical data: Baltic Sea and Warnow Estuary", 
       x = "Variables", y = "Importance") +
  theme_minimal()

top_10_otc_all <- importance_reg_df[1:10, ]

test <- test_data

test$v_vul_presence <- ifelse(test_data$v_vul_presence == "Present", 1, 0)

# Get predicted probabilities
predicted_prob <- predict(rf_model_cv, test, type = "prob")[, "Present"]

# Compute Precision-Recall Curve
pr_curve <- pr.curve(scores.class0 = predicted_prob, weights.class0 = test$v_vul_presence, curve = TRUE)

# Extract curve data
pr_data <- data.frame(
  Recall = pr_curve$curve[, 1],   # Recall
  Precision = pr_curve$curve[, 2] # Precision
)

# Create a ggplot object for the PR curve
pr_plot_otc_all_loc <- ggplot(pr_data, aes(x = Recall, y = Precision)) +
  geom_line(color = "blue", linewidth =2) +
  ggtitle(paste("Precision-Recall Curve (AUPR =", round(pr_curve$auc.integral, 3), ")")) +
  xlab("Recall") +
  ylab("Precision") +
  theme_minimal()

# Print the plot (optional)
print(pr_plot_otc_all_loc)

pr_value_otc_all_loc <-pr_curve$auc.integral

######### copernicus data

join_all_current_discharge_hplc_temp_weather <- read_csv("data_all/join_all_current_discharge_hplc_temp_weather_full_year.csv")


join_all_current_discharge_hplc_temp_weather <- join_all_current_discharge_hplc_temp_weather %>% 
  select(c(
    "sample", "quality_windspeed", 
    "mean_windspeed", "precipitation_height", "precipitation_form", 
    "sunshine_duration", "snow_depth", "mean_cloud_cover",
    "mean_pressure", "mean_relative_humidity", 
    "diffuse_solar_radiation", 
    "sunshine_duration_solar", "v_vul_total_abundance"
  ))


join_all_current_discharge_hplc_temp_weather$location <- as.numeric(
  substr(join_all_current_discharge_hplc_temp_weather$sample, 2, 3))


copernicus_data <- read_csv("data_all/copernicus_data.csv")


copernicus_data <- copernicus_data %>% 
  select((!c( "ssh_1m", "ssh_15_m",  "pot_temp_sea_floor_1m", "mixed_layer_thickness_1m","salinity_1m", "location",
              "salinity_sea_floor_1m", "temp_potent_1m" , "velocity_east", "velocity_north",        
              "velocity_south_1m", "velocity_north_1m", "velocity_upward_1m", "ssh_untided_1d",
              "pot_temp_sea_floor_1d", "pot_temp_sea_floor_1d", "mixed_layer_thickness_1d", "salinity_1d",
              "salinity_sea_floor_1d", "temp_potent_1d", "velocity_south_1d", "velocity_north_1d",
              "velocity_upward_1d", "velocity_east_detided", "pot_temp_sea_floor_1h","velocity_north_detided")))

colnames(join_all_current_discharge_hplc_temp_weather)[which(names(
  join_all_current_discharge_hplc_temp_weather) ==  "date.x")] <- "date"

copernicus_data_ddpcr <- left_join(copernicus_data, join_all_current_discharge_hplc_temp_weather, by = "sample")


copernicus_data_ddpcr <- na.omit(copernicus_data_ddpcr)

# remove columns of all same value

logistic_reg <- copernicus_data_ddpcr[vapply(copernicus_data_ddpcr, function(x) length(unique(x)) > 1, logical(1L))]


#logistic_reg <- subset(logistic_reg, location %in%  
#                         c("Heiligendamm", "Borgerende", "Nienhagen", "Warnemuende"))
#logistic_reg <-join_all_current_discharge_hplc_temp_weather

# Add a new column 'v_vul_presence' to the 'logistic_reg' data frame
logistic_reg <- logistic_reg %>%
  mutate(v_vul_presence = ifelse(v_vul_total_abundance == 0, 0, 1))

logistic_reg$v_vul_presence <- as.factor(logistic_reg$v_vul_presence)

logistic_reg <- logistic_reg %>% 
  select((!c("lon", "lat", "sample",
             "date_time", "...1", "time", "v_vul_total_abundance" )))



#logistic_reg <- logistic_reg %>% 
#  select((!c( "sample", "v_vul_total_abundance" )))

# Remove missing values
logistic_reg <- na.omit(logistic_reg)

otu_16s_filtered_rel_rf <- logistic_reg

# Prepare the data
set.seed(7896)  # For reproducibility
X <- otu_16s_filtered_rel_rf

# Ensure the response variable is a factor with valid level names
y <- factor(otu_16s_filtered_rel_rf$v_vul_presence, levels = c(0, 1), labels = c("Absent", "Present"))

# Combine the predictors and response into one data frame for caret
data <- cbind(X, v_vul_presence = y)

# Remove missing values
data <- na.omit(data)

# Check the class distribution
table(y)

# Remove duplicated columns
# Remove duplicated columns
data <- data %>% 
  select(-c("v_vul_presence"))

# Check the class distribution
table(y)

# Step 2: Split dataset into training and testing
#train_data <- otu_16s_filtered_rel_rf %>% filter(!location %in% c(6, 12))
#test_data  <- otu_16s_filtered_rel_rf %>% filter(location %in% c(6, 12))
# Step 2: Split dataset into training and testing
data <- subset(data, location %in%  
                 c("1", "2", "3", "4", "5", "6", "7"))#, "8", "9", "10", "11", "12", "13", "14", "15"))

train_data <- data %>% filter(!location %in% c(7, 11, 13))
test_data  <- data %>% filter(location %in% c(7, 11, 13))

test_data_with_date <- test_data

# Remove duplicated columns
train_data <- train_data %>% 
  select(-c("location", "date"))

test_data <- test_data %>% 
  select(-c("location", "date"))
# Apply SMOTE to the training data
# Apply SMOTE to the training data
set.seed(7896)
train_data_smote <- SMOTE(v_vul_presence ~ ., data = train_data, perc.over = 200, perc.under = 100)

# Define the train control for k-fold cross-validation
train_control <- trainControl(method = "cv",
                              number = 10,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary,
                              savePredictions = "final")

# Tuning grid for Random Forest
tune_grid <- expand.grid(mtry = c(2, 3))

# Train the Random Forest model with cross-validation on the SMOTE balanced data
set.seed(7896)
rf_model_cv <- caret::train(v_vul_presence ~ ., data = train_data_smote,
                            method = "rf",
                            metric = "ROC",
                            trControl = train_control,
                            tuneGrid = tune_grid,
                            ntree = 500)

rf_model_cv$bestTune

# Evaluate the model's performance on the test data
predicted_class <- predict(rf_model_cv, test_data)
conf_matrix <- confusionMatrix(predicted_class, test_data$v_vul_presence)
print(conf_matrix)

# Plot the ROC curve and calculate AUC for the test data
predicted_prob <- predict(rf_model_cv, test_data, type = "prob")[, "Present"]
roc_curve <- roc(test_data$v_vul_presence, predicted_prob)
plot(roc_curve, col = "blue", main = "ROC Curve")
auc_value_copernicus <- auc(roc_curve)
print(auc_value_copernicus)  # Area Under the Curve (AUC)

# Adjust the thresh0ld to fix false positives
threshold <- 0.656
predicted_class_threshold <- ifelse(predicted_prob > threshold, "Present", "Absent")
conf_matrix_copernicus <- confusionMatrix(as.factor(predicted_class_threshold), test_data$v_vul_presence)
print(conf_matrix_copernicus)

# Create results_manual dataframe with actual and predicted data
results_manual_rf_copernicus <- data.frame(
  Date = test_data_with_date$date,
  Week = week(test_data_with_date$date),
  Actual = test_data$v_vul_presence,
  Predicted_Class = predicted_class_threshold,
  Predicted_probability = predicted_prob
) %>%
  mutate(
    Category = case_when(
      Actual == "Present" & Predicted_Class == "Present" ~ "True Positive",
      Actual == "Absent" & Predicted_Class == "Absent" ~ "True Negative",
      Actual == "Absent" & Predicted_Class == "Present" ~ "False Positive",
      Actual == "Present" & Predicted_Class == "Absent" ~ "False Negative"
    )
  ) %>%
  arrange(Date)  # Sort by Date to ensure proper alignment


# Print the results_manual to verify
print(results_manual_rf_copernicus)


# Step 3: Group by Week and Category to count occurrences
weekly_summary <- results_manual_rf_copernicus %>%
  group_by(Week, Category) %>%
  dplyr::summarise(Count = n()) %>%
  ungroup()

# Step 4: Create stacked bar chart with ggplot
ggplot(weekly_summary, aes(x = Week, y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Prediction Performance by Week", x = "Week", y = "Count") +
  scale_fill_manual(values = c("True Positive" = "green", "True Negative" = "blue",
                               "False Positive" = "red", "False Negative" = "orange")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the importance of predictors
importance_values_reg <- varImp(rf_model_cv, scale = FALSE)

print("Variable Importance for Regression Model:")
print(importance_values_reg)

# Visualize the top 10 important variables for the Regression Model
importance_reg_df <- as.data.frame(importance_values_reg$importance)
importance_reg_df$Variables <- rownames(importance_reg_df)
importance_reg_df <- importance_reg_df %>% arrange(desc(Overall))

# Top 10 important variables for Regression Model
top_10_copernicus_plot <- ggplot(importance_reg_df[1:10, ], aes(x = reorder(Variables, Overall), y = Overall)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Copernicus satellite data: Baltic Sea", x = "Variables", y = "Importance") +
  theme_minimal()

top_10_copernicus <- importance_reg_df[1:10, ]

test <- test_data

test$v_vul_presence <- ifelse(test_data$v_vul_presence == "Present", 1, 0)

# Get predicted probabilities
predicted_prob <- predict(rf_model_cv, test, type = "prob")[, "Present"]

# Compute Precision-Recall Curve
pr_curve <- pr.curve(scores.class0 = predicted_prob, weights.class0 = test$v_vul_presence, curve = TRUE)

# Extract curve data
pr_data <- data.frame(
  Recall = pr_curve$curve[, 1],   # Recall
  Precision = pr_curve$curve[, 2] # Precision
)

# Create a ggplot object for the PR curve
pr_plot_copernicus <- ggplot(pr_data, aes(x = Recall, y = Precision)) +
  geom_line(color = "blue", linewidth =2) +
  ggtitle(paste("Precision-Recall Curve (AUPR =", round(pr_curve$auc.integral, 3), ")")) +
  xlab("Recall") +
  ylab("Precision") +
  theme_minimal()

# Print the plot (optional)
print(pr_plot_copernicus)

pr_value_cop <-pr_curve$auc.integral



######### copernicus data with ulf data

join_all_current_discharge_hplc_temp_weather <- read_csv("data_all/join_all_current_discharge_hplc_temp_weather_full_year.csv")


join_all_current_discharge_hplc_temp_weather <- join_all_current_discharge_hplc_temp_weather %>% 
  select(c(
    "sample", "quality_windspeed", 
    "mean_windspeed", "precipitation_height", "precipitation_form", 
    "sunshine_duration", "snow_depth", "mean_cloud_cover",
    "mean_pressure", "mean_relative_humidity", 
    "diffuse_solar_radiation", 
    "sunshine_duration_solar", "v_vul_total_abundance", "O2",
    "salt", "uu", "vv" 
  ))


join_all_current_discharge_hplc_temp_weather$location <- as.numeric(
  substr(join_all_current_discharge_hplc_temp_weather$sample, 2, 3))


copernicus_data <- read_csv("data_all/copernicus_data.csv")


copernicus_data <- copernicus_data %>% 
  select((!c( "ssh_15_m",  "pot_temp_sea_floor_1m", "mixed_layer_thickness_1m","salinity_1m", "location",
              "salinity_sea_floor_1m", "temp_potent_1m" , "velocity_east", "velocity_north",        
              "velocity_south_1m", "velocity_north_1m", "velocity_upward_1m", "ssh_untided_1d",
              "pot_temp_sea_floor_1d", "pot_temp_sea_floor_1d", "mixed_layer_thickness_1d", "salinity_1d",
              "salinity_sea_floor_1d", "temp_potent_1d", "velocity_south_1d", "velocity_north_1d",
              "velocity_upward_1d", "velocity_east_detided", "pot_temp_sea_floor_1h","velocity_north_detided")))

colnames(join_all_current_discharge_hplc_temp_weather)[which(names(
  join_all_current_discharge_hplc_temp_weather) ==  "date.x")] <- "date"

copernicus_data_ddpcr <- left_join(copernicus_data, join_all_current_discharge_hplc_temp_weather, by = "sample")


copernicus_data_ddpcr <- na.omit(copernicus_data_ddpcr)

# remove columns of all same value

logistic_reg <- copernicus_data_ddpcr[vapply(copernicus_data_ddpcr, function(x) length(unique(x)) > 1, logical(1L))]


#logistic_reg <- subset(logistic_reg, location %in%  
#                         c("Heiligendamm", "Borgerende", "Nienhagen", "Warnemuende"))
#logistic_reg <-join_all_current_discharge_hplc_temp_weather

# Add a new column 'v_vul_presence' to the 'logistic_reg' data frame
logistic_reg <- logistic_reg %>%
  mutate(v_vul_presence = ifelse(v_vul_total_abundance == 0, 0, 1))

logistic_reg$v_vul_presence <- as.factor(logistic_reg$v_vul_presence)

logistic_reg <- logistic_reg %>% 
  select((!c("lon", "lat", "sample",
             "date_time", "...1", "time", "v_vul_total_abundance" )))



#logistic_reg <- logistic_reg %>% 
#  select((!c( "sample", "v_vul_total_abundance" )))

# Remove missing values
logistic_reg <- na.omit(logistic_reg)

otu_16s_filtered_rel_rf <- logistic_reg


# Prepare the data
set.seed(7896)  # For reproducibility
X <- otu_16s_filtered_rel_rf

# Ensure the response variable is a factor with valid level names
y <- factor(otu_16s_filtered_rel_rf$v_vul_presence, levels = c(0, 1), labels = c("Absent", "Present"))

# Combine the predictors and response into one data frame for caret
data <- cbind(X, v_vul_presence = y)

# Remove missing values
data <- na.omit(data)

# Check the class distribution
table(y)

# Remove duplicated columns
# Remove duplicated columns
data <- data %>% 
  select(-c("v_vul_presence"))

# Check the class distribution
table(y)

# Step 2: Split dataset into training and testing
#train_data <- otu_16s_filtered_rel_rf %>% filter(!location %in% c(6, 12))
#test_data  <- otu_16s_filtered_rel_rf %>% filter(location %in% c(6, 12))
# Step 2: Split dataset into training and testing

train_data <- data %>% filter(!location %in% c(7, 11, 13))
test_data  <- data %>% filter(location %in% c(7, 11, 13))

test_data_with_date <- test_data

# Remove duplicated columns
train_data <- train_data %>% 
  select(-c("location", "date"))

test_data <- test_data %>% 
  select(-c("location", "date"))
# Apply SMOTE to the training data
# Apply SMOTE to the training data
set.seed(7896)
train_data_smote <- SMOTE(v_vul_presence ~ ., data = train_data, perc.over = 200, perc.under = 100)

# Define the train control for k-fold cross-validation
train_control <- trainControl(method = "cv",
                              number = 10,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary,
                              savePredictions = "final")

# Tuning grid for Random Forest
tune_grid <- expand.grid(mtry = c(2, 3))

# Train the Random Forest model with cross-validation on the SMOTE balanced data
set.seed(7896)
rf_model_cv <- caret::train(v_vul_presence ~ ., data = train_data_smote,
                            method = "rf",
                            metric = "ROC",
                            trControl = train_control,
                            tuneGrid = tune_grid,
                            ntree = 500)

rf_model_cv$bestTune

# Evaluate the model's performance on the test data
predicted_class <- predict(rf_model_cv, test_data)
conf_matrix <- confusionMatrix(predicted_class, test_data$v_vul_presence)
print(conf_matrix)

# Plot the ROC curve and calculate AUC for the test data
predicted_prob <- predict(rf_model_cv, test_data, type = "prob")[, "Present"]
roc_curve <- roc(test_data$v_vul_presence, predicted_prob)
plot(roc_curve, col = "blue", main = "ROC Curve")
auc_value_ulf <- auc(roc_curve)
print(auc_value_ulf)  # Area Under the Curve (AUC)

# Adjust the thresh0ld to fix false positives
threshold <- 0.656
predicted_class_threshold <- ifelse(predicted_prob > threshold, "Present", "Absent")
conf_matrix_ulf <- confusionMatrix(as.factor(predicted_class_threshold), test_data$v_vul_presence)
print(conf_matrix_ulf)

# Create results_manual dataframe with actual and predicted data
results_manual <- data.frame(
  Date = test_data_with_date$date,
  Week = week(test_data_with_date$date),
  Actual = test_data$v_vul_presence,
  Predicted_Class = predicted_class_threshold,
  Predicted_probability = predicted_prob
) %>%
  mutate(
    Category = case_when(
      Actual == "Present" & Predicted_Class == "Present" ~ "True Positive",
      Actual == "Absent" & Predicted_Class == "Absent" ~ "True Negative",
      Actual == "Absent" & Predicted_Class == "Present" ~ "False Positive",
      Actual == "Present" & Predicted_Class == "Absent" ~ "False Negative"
    )
  ) %>%
  arrange(Date)  # Sort by Date to ensure proper alignment


# Print the results_manual to verify
print(results_manual)


# Step 3: Group by Week and Category to count occurrences
weekly_summary <- results_manual %>%
  group_by(Week, Category) %>%
  dplyr::summarise(Count = n()) %>%
  ungroup()

# Step 4: Create stacked bar chart with ggplot
ggplot(weekly_summary, aes(x = Week, y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Prediction Performance by Week", x = "Week", y = "Count") +
  scale_fill_manual(values = c("True Positive" = "green", "True Negative" = "blue",
                               "False Positive" = "red", "False Negative" = "orange")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the importance of predictors
importance_values_reg <- varImp(rf_model_cv, scale = FALSE)

print("Variable Importance for Regression Model:")
print(importance_values_reg)

# Visualize the top 10 important variables for the Regression Model
importance_reg_df <- as.data.frame(importance_values_reg$importance)
importance_reg_df$Variables <- rownames(importance_reg_df)
importance_reg_df <- importance_reg_df %>% arrange(desc(Overall))

# Top 10 important variables for Regression Model
top_10_time_lag_0 <- ggplot(importance_reg_df[1:10, ], aes(x = reorder(Variables, Overall), y = Overall)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Important Variables (Regression Model)", x = "Variables", y = "Importance") +
  theme_minimal()

top_10_otus_ulf <- importance_reg_df[1:10, ]

test <- test_data

test$v_vul_presence <- ifelse(test_data$v_vul_presence == "Present", 1, 0)

# Get predicted probabilities
predicted_prob <- predict(rf_model_cv, test, type = "prob")[, "Present"]

# Compute Precision-Recall Curve
pr_curve <- pr.curve(scores.class0 = predicted_prob, weights.class0 = test$v_vul_presence, curve = TRUE)

# Extract curve data
pr_data <- data.frame(
  Recall = pr_curve$curve[, 1],   # Recall
  Precision = pr_curve$curve[, 2] # Precision
)

# Create a ggplot object for the PR curve
pr_plot_ulf <- ggplot(pr_data, aes(x = Recall, y = Precision)) +
  geom_line(color = "blue", linewidth =2) +
  ggtitle(paste("Precision-Recall Curve (AUPR =", round(pr_curve$auc.integral, 3), ")")) +
  xlab("Recall") +
  ylab("Precision") +
  theme_minimal()

# Print the plot (optional)
print(pr_plot_ulf)

pr_value_ulf <-pr_curve$auc.integral


##### for curves


# Store AUC values in a vector

pr_value_values <- c(pr_value_all, pr_value_otc, pr_value_16s, pr_value_18s, pr_value_cop)

# Create a data frame
# Define custom labels for the x-axis
custom_labels <- c("All data", "Physical data", "16S data", "18S data", "Copernicus data")  # Adjust labels as needed

# Create a data frame
pr_value_data <- data.frame(
  Model = factor(custom_labels, levels = custom_labels),  # Custom x-axis labels
  AUC = pr_value_values  # Y-axis (AUC values)
)

# Create AUC plot
pr_value_plot <- ggplot(pr_value_data, aes(x = Model, y = AUC)) +
  geom_point(size = 3, color = "blue") +   
  geom_line(group = 1, color = "blue") +   
  ggtitle("AUPRC for Random Forest Time-Lag 0-10") +
  xlab("Time-lag") +
  ylab("Area Under the Precision-Recall Curve") +
  ylim(0, 1) +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Print the plot
print(pr_value_plot)


# Store AUC values in a vector
auc_value_values <- c(auc_value_all, auc_value_otc, auc_value_16s, auc_value_18s, 
                      auc_value_cop)
# Define custom labels for the x-axis
custom_labels <- c("All data", "Physical data", "16S data", "18S data", "Copernicus data")  # Adjust labels as needed

# Create a data frame
auc_value_data <- data.frame(
  Model = factor(custom_labels, levels = custom_labels),  # Custom x-axis labels
  AUC = auc_value_values  # Y-axis (AUC values)
)

# Ensure both plots have the same x-axis labels and formatting
pr_value_plot <- ggplot(pr_value_data, aes(x = Model, y = AUC)) +
  geom_point(size = 3, color = "blue") +   
  geom_line(group = 1, color = "blue") +   
  ggtitle("AUPRC for Random Forest Time-Lag 0-10") +
  xlab("Time-lag") +
  ylab("AUPRC") +
  ylim(0.3, 0.65) +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

auc_value_plot <- ggplot(auc_value_data, aes(x = Model, y = AUC)) +
  geom_point(size = 3, color = "red") +   
  geom_line(group = 1, color = "red") +   
  ggtitle("ROC-AUC for Random Forest Time-Lag 0-10") +
  xlab("Time-lag") +
  ylab("AUROC") +
  ylim(0.9, 0.97) +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Combine the two plots in a single figure
final_plot <- pr_value_plot / auc_value_plot  # Places them vertically

# Print the combined plot
print(final_plot)

################################## most important parameters graph

# Rename each plot clearly
p1 <- top_10_all
p2 <- top_10_all_all_loc
p3 <- top_10_16s
p4 <- top_10_16s_all
p5 <- top_10_18s
p6 <- top_10_18s_all
p7 <- top_10_otc_plot
p8 <- top_10_otc_all_plot
p9 <- top_10_copernicus_plot

combined_plot <- ( plot_spacer()| p9  ) /
  (p2 | p1 ) /
  (p4 | p3 ) /
  (p6 | p5 ) /
  (p8 | p7 )

print(combined_plot)



# Define test set names
test_sets <- c("copernicus", "16s", "16s_all", "18s", "18s_all", 
               "all", "all_all", "otc", "otc_all")

# Define AUPRC and ROC AUC values
auprc_values <- c(
  pr_value_cop, pr_value_16s, pr_value_16s_all_loc, 
  pr_value_18s, pr_value_18s_all_loc, pr_value_all, 
  pr_value_all_all_loc, pr_value_otc, pr_value_otc_all_loc
)

rocauc_values <- c(
  auc_value_copernicus, auc_value_16s, auc_value_16s_all, 
  auc_value_18s, auc_value_18s_all, auc_value_all, 
  auc_value_all_all, auc_value_otc, auc_value_otc_all
)

# Create the data frame
summary_table <- data.frame(
  Dataset = test_sets,
  Test_AUPRC = round(auprc_values, 3),
  Test_ROC_AUC = round(rocauc_values, 3)
)

# Print the table
print(summary_table)

# Save to CSV
write.csv(summary_table, "rf_summary_table.csv", row.names = FALSE)

##### save all otu tables
# 
# write.csv(
#   top_10_otus_16s,
#   "data_all/excel_jmp_tables/16s_rf_top_otus.csv",
#   row.names = TRUE
# )
# 
# write.csv(
#   top_10_otus_18s,
#   "data_all/excel_jmp_tables/18s_rf_top_otus.csv",
#   row.names = TRUE
# )
# 
# write.csv(
#   top_10_otus_all,
#   "data_all/excel_jmp_tables/all_rf_top_otus.csv",
#   row.names = TRUE
# )
# 
# write.csv(
#   top_10_otus_copernicus,
#   "data_all/excel_jmp_tables/copernicus_rf_top_otus.csv",
#   row.names = TRUE
# )
# 
# write.csv(
#   top_10_otus_otc,
#   "data_all/excel_jmp_tables/otc_rf_top_otus.csv",
#   row.names = TRUE
# )
# 
