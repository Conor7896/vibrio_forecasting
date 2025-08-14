
# Name: Conor Glackin
# Date: 25 Feb 2025
# Description: random forest models for all timepoints

set.seed(7896)

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
  df <- df[order(df$year, df$week, df$day), ]
  
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


otu_16s_filtered_rel_rf <- subset(otu_16s_filtered_rel_rf, location %in%  
                                    c("1", "2", "3", "4", "5", "6", "7"))
# 
# 
# # Subset rows after 09-05-22 and before 28-09-22
# otu_16s_filtered_rel_rf <- otu_16s_filtered_rel_rf[otu_16s_filtered_rel_rf$date > as.Date("2022-06-20") & 
#                                                      otu_16s_filtered_rel_rf$date < as.Date("2022-09-14"), ]
# 
# #### trying cross validation

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
  select((!c("v_vul_presence", "sample", "year", "week", "day",
             "Year", "Week", "Monday_Date", "Day_Shift")))
# Remove 'sample' from the predictors for training
#data_for_model <- data %>% select(-sample)

# Check the class distribution
table(y)

# Manually define validation and test location(s)
val_locations <- c(1, 2, 3, 4, 5, 6)
test_locations <- c(7)
mtry_vals <- c(2, 3)

# Prepare dataset
data_rf <- data

# Initialize storage
val_auprcs <- c()
val_roc_aucs <- c()
test_auprcs <- c()
test_roc_aucs <- c()
models_list <- list()
best_model <- NULL
best_val_score <- -Inf
skipped_locs <- c()

# Loop over each validation location
for (val_loc in train_locations) {
  cat("\nValidating on location:", val_loc, "\n")
  
  train_data <- data_rf[data_rf$location %in% train_locations & data_rf$location != val_loc, ]
  val_data   <- data_rf[data_rf$location == val_loc, ]
  test_data  <- data_rf[data_rf$location %in% test_locations, ]
  
  train_data_model <- train_data[, !(names(train_data) %in% c("sample", "date", "location"))]
  train_data_smote <- SMOTE(v_vul_presence ~ ., data = train_data_model, perc.over = 200, perc.under = 100)
  
  val_data_model <- val_data[, !(names(val_data) %in% c("sample", "date", "location"))]
  test_data_model <- test_data[, !(names(test_data) %in% c("sample", "date", "location"))]
  
  best_fold_model <- NULL
  best_fold_score <- -Inf
  best_fold_auc <- NA
  
  val_binary_check <- ifelse(val_data_model$v_vul_presence == "Present", 1, 0)
  if (sum(val_binary_check) == 0) {
    warning(paste("Skipping location", val_loc, "— no 'Present' samples"))
    skipped_locs <- c(skipped_locs, val_loc)
    next
  }
  
  for (mtry_val in mtry_vals) {
    rf_model <- caret::train(v_vul_presence ~ ., data = train_data_smote,
                             method = "rf",
                             metric = "ROC",
                             trControl = trainControl(method = "none", classProbs = TRUE),
                             tuneGrid = data.frame(mtry = mtry_val),
                             ntree = 500)
    
    val_probs <- predict(rf_model, val_data_model, type = "prob")[, "Present"]
    val_binary <- ifelse(val_data_model$v_vul_presence == "Present", 1, 0)
    
    val_pr <- pr.curve(scores.class0 = val_probs, weights.class0 = val_binary, curve = FALSE)
    val_auprc <- val_pr$auc.integral
    
    val_auc <- as.numeric(pROC::auc(val_binary, val_probs))
    
    if (val_auprc > best_fold_score) {
      best_fold_score <- val_auprc
      best_fold_model <- rf_model
      best_fold_auc <- val_auc
    }
  }
  
  val_auprcs <- c(val_auprcs, best_fold_score)
  val_roc_aucs <- c(val_roc_aucs, best_fold_auc)
  models_list[[as.character(val_loc)]] <- best_fold_model
  
  if (best_fold_score > best_val_score) {
    best_val_score <- best_fold_score
    best_model <- best_fold_model
  }
  
  # Test evaluation using best model from this fold
  test_probs <- predict(best_fold_model, test_data_model, type = "prob")[, "Present"]
  test_binary <- ifelse(test_data_model$v_vul_presence == "Present", 1, 0)
  
  test_pr <- pr.curve(scores.class0 = test_probs, weights.class0 = test_binary, curve = FALSE)
  test_auprc <- test_pr$auc.integral
  test_auc <- as.numeric(pROC::auc(test_binary, test_probs))
  
  test_auprcs <- c(test_auprcs, test_auprc)
  test_roc_aucs <- c(test_roc_aucs, test_auc)
}

# 📊 Summary output
cat("\n📊 Validation AUPRCs (by location):", round(val_auprcs, 3), "\n")
cat("📈 Average Validation AUPRC:", round(mean(val_auprcs), 3), "\n")
cat("📉 Average Validation ROC-AUC:", round(mean(val_roc_aucs, na.rm = TRUE), 3), "\n")
cat("🧪 Average Test AUPRC:", round(mean(test_auprcs), 3), "\n")
cat("🧪 Average Test ROC-AUC:", round(mean(test_roc_aucs, na.rm = TRUE), 3), "\n")
cat("⚠️ Skipped validation locations:", paste(skipped_locs, collapse = ", "), "\n")

avg_val_auprc_16s_0 <- mean(val_auprcs)
avg_test_auprc_16s_0 <- mean(test_auprcs)
avg_test_rocauc_16s_0 <- mean(test_roc_aucs, na.rm = TRUE)


final_test_probs <- predict(best_model, test_data_model, type = "prob")[, "Present"]
final_test_labels <- test_data_model$v_vul_presence
final_binary <- ifelse(final_test_labels == "Present", 1, 0)
final_pr <- pr.curve(scores.class0 = final_test_probs, weights.class0 = final_binary, curve = TRUE)
final_auprc <- final_pr$auc.integral

cat("🎯 Final Test AUPRC (best model from validation):", round(final_auprc, 3), "\n")

pr_data <- data.frame(Recall = final_pr$curve[, 1], Precision = final_pr$curve[, 2])
ggplot(pr_data, aes(x = Recall, y = Precision)) +
  geom_line(color = "darkblue", linewidth = 1.5) +
  labs(title = paste("Final PR Curve (AUPRC =", round(final_auprc, 3), ")"),
       x = "Recall", y = "Precision") +
  theme_minimal()

#################### time lag 1 
set.seed(7896)

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
  df <- df[order(df$year, df$week, df$day), ]
  
  # Shift the sample column up by two rows
  df$sample_shifted <- c(rep(NA, 1), df$sample[1:(nrow(df) - 1)])
  
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


otu_16s_filtered_rel_rf <- subset(otu_16s_filtered_rel_rf, location %in%  
                                    c("1", "2", "3", "4", "5", "6", "7"))
# 
# 
# # Subset rows after 09-05-22 and before 28-09-22
# otu_16s_filtered_rel_rf <- otu_16s_filtered_rel_rf[otu_16s_filtered_rel_rf$date > as.Date("2022-06-20") & 
#                                                      otu_16s_filtered_rel_rf$date < as.Date("2022-09-14"), ]
# 
# #### trying cross validation

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
  select((!c("v_vul_presence", "sample", "year", "week", "day",
             "Year", "Week", "Monday_Date", "Day_Shift")))
# Remove 'sample' from the predictors for training
#data_for_model <- data %>% select(-sample)

# Check the class distribution
table(y)

# Manually define validation and test location(s)
val_locations <- c(1, 2, 3, 4, 5, 6)
test_locations <- c(7)
mtry_vals <- c(2, 3)

# Prepare dataset
data_rf <- data

# Initialize storage
val_auprcs <- c()
val_roc_aucs <- c()
test_auprcs <- c()
test_roc_aucs <- c()
models_list <- list()
best_model <- NULL
best_val_score <- -Inf
skipped_locs <- c()

# Loop over each validation location
for (val_loc in train_locations) {
  cat("\nValidating on location:", val_loc, "\n")
  
  train_data <- data_rf[data_rf$location %in% train_locations & data_rf$location != val_loc, ]
  val_data   <- data_rf[data_rf$location == val_loc, ]
  test_data  <- data_rf[data_rf$location %in% test_locations, ]
  
  train_data_model <- train_data[, !(names(train_data) %in% c("sample", "date", "location"))]
  train_data_smote <- SMOTE(v_vul_presence ~ ., data = train_data_model, perc.over = 200, perc.under = 100)
  
  val_data_model <- val_data[, !(names(val_data) %in% c("sample", "date", "location"))]
  test_data_model <- test_data[, !(names(test_data) %in% c("sample", "date", "location"))]
  
  best_fold_model <- NULL
  best_fold_score <- -Inf
  best_fold_auc <- NA
  
  val_binary_check <- ifelse(val_data_model$v_vul_presence == "Present", 1, 0)
  if (sum(val_binary_check) == 0) {
    warning(paste("Skipping location", val_loc, "— no 'Present' samples"))
    skipped_locs <- c(skipped_locs, val_loc)
    next
  }
  
  for (mtry_val in mtry_vals) {
    rf_model <- caret::train(v_vul_presence ~ ., data = train_data_smote,
                             method = "rf",
                             metric = "ROC",
                             trControl = trainControl(method = "none", classProbs = TRUE),
                             tuneGrid = data.frame(mtry = mtry_val),
                             ntree = 500)
    
    val_probs <- predict(rf_model, val_data_model, type = "prob")[, "Present"]
    val_binary <- ifelse(val_data_model$v_vul_presence == "Present", 1, 0)
    
    val_pr <- pr.curve(scores.class0 = val_probs, weights.class0 = val_binary, curve = FALSE)
    val_auprc <- val_pr$auc.integral
    
    val_auc <- as.numeric(pROC::auc(val_binary, val_probs))
    
    if (val_auprc > best_fold_score) {
      best_fold_score <- val_auprc
      best_fold_model <- rf_model
      best_fold_auc <- val_auc
    }
  }
  
  val_auprcs <- c(val_auprcs, best_fold_score)
  val_roc_aucs <- c(val_roc_aucs, best_fold_auc)
  models_list[[as.character(val_loc)]] <- best_fold_model
  
  if (best_fold_score > best_val_score) {
    best_val_score <- best_fold_score
    best_model <- best_fold_model
  }
  
  # Test evaluation using best model from this fold
  test_probs <- predict(best_fold_model, test_data_model, type = "prob")[, "Present"]
  test_binary <- ifelse(test_data_model$v_vul_presence == "Present", 1, 0)
  
  test_pr <- pr.curve(scores.class0 = test_probs, weights.class0 = test_binary, curve = FALSE)
  test_auprc <- test_pr$auc.integral
  test_auc <- as.numeric(pROC::auc(test_binary, test_probs))
  
  test_auprcs <- c(test_auprcs, test_auprc)
  test_roc_aucs <- c(test_roc_aucs, test_auc)
}

# 📊 Summary output
cat("\n📊 Validation AUPRCs (by location):", round(val_auprcs, 3), "\n")
cat("📈 Average Validation AUPRC:", round(mean(val_auprcs), 3), "\n")
cat("📉 Average Validation ROC-AUC:", round(mean(val_roc_aucs, na.rm = TRUE), 3), "\n")
cat("🧪 Average Test AUPRC:", round(mean(test_auprcs), 3), "\n")
cat("🧪 Average Test ROC-AUC:", round(mean(test_roc_aucs, na.rm = TRUE), 3), "\n")
cat("⚠️ Skipped validation locations:", paste(skipped_locs, collapse = ", "), "\n")

avg_val_auprc_16s_1 <- mean(val_auprcs)
avg_test_auprc_16s_1 <- mean(test_auprcs)
avg_test_rocauc_16s_1 <- mean(test_roc_aucs, na.rm = TRUE)


final_test_probs <- predict(best_model, test_data_model, type = "prob")[, "Present"]
final_test_labels <- test_data_model$v_vul_presence
final_binary <- ifelse(final_test_labels == "Present", 1, 0)
final_pr <- pr.curve(scores.class0 = final_test_probs, weights.class0 = final_binary, curve = TRUE)
final_auprc <- final_pr$auc.integral

cat("🎯 Final Test AUPRC (best model from validation):", round(final_auprc, 3), "\n")

pr_data <- data.frame(Recall = final_pr$curve[, 1], Precision = final_pr$curve[, 2])
ggplot(pr_data, aes(x = Recall, y = Precision)) +
  geom_line(color = "darkblue", linewidth = 1.5) +
  labs(title = paste("Final PR Curve (AUPRC =", round(final_auprc, 3), ")"),
       x = "Recall", y = "Precision") +
  theme_minimal()


#################### time lag 2
set.seed(7896)

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
  df <- df[order(df$year, df$week, df$day), ]
  
  # Shift the sample column up by two rows
  df$sample_shifted <- c(rep(NA, 2), df$sample[1:(nrow(df) - 2)])
  
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


otu_16s_filtered_rel_rf <- subset(otu_16s_filtered_rel_rf, location %in%  
                                    c("1", "2", "3", "4", "5", "6", "7"))
# 
# 
# # Subset rows after 09-05-22 and before 28-09-22
# otu_16s_filtered_rel_rf <- otu_16s_filtered_rel_rf[otu_16s_filtered_rel_rf$date > as.Date("2022-06-20") & 
#                                                      otu_16s_filtered_rel_rf$date < as.Date("2022-09-14"), ]
# 
# #### trying cross validation

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
  select((!c("v_vul_presence", "sample", "year", "week", "day",
             "Year", "Week", "Monday_Date", "Day_Shift")))
# Remove 'sample' from the predictors for training
#data_for_model <- data %>% select(-sample)

# Check the class distribution
table(y)

# Manually define validation and test location(s)
val_locations <- c(1, 2, 3, 4, 5, 6)
test_locations <- c(7)
mtry_vals <- c(2, 3)

# Prepare dataset
data_rf <- data

# Initialize storage
val_auprcs <- c()
val_roc_aucs <- c()
test_auprcs <- c()
test_roc_aucs <- c()
models_list <- list()
best_model <- NULL
best_val_score <- -Inf
skipped_locs <- c()

# Loop over each validation location
for (val_loc in train_locations) {
  cat("\nValidating on location:", val_loc, "\n")
  
  train_data <- data_rf[data_rf$location %in% train_locations & data_rf$location != val_loc, ]
  val_data   <- data_rf[data_rf$location == val_loc, ]
  test_data  <- data_rf[data_rf$location %in% test_locations, ]
  
  train_data_model <- train_data[, !(names(train_data) %in% c("sample", "date", "location"))]
  train_data_smote <- SMOTE(v_vul_presence ~ ., data = train_data_model, perc.over = 200, perc.under = 100)
  
  val_data_model <- val_data[, !(names(val_data) %in% c("sample", "date", "location"))]
  test_data_model <- test_data[, !(names(test_data) %in% c("sample", "date", "location"))]
  
  best_fold_model <- NULL
  best_fold_score <- -Inf
  best_fold_auc <- NA
  
  val_binary_check <- ifelse(val_data_model$v_vul_presence == "Present", 1, 0)
  if (sum(val_binary_check) == 0) {
    warning(paste("Skipping location", val_loc, "— no 'Present' samples"))
    skipped_locs <- c(skipped_locs, val_loc)
    next
  }
  
  for (mtry_val in mtry_vals) {
    rf_model <- caret::train(v_vul_presence ~ ., data = train_data_smote,
                             method = "rf",
                             metric = "ROC",
                             trControl = trainControl(method = "none", classProbs = TRUE),
                             tuneGrid = data.frame(mtry = mtry_val),
                             ntree = 500)
    
    val_probs <- predict(rf_model, val_data_model, type = "prob")[, "Present"]
    val_binary <- ifelse(val_data_model$v_vul_presence == "Present", 1, 0)
    
    val_pr <- pr.curve(scores.class0 = val_probs, weights.class0 = val_binary, curve = FALSE)
    val_auprc <- val_pr$auc.integral
    
    val_auc <- as.numeric(pROC::auc(val_binary, val_probs))
    
    if (val_auprc > best_fold_score) {
      best_fold_score <- val_auprc
      best_fold_model <- rf_model
      best_fold_auc <- val_auc
    }
  }
  
  val_auprcs <- c(val_auprcs, best_fold_score)
  val_roc_aucs <- c(val_roc_aucs, best_fold_auc)
  models_list[[as.character(val_loc)]] <- best_fold_model
  
  if (best_fold_score > best_val_score) {
    best_val_score <- best_fold_score
    best_model <- best_fold_model
  }
  
  # Test evaluation using best model from this fold
  test_probs <- predict(best_fold_model, test_data_model, type = "prob")[, "Present"]
  test_binary <- ifelse(test_data_model$v_vul_presence == "Present", 1, 0)
  
  test_pr <- pr.curve(scores.class0 = test_probs, weights.class0 = test_binary, curve = FALSE)
  test_auprc <- test_pr$auc.integral
  test_auc <- as.numeric(pROC::auc(test_binary, test_probs))
  
  test_auprcs <- c(test_auprcs, test_auprc)
  test_roc_aucs <- c(test_roc_aucs, test_auc)
}

# 📊 Summary output
cat("\n📊 Validation AUPRCs (by location):", round(val_auprcs, 3), "\n")
cat("📈 Average Validation AUPRC:", round(mean(val_auprcs), 3), "\n")
cat("📉 Average Validation ROC-AUC:", round(mean(val_roc_aucs, na.rm = TRUE), 3), "\n")
cat("🧪 Average Test AUPRC:", round(mean(test_auprcs), 3), "\n")
cat("🧪 Average Test ROC-AUC:", round(mean(test_roc_aucs, na.rm = TRUE), 3), "\n")
cat("⚠️ Skipped validation locations:", paste(skipped_locs, collapse = ", "), "\n")

avg_val_auprc_16s_2 <- mean(val_auprcs)
avg_test_auprc_16s_2 <- mean(test_auprcs)
avg_test_rocauc_16s_2 <- mean(test_roc_aucs, na.rm = TRUE)

final_test_probs <- predict(best_model, test_data_model, type = "prob")[, "Present"]
final_test_labels <- test_data_model$v_vul_presence
final_binary <- ifelse(final_test_labels == "Present", 1, 0)
final_pr <- pr.curve(scores.class0 = final_test_probs, weights.class0 = final_binary, curve = TRUE)
final_auprc <- final_pr$auc.integral

cat("🎯 Final Test AUPRC (best model from validation):", round(final_auprc, 3), "\n")

pr_data <- data.frame(Recall = final_pr$curve[, 1], Precision = final_pr$curve[, 2])
ggplot(pr_data, aes(x = Recall, y = Precision)) +
  geom_line(color = "darkblue", linewidth = 1.5) +
  labs(title = paste("Final PR Curve (AUPRC =", round(final_auprc, 3), ")"),
       x = "Recall", y = "Precision") +
  theme_minimal()



#################### time lag 3 
set.seed(7896)

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
  df <- df[order(df$year, df$week, df$day), ]
  
  # Shift the sample column up by two rows
  df$sample_shifted <- c(rep(NA, 3), df$sample[1:(nrow(df) - 3)])
  
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


otu_16s_filtered_rel_rf <- subset(otu_16s_filtered_rel_rf, location %in%  
                                    c("1", "2", "3", "4", "5", "6", "7"))
# 
# 
# # Subset rows after 09-05-22 and before 28-09-22
# otu_16s_filtered_rel_rf <- otu_16s_filtered_rel_rf[otu_16s_filtered_rel_rf$date > as.Date("2022-06-20") & 
#                                                      otu_16s_filtered_rel_rf$date < as.Date("2022-09-14"), ]
# 
# #### trying cross validation

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
  select((!c("v_vul_presence", "sample", "year", "week", "day",
             "Year", "Week", "Monday_Date", "Day_Shift")))
# Remove 'sample' from the predictors for training
#data_for_model <- data %>% select(-sample)

# Check the class distribution
table(y)

# Manually define validation and test location(s)
val_locations <- c(1, 2, 3, 4, 5, 6)
test_locations <- c(7)
mtry_vals <- c(2, 3)

# Prepare dataset
data_rf <- data

# Initialize storage
val_auprcs <- c()
val_roc_aucs <- c()
test_auprcs <- c()
test_roc_aucs <- c()
models_list <- list()
best_model <- NULL
best_val_score <- -Inf
skipped_locs <- c()

# Loop over each validation location
for (val_loc in train_locations) {
  cat("\nValidating on location:", val_loc, "\n")
  
  train_data <- data_rf[data_rf$location %in% train_locations & data_rf$location != val_loc, ]
  val_data   <- data_rf[data_rf$location == val_loc, ]
  test_data  <- data_rf[data_rf$location %in% test_locations, ]
  
  train_data_model <- train_data[, !(names(train_data) %in% c("sample", "date", "location"))]
  train_data_smote <- SMOTE(v_vul_presence ~ ., data = train_data_model, perc.over = 200, perc.under = 100)
  
  val_data_model <- val_data[, !(names(val_data) %in% c("sample", "date", "location"))]
  test_data_model <- test_data[, !(names(test_data) %in% c("sample", "date", "location"))]
  
  best_fold_model <- NULL
  best_fold_score <- -Inf
  best_fold_auc <- NA
  
  val_binary_check <- ifelse(val_data_model$v_vul_presence == "Present", 1, 0)
  if (sum(val_binary_check) == 0) {
    warning(paste("Skipping location", val_loc, "— no 'Present' samples"))
    skipped_locs <- c(skipped_locs, val_loc)
    next
  }
  
  for (mtry_val in mtry_vals) {
    rf_model <- caret::train(v_vul_presence ~ ., data = train_data_smote,
                             method = "rf",
                             metric = "ROC",
                             trControl = trainControl(method = "none", classProbs = TRUE),
                             tuneGrid = data.frame(mtry = mtry_val),
                             ntree = 500)
    
    val_probs <- predict(rf_model, val_data_model, type = "prob")[, "Present"]
    val_binary <- ifelse(val_data_model$v_vul_presence == "Present", 1, 0)
    
    val_pr <- pr.curve(scores.class0 = val_probs, weights.class0 = val_binary, curve = FALSE)
    val_auprc <- val_pr$auc.integral
    
    val_auc <- as.numeric(pROC::auc(val_binary, val_probs))
    
    if (val_auprc > best_fold_score) {
      best_fold_score <- val_auprc
      best_fold_model <- rf_model
      best_fold_auc <- val_auc
    }
  }
  
  val_auprcs <- c(val_auprcs, best_fold_score)
  val_roc_aucs <- c(val_roc_aucs, best_fold_auc)
  models_list[[as.character(val_loc)]] <- best_fold_model
  
  if (best_fold_score > best_val_score) {
    best_val_score <- best_fold_score
    best_model <- best_fold_model
  }
  
  # Test evaluation using best model from this fold
  test_probs <- predict(best_fold_model, test_data_model, type = "prob")[, "Present"]
  test_binary <- ifelse(test_data_model$v_vul_presence == "Present", 1, 0)
  
  test_pr <- pr.curve(scores.class0 = test_probs, weights.class0 = test_binary, curve = FALSE)
  test_auprc <- test_pr$auc.integral
  test_auc <- as.numeric(pROC::auc(test_binary, test_probs))
  
  test_auprcs <- c(test_auprcs, test_auprc)
  test_roc_aucs <- c(test_roc_aucs, test_auc)
}

# 📊 Summary output
cat("\n📊 Validation AUPRCs (by location):", round(val_auprcs, 3), "\n")
cat("📈 Average Validation AUPRC:", round(mean(val_auprcs), 3), "\n")
cat("📉 Average Validation ROC-AUC:", round(mean(val_roc_aucs, na.rm = TRUE), 3), "\n")
cat("🧪 Average Test AUPRC:", round(mean(test_auprcs), 3), "\n")
cat("🧪 Average Test ROC-AUC:", round(mean(test_roc_aucs, na.rm = TRUE), 3), "\n")
cat("⚠️ Skipped validation locations:", paste(skipped_locs, collapse = ", "), "\n")

avg_val_auprc_16s_3 <- mean(val_auprcs)
avg_test_auprc_16s_3 <- mean(test_auprcs)
avg_test_rocauc_16s_3 <- mean(test_roc_aucs, na.rm = TRUE)


final_test_probs <- predict(best_model, test_data_model, type = "prob")[, "Present"]
final_test_labels <- test_data_model$v_vul_presence
final_binary <- ifelse(final_test_labels == "Present", 1, 0)
final_pr <- pr.curve(scores.class0 = final_test_probs, weights.class0 = final_binary, curve = TRUE)
final_auprc <- final_pr$auc.integral

cat("🎯 Final Test AUPRC (best model from validation):", round(final_auprc, 3), "\n")

pr_data <- data.frame(Recall = final_pr$curve[, 1], Precision = final_pr$curve[, 2])
ggplot(pr_data, aes(x = Recall, y = Precision)) +
  geom_line(color = "darkblue", linewidth = 1.5) +
  labs(title = paste("Final PR Curve (AUPRC =", round(final_auprc, 3), ")"),
       x = "Recall", y = "Precision") +
  theme_minimal()



#################### time lag 4 
set.seed(7896)

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
  df <- df[order(df$year, df$week, df$day), ]
  
  # Shift the sample column up by two rows
  df$sample_shifted <- c(rep(NA, 4), df$sample[1:(nrow(df) - 4)])
  
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


otu_16s_filtered_rel_rf <- subset(otu_16s_filtered_rel_rf, location %in%  
                                    c("1", "2", "3", "4", "5", "6", "7"))
# 
# 
# # Subset rows after 09-05-22 and before 28-09-22
# otu_16s_filtered_rel_rf <- otu_16s_filtered_rel_rf[otu_16s_filtered_rel_rf$date > as.Date("2022-06-20") & 
#                                                      otu_16s_filtered_rel_rf$date < as.Date("2022-09-14"), ]
# 
# #### trying cross validation

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
  select((!c("v_vul_presence", "sample", "year", "week", "day",
             "Year", "Week", "Monday_Date", "Day_Shift")))
# Remove 'sample' from the predictors for training
#data_for_model <- data %>% select(-sample)

# Check the class distribution
table(y)

# Manually define validation and test location(s)
val_locations <- c(1, 2, 3, 4, 5, 6)
test_locations <- c(7)
mtry_vals <- c(2, 3)

# Prepare dataset
data_rf <- data

# Initialize storage
val_auprcs <- c()
val_roc_aucs <- c()
test_auprcs <- c()
test_roc_aucs <- c()
models_list <- list()
best_model <- NULL
best_val_score <- -Inf
skipped_locs <- c()

# Loop over each validation location
for (val_loc in train_locations) {
  cat("\nValidating on location:", val_loc, "\n")
  
  train_data <- data_rf[data_rf$location %in% train_locations & data_rf$location != val_loc, ]
  val_data   <- data_rf[data_rf$location == val_loc, ]
  test_data  <- data_rf[data_rf$location %in% test_locations, ]
  
  train_data_model <- train_data[, !(names(train_data) %in% c("sample", "date", "location"))]
  train_data_smote <- SMOTE(v_vul_presence ~ ., data = train_data_model, perc.over = 200, perc.under = 100)
  
  val_data_model <- val_data[, !(names(val_data) %in% c("sample", "date", "location"))]
  test_data_model <- test_data[, !(names(test_data) %in% c("sample", "date", "location"))]
  
  best_fold_model <- NULL
  best_fold_score <- -Inf
  best_fold_auc <- NA
  
  val_binary_check <- ifelse(val_data_model$v_vul_presence == "Present", 1, 0)
  if (sum(val_binary_check) == 0) {
    warning(paste("Skipping location", val_loc, "— no 'Present' samples"))
    skipped_locs <- c(skipped_locs, val_loc)
    next
  }
  
  for (mtry_val in mtry_vals) {
    rf_model <- caret::train(v_vul_presence ~ ., data = train_data_smote,
                             method = "rf",
                             metric = "ROC",
                             trControl = trainControl(method = "none", classProbs = TRUE),
                             tuneGrid = data.frame(mtry = mtry_val),
                             ntree = 500)
    
    val_probs <- predict(rf_model, val_data_model, type = "prob")[, "Present"]
    val_binary <- ifelse(val_data_model$v_vul_presence == "Present", 1, 0)
    
    val_pr <- pr.curve(scores.class0 = val_probs, weights.class0 = val_binary, curve = FALSE)
    val_auprc <- val_pr$auc.integral
    
    val_auc <- as.numeric(pROC::auc(val_binary, val_probs))
    
    if (val_auprc > best_fold_score) {
      best_fold_score <- val_auprc
      best_fold_model <- rf_model
      best_fold_auc <- val_auc
    }
  }
  
  val_auprcs <- c(val_auprcs, best_fold_score)
  val_roc_aucs <- c(val_roc_aucs, best_fold_auc)
  models_list[[as.character(val_loc)]] <- best_fold_model
  
  if (best_fold_score > best_val_score) {
    best_val_score <- best_fold_score
    best_model <- best_fold_model
  }
  
  # Test evaluation using best model from this fold
  test_probs <- predict(best_fold_model, test_data_model, type = "prob")[, "Present"]
  test_binary <- ifelse(test_data_model$v_vul_presence == "Present", 1, 0)
  
  test_pr <- pr.curve(scores.class0 = test_probs, weights.class0 = test_binary, curve = FALSE)
  test_auprc <- test_pr$auc.integral
  test_auc <- as.numeric(pROC::auc(test_binary, test_probs))
  
  test_auprcs <- c(test_auprcs, test_auprc)
  test_roc_aucs <- c(test_roc_aucs, test_auc)
}

# 📊 Summary output
cat("\n📊 Validation AUPRCs (by location):", round(val_auprcs, 3), "\n")
cat("📈 Average Validation AUPRC:", round(mean(val_auprcs), 3), "\n")
cat("📉 Average Validation ROC-AUC:", round(mean(val_roc_aucs, na.rm = TRUE), 3), "\n")
cat("🧪 Average Test AUPRC:", round(mean(test_auprcs), 3), "\n")
cat("🧪 Average Test ROC-AUC:", round(mean(test_roc_aucs, na.rm = TRUE), 3), "\n")
cat("⚠️ Skipped validation locations:", paste(skipped_locs, collapse = ", "), "\n")

avg_val_auprc_16s_4 <- mean(val_auprcs)
avg_test_auprc_16s_4 <- mean(test_auprcs)
avg_test_rocauc_16s_4 <- mean(test_roc_aucs, na.rm = TRUE)


final_test_probs <- predict(best_model, test_data_model, type = "prob")[, "Present"]
final_test_labels <- test_data_model$v_vul_presence
final_binary <- ifelse(final_test_labels == "Present", 1, 0)
final_pr <- pr.curve(scores.class0 = final_test_probs, weights.class0 = final_binary, curve = TRUE)
final_auprc <- final_pr$auc.integral

cat("🎯 Final Test AUPRC (best model from validation):", round(final_auprc, 3), "\n")

pr_data <- data.frame(Recall = final_pr$curve[, 1], Precision = final_pr$curve[, 2])
ggplot(pr_data, aes(x = Recall, y = Precision)) +
  geom_line(color = "darkblue", linewidth = 1.5) +
  labs(title = paste("Final PR Curve (AUPRC =", round(final_auprc, 3), ")"),
       x = "Recall", y = "Precision") +
  theme_minimal()



#################### time lag 5 
set.seed(7896)

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
  df <- df[order(df$year, df$week, df$day), ]
  
  # Shift the sample column up by two rows
  df$sample_shifted <- c(rep(NA, 5), df$sample[1:(nrow(df) - 5)])
  
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


otu_16s_filtered_rel_rf <- subset(otu_16s_filtered_rel_rf, location %in%  
                                    c("1", "2", "3", "4", "5", "6", "7"))
# 
# 
# # Subset rows after 09-05-22 and before 28-09-22
# otu_16s_filtered_rel_rf <- otu_16s_filtered_rel_rf[otu_16s_filtered_rel_rf$date > as.Date("2022-06-20") & 
#                                                      otu_16s_filtered_rel_rf$date < as.Date("2022-09-14"), ]
# 
# #### trying cross validation

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
  select((!c("v_vul_presence", "sample", "year", "week", "day",
             "Year", "Week", "Monday_Date", "Day_Shift")))
# Remove 'sample' from the predictors for training
#data_for_model <- data %>% select(-sample)

# Check the class distribution
table(y)

# Manually define validation and test location(s)
val_locations <- c(1, 2, 3, 4, 5, 6)
test_locations <- c(7)
mtry_vals <- c(2, 3)

# Prepare dataset
data_rf <- data

# Initialize storage
val_auprcs <- c()
val_roc_aucs <- c()
test_auprcs <- c()
test_roc_aucs <- c()
models_list <- list()
best_model <- NULL
best_val_score <- -Inf
skipped_locs <- c()

# Loop over each validation location
for (val_loc in train_locations) {
  cat("\nValidating on location:", val_loc, "\n")
  
  train_data <- data_rf[data_rf$location %in% train_locations & data_rf$location != val_loc, ]
  val_data   <- data_rf[data_rf$location == val_loc, ]
  test_data  <- data_rf[data_rf$location %in% test_locations, ]
  
  train_data_model <- train_data[, !(names(train_data) %in% c("sample", "date", "location"))]
  train_data_smote <- SMOTE(v_vul_presence ~ ., data = train_data_model, perc.over = 200, perc.under = 100)
  
  val_data_model <- val_data[, !(names(val_data) %in% c("sample", "date", "location"))]
  test_data_model <- test_data[, !(names(test_data) %in% c("sample", "date", "location"))]
  
  best_fold_model <- NULL
  best_fold_score <- -Inf
  best_fold_auc <- NA
  
  val_binary_check <- ifelse(val_data_model$v_vul_presence == "Present", 1, 0)
  if (sum(val_binary_check) == 0) {
    warning(paste("Skipping location", val_loc, "— no 'Present' samples"))
    skipped_locs <- c(skipped_locs, val_loc)
    next
  }
  
  for (mtry_val in mtry_vals) {
    rf_model <- caret::train(v_vul_presence ~ ., data = train_data_smote,
                             method = "rf",
                             metric = "ROC",
                             trControl = trainControl(method = "none", classProbs = TRUE),
                             tuneGrid = data.frame(mtry = mtry_val),
                             ntree = 500)
    
    val_probs <- predict(rf_model, val_data_model, type = "prob")[, "Present"]
    val_binary <- ifelse(val_data_model$v_vul_presence == "Present", 1, 0)
    
    val_pr <- pr.curve(scores.class0 = val_probs, weights.class0 = val_binary, curve = FALSE)
    val_auprc <- val_pr$auc.integral
    
    val_auc <- as.numeric(pROC::auc(val_binary, val_probs))
    
    if (val_auprc > best_fold_score) {
      best_fold_score <- val_auprc
      best_fold_model <- rf_model
      best_fold_auc <- val_auc
    }
  }
  
  val_auprcs <- c(val_auprcs, best_fold_score)
  val_roc_aucs <- c(val_roc_aucs, best_fold_auc)
  models_list[[as.character(val_loc)]] <- best_fold_model
  
  if (best_fold_score > best_val_score) {
    best_val_score <- best_fold_score
    best_model <- best_fold_model
  }
  
  # Test evaluation using best model from this fold
  test_probs <- predict(best_fold_model, test_data_model, type = "prob")[, "Present"]
  test_binary <- ifelse(test_data_model$v_vul_presence == "Present", 1, 0)
  
  test_pr <- pr.curve(scores.class0 = test_probs, weights.class0 = test_binary, curve = FALSE)
  test_auprc <- test_pr$auc.integral
  test_auc <- as.numeric(pROC::auc(test_binary, test_probs))
  
  test_auprcs <- c(test_auprcs, test_auprc)
  test_roc_aucs <- c(test_roc_aucs, test_auc)
}

# 📊 Summary output
cat("\n📊 Validation AUPRCs (by location):", round(val_auprcs, 3), "\n")
cat("📈 Average Validation AUPRC:", round(mean(val_auprcs), 3), "\n")
cat("📉 Average Validation ROC-AUC:", round(mean(val_roc_aucs, na.rm = TRUE), 3), "\n")
cat("🧪 Average Test AUPRC:", round(mean(test_auprcs), 3), "\n")
cat("🧪 Average Test ROC-AUC:", round(mean(test_roc_aucs, na.rm = TRUE), 3), "\n")
cat("⚠️ Skipped validation locations:", paste(skipped_locs, collapse = ", "), "\n")

avg_val_auprc_16s_5 <- mean(val_auprcs)
avg_test_auprc_16s_5 <- mean(test_auprcs)
avg_test_rocauc_16s_5 <- mean(test_roc_aucs, na.rm = TRUE)


final_test_probs <- predict(best_model, test_data_model, type = "prob")[, "Present"]
final_test_labels <- test_data_model$v_vul_presence
final_binary <- ifelse(final_test_labels == "Present", 1, 0)
final_pr <- pr.curve(scores.class0 = final_test_probs, weights.class0 = final_binary, curve = TRUE)
final_auprc <- final_pr$auc.integral

cat("🎯 Final Test AUPRC (best model from validation):", round(final_auprc, 3), "\n")

pr_data <- data.frame(Recall = final_pr$curve[, 1], Precision = final_pr$curve[, 2])
ggplot(pr_data, aes(x = Recall, y = Precision)) +
  geom_line(color = "darkblue", linewidth = 1.5) +
  labs(title = paste("Final PR Curve (AUPRC =", round(final_auprc, 3), ")"),
       x = "Recall", y = "Precision") +
  theme_minimal()







#################### time lag 6 
set.seed(7896)

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
  df <- df[order(df$year, df$week, df$day), ]
  
  # Shift the sample column up by two rows
  df$sample_shifted <- c(rep(NA, 6), df$sample[1:(nrow(df) - 6)])
  
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


otu_16s_filtered_rel_rf <- subset(otu_16s_filtered_rel_rf, location %in%  
                                    c("1", "2", "3", "4", "5", "6", "7"))
# 
# 
# # Subset rows after 09-05-22 and before 28-09-22
# otu_16s_filtered_rel_rf <- otu_16s_filtered_rel_rf[otu_16s_filtered_rel_rf$date > as.Date("2022-06-20") & 
#                                                      otu_16s_filtered_rel_rf$date < as.Date("2022-09-14"), ]
# 
# #### trying cross validation

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
  select((!c("v_vul_presence", "sample", "year", "week", "day",
             "Year", "Week", "Monday_Date", "Day_Shift")))
# Remove 'sample' from the predictors for training
#data_for_model <- data %>% select(-sample)

# Check the class distribution
table(y)

# Manually define validation and test location(s)
val_locations <- c(1, 2, 3, 4, 5, 6)
test_locations <- c(7)
mtry_vals <- c(2, 3)

# Prepare dataset
data_rf <- data


# Initialize storage
val_auprcs <- c()
val_roc_aucs <- c()
test_auprcs <- c()
test_roc_aucs <- c()
models_list <- list()
best_model <- NULL
best_val_score <- -Inf
skipped_locs <- c()

# Loop over each validation location
for (val_loc in train_locations) {
  cat("\nValidating on location:", val_loc, "\n")
  
  train_data <- data_rf[data_rf$location %in% train_locations & data_rf$location != val_loc, ]
  val_data   <- data_rf[data_rf$location == val_loc, ]
  test_data  <- data_rf[data_rf$location %in% test_locations, ]
  
  train_data_model <- train_data[, !(names(train_data) %in% c("sample", "date", "location"))]
  train_data_smote <- SMOTE(v_vul_presence ~ ., data = train_data_model, perc.over = 200, perc.under = 100)
  
  val_data_model <- val_data[, !(names(val_data) %in% c("sample", "date", "location"))]
  test_data_model <- test_data[, !(names(test_data) %in% c("sample", "date", "location"))]
  
  best_fold_model <- NULL
  best_fold_score <- -Inf
  best_fold_auc <- NA
  
  val_binary_check <- ifelse(val_data_model$v_vul_presence == "Present", 1, 0)
  if (sum(val_binary_check) == 0) {
    warning(paste("Skipping location", val_loc, "— no 'Present' samples"))
    skipped_locs <- c(skipped_locs, val_loc)
    next
  }
  
  for (mtry_val in mtry_vals) {
    rf_model <- caret::train(v_vul_presence ~ ., data = train_data_smote,
                             method = "rf",
                             metric = "ROC",
                             trControl = trainControl(method = "none", classProbs = TRUE),
                             tuneGrid = data.frame(mtry = mtry_val),
                             ntree = 500)
    
    val_probs <- predict(rf_model, val_data_model, type = "prob")[, "Present"]
    val_binary <- ifelse(val_data_model$v_vul_presence == "Present", 1, 0)
    
    val_pr <- pr.curve(scores.class0 = val_probs, weights.class0 = val_binary, curve = FALSE)
    val_auprc <- val_pr$auc.integral
    
    val_auc <- as.numeric(pROC::auc(val_binary, val_probs))
    
    if (val_auprc > best_fold_score) {
      best_fold_score <- val_auprc
      best_fold_model <- rf_model
      best_fold_auc <- val_auc
    }
  }
  
  val_auprcs <- c(val_auprcs, best_fold_score)
  val_roc_aucs <- c(val_roc_aucs, best_fold_auc)
  models_list[[as.character(val_loc)]] <- best_fold_model
  
  if (best_fold_score > best_val_score) {
    best_val_score <- best_fold_score
    best_model <- best_fold_model
  }
  
  # Test evaluation using best model from this fold
  test_probs <- predict(best_fold_model, test_data_model, type = "prob")[, "Present"]
  test_binary <- ifelse(test_data_model$v_vul_presence == "Present", 1, 0)
  
  test_pr <- pr.curve(scores.class0 = test_probs, weights.class0 = test_binary, curve = FALSE)
  test_auprc <- test_pr$auc.integral
  test_auc <- as.numeric(pROC::auc(test_binary, test_probs))
  
  test_auprcs <- c(test_auprcs, test_auprc)
  test_roc_aucs <- c(test_roc_aucs, test_auc)
}

# 📊 Summary output
cat("\n📊 Validation AUPRCs (by location):", round(val_auprcs, 3), "\n")
cat("📈 Average Validation AUPRC:", round(mean(val_auprcs), 3), "\n")
cat("📉 Average Validation ROC-AUC:", round(mean(val_roc_aucs, na.rm = TRUE), 3), "\n")
cat("🧪 Average Test AUPRC:", round(mean(test_auprcs), 3), "\n")
cat("🧪 Average Test ROC-AUC:", round(mean(test_roc_aucs, na.rm = TRUE), 3), "\n")
cat("⚠️ Skipped validation locations:", paste(skipped_locs, collapse = ", "), "\n")

avg_val_auprc_16s_6 <- mean(val_auprcs)
avg_test_auprc_16s_6<- mean(test_auprcs)
avg_test_rocauc_16s_6 <- mean(test_roc_aucs, na.rm = TRUE)


final_test_probs <- predict(best_model, test_data_model, type = "prob")[, "Present"]
final_test_labels <- test_data_model$v_vul_presence
final_binary <- ifelse(final_test_labels == "Present", 1, 0)
final_pr <- pr.curve(scores.class0 = final_test_probs, weights.class0 = final_binary, curve = TRUE)
final_auprc <- final_pr$auc.integral

cat("🎯 Final Test AUPRC (best model from validation):", round(final_auprc, 3), "\n")

pr_data <- data.frame(Recall = final_pr$curve[, 1], Precision = final_pr$curve[, 2])
ggplot(pr_data, aes(x = Recall, y = Precision)) +
  geom_line(color = "darkblue", linewidth = 1.5) +
  labs(title = paste("Final PR Curve (AUPRC =", round(final_auprc, 3), ")"),
       x = "Recall", y = "Precision") +
  theme_minimal()


#################### time lag 7
set.seed(7896)

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
  df <- df[order(df$year, df$week, df$day), ]
  
  # Shift the sample column up by two rows
  df$sample_shifted <- c(rep(NA, 7), df$sample[1:(nrow(df) - 7)])
  
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


otu_16s_filtered_rel_rf <- subset(otu_16s_filtered_rel_rf, location %in%  
                                    c("1", "2", "3", "4", "5", "6", "7"))
# 
# 
# # Subset rows after 09-05-22 and before 28-09-22
# otu_16s_filtered_rel_rf <- otu_16s_filtered_rel_rf[otu_16s_filtered_rel_rf$date > as.Date("2022-06-20") & 
#                                                      otu_16s_filtered_rel_rf$date < as.Date("2022-09-14"), ]
# 
# #### trying cross validation

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
  select((!c("v_vul_presence", "sample", "year", "week", "day",
             "Year", "Week", "Monday_Date", "Day_Shift")))
# Remove 'sample' from the predictors for training
#data_for_model <- data %>% select(-sample)

# Check the class distribution
table(y)

# Manually define validation and test location(s)
val_locations <- c(1, 2, 3, 4, 5, 6)
test_locations <- c(7)
mtry_vals <- c(2, 3)

# Prepare dataset
data_rf <- data


# Initialize storage
val_auprcs <- c()
val_roc_aucs <- c()
test_auprcs <- c()
test_roc_aucs <- c()
models_list <- list()
best_model <- NULL
best_val_score <- -Inf
skipped_locs <- c()

# Loop over each validation location
for (val_loc in train_locations) {
  cat("\nValidating on location:", val_loc, "\n")
  
  train_data <- data_rf[data_rf$location %in% train_locations & data_rf$location != val_loc, ]
  val_data   <- data_rf[data_rf$location == val_loc, ]
  test_data  <- data_rf[data_rf$location %in% test_locations, ]
  
  train_data_model <- train_data[, !(names(train_data) %in% c("sample", "date", "location"))]
  train_data_smote <- SMOTE(v_vul_presence ~ ., data = train_data_model, perc.over = 200, perc.under = 100)
  
  val_data_model <- val_data[, !(names(val_data) %in% c("sample", "date", "location"))]
  test_data_model <- test_data[, !(names(test_data) %in% c("sample", "date", "location"))]
  
  best_fold_model <- NULL
  best_fold_score <- -Inf
  best_fold_auc <- NA
  
  val_binary_check <- ifelse(val_data_model$v_vul_presence == "Present", 1, 0)
  if (sum(val_binary_check) == 0) {
    warning(paste("Skipping location", val_loc, "— no 'Present' samples"))
    skipped_locs <- c(skipped_locs, val_loc)
    next
  }
  
  for (mtry_val in mtry_vals) {
    rf_model <- caret::train(v_vul_presence ~ ., data = train_data_smote,
                             method = "rf",
                             metric = "ROC",
                             trControl = trainControl(method = "none", classProbs = TRUE),
                             tuneGrid = data.frame(mtry = mtry_val),
                             ntree = 500)
    
    val_probs <- predict(rf_model, val_data_model, type = "prob")[, "Present"]
    val_binary <- ifelse(val_data_model$v_vul_presence == "Present", 1, 0)
    
    val_pr <- pr.curve(scores.class0 = val_probs, weights.class0 = val_binary, curve = FALSE)
    val_auprc <- val_pr$auc.integral
    
    val_auc <- as.numeric(pROC::auc(val_binary, val_probs))
    
    if (val_auprc > best_fold_score) {
      best_fold_score <- val_auprc
      best_fold_model <- rf_model
      best_fold_auc <- val_auc
    }
  }
  
  val_auprcs <- c(val_auprcs, best_fold_score)
  val_roc_aucs <- c(val_roc_aucs, best_fold_auc)
  models_list[[as.character(val_loc)]] <- best_fold_model
  
  if (best_fold_score > best_val_score) {
    best_val_score <- best_fold_score
    best_model <- best_fold_model
  }
  
  # Test evaluation using best model from this fold
  test_probs <- predict(best_fold_model, test_data_model, type = "prob")[, "Present"]
  test_binary <- ifelse(test_data_model$v_vul_presence == "Present", 1, 0)
  
  test_pr <- pr.curve(scores.class0 = test_probs, weights.class0 = test_binary, curve = FALSE)
  test_auprc <- test_pr$auc.integral
  test_auc <- as.numeric(pROC::auc(test_binary, test_probs))
  
  test_auprcs <- c(test_auprcs, test_auprc)
  test_roc_aucs <- c(test_roc_aucs, test_auc)
}

# 📊 Summary output
cat("\n📊 Validation AUPRCs (by location):", round(val_auprcs, 3), "\n")
cat("📈 Average Validation AUPRC:", round(mean(val_auprcs), 3), "\n")
cat("📉 Average Validation ROC-AUC:", round(mean(val_roc_aucs, na.rm = TRUE), 3), "\n")
cat("🧪 Average Test AUPRC:", round(mean(test_auprcs), 3), "\n")
cat("🧪 Average Test ROC-AUC:", round(mean(test_roc_aucs, na.rm = TRUE), 3), "\n")
cat("⚠️ Skipped validation locations:", paste(skipped_locs, collapse = ", "), "\n")

avg_val_auprc_16s_7 <- mean(val_auprcs)
avg_test_auprc_16s_7 <- mean(test_auprcs)
avg_test_rocauc_16s_7 <- mean(test_roc_aucs, na.rm = TRUE)


final_test_probs <- predict(best_model, test_data_model, type = "prob")[, "Present"]
final_test_labels <- test_data_model$v_vul_presence
final_binary <- ifelse(final_test_labels == "Present", 1, 0)
final_pr <- pr.curve(scores.class0 = final_test_probs, weights.class0 = final_binary, curve = TRUE)
final_auprc <- final_pr$auc.integral

cat("🎯 Final Test AUPRC (best model from validation):", round(final_auprc, 3), "\n")

pr_data <- data.frame(Recall = final_pr$curve[, 1], Precision = final_pr$curve[, 2])
ggplot(pr_data, aes(x = Recall, y = Precision)) +
  geom_line(color = "darkblue", linewidth = 1.5) +
  labs(title = paste("Final PR Curve (AUPRC =", round(final_auprc, 3), ")"),
       x = "Recall", y = "Precision") +
  theme_minimal()

# Use the test data that includes sample and date
results_manual_rf_16s <- test_data %>%
  mutate(
    Predicted_Probability = predict(best_model, ., type = "prob")[, "Present"],
    Actual = ifelse(v_vul_presence == "Present", 1, 0)
  )

# Threshold selection to ensure ≥70% recall
actual_positives <- sum(results_manual_rf_16s$Actual == 1)
thresholds <- seq(1, 0.01, -0.01)
best_threshold_rf <- 0.01

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_16s$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_16s$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.7) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥70%% recall): %.2f\n", best_threshold_rf))

# Apply threshold and classify
results_manual_rf_16s <- results_manual_rf_16s %>%
  mutate(
    Predicted_Class = ifelse(Predicted_Probability > best_threshold_rf, 1, 0),
    Category = case_when(
      Actual == 1 & Predicted_Class == 1 ~ "True Positive",
      Actual == 0 & Predicted_Class == 0 ~ "True Negative",
      Actual == 0 & Predicted_Class == 1 ~ "False Positive",
      Actual == 1 & Predicted_Class == 0 ~ "False Negative"
    ),
    date = as.Date(date),
    week_start = lubridate::floor_date(date, unit = "week", week_start = 1)
  )


results_manual_rf_18s <- results_manual_rf_18s %>% 
  select(c("location", "date","v_vul_presence", "Predicted_Probability", "Actual", "Predicted_Class",
           "Category", "week_start"))

# --- Weekly Summary Table for Plotting ---
weekly_summary_rf_16s <- results_manual_rf_16s %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")

# --- Confusion Matrix ---
confusion_matrix_rf_16s <- table(
  Predicted = results_manual_rf_16s$Predicted_Class,
  Actual = results_manual_rf_16s$Actual
)

cat("\n📌 Confusion Matrix (Random Forest):\n")
print(confusion_matrix_rf_16s)

# --- Sanity check output
head(weekly_summary_rf_16s)




#################### time lag 8
set.seed(7896)

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
  df <- df[order(df$year, df$week, df$day), ]
  
  # Shift the sample column up by two rows
  df$sample_shifted <- c(rep(NA, 8), df$sample[1:(nrow(df) - 8)])
  
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


otu_16s_filtered_rel_rf <- subset(otu_16s_filtered_rel_rf, location %in%  
                                    c("1", "2", "3", "4", "5", "6", "7"))
# 
# 
# # Subset rows after 09-05-22 and before 28-09-22
# otu_16s_filtered_rel_rf <- otu_16s_filtered_rel_rf[otu_16s_filtered_rel_rf$date > as.Date("2022-06-20") & 
#                                                      otu_16s_filtered_rel_rf$date < as.Date("2022-09-14"), ]
# 
# #### trying cross validation

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
  select((!c("v_vul_presence", "sample", "year", "week", "day",
             "Year", "Week", "Monday_Date", "Day_Shift")))
# Remove 'sample' from the predictors for training
#data_for_model <- data %>% select(-sample)

# Check the class distribution
table(y)

# Manually define validation and test location(s)
val_locations <- c(1, 2, 3, 4, 5, 6)
test_locations <- c(7)
mtry_vals <- c(2, 3)

# Prepare dataset
data_rf <- data


# Initialize storage
val_auprcs <- c()
val_roc_aucs <- c()
test_auprcs <- c()
test_roc_aucs <- c()
models_list <- list()
best_model <- NULL
best_val_score <- -Inf
skipped_locs <- c()

# Loop over each validation location
for (val_loc in train_locations) {
  cat("\nValidating on location:", val_loc, "\n")
  
  train_data <- data_rf[data_rf$location %in% train_locations & data_rf$location != val_loc, ]
  val_data   <- data_rf[data_rf$location == val_loc, ]
  test_data  <- data_rf[data_rf$location %in% test_locations, ]
  
  train_data_model <- train_data[, !(names(train_data) %in% c("sample", "date", "location"))]
  train_data_smote <- SMOTE(v_vul_presence ~ ., data = train_data_model, perc.over = 200, perc.under = 100)
  
  val_data_model <- val_data[, !(names(val_data) %in% c("sample", "date", "location"))]
  test_data_model <- test_data[, !(names(test_data) %in% c("sample", "date", "location"))]
  
  best_fold_model <- NULL
  best_fold_score <- -Inf
  best_fold_auc <- NA
  
  val_binary_check <- ifelse(val_data_model$v_vul_presence == "Present", 1, 0)
  if (sum(val_binary_check) == 0) {
    warning(paste("Skipping location", val_loc, "— no 'Present' samples"))
    skipped_locs <- c(skipped_locs, val_loc)
    next
  }
  
  for (mtry_val in mtry_vals) {
    rf_model <- caret::train(v_vul_presence ~ ., data = train_data_smote,
                             method = "rf",
                             metric = "ROC",
                             trControl = trainControl(method = "none", classProbs = TRUE),
                             tuneGrid = data.frame(mtry = mtry_val),
                             ntree = 500)
    
    val_probs <- predict(rf_model, val_data_model, type = "prob")[, "Present"]
    val_binary <- ifelse(val_data_model$v_vul_presence == "Present", 1, 0)
    
    val_pr <- pr.curve(scores.class0 = val_probs, weights.class0 = val_binary, curve = FALSE)
    val_auprc <- val_pr$auc.integral
    
    val_auc <- as.numeric(pROC::auc(val_binary, val_probs))
    
    if (val_auprc > best_fold_score) {
      best_fold_score <- val_auprc
      best_fold_model <- rf_model
      best_fold_auc <- val_auc
    }
  }
  
  val_auprcs <- c(val_auprcs, best_fold_score)
  val_roc_aucs <- c(val_roc_aucs, best_fold_auc)
  models_list[[as.character(val_loc)]] <- best_fold_model
  
  if (best_fold_score > best_val_score) {
    best_val_score <- best_fold_score
    best_model <- best_fold_model
  }
  
  # Test evaluation using best model from this fold
  test_probs <- predict(best_fold_model, test_data_model, type = "prob")[, "Present"]
  test_binary <- ifelse(test_data_model$v_vul_presence == "Present", 1, 0)
  
  test_pr <- pr.curve(scores.class0 = test_probs, weights.class0 = test_binary, curve = FALSE)
  test_auprc <- test_pr$auc.integral
  test_auc <- as.numeric(pROC::auc(test_binary, test_probs))
  
  test_auprcs <- c(test_auprcs, test_auprc)
  test_roc_aucs <- c(test_roc_aucs, test_auc)
}

# 📊 Summary output
cat("\n📊 Validation AUPRCs (by location):", round(val_auprcs, 3), "\n")
cat("📈 Average Validation AUPRC:", round(mean(val_auprcs), 3), "\n")
cat("📉 Average Validation ROC-AUC:", round(mean(val_roc_aucs, na.rm = TRUE), 3), "\n")
cat("🧪 Average Test AUPRC:", round(mean(test_auprcs), 3), "\n")
cat("🧪 Average Test ROC-AUC:", round(mean(test_roc_aucs, na.rm = TRUE), 3), "\n")
cat("⚠️ Skipped validation locations:", paste(skipped_locs, collapse = ", "), "\n")

avg_val_auprc_16s_8 <- mean(val_auprcs)
avg_test_auprc_16s_8 <- mean(test_auprcs)
avg_test_rocauc_16s_8 <- mean(test_roc_aucs, na.rm = TRUE)


final_test_probs <- predict(best_model, test_data_model, type = "prob")[, "Present"]
final_test_labels <- test_data_model$v_vul_presence
final_binary <- ifelse(final_test_labels == "Present", 1, 0)
final_pr <- pr.curve(scores.class0 = final_test_probs, weights.class0 = final_binary, curve = TRUE)
final_auprc <- final_pr$auc.integral

cat("🎯 Final Test AUPRC (best model from validation):", round(final_auprc, 3), "\n")

pr_data <- data.frame(Recall = final_pr$curve[, 1], Precision = final_pr$curve[, 2])
ggplot(pr_data, aes(x = Recall, y = Precision)) +
  geom_line(color = "darkblue", linewidth = 1.5) +
  labs(title = paste("Final PR Curve (AUPRC =", round(final_auprc, 3), ")"),
       x = "Recall", y = "Precision") +
  theme_minimal()



#################### time lag 9 
set.seed(7896)

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
  df <- df[order(df$year, df$week, df$day), ]
  
  # Shift the sample column up by two rows
  df$sample_shifted <- c(rep(NA, 9), df$sample[1:(nrow(df) - 9)])
  
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


otu_16s_filtered_rel_rf <- subset(otu_16s_filtered_rel_rf, location %in%  
                                    c("1", "2", "3", "4", "5", "6", "7"))
# 
# 
# # Subset rows after 09-05-22 and before 28-09-22
# otu_16s_filtered_rel_rf <- otu_16s_filtered_rel_rf[otu_16s_filtered_rel_rf$date > as.Date("2022-06-20") & 
#                                                      otu_16s_filtered_rel_rf$date < as.Date("2022-09-14"), ]
# 
# #### trying cross validation

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
  select((!c("v_vul_presence", "sample", "year", "week", "day",
             "Year", "Week", "Monday_Date", "Day_Shift")))
# Remove 'sample' from the predictors for training
#data_for_model <- data %>% select(-sample)

# Check the class distribution
table(y)

# Manually define validation and test location(s)
val_locations <- c(1, 2, 3, 4, 5, 6)
test_locations <- c(7)
mtry_vals <- c(2, 3)

# Prepare dataset
data_rf <- data


# Initialize storage
val_auprcs <- c()
val_roc_aucs <- c()
test_auprcs <- c()
test_roc_aucs <- c()
models_list <- list()
best_model <- NULL
best_val_score <- -Inf
skipped_locs <- c()

# Loop over each validation location
for (val_loc in train_locations) {
  cat("\nValidating on location:", val_loc, "\n")
  
  train_data <- data_rf[data_rf$location %in% train_locations & data_rf$location != val_loc, ]
  val_data   <- data_rf[data_rf$location == val_loc, ]
  test_data  <- data_rf[data_rf$location %in% test_locations, ]
  
  train_data_model <- train_data[, !(names(train_data) %in% c("sample", "date", "location"))]
  train_data_smote <- SMOTE(v_vul_presence ~ ., data = train_data_model, perc.over = 200, perc.under = 100)
  
  val_data_model <- val_data[, !(names(val_data) %in% c("sample", "date", "location"))]
  test_data_model <- test_data[, !(names(test_data) %in% c("sample", "date", "location"))]
  
  best_fold_model <- NULL
  best_fold_score <- -Inf
  best_fold_auc <- NA
  
  val_binary_check <- ifelse(val_data_model$v_vul_presence == "Present", 1, 0)
  if (sum(val_binary_check) == 0) {
    warning(paste("Skipping location", val_loc, "— no 'Present' samples"))
    skipped_locs <- c(skipped_locs, val_loc)
    next
  }
  
  for (mtry_val in mtry_vals) {
    rf_model <- caret::train(v_vul_presence ~ ., data = train_data_smote,
                             method = "rf",
                             metric = "ROC",
                             trControl = trainControl(method = "none", classProbs = TRUE),
                             tuneGrid = data.frame(mtry = mtry_val),
                             ntree = 500)
    
    val_probs <- predict(rf_model, val_data_model, type = "prob")[, "Present"]
    val_binary <- ifelse(val_data_model$v_vul_presence == "Present", 1, 0)
    
    val_pr <- pr.curve(scores.class0 = val_probs, weights.class0 = val_binary, curve = FALSE)
    val_auprc <- val_pr$auc.integral
    
    val_auc <- as.numeric(pROC::auc(val_binary, val_probs))
    
    if (val_auprc > best_fold_score) {
      best_fold_score <- val_auprc
      best_fold_model <- rf_model
      best_fold_auc <- val_auc
    }
  }
  
  val_auprcs <- c(val_auprcs, best_fold_score)
  val_roc_aucs <- c(val_roc_aucs, best_fold_auc)
  models_list[[as.character(val_loc)]] <- best_fold_model
  
  if (best_fold_score > best_val_score) {
    best_val_score <- best_fold_score
    best_model <- best_fold_model
  }
  
  # Test evaluation using best model from this fold
  test_probs <- predict(best_fold_model, test_data_model, type = "prob")[, "Present"]
  test_binary <- ifelse(test_data_model$v_vul_presence == "Present", 1, 0)
  
  test_pr <- pr.curve(scores.class0 = test_probs, weights.class0 = test_binary, curve = FALSE)
  test_auprc <- test_pr$auc.integral
  test_auc <- as.numeric(pROC::auc(test_binary, test_probs))
  
  test_auprcs <- c(test_auprcs, test_auprc)
  test_roc_aucs <- c(test_roc_aucs, test_auc)
}

# 📊 Summary output
cat("\n📊 Validation AUPRCs (by location):", round(val_auprcs, 3), "\n")
cat("📈 Average Validation AUPRC:", round(mean(val_auprcs), 3), "\n")
cat("📉 Average Validation ROC-AUC:", round(mean(val_roc_aucs, na.rm = TRUE), 3), "\n")
cat("🧪 Average Test AUPRC:", round(mean(test_auprcs), 3), "\n")
cat("🧪 Average Test ROC-AUC:", round(mean(test_roc_aucs, na.rm = TRUE), 3), "\n")
cat("⚠️ Skipped validation locations:", paste(skipped_locs, collapse = ", "), "\n")

avg_val_auprc_16s_9 <- mean(val_auprcs)
avg_test_auprc_16s_9 <- mean(test_auprcs)
avg_test_rocauc_16s_9 <- mean(test_roc_aucs, na.rm = TRUE)


final_test_probs <- predict(best_model, test_data_model, type = "prob")[, "Present"]
final_test_labels <- test_data_model$v_vul_presence
final_binary <- ifelse(final_test_labels == "Present", 1, 0)
final_pr <- pr.curve(scores.class0 = final_test_probs, weights.class0 = final_binary, curve = TRUE)
final_auprc <- final_pr$auc.integral

cat("🎯 Final Test AUPRC (best model from validation):", round(final_auprc, 3), "\n")

pr_data <- data.frame(Recall = final_pr$curve[, 1], Precision = final_pr$curve[, 2])
ggplot(pr_data, aes(x = Recall, y = Precision)) +
  geom_line(color = "darkblue", linewidth = 1.5) +
  labs(title = paste("Final PR Curve (AUPRC =", round(final_auprc, 3), ")"),
       x = "Recall", y = "Precision") +
  theme_minimal()



#################### time lag 10 
set.seed(7896)

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
  df <- df[order(df$year, df$week, df$day), ]
  
  # Shift the sample column up by two rows
  df$sample_shifted <- c(rep(NA, 10), df$sample[1:(nrow(df) - 10)])
  
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


otu_16s_filtered_rel_rf <- subset(otu_16s_filtered_rel_rf, location %in%  
                                    c("1", "2", "3", "4", "5", "6", "7"))
# 
# 
# # Subset rows after 09-05-22 and before 28-09-22
# otu_16s_filtered_rel_rf <- otu_16s_filtered_rel_rf[otu_16s_filtered_rel_rf$date > as.Date("2022-06-20") & 
#                                                      otu_16s_filtered_rel_rf$date < as.Date("2022-09-14"), ]
# 
# #### trying cross validation

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
  select((!c("v_vul_presence", "sample", "year", "week", "day",
             "Year", "Week", "Monday_Date", "Day_Shift")))
# Remove 'sample' from the predictors for training
#data_for_model <- data %>% select(-sample)

# Check the class distribution
table(y)

# Manually define validation and test location(s)
val_locations <- c(1, 2, 3, 4, 5, 6)
test_locations <- c(7)
mtry_vals <- c(2, 3)

# Prepare dataset
data_rf <- data


# Initialize storage
val_auprcs <- c()
val_roc_aucs <- c()
test_auprcs <- c()
test_roc_aucs <- c()
models_list <- list()
best_model <- NULL
best_val_score <- -Inf
skipped_locs <- c()

# Loop over each validation location
for (val_loc in train_locations) {
  cat("\nValidating on location:", val_loc, "\n")
  
  train_data <- data_rf[data_rf$location %in% train_locations & data_rf$location != val_loc, ]
  val_data   <- data_rf[data_rf$location == val_loc, ]
  test_data  <- data_rf[data_rf$location %in% test_locations, ]
  
  train_data_model <- train_data[, !(names(train_data) %in% c("sample", "date", "location"))]
  train_data_smote <- SMOTE(v_vul_presence ~ ., data = train_data_model, perc.over = 200, perc.under = 100)
  
  val_data_model <- val_data[, !(names(val_data) %in% c("sample", "date", "location"))]
  test_data_model <- test_data[, !(names(test_data) %in% c("sample", "date", "location"))]
  
  best_fold_model <- NULL
  best_fold_score <- -Inf
  best_fold_auc <- NA
  
  val_binary_check <- ifelse(val_data_model$v_vul_presence == "Present", 1, 0)
  if (sum(val_binary_check) == 0) {
    warning(paste("Skipping location", val_loc, "— no 'Present' samples"))
    skipped_locs <- c(skipped_locs, val_loc)
    next
  }
  
  for (mtry_val in mtry_vals) {
    rf_model <- caret::train(v_vul_presence ~ ., data = train_data_smote,
                             method = "rf",
                             metric = "ROC",
                             trControl = trainControl(method = "none", classProbs = TRUE),
                             tuneGrid = data.frame(mtry = mtry_val),
                             ntree = 500)
    
    val_probs <- predict(rf_model, val_data_model, type = "prob")[, "Present"]
    val_binary <- ifelse(val_data_model$v_vul_presence == "Present", 1, 0)
    
    val_pr <- pr.curve(scores.class0 = val_probs, weights.class0 = val_binary, curve = FALSE)
    val_auprc <- val_pr$auc.integral
    
    val_auc <- as.numeric(pROC::auc(val_binary, val_probs))
    
    if (val_auprc > best_fold_score) {
      best_fold_score <- val_auprc
      best_fold_model <- rf_model
      best_fold_auc <- val_auc
    }
  }
  
  val_auprcs <- c(val_auprcs, best_fold_score)
  val_roc_aucs <- c(val_roc_aucs, best_fold_auc)
  models_list[[as.character(val_loc)]] <- best_fold_model
  
  if (best_fold_score > best_val_score) {
    best_val_score <- best_fold_score
    best_model <- best_fold_model
  }
  
  # Test evaluation using best model from this fold
  test_probs <- predict(best_fold_model, test_data_model, type = "prob")[, "Present"]
  test_binary <- ifelse(test_data_model$v_vul_presence == "Present", 1, 0)
  
  test_pr <- pr.curve(scores.class0 = test_probs, weights.class0 = test_binary, curve = FALSE)
  test_auprc <- test_pr$auc.integral
  test_auc <- as.numeric(pROC::auc(test_binary, test_probs))
  
  test_auprcs <- c(test_auprcs, test_auprc)
  test_roc_aucs <- c(test_roc_aucs, test_auc)
}

# 📊 Summary output
cat("\n📊 Validation AUPRCs (by location):", round(val_auprcs, 3), "\n")
cat("📈 Average Validation AUPRC:", round(mean(val_auprcs), 3), "\n")
cat("📉 Average Validation ROC-AUC:", round(mean(val_roc_aucs, na.rm = TRUE), 3), "\n")
cat("🧪 Average Test AUPRC:", round(mean(test_auprcs), 3), "\n")
cat("🧪 Average Test ROC-AUC:", round(mean(test_roc_aucs, na.rm = TRUE), 3), "\n")
cat("⚠️ Skipped validation locations:", paste(skipped_locs, collapse = ", "), "\n")

avg_val_auprc_16s_10 <- mean(val_auprcs)
avg_test_auprc_16s_10 <- mean(test_auprcs)
avg_test_rocauc_16s_10 <- mean(test_roc_aucs, na.rm = TRUE)


final_test_probs <- predict(best_model, test_data_model, type = "prob")[, "Present"]
final_test_labels <- test_data_model$v_vul_presence
final_binary <- ifelse(final_test_labels == "Present", 1, 0)
final_pr <- pr.curve(scores.class0 = final_test_probs, weights.class0 = final_binary, curve = TRUE)
final_auprc <- final_pr$auc.integral

cat("🎯 Final Test AUPRC (best model from validation):", round(final_auprc, 3), "\n")

pr_data <- data.frame(Recall = final_pr$curve[, 1], Precision = final_pr$curve[, 2])
ggplot(pr_data, aes(x = Recall, y = Precision)) +
  geom_line(color = "darkblue", linewidth = 1.5) +
  labs(title = paste("Final PR Curve (AUPRC =", round(final_auprc, 3), ")"),
       x = "Recall", y = "Precision") +
  theme_minimal()






