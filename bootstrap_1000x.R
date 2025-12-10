# ==========================
# 🔁 PART 1: Null Model AUPR Distribution (scrambled labels)
# ==========================

###### 16s check for robustness

# Set number of bootstrap iterations
n_boot <- 1000
aupr_bootstrap <- numeric(n_boot)

set.seed(7896)  # For reproducibility

# Loop for bootstrap resampling
for (i in 1:n_boot) {
  indices <- sample(1:nrow(results_manual_rf_16s), replace = TRUE)
  
  resampled_obs <- ifelse(results_manual_rf_16s$Actual[indices] == "Present", 1, 0)
  resampled_pred <- as.numeric(results_manual_rf_16s$Predicted_probability[indices])
  
  # Skip if only one class in resampled_obs
  if (length(unique(resampled_obs)) < 2) {
    aupr_bootstrap[i] <- NA
  } else {
    pr <- pr.curve(scores.class0 = resampled_pred, weights.class0 = resampled_obs)$auc.integral
    aupr_bootstrap[i] <- pr
  }
}

# Remove any NAs
aupr_bootstrap_16s <- na.omit(aupr_bootstrap)

# Compute summary statistics
mean_aupr <- mean(aupr_bootstrap_16s)
ci_95 <- quantile(aupr_bootstrap_16s, probs = c(0.025, 0.975))

# Print stats
cat("Bootstrapped Mean AUPR:", round(mean_aupr, 3), "\n")
cat("Bootstrapped 95% CI for AUPR:", round(ci_95[1], 3), "-", round(ci_95[2], 3), "\n")

# Create dataframe for plotting
df_aupr_bootstrap <- data.frame(AUPR = aupr_bootstrap_16s)

# Plot with ggplot2
hist_16s_bootstrap <- ggplot(df_aupr_bootstrap, aes(x = AUPR)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  geom_vline(xintercept = mean_aupr, color = "red", linewidth = 1.2) +
  geom_vline(xintercept = ci_95, color = "darkblue", linetype = "dashed", linewidth = 1) +
  labs(title = "16S rRNA relative abundance:\n Baltic Sea",
       x = "AUPR", y = "Count") +
  theme_minimal()+
  theme(plot.title = element_text(size = 10)) 

# Show the plot
print(hist_16s_bootstrap)


###### 16s all
# Set number of bootstrap iterations
n_boot <- 1000
aupr_bootstrap <- numeric(n_boot)

set.seed(7896)  # For reproducibility

# Loop for bootstrap resampling
for (i in 1:n_boot) {
  indices <- sample(1:nrow(results_manual_rf_16s_all), replace = TRUE)
  
  resampled_obs <- ifelse(results_manual_rf_16s_all$Actual[indices] == "Present", 1, 0)
  resampled_pred <- as.numeric(results_manual_rf_16s_all$Predicted_probability[indices])
  
  # Skip if only one class in resampled_obs
  if (length(unique(resampled_obs)) < 2) {
    aupr_bootstrap[i] <- NA
  } else {
    pr <- pr.curve(scores.class0 = resampled_pred, weights.class0 = resampled_obs)$auc.integral
    aupr_bootstrap[i] <- pr
  }
}

# Remove any NAs
aupr_bootstrap_16s <- na.omit(aupr_bootstrap)

# Compute summary statistics
mean_aupr <- mean(aupr_bootstrap_16s)
ci_95 <- quantile(aupr_bootstrap_16s, probs = c(0.025, 0.975))

# Print stats
cat("Bootstrapped Mean AUPR:", round(mean_aupr, 3), "\n")
cat("Bootstrapped 95% CI for AUPR:", round(ci_95[1], 3), "-", round(ci_95[2], 3), "\n")

# Create dataframe for plotting
df_aupr_bootstrap <- data.frame(AUPR = aupr_bootstrap_16s)

# Plot with ggplot2
hist_16s_all_bootstrap <- ggplot(df_aupr_bootstrap, aes(x = AUPR)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  geom_vline(xintercept = mean_aupr, color = "red", linewidth = 1.2) +
  geom_vline(xintercept = ci_95, color = "darkblue", linetype = "dashed", linewidth = 1) +
  labs(title = "16S rRNA relative abundance:\n Baltic Sea and Warnow Estuary",
       x = "AUPR", y = "Count") +
  theme_minimal()+
  theme(plot.title = element_text(size = 10)) 

# Show the plot
print(hist_16s_all_bootstrap)

###### 18s
# Set number of bootstrap iterations
n_boot <- 1000
aupr_bootstrap <- numeric(n_boot)

set.seed(7896)  # For reproducibility

# Loop for bootstrap resampling
for (i in 1:n_boot) {
  indices <- sample(1:nrow(results_manual_rf_18s), replace = TRUE)
  
  resampled_obs <- ifelse(results_manual_rf_18s$Actual[indices] == "Present", 1, 0)
  resampled_pred <- as.numeric(results_manual_rf_18s$Predicted_probability[indices])
  
  # Skip if only one class in resampled_obs
  if (length(unique(resampled_obs)) < 2) {
    aupr_bootstrap[i] <- NA
  } else {
    pr <- pr.curve(scores.class0 = resampled_pred, weights.class0 = resampled_obs)$auc.integral
    aupr_bootstrap[i] <- pr
  }
}

# Remove any NAs
aupr_bootstrap_18s <- na.omit(aupr_bootstrap)

# Compute summary statistics
mean_aupr <- mean(aupr_bootstrap_16s)
ci_95 <- quantile(aupr_bootstrap_16s, probs = c(0.025, 0.975))

# Print stats
cat("Bootstrapped Mean AUPR:", round(mean_aupr, 3), "\n")
cat("Bootstrapped 95% CI for AUPR:", round(ci_95[1], 3), "-", round(ci_95[2], 3), "\n")

# Create dataframe for plotting
df_aupr_bootstrap <- data.frame(AUPR = aupr_bootstrap_18s)

# Plot with ggplot2
hist_18s_bootstrap <- ggplot(df_aupr_bootstrap, aes(x = AUPR)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  geom_vline(xintercept = mean_aupr, color = "red", linewidth = 1.2) +
  geom_vline(xintercept = ci_95, color = "darkblue", linetype = "dashed", linewidth = 1) +
  labs(title = "18S rRNA relative abundance:\n Baltic Sea",
       x = "AUPR", y = "Count") +
  theme_minimal()+
  theme(plot.title = element_text(size = 10)) 

# Show the plot
print(hist_18s_bootstrap)

###### 18s all
# Set number of bootstrap iterations
n_boot <- 1000
aupr_bootstrap <- numeric(n_boot)

set.seed(7896)  # For reproducibility

# Loop for bootstrap resampling
for (i in 1:n_boot) {
  indices <- sample(1:nrow(results_manual_rf_18s_all), replace = TRUE)
  
  resampled_obs <- ifelse(results_manual_rf_18s_all$Actual[indices] == "Present", 1, 0)
  resampled_pred <- as.numeric(results_manual_rf_18s_all$Predicted_probability[indices])
  
  # Skip if only one class in resampled_obs
  if (length(unique(resampled_obs)) < 2) {
    aupr_bootstrap[i] <- NA
  } else {
    pr <- pr.curve(scores.class0 = resampled_pred, weights.class0 = resampled_obs)$auc.integral
    aupr_bootstrap[i] <- pr
  }
}

# Remove any NAs
aupr_bootstrap_18s_all <- na.omit(aupr_bootstrap)

# Compute summary statistics
mean_aupr <- mean(aupr_bootstrap_18s_all)
ci_95 <- quantile(aupr_bootstrap_18s_all, probs = c(0.025, 0.975))

# Print stats
cat("Bootstrapped Mean AUPR:", round(mean_aupr, 3), "\n")
cat("Bootstrapped 95% CI for AUPR:", round(ci_95[1], 3), "-", round(ci_95[2], 3), "\n")

# Create dataframe for plotting
df_aupr_bootstrap <- data.frame(AUPR = aupr_bootstrap_18s_all)

# Plot with ggplot2
hist_18s_all_bootstrap <- ggplot(df_aupr_bootstrap, aes(x = AUPR)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  geom_vline(xintercept = mean_aupr, color = "red", linewidth = 1.2) +
  geom_vline(xintercept = ci_95, color = "darkblue", linetype = "dashed", linewidth = 1) +
  labs(title = "18S rRNA relative abundance:\n Baltic Sea and Warnow Estuary",
       x = "AUPR", y = "Count") +
  theme_minimal()+
  theme(plot.title = element_text(size = 10)) 

# Show the plot
print(hist_18s_all_bootstrap)


###### copernicus

# Set number of bootstrap iterations
n_boot <- 1000
aupr_bootstrap <- numeric(n_boot)

set.seed(7896)  # For reproducibility

# Loop for bootstrap resampling
for (i in 1:n_boot) {
  indices <- sample(1:nrow(results_manual_rf_copernicus), replace = TRUE)
  
  resampled_obs <- ifelse(results_manual_rf_copernicus$Actual[indices] == "Present", 1, 0)
  resampled_pred <- as.numeric(results_manual_rf_copernicus$Predicted_probability[indices])
  
  # Skip if only one class in resampled_obs
  if (length(unique(resampled_obs)) < 2) {
    aupr_bootstrap[i] <- NA
  } else {
    pr <- pr.curve(scores.class0 = resampled_pred, weights.class0 = resampled_obs)$auc.integral
    aupr_bootstrap[i] <- pr
  }
}

# Remove any NAs
aupr_bootstrap_copernicus <- na.omit(aupr_bootstrap)

# Compute summary statistics
mean_aupr <- mean(aupr_bootstrap_copernicus)
ci_95 <- quantile(aupr_bootstrap_copernicus, probs = c(0.025, 0.975))

# Print stats
cat("Bootstrapped Mean AUPR:", round(mean_aupr, 3), "\n")
cat("Bootstrapped 95% CI for AUPR:", round(ci_95[1], 3), "-", round(ci_95[2], 3), "\n")

# Create dataframe for plotting
df_aupr_bootstrap <- data.frame(AUPR = aupr_bootstrap_copernicus)

# Plot with ggplot2
hist_copernicus_bootstrap <- ggplot(df_aupr_bootstrap, aes(x = AUPR)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  geom_vline(xintercept = mean_aupr, color = "red", linewidth = 1.2) +
  geom_vline(xintercept = ci_95, color = "darkblue", linetype = "dashed", linewidth = 1) +
  labs(title = "Copernicus satellite data\n Baltic Sea",
       x = "AUPR", y = "Count") +
  theme_minimal()+
  theme(plot.title = element_text(size = 10)) 

# Show the plot
print(hist_copernicus_bootstrap)

######## all data
# Set number of bootstrap iterations
n_boot <- 1000
aupr_bootstrap <- numeric(n_boot)

set.seed(7896)  # For reproducibility

# Loop for bootstrap resampling
for (i in 1:n_boot) {
  indices <- sample(1:nrow(results_manual_rf_all), replace = TRUE)
  
  resampled_obs <- ifelse(results_manual_rf_all$Actual[indices] == "Present", 1, 0)
  resampled_pred <- as.numeric(results_manual_rf_all$Predicted_probability[indices])
  
  # Skip if only one class in resampled_obs
  if (length(unique(resampled_obs)) < 2) {
    aupr_bootstrap[i] <- NA
  } else {
    pr <- pr.curve(scores.class0 = resampled_pred, weights.class0 = resampled_obs)$auc.integral
    aupr_bootstrap[i] <- pr
  }
}

# Remove any NAs
aupr_bootstrap_all <- na.omit(aupr_bootstrap)

# Compute summary statistics
mean_aupr <- mean(aupr_bootstrap_all)
ci_95 <- quantile(aupr_bootstrap_all, probs = c(0.025, 0.975))

# Print stats
cat("Bootstrapped Mean AUPR:", round(mean_aupr, 3), "\n")
cat("Bootstrapped 95% CI for AUPR:", round(ci_95[1], 3), "-", round(ci_95[2], 3), "\n")

# Create dataframe for plotting
df_aupr_bootstrap <- data.frame(AUPR = aupr_bootstrap_all)

# Plot with ggplot2
hist_all_bootstrap <- ggplot(df_aupr_bootstrap, aes(x = AUPR)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  geom_vline(xintercept = mean_aupr, color = "red", linewidth = 1.2) +
  geom_vline(xintercept = ci_95, color = "darkblue", linetype = "dashed", linewidth = 1) +
  labs(title = "Combined dataset:\n Baltic Sea",
       x = "AUPR", y = "Count") +
  theme_minimal()+
  theme(plot.title = element_text(size = 10)) 

# Show the plot
print(hist_all_bootstrap)

######## all data all locations
# Set number of bootstrap iterations
n_boot <- 1000
aupr_bootstrap <- numeric(n_boot)

set.seed(7896)  # For reproducibility

# Loop for bootstrap resampling
for (i in 1:n_boot) {
  indices <- sample(1:nrow(results_manual_rf_all_all_loc), replace = TRUE)
  
  resampled_obs <- ifelse(results_manual_rf_all_all_loc$Actual[indices] == "Present", 1, 0)
  resampled_pred <- as.numeric(results_manual_rf_all_all_loc$Predicted_probability[indices])
  
  # Skip if only one class in resampled_obs
  if (length(unique(resampled_obs)) < 2) {
    aupr_bootstrap[i] <- NA
  } else {
    pr <- pr.curve(scores.class0 = resampled_pred, weights.class0 = resampled_obs)$auc.integral
    aupr_bootstrap[i] <- pr
  }
}

# Remove any NAs
aupr_bootstrap_all_all_loc <- na.omit(aupr_bootstrap)

# Compute summary statistics
mean_aupr <- mean(aupr_bootstrap_all_all_loc)
ci_95 <- quantile(aupr_bootstrap_all_all_loc, probs = c(0.025, 0.975))

# Print stats
cat("Bootstrapped Mean AUPR:", round(mean_aupr, 3), "\n")
cat("Bootstrapped 95% CI for AUPR:", round(ci_95[1], 3), "-", round(ci_95[2], 3), "\n")

# Create dataframe for plotting
df_aupr_bootstrap <- data.frame(AUPR = aupr_bootstrap_all_all_loc)

# Plot with ggplot2
hist_all_all_loc_bootstrap <- ggplot(df_aupr_bootstrap, aes(x = AUPR)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  geom_vline(xintercept = mean_aupr, color = "red", linewidth = 1.2) +
  geom_vline(xintercept = ci_95, color = "darkblue", linetype = "dashed", linewidth = 1) +
  labs(title = "Combined dataset:\n Baltic Sea and Warnow Estuary",
       x = "AUPR", y = "Count") +
  theme_minimal()+
  theme(plot.title = element_text(size = 10)) 

# Show the plot
print(hist_all_all_loc_bootstrap)

######## otc data
# Set number of bootstrap iterations
n_boot <- 1000
aupr_bootstrap <- numeric(n_boot)

set.seed(7896)  # For reproducibility

# Loop for bootstrap resampling
for (i in 1:n_boot) {
  indices <- sample(1:nrow(results_manual_rf_otc), replace = TRUE)
  
  resampled_obs <- ifelse(results_manual_rf_otc$Actual[indices] == "Present", 1, 0)
  resampled_pred <- as.numeric(results_manual_rf_otc$Predicted_probability[indices])
  
  # Skip if only one class in resampled_obs
  if (length(unique(resampled_obs)) < 2) {
    aupr_bootstrap[i] <- NA
  } else {
    pr <- pr.curve(scores.class0 = resampled_pred, weights.class0 = resampled_obs)$auc.integral
    aupr_bootstrap[i] <- pr
  }
}

# Remove any NAs
aupr_bootstrap_otc <- na.omit(aupr_bootstrap)

# Compute summary statistics
mean_aupr <- mean(aupr_bootstrap_otc)
ci_95 <- quantile(aupr_bootstrap_otc, probs = c(0.025, 0.975))

# Print stats
cat("Bootstrapped Mean AUPR:", round(mean_aupr, 3), "\n")
cat("Bootstrapped 95% CI for AUPR:", round(ci_95[1], 3), "-", round(ci_95[2], 3), "\n")

# Create dataframe for plotting
df_aupr_bootstrap <- data.frame(AUPR = aupr_bootstrap_otc)

# Plot with ggplot2
hist_otc_bootstrap <- ggplot(df_aupr_bootstrap, aes(x = AUPR)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  geom_vline(xintercept = mean_aupr, color = "red", linewidth = 1.2) +
  geom_vline(xintercept = ci_95, color = "darkblue", linetype = "dashed", linewidth = 1) +
  labs(title = "Biological and Physical parameters:\n Baltic Sea",
       x = "AUPR", y = "Count") +
  theme_minimal()+
  theme(plot.title = element_text(size = 10)) 

# Show the plot
print(hist_otc_bootstrap)


######## otc data all locations

# Set number of bootstrap iterations
n_boot <- 1000
aupr_bootstrap <- numeric(n_boot)

set.seed(7896)  # For reproducibility

# Loop for bootstrap resampling
for (i in 1:n_boot) {
  indices <- sample(1:nrow(results_manual_rf_otc_all), replace = TRUE)
  
  resampled_obs <- ifelse(results_manual_rf_otc_all$Actual[indices] == "Present", 1, 0)
  resampled_pred <- as.numeric(results_manual_rf_otc_all$Predicted_probability[indices])
  
  # Skip if only one class in resampled_obs
  if (length(unique(resampled_obs)) < 2) {
    aupr_bootstrap[i] <- NA
  } else {
    pr <- pr.curve(scores.class0 = resampled_pred, weights.class0 = resampled_obs)$auc.integral
    aupr_bootstrap[i] <- pr
  }
}

# Remove any NAs
aupr_bootstrap_otc_all <- na.omit(aupr_bootstrap)

# Compute summary statistics
mean_aupr <- mean(aupr_bootstrap_otc_all)
ci_95 <- quantile(aupr_bootstrap_otc_all, probs = c(0.025, 0.975))

# Print stats
cat("Bootstrapped Mean AUPR:", round(mean_aupr, 3), "\n")
cat("Bootstrapped 95% CI for AUPR:", round(ci_95[1], 3), "-", round(ci_95[2], 3), "\n")

# Create dataframe for plotting
df_aupr_bootstrap <- data.frame(AUPR = aupr_bootstrap_otc_all)

# Plot with ggplot2
hist_otc_all_bootstrap <- ggplot(df_aupr_bootstrap, aes(x = AUPR)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  geom_vline(xintercept = mean_aupr, color = "red", linewidth = 1.2) +
  geom_vline(xintercept = ci_95, color = "darkblue", linetype = "dashed", linewidth = 1) +
  labs(title = "Biological and Physical parameters: Baltic Sea and Warnow Estuary",
       x = "AUPR", y = "Count") +
  theme_minimal()+
  theme(plot.title = element_text(size = 10)) 

# Show the plot
print(hist_otc_all_bootstrap)
# Example: you must have aupr_bootstrap_16s, aupr_bootstrap_16s_all, etc.
# Replace with your actual vectors for each model



# Combine and annotate plots
combined_plot <- (
  hist_16s_bootstrap + hist_16s_all_bootstrap +
    hist_18s_bootstrap + hist_18s_all_bootstrap +
    hist_copernicus_bootstrap + hist_all_bootstrap +
    hist_all_all_loc_bootstrap + hist_otc_bootstrap +
    hist_otc_all_bootstrap
) +
  plot_layout(ncol = 3) +
  plot_annotation(
    caption = "Bootstrapped AUPR Distributions Across All Models",
    theme = theme(
      plot.caption = element_text(
        hjust = 0.5,           # Center align
        size = 16,             # Increase font size
        face = "bold"          # Optional: make bold
      )
    )
  )

# Display the plot
print(combined_plot)

ggsave(
  "images/bootstrap.tiff",
  combined_plot,
  width = 9,     # inches
  height = 6,    # adjust if needed
  dpi  = 1000,   # resolution for line art
  compression = "lzw"
)


