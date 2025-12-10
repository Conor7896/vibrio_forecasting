############# needs all the best validation models to run again



#### you have to run v_vul_all_rf_models_loc script before this
# 
# 
# #### change Predicted_Probability to Predicted_probability
# #### change Predicted_Probability to Predicted_probability
# 
# results_manual_rf_copernicus <- results_manual_rf_copernicus %>% 
#   mutate(date = as.Date(Date))
# 
# results_manual_rf_16s <- results_manual_rf_16s %>% 
#   mutate(date = as.Date(Date))
# 
# results_manual_rf_16s_all <- results_manual_rf_16s_all %>% 
#   mutate(date = as.Date(Date))
# 
# results_manual_rf_18s <- results_manual_rf_18s %>% 
#   mutate(date = as.Date(Date))
# 
# results_manual_rf_18s_all <- results_manual_rf_18s_all %>% 
#   mutate(date = as.Date(Date))
# 
# colnames(results_manual_rf_copernicus)[colnames(results_manual_rf_copernicus) == "Predicted_probability"] <- "Predicted_Probability"
# colnames(results_manual_rf_16s)[colnames(results_manual_rf_16s) == "Predicted_probability"] <- "Predicted_Probability"
# colnames(results_manual_rf_16s_all)[colnames(results_manual_rf_16s_all) == "Predicted_probability"] <- "Predicted_Probability"
# colnames(results_manual_rf_18s)[colnames(results_manual_rf_18s) == "Predicted_probability"] <- "Predicted_Probability"
# colnames(results_manual_rf_18s_all)[colnames(results_manual_rf_18s_all) == "Predicted_probability"] <- "Predicted_Probability"
# 
# # Convert Actual from "Present"/"Absent" to 1/0, but ONLY if it's not already numeric
# if (!is.numeric(results_manual_rf_copernicus$Actual)) {
#   results_manual_rf_copernicus$Actual <- ifelse(results_manual_rf_copernicus$Actual == "Present", 1, 0)
# }
# if (!is.numeric(results_manual_rf_16s$Actual)) {
#   results_manual_rf_16s$Actual <- ifelse(results_manual_rf_16s$Actual == "Present", 1, 0)
# }
# if (!is.numeric(results_manual_rf_16s_all$Actual)) {
#   results_manual_rf_16s_all$Actual <- ifelse(results_manual_rf_16s_all$Actual == "Present", 1, 0)
# }
# if (!is.numeric(results_manual_rf_18s$Actual)) {
#   results_manual_rf_18s$Actual <- ifelse(results_manual_rf_18s$Actual == "Present", 1, 0)
# }
# if (!is.numeric(results_manual_rf_18s_all$Actual)) {
#   results_manual_rf_18s_all$Actual <- ifelse(results_manual_rf_18s_all$Actual == "Present", 1, 0)
# }

# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_copernicus$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_copernicus$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_copernicus$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.5) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

# Apply threshold and classify
results_manual_rf_copernicus <- results_manual_rf_copernicus %>%
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

# --- Weekly Summary Table for Plotting ---
weekly_summary_rf_copernicus <- results_manual_rf_copernicus %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")

# --- Confusion Matrix ---
confusion_matrix_rf_copernicus <- table(
  Predicted = results_manual_rf_copernicus$Predicted_Class,
  Actual = results_manual_rf_copernicus$Actual
)

cat("\n📌 Confusion Matrix (Random Forest):\n")
print(confusion_matrix_rf_copernicus)

# --- Sanity check output
head(weekly_summary_rf_copernicus)


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_16s$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_16s$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_16s$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.5) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

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


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_16s_all$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_16s_all$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_16s_all$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.5) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

# Apply threshold and classify
results_manual_rf_16s_all <- results_manual_rf_16s_all %>%
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

# --- Weekly Summary Table for Plotting ---
weekly_summary_rf_16s_all <- results_manual_rf_16s_all %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_18s$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_18s$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_18s$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.5) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

# Apply threshold and classify
results_manual_rf_18s <- results_manual_rf_18s %>%
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

# --- Weekly Summary Table for Plotting ---
weekly_summary_rf_18s <- results_manual_rf_18s %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_18s_all$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_18s_all$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_18s_all$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.5) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

# Apply threshold and classify
results_manual_rf_18s_all <- results_manual_rf_18s_all %>%
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

# --- Weekly Summary Table for Plotting ---
weekly_summary_rf_18s_all <- results_manual_rf_18s_all %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")



# 1. Get full date range
all_dates <- c(
  weekly_summary_rf_copernicus$week_start,
  weekly_summary_rf_16s$week_start,
  weekly_summary_rf_18s$week_start,
  weekly_summary_rf_16s_all$week_start,
  weekly_summary_rf_18s_all$week_start
)
min_date <- min(all_dates)
max_date <- max(all_dates)
full_weeks <- seq(min_date, max_date, by = "7 days")

# 2. Define all categories
all_categories <- c("True Positive", "False Negative", "False Positive", "True Negative")

# 3. Padding function
pad_weekly_data <- function(df) {
  expand_grid(week_start = full_weeks, Category = all_categories) %>%
    left_join(df, by = c("week_start", "Category")) %>%
    mutate(Count = replace_na(Count, 0))
}

# 4. Apply to each dataset
weekly_summary_rf_copernicus <- pad_weekly_data(weekly_summary_rf_copernicus)
weekly_summary_rf_16s        <- pad_weekly_data(weekly_summary_rf_16s)
weekly_summary_rf_18s        <- pad_weekly_data(weekly_summary_rf_18s)
weekly_summary_rf_16s_all    <- pad_weekly_data(weekly_summary_rf_16s_all)
weekly_summary_rf_18s_all    <- pad_weekly_data(weekly_summary_rf_18s_all)

# 5. Plotting function (updated)
make_weekly_plot <- function(df, model_title, category_colors, show_x_axis = FALSE) {
  df <- df %>%
    group_by(week_start) %>%
    mutate(Percent = Count / sum(Count) * 100) %>%
    ungroup()
  
  ggplot(df, aes(x = week_start, y = Percent, fill = Category)) +
    geom_bar(stat = "identity", position = "stack", width = 6) +
    scale_fill_manual(values = category_colors) +
    scale_x_date(
      date_labels = "%b %Y",
      date_breaks = "1 month",
      limits = c(min_date, max_date)  # Ensure shared limits
    ) +
    labs(x = NULL, y = NULL, title = model_title) +
    theme_minimal(base_size = 9) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
      axis.text.x = if (show_x_axis) element_text(angle = 45, hjust = 1, size = 8) else element_blank(),
      axis.ticks.x = if (show_x_axis) element_line() else element_blank(),
      panel.grid.minor = element_blank()
    )
}

# Function to create confusion matrix tile plot (updated)
make_conf_matrix_plot <- function(results_df, category_colors) {
  conf_df <- as.data.frame(table(
    Predicted = results_df$Predicted_Class,
    Actual    = results_df$Actual
  )) %>%
    mutate(
      Label = case_when(
        Predicted == 1 & Actual == 1 ~ "True Positive",
        Predicted == 0 & Actual == 0 ~ "True Negative",
        Predicted == 1 & Actual == 0 ~ "False Positive",
        Predicted == 0 & Actual == 1 ~ "False Negative"
      )
    )
  
  ggplot(conf_df, aes(x = factor(Actual), y = factor(Predicted), fill = Label)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), color = "white", size = 3.5, fontface = "bold") +
    scale_fill_manual(values = category_colors) +
    labs(x = "Actual", y = "Predicted") +
    theme_minimal(base_size = 9) +
    theme(
      aspect.ratio = 1,
      legend.position = "none",
      panel.grid = element_blank(),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 9, face = "bold")
    )
}

# ---- Color palette for categories ----
category_colors <- c(
  "True Positive"  = "#0072B2",  # blue
  "False Negative" = "#D55E00",  # vermillion
  "False Positive" = "#E69F00",  # orange
  "True Negative"  = "#56B4E9"   # light blue
)

# ---- Top color key (label + coloured circle) ----
color_key_df <- data.frame(
  Category = factor(names(category_colors), levels = names(category_colors)),
  x = seq_along(category_colors)
)

color_key_plot <- ggplot(color_key_df, aes(y = 1)) +
  # Category text
  geom_text(aes(x = x, label = Category),
            hjust = 0.2,
            vjust = 0.5,
            fontface = "bold",
            size = 3) +
  # small colour circles to the right of the text
  geom_point(aes(x = x + 0.68, color = Category),
             size = 3) +
  scale_color_manual(values = category_colors) +
  # make sure nothing is clipped on left or right
  scale_x_continuous(
    limits = c(0.5, max(color_key_df$x) + 1.2),
    expand = expansion(mult = 0)
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = unit(c(0, 0, 5, 0), "pt")
  )

# ---- Create vertically stacked bar plots ----
bar_grobs <- list(
  make_weekly_plot(
    weekly_summary_rf_copernicus,
    "Copernicus satellite data: Baltic Sea. Time-lag 5",
    category_colors,
    show_x_axis = FALSE
  ),
  make_weekly_plot(
    weekly_summary_rf_16s,
    "16S RA dataset: Baltic Sea. Time-lag 7",
    category_colors,
    show_x_axis = FALSE
  ),
  make_weekly_plot(
    weekly_summary_rf_18s,
    "18S RA dataset: Baltic Sea. Time-lag 7",
    category_colors,
    show_x_axis = FALSE
  ),
  make_weekly_plot(
    weekly_summary_rf_16s_all,
    "16S RA dataset: Baltic Sea and Warnow Estuary. Time-lag 9",
    category_colors,
    show_x_axis = FALSE
  ),
  make_weekly_plot(
    weekly_summary_rf_18s_all,
    "18S RA dataset: Baltic Sea and Warnow Estuary. Time-lag 7",
    category_colors,
    show_x_axis = TRUE
  )
)

# ---- Create vertically stacked confusion matrices ----
conf_grobs <- list(
  make_conf_matrix_plot(results_manual_rf_copernicus, category_colors),
  make_conf_matrix_plot(results_manual_rf_16s, category_colors),
  make_conf_matrix_plot(results_manual_rf_18s, category_colors),
  make_conf_matrix_plot(results_manual_rf_16s_all, category_colors),
  make_conf_matrix_plot(results_manual_rf_18s_all, category_colors)
)

# ---- Bar plot rows with preserved custom heights ----
bar_rows <- list(
  bar_grobs[[1]],  # Copernicus
  bar_grobs[[2]],  # 16S
  bar_grobs[[3]],  # 18S
  bar_grobs[[4]],  # 16S All Locations
  bar_grobs[[5]]   # 18S All Locations
)

# Bar column without region group labels
bar_column <- arrangeGrob(
  grobs = bar_rows,
  ncol = 1,
  heights = unit(c(1, 1, 1, 1, 1.6), "null")  # custom plot heights
)

# ---- Confusion matrix column with consistent height ----
conf_column <- arrangeGrob(
  grobs = conf_grobs,
  ncol = 1,
  heights = unit(c(2, 2, 2, 2, 2), "null")
)

# ---- RF footer title ----
rf_footer <- textGrob(
  "RF model",
  gp   = gpar(fontsize = 10, fontface = "bold"),
  just = "center"
)

# Combine bar plots and RF label vertically
rf_column <- arrangeGrob(
  bar_column,
  #rf_footer,
  ncol    = 1,
  heights = unit.c(unit(1, "null"), unit(1.2, "lines"))
)

# ---- Final layout: top color key + main panel with shared y-axis ----
main_panel <- grid.arrange(
  textGrob(
    "Percentage correctly identified weekly [%]",
    rot = 90,
    gp  = gpar(fontsize = 10, fontface = "bold")
  ),
  rf_column,
  conf_column,
  ncol    = 4,
  widths  = unit.c(
    unit(1, "lines"),
    unit(3, "null"),
    unit(0.9, "null"),
    unit(0.1, "null")
  )
)

rf_50 <- arrangeGrob(
  color_key_plot,   # top part: colour code with labels
  main_panel,
  ncol    = 1,
  heights = c(0.6, 8)  # adjust if you want more/less space for the key
)

ggsave(
  "images/rf_50.tiff",
  rf_50,
  width = 7,     # inches
  height = 5.5,    # adjust if needed
  dpi  = 1000,
  compression = "lzw"
)

##### 60


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_copernicus$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_copernicus$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_copernicus$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.6) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

# Apply threshold and classify
results_manual_rf_copernicus <- results_manual_rf_copernicus %>%
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

# --- Weekly Summary Table for Plotting ---
weekly_summary_rf_copernicus <- results_manual_rf_copernicus %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")

# --- Confusion Matrix ---
confusion_matrix_rf_copernicus <- table(
  Predicted = results_manual_rf_copernicus$Predicted_Class,
  Actual = results_manual_rf_copernicus$Actual
)

cat("\n📌 Confusion Matrix (Random Forest):\n")
print(confusion_matrix_rf_copernicus)

# --- Sanity check output
head(weekly_summary_rf_copernicus)


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_16s$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_16s$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_16s$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.6) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

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


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_16s_all$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_16s_all$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_16s_all$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.6) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

# Apply threshold and classify
results_manual_rf_16s_all <- results_manual_rf_16s_all %>%
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

# --- Weekly Summary Table for Plotting ---
weekly_summary_rf_16s_all <- results_manual_rf_16s_all %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_18s$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_18s$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_18s$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.6) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

# Apply threshold and classify
results_manual_rf_18s <- results_manual_rf_18s %>%
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

# --- Weekly Summary Table for Plotting ---
weekly_summary_rf_18s <- results_manual_rf_18s %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_18s_all$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_18s_all$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_18s_all$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.6) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

# Apply threshold and classify
results_manual_rf_18s_all <- results_manual_rf_18s_all %>%
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

# --- Weekly Summary Table for Plotting ---
weekly_summary_rf_18s_all <- results_manual_rf_18s_all %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")



# 1. Get full date range
all_dates <- c(
  weekly_summary_rf_copernicus$week_start,
  weekly_summary_rf_16s$week_start,
  weekly_summary_rf_18s$week_start,
  weekly_summary_rf_16s_all$week_start,
  weekly_summary_rf_18s_all$week_start
)
min_date <- min(all_dates)
max_date <- max(all_dates)
full_weeks <- seq(min_date, max_date, by = "7 days")

# 2. Define all categories
all_categories <- c("True Positive", "False Negative", "False Positive", "True Negative")

# 3. Padding function
pad_weekly_data <- function(df) {
  expand_grid(week_start = full_weeks, Category = all_categories) %>%
    left_join(df, by = c("week_start", "Category")) %>%
    mutate(Count = replace_na(Count, 0))
}

# 4. Apply to each dataset
weekly_summary_rf_copernicus <- pad_weekly_data(weekly_summary_rf_copernicus)
weekly_summary_rf_16s        <- pad_weekly_data(weekly_summary_rf_16s)
weekly_summary_rf_18s        <- pad_weekly_data(weekly_summary_rf_18s)
weekly_summary_rf_16s_all    <- pad_weekly_data(weekly_summary_rf_16s_all)
weekly_summary_rf_18s_all    <- pad_weekly_data(weekly_summary_rf_18s_all)

# 5. Plotting function (updated)
make_weekly_plot <- function(df, model_title, category_colors, show_x_axis = FALSE) {
  df <- df %>%
    group_by(week_start) %>%
    mutate(Percent = Count / sum(Count) * 100) %>%
    ungroup()
  
  ggplot(df, aes(x = week_start, y = Percent, fill = Category)) +
    geom_bar(stat = "identity", position = "stack", width = 6) +
    scale_fill_manual(values = category_colors) +
    scale_x_date(
      date_labels = "%b %Y",
      date_breaks = "1 month",
      limits = c(min_date, max_date)  # Ensure shared limits
    ) +
    labs(x = NULL, y = NULL, title = model_title) +
    theme_minimal(base_size = 9) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
      axis.text.x = if (show_x_axis) element_text(angle = 45, hjust = 1, size = 8) else element_blank(),
      axis.ticks.x = if (show_x_axis) element_line() else element_blank(),
      panel.grid.minor = element_blank()
    )
}

# Function to create confusion matrix tile plot (updated)
make_conf_matrix_plot <- function(results_df, category_colors) {
  conf_df <- as.data.frame(table(
    Predicted = results_df$Predicted_Class,
    Actual    = results_df$Actual
  )) %>%
    mutate(
      Label = case_when(
        Predicted == 1 & Actual == 1 ~ "True Positive",
        Predicted == 0 & Actual == 0 ~ "True Negative",
        Predicted == 1 & Actual == 0 ~ "False Positive",
        Predicted == 0 & Actual == 1 ~ "False Negative"
      )
    )
  
  ggplot(conf_df, aes(x = factor(Actual), y = factor(Predicted), fill = Label)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), color = "white", size = 3.5, fontface = "bold") +
    scale_fill_manual(values = category_colors) +
    labs(x = "Actual", y = "Predicted") +
    theme_minimal(base_size = 9) +
    theme(
      aspect.ratio = 1,
      legend.position = "none",
      panel.grid = element_blank(),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 9, face = "bold")
    )
}

# ---- Color palette for categories ----
category_colors <- c(
  "True Positive"  = "#0072B2",  # blue
  "False Negative" = "#D55E00",  # vermillion
  "False Positive" = "#E69F00",  # orange
  "True Negative"  = "#56B4E9"   # light blue
)

# ---- Top color key (label + coloured circle) ----
color_key_df <- data.frame(
  Category = factor(names(category_colors), levels = names(category_colors)),
  x = seq_along(category_colors)
)

color_key_plot <- ggplot(color_key_df, aes(y = 1)) +
  # Category text
  geom_text(aes(x = x, label = Category),
            hjust = 0.2,
            vjust = 0.5,
            fontface = "bold",
            size = 3) +
  # small colour circles to the right of the text
  geom_point(aes(x = x + 0.68, color = Category),
             size = 3) +
  scale_color_manual(values = category_colors) +
  # make sure nothing is clipped on left or right
  scale_x_continuous(
    limits = c(0.5, max(color_key_df$x) + 1.2),
    expand = expansion(mult = 0)
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = unit(c(0, 0, 5, 0), "pt")
  )

# ---- Create vertically stacked bar plots ----
bar_grobs <- list(
  make_weekly_plot(
    weekly_summary_rf_copernicus,
    "Copernicus satellite data: Baltic Sea. Time-lag 5",
    category_colors,
    show_x_axis = FALSE
  ),
  make_weekly_plot(
    weekly_summary_rf_16s,
    "16S RA dataset: Baltic Sea. Time-lag 7",
    category_colors,
    show_x_axis = FALSE
  ),
  make_weekly_plot(
    weekly_summary_rf_18s,
    "18S RA dataset: Baltic Sea. Time-lag 7",
    category_colors,
    show_x_axis = FALSE
  ),
  make_weekly_plot(
    weekly_summary_rf_16s_all,
    "16S RA dataset: Baltic Sea and Warnow Estuary. Time-lag 9",
    category_colors,
    show_x_axis = FALSE
  ),
  make_weekly_plot(
    weekly_summary_rf_18s_all,
    "18S RA dataset: Baltic Sea and Warnow Estuary. Time-lag 7",
    category_colors,
    show_x_axis = TRUE
  )
)

# ---- Create vertically stacked confusion matrices ----
conf_grobs <- list(
  make_conf_matrix_plot(results_manual_rf_copernicus, category_colors),
  make_conf_matrix_plot(results_manual_rf_16s, category_colors),
  make_conf_matrix_plot(results_manual_rf_18s, category_colors),
  make_conf_matrix_plot(results_manual_rf_16s_all, category_colors),
  make_conf_matrix_plot(results_manual_rf_18s_all, category_colors)
)

# ---- Bar plot rows with preserved custom heights ----
bar_rows <- list(
  bar_grobs[[1]],  # Copernicus
  bar_grobs[[2]],  # 16S
  bar_grobs[[3]],  # 18S
  bar_grobs[[4]],  # 16S All Locations
  bar_grobs[[5]]   # 18S All Locations
)

# Bar column without region group labels
bar_column <- arrangeGrob(
  grobs = bar_rows,
  ncol = 1,
  heights = unit(c(1, 1, 1, 1, 1.6), "null")  # custom plot heights
)

# ---- Confusion matrix column with consistent height ----
conf_column <- arrangeGrob(
  grobs = conf_grobs,
  ncol = 1,
  heights = unit(c(2, 2, 2, 2, 2), "null")
)

# ---- RF footer title ----
rf_footer <- textGrob(
  "RF model",
  gp   = gpar(fontsize = 10, fontface = "bold"),
  just = "center"
)

# Combine bar plots and RF label vertically
rf_column <- arrangeGrob(
  bar_column,
  #rf_footer,
  ncol    = 1,
  heights = unit.c(unit(1, "null"), unit(1.2, "lines"))
)

# ---- Final layout: top color key + main panel with shared y-axis ----
main_panel <- grid.arrange(
  textGrob(
    "Percentage correctly identified weekly [%]",
    rot = 90,
    gp  = gpar(fontsize = 10, fontface = "bold")
  ),
  rf_column,
  conf_column,
  ncol    = 4,
  widths  = unit.c(
    unit(1, "lines"),
    unit(3, "null"),
    unit(0.9, "null"),
    unit(0.1, "null")
  )
)

rf_60 <- arrangeGrob(
  color_key_plot,   # top part: colour code with labels
  main_panel,
  ncol    = 1,
  heights = c(0.6, 8)  # adjust if you want more/less space for the key
)

ggsave(
  "images/rf_60.tiff",
  rf_60,
  width = 7,     # inches
  height = 5.5,    # adjust if needed
  dpi  = 1000,   # resolution for line art
  compression = "lzw"
)


##### 70


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_copernicus$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_copernicus$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_copernicus$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.7) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

# Apply threshold and classify
results_manual_rf_copernicus <- results_manual_rf_copernicus %>%
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

# --- Weekly Summary Table for Plotting ---
weekly_summary_rf_copernicus <- results_manual_rf_copernicus %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")

# --- Confusion Matrix ---
confusion_matrix_rf_copernicus <- table(
  Predicted = results_manual_rf_copernicus$Predicted_Class,
  Actual = results_manual_rf_copernicus$Actual
)

cat("\n📌 Confusion Matrix (Random Forest):\n")
print(confusion_matrix_rf_copernicus)

# --- Sanity check output
head(weekly_summary_rf_copernicus)


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_16s$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_16s$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_16s$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.7) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

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


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_16s_all$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_16s_all$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_16s_all$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.7) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

# Apply threshold and classify
results_manual_rf_16s_all <- results_manual_rf_16s_all %>%
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

# --- Weekly Summary Table for Plotting ---
weekly_summary_rf_16s_all <- results_manual_rf_16s_all %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_18s$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_18s$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_18s$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.7) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

# Apply threshold and classify
results_manual_rf_18s <- results_manual_rf_18s %>%
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

# --- Weekly Summary Table for Plotting ---
weekly_summary_rf_18s <- results_manual_rf_18s %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_18s_all$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_18s_all$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_18s_all$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.7) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

# Apply threshold and classify
results_manual_rf_18s_all <- results_manual_rf_18s_all %>%
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

# --- Weekly Summary Table for Plotting ---
weekly_summary_rf_18s_all <- results_manual_rf_18s_all %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")



# 1. Get full date range
all_dates <- c(
  weekly_summary_rf_copernicus$week_start,
  weekly_summary_rf_16s$week_start,
  weekly_summary_rf_18s$week_start,
  weekly_summary_rf_16s_all$week_start,
  weekly_summary_rf_18s_all$week_start
)
min_date <- min(all_dates)
max_date <- max(all_dates)
full_weeks <- seq(min_date, max_date, by = "7 days")

# 2. Define all categories
all_categories <- c("True Positive", "False Negative", "False Positive", "True Negative")

# 3. Padding function
pad_weekly_data <- function(df) {
  expand_grid(week_start = full_weeks, Category = all_categories) %>%
    left_join(df, by = c("week_start", "Category")) %>%
    mutate(Count = replace_na(Count, 0))
}

# 4. Apply to each dataset
weekly_summary_rf_copernicus <- pad_weekly_data(weekly_summary_rf_copernicus)
weekly_summary_rf_16s        <- pad_weekly_data(weekly_summary_rf_16s)
weekly_summary_rf_18s        <- pad_weekly_data(weekly_summary_rf_18s)
weekly_summary_rf_16s_all    <- pad_weekly_data(weekly_summary_rf_16s_all)
weekly_summary_rf_18s_all    <- pad_weekly_data(weekly_summary_rf_18s_all)

# 5. Plotting function (updated)
make_weekly_plot <- function(df, model_title, category_colors, show_x_axis = FALSE) {
  df <- df %>%
    group_by(week_start) %>%
    mutate(Percent = Count / sum(Count) * 100) %>%
    ungroup()
  
  ggplot(df, aes(x = week_start, y = Percent, fill = Category)) +
    geom_bar(stat = "identity", position = "stack", width = 6) +
    scale_fill_manual(values = category_colors) +
    scale_x_date(
      date_labels = "%b %Y",
      date_breaks = "1 month",
      limits = c(min_date, max_date)  # Ensure shared limits
    ) +
    labs(x = NULL, y = NULL, title = model_title) +
    theme_minimal(base_size = 9) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
      axis.text.x = if (show_x_axis) element_text(angle = 45, hjust = 1, size = 8) else element_blank(),
      axis.ticks.x = if (show_x_axis) element_line() else element_blank(),
      panel.grid.minor = element_blank()
    )
}

# Function to create confusion matrix tile plot (updated)
make_conf_matrix_plot <- function(results_df, category_colors) {
  conf_df <- as.data.frame(table(
    Predicted = results_df$Predicted_Class,
    Actual    = results_df$Actual
  )) %>%
    mutate(
      Label = case_when(
        Predicted == 1 & Actual == 1 ~ "True Positive",
        Predicted == 0 & Actual == 0 ~ "True Negative",
        Predicted == 1 & Actual == 0 ~ "False Positive",
        Predicted == 0 & Actual == 1 ~ "False Negative"
      )
    )
  
  ggplot(conf_df, aes(x = factor(Actual), y = factor(Predicted), fill = Label)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), color = "white", size = 3.5, fontface = "bold") +
    scale_fill_manual(values = category_colors) +
    labs(x = "Actual", y = "Predicted") +
    theme_minimal(base_size = 9) +
    theme(
      aspect.ratio = 1,
      legend.position = "none",
      panel.grid = element_blank(),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 9, face = "bold")
    )
}

# ---- Color palette for categories ----
category_colors <- c(
  "True Positive"  = "#0072B2",  # blue
  "False Negative" = "#D55E00",  # vermillion
  "False Positive" = "#E69F00",  # orange
  "True Negative"  = "#56B4E9"   # light blue
)

# ---- Top color key (label + coloured circle) ----
color_key_df <- data.frame(
  Category = factor(names(category_colors), levels = names(category_colors)),
  x = seq_along(category_colors)
)

color_key_plot <- ggplot(color_key_df, aes(y = 1)) +
  # Category text
  geom_text(aes(x = x, label = Category),
            hjust = 0.2,
            vjust = 0.5,
            fontface = "bold",
            size = 3) +
  # small colour circles to the right of the text
  geom_point(aes(x = x + 0.68, color = Category),
             size = 3) +
  scale_color_manual(values = category_colors) +
  # make sure nothing is clipped on left or right
  scale_x_continuous(
    limits = c(0.5, max(color_key_df$x) + 1.2),
    expand = expansion(mult = 0)
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = unit(c(0, 0, 5, 0), "pt")
  )

# ---- Create vertically stacked bar plots ----
bar_grobs <- list(
  make_weekly_plot(
    weekly_summary_rf_copernicus,
    "Copernicus satellite data: Baltic Sea. Time-lag 5",
    category_colors,
    show_x_axis = FALSE
  ),
  make_weekly_plot(
    weekly_summary_rf_16s,
    "16S RA dataset: Baltic Sea. Time-lag 7",
    category_colors,
    show_x_axis = FALSE
  ),
  make_weekly_plot(
    weekly_summary_rf_18s,
    "18S RA dataset: Baltic Sea. Time-lag 7",
    category_colors,
    show_x_axis = FALSE
  ),
  make_weekly_plot(
    weekly_summary_rf_16s_all,
    "16S RA dataset: Baltic Sea and Warnow Estuary. Time-lag 9",
    category_colors,
    show_x_axis = FALSE
  ),
  make_weekly_plot(
    weekly_summary_rf_18s_all,
    "18S RA dataset: Baltic Sea and Warnow Estuary. Time-lag 7",
    category_colors,
    show_x_axis = TRUE
  )
)

# ---- Create vertically stacked confusion matrices ----
conf_grobs <- list(
  make_conf_matrix_plot(results_manual_rf_copernicus, category_colors),
  make_conf_matrix_plot(results_manual_rf_16s, category_colors),
  make_conf_matrix_plot(results_manual_rf_18s, category_colors),
  make_conf_matrix_plot(results_manual_rf_16s_all, category_colors),
  make_conf_matrix_plot(results_manual_rf_18s_all, category_colors)
)

# ---- Bar plot rows with preserved custom heights ----
bar_rows <- list(
  bar_grobs[[1]],  # Copernicus
  bar_grobs[[2]],  # 16S
  bar_grobs[[3]],  # 18S
  bar_grobs[[4]],  # 16S All Locations
  bar_grobs[[5]]   # 18S All Locations
)

# Bar column without region group labels
bar_column <- arrangeGrob(
  grobs = bar_rows,
  ncol = 1,
  heights = unit(c(1, 1, 1, 1, 1.6), "null")  # custom plot heights
)

# ---- Confusion matrix column with consistent height ----
conf_column <- arrangeGrob(
  grobs = conf_grobs,
  ncol = 1,
  heights = unit(c(2, 2, 2, 2, 2), "null")
)

# ---- RF footer title ----
rf_footer <- textGrob(
  "RF model",
  gp   = gpar(fontsize = 10, fontface = "bold"),
  just = "center"
)

# Combine bar plots and RF label vertically
rf_column <- arrangeGrob(
  bar_column,
  #rf_footer,
  ncol    = 1,
  heights = unit.c(unit(1, "null"), unit(1.2, "lines"))
)

# ---- Final layout: top color key + main panel with shared y-axis ----
main_panel <- grid.arrange(
  textGrob(
    "Percentage correctly identified weekly [%]",
    rot = 90,
    gp  = gpar(fontsize = 10, fontface = "bold")
  ),
  rf_column,
  conf_column,
  ncol    = 4,
  widths  = unit.c(
    unit(1, "lines"),
    unit(3, "null"),
    unit(0.9, "null"),
    unit(0.1, "null")
  )
)

rf_70 <- arrangeGrob(
  color_key_plot,   # top part: colour code with labels
  main_panel,
  ncol    = 1,
  heights = c(0.6, 8)  # adjust if you want more/less space for the key
)

ggsave(
  "images/rf_70.tiff",
  rf_70,
  width = 7,     # inches
  height = 5.5,    # adjust if needed
  dpi  = 1000,   # resolution for line art
  compression = "lzw"
)


##### 80


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_copernicus$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_copernicus$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_copernicus$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.8) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

# Apply threshold and classify
results_manual_rf_copernicus <- results_manual_rf_copernicus %>%
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

# --- Weekly Summary Table for Plotting ---
weekly_summary_rf_copernicus <- results_manual_rf_copernicus %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")

# --- Confusion Matrix ---
confusion_matrix_rf_copernicus <- table(
  Predicted = results_manual_rf_copernicus$Predicted_Class,
  Actual = results_manual_rf_copernicus$Actual
)

cat("\n📌 Confusion Matrix (Random Forest):\n")
print(confusion_matrix_rf_copernicus)

# --- Sanity check output
head(weekly_summary_rf_copernicus)


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_16s$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_16s$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_16s$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.8) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

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


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_16s_all$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_16s_all$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_16s_all$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.8) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

# Apply threshold and classify
results_manual_rf_16s_all <- results_manual_rf_16s_all %>%
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

# --- Weekly Summary Table for Plotting ---
weekly_summary_rf_16s_all <- results_manual_rf_16s_all %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_18s$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_18s$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_18s$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.8) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

# Apply threshold and classify
results_manual_rf_18s <- results_manual_rf_18s %>%
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

# --- Weekly Summary Table for Plotting ---
weekly_summary_rf_18s <- results_manual_rf_18s %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_18s_all$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_18s_all$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_18s_all$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.8) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

# Apply threshold and classify
results_manual_rf_18s_all <- results_manual_rf_18s_all %>%
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

# --- Weekly Summary Table for Plotting ---
weekly_summary_rf_18s_all <- results_manual_rf_18s_all %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")



# 1. Get full date range
all_dates <- c(
  weekly_summary_rf_copernicus$week_start,
  weekly_summary_rf_16s$week_start,
  weekly_summary_rf_18s$week_start,
  weekly_summary_rf_16s_all$week_start,
  weekly_summary_rf_18s_all$week_start
)
min_date <- min(all_dates)
max_date <- max(all_dates)
full_weeks <- seq(min_date, max_date, by = "7 days")

# 2. Define all categories
all_categories <- c("True Positive", "False Negative", "False Positive", "True Negative")

# 3. Padding function
pad_weekly_data <- function(df) {
  expand_grid(week_start = full_weeks, Category = all_categories) %>%
    left_join(df, by = c("week_start", "Category")) %>%
    mutate(Count = replace_na(Count, 0))
}

# 4. Apply to each dataset
weekly_summary_rf_copernicus <- pad_weekly_data(weekly_summary_rf_copernicus)
weekly_summary_rf_16s        <- pad_weekly_data(weekly_summary_rf_16s)
weekly_summary_rf_18s        <- pad_weekly_data(weekly_summary_rf_18s)
weekly_summary_rf_16s_all    <- pad_weekly_data(weekly_summary_rf_16s_all)
weekly_summary_rf_18s_all    <- pad_weekly_data(weekly_summary_rf_18s_all)

# 5. Plotting function (updated)
make_weekly_plot <- function(df, model_title, category_colors, show_x_axis = FALSE) {
  df <- df %>%
    group_by(week_start) %>%
    mutate(Percent = Count / sum(Count) * 100) %>%
    ungroup()
  
  ggplot(df, aes(x = week_start, y = Percent, fill = Category)) +
    geom_bar(stat = "identity", position = "stack", width = 6) +
    scale_fill_manual(values = category_colors) +
    scale_x_date(
      date_labels = "%b %Y",
      date_breaks = "1 month",
      limits = c(min_date, max_date)  # Ensure shared limits
    ) +
    labs(x = NULL, y = NULL, title = model_title) +
    theme_minimal(base_size = 9) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
      axis.text.x = if (show_x_axis) element_text(angle = 45, hjust = 1, size = 8) else element_blank(),
      axis.ticks.x = if (show_x_axis) element_line() else element_blank(),
      panel.grid.minor = element_blank()
    )
}

# Function to create confusion matrix tile plot (updated)
make_conf_matrix_plot <- function(results_df, category_colors) {
  conf_df <- as.data.frame(table(
    Predicted = results_df$Predicted_Class,
    Actual    = results_df$Actual
  )) %>%
    mutate(
      Label = case_when(
        Predicted == 1 & Actual == 1 ~ "True Positive",
        Predicted == 0 & Actual == 0 ~ "True Negative",
        Predicted == 1 & Actual == 0 ~ "False Positive",
        Predicted == 0 & Actual == 1 ~ "False Negative"
      )
    )
  
  ggplot(conf_df, aes(x = factor(Actual), y = factor(Predicted), fill = Label)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), color = "white", size = 3.5, fontface = "bold") +
    scale_fill_manual(values = category_colors) +
    labs(x = "Actual", y = "Predicted") +
    theme_minimal(base_size = 9) +
    theme(
      aspect.ratio = 1,
      legend.position = "none",
      panel.grid = element_blank(),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 9, face = "bold")
    )
}

# ---- Color palette for categories ----
category_colors <- c(
  "True Positive"  = "#0072B2",  # blue
  "False Negative" = "#D55E00",  # vermillion
  "False Positive" = "#E69F00",  # orange
  "True Negative"  = "#56B4E9"   # light blue
)

# ---- Top color key (label + coloured circle) ----
color_key_df <- data.frame(
  Category = factor(names(category_colors), levels = names(category_colors)),
  x = seq_along(category_colors)
)

color_key_plot <- ggplot(color_key_df, aes(y = 1)) +
  # Category text
  geom_text(aes(x = x, label = Category),
            hjust = 0.2,
            vjust = 0.5,
            fontface = "bold",
            size = 3) +
  # small colour circles to the right of the text
  geom_point(aes(x = x + 0.68, color = Category),
             size = 3) +
  scale_color_manual(values = category_colors) +
  # make sure nothing is clipped on left or right
  scale_x_continuous(
    limits = c(0.5, max(color_key_df$x) + 1.2),
    expand = expansion(mult = 0)
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = unit(c(0, 0, 5, 0), "pt")
  )

# ---- Create vertically stacked bar plots ----
bar_grobs <- list(
  make_weekly_plot(
    weekly_summary_rf_copernicus,
    "Copernicus satellite data: Baltic Sea. Time-lag 5",
    category_colors,
    show_x_axis = FALSE
  ),
  make_weekly_plot(
    weekly_summary_rf_16s,
    "16S RA dataset: Baltic Sea. Time-lag 7",
    category_colors,
    show_x_axis = FALSE
  ),
  make_weekly_plot(
    weekly_summary_rf_18s,
    "18S RA dataset: Baltic Sea. Time-lag 7",
    category_colors,
    show_x_axis = FALSE
  ),
  make_weekly_plot(
    weekly_summary_rf_16s_all,
    "16S RA dataset: Baltic Sea and Warnow Estuary. Time-lag 9",
    category_colors,
    show_x_axis = FALSE
  ),
  make_weekly_plot(
    weekly_summary_rf_18s_all,
    "18S RA dataset: Baltic Sea and Warnow Estuary. Time-lag 7",
    category_colors,
    show_x_axis = TRUE
  )
)

# ---- Create vertically stacked confusion matrices ----
conf_grobs <- list(
  make_conf_matrix_plot(results_manual_rf_copernicus, category_colors),
  make_conf_matrix_plot(results_manual_rf_16s, category_colors),
  make_conf_matrix_plot(results_manual_rf_18s, category_colors),
  make_conf_matrix_plot(results_manual_rf_16s_all, category_colors),
  make_conf_matrix_plot(results_manual_rf_18s_all, category_colors)
)

# ---- Bar plot rows with preserved custom heights ----
bar_rows <- list(
  bar_grobs[[1]],  # Copernicus
  bar_grobs[[2]],  # 16S
  bar_grobs[[3]],  # 18S
  bar_grobs[[4]],  # 16S All Locations
  bar_grobs[[5]]   # 18S All Locations
)

# Bar column without region group labels
bar_column <- arrangeGrob(
  grobs = bar_rows,
  ncol = 1,
  heights = unit(c(1, 1, 1, 1, 1.6), "null")  # custom plot heights
)

# ---- Confusion matrix column with consistent height ----
conf_column <- arrangeGrob(
  grobs = conf_grobs,
  ncol = 1,
  heights = unit(c(2, 2, 2, 2, 2), "null")
)

# ---- RF footer title ----
rf_footer <- textGrob(
  "RF model",
  gp   = gpar(fontsize = 10, fontface = "bold"),
  just = "center"
)

# Combine bar plots and RF label vertically
rf_column <- arrangeGrob(
  bar_column,
  #rf_footer,
  ncol    = 1,
  heights = unit.c(unit(1, "null"), unit(1.2, "lines"))
)

# ---- Final layout: top color key + main panel with shared y-axis ----
main_panel <- grid.arrange(
  textGrob(
    "Percentage correctly identified weekly [%]",
    rot = 90,
    gp  = gpar(fontsize = 10, fontface = "bold")
  ),
  rf_column,
  conf_column,
  ncol    = 4,
  widths  = unit.c(
    unit(1, "lines"),
    unit(3, "null"),
    unit(0.9, "null"),
    unit(0.1, "null")
  )
)

rf_80 <- arrangeGrob(
  color_key_plot,   # top part: colour code with labels
  main_panel,
  ncol    = 1,
  heights = c(0.6, 8)  # adjust if you want more/less space for the key
)

ggsave(
  "images/rf_80.tiff",
  rf_80,
  width = 7,     # inches
  height = 5.5,    # adjust if needed
  dpi  = 1000,   # resolution for line art
  compression = "lzw"
)


##### 90


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_copernicus$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_copernicus$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_copernicus$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.9) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

# Apply threshold and classify
results_manual_rf_copernicus <- results_manual_rf_copernicus %>%
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

# --- Weekly Summary Table for Plotting ---
weekly_summary_rf_copernicus <- results_manual_rf_copernicus %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")

# --- Confusion Matrix ---
confusion_matrix_rf_copernicus <- table(
  Predicted = results_manual_rf_copernicus$Predicted_Class,
  Actual = results_manual_rf_copernicus$Actual
)

cat("\n📌 Confusion Matrix (Random Forest):\n")
print(confusion_matrix_rf_copernicus)

# --- Sanity check output
head(weekly_summary_rf_copernicus)


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_16s$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_16s$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_16s$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.9) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

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


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_16s_all$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_16s_all$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_16s_all$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.9) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

# Apply threshold and classify
results_manual_rf_16s_all <- results_manual_rf_16s_all %>%
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

# --- Weekly Summary Table for Plotting ---
weekly_summary_rf_16s_all <- results_manual_rf_16s_all %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_18s$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_18s$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_18s$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.9) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

# Apply threshold and classify
results_manual_rf_18s <- results_manual_rf_18s %>%
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

# --- Weekly Summary Table for Plotting ---
weekly_summary_rf_18s <- results_manual_rf_18s %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_18s_all$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_18s_all$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_18s_all$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.9) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

# Apply threshold and classify
results_manual_rf_18s_all <- results_manual_rf_18s_all %>%
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

# --- Weekly Summary Table for Plotting ---
weekly_summary_rf_18s_all <- results_manual_rf_18s_all %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")



# 1. Get full date range
all_dates <- c(
  weekly_summary_rf_copernicus$week_start,
  weekly_summary_rf_16s$week_start,
  weekly_summary_rf_18s$week_start,
  weekly_summary_rf_16s_all$week_start,
  weekly_summary_rf_18s_all$week_start
)
min_date <- min(all_dates)
max_date <- max(all_dates)
full_weeks <- seq(min_date, max_date, by = "7 days")

# 2. Define all categories
all_categories <- c("True Positive", "False Negative", "False Positive", "True Negative")

# 3. Padding function
pad_weekly_data <- function(df) {
  expand_grid(week_start = full_weeks, Category = all_categories) %>%
    left_join(df, by = c("week_start", "Category")) %>%
    mutate(Count = replace_na(Count, 0))
}

# 4. Apply to each dataset
weekly_summary_rf_copernicus <- pad_weekly_data(weekly_summary_rf_copernicus)
weekly_summary_rf_16s        <- pad_weekly_data(weekly_summary_rf_16s)
weekly_summary_rf_18s        <- pad_weekly_data(weekly_summary_rf_18s)
weekly_summary_rf_16s_all    <- pad_weekly_data(weekly_summary_rf_16s_all)
weekly_summary_rf_18s_all    <- pad_weekly_data(weekly_summary_rf_18s_all)

# 5. Plotting function (updated)
make_weekly_plot <- function(df, model_title, category_colors, show_x_axis = FALSE) {
  df <- df %>%
    group_by(week_start) %>%
    mutate(Percent = Count / sum(Count) * 100) %>%
    ungroup()
  
  ggplot(df, aes(x = week_start, y = Percent, fill = Category)) +
    geom_bar(stat = "identity", position = "stack", width = 6) +
    scale_fill_manual(values = category_colors) +
    scale_x_date(
      date_labels = "%b %Y",
      date_breaks = "1 month",
      limits = c(min_date, max_date)  # Ensure shared limits
    ) +
    labs(x = NULL, y = NULL, title = model_title) +
    theme_minimal(base_size = 9) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
      axis.text.x = if (show_x_axis) element_text(angle = 45, hjust = 1, size = 8) else element_blank(),
      axis.ticks.x = if (show_x_axis) element_line() else element_blank(),
      panel.grid.minor = element_blank()
    )
}

# Function to create confusion matrix tile plot (updated)
make_conf_matrix_plot <- function(results_df, category_colors) {
  conf_df <- as.data.frame(table(
    Predicted = results_df$Predicted_Class,
    Actual    = results_df$Actual
  )) %>%
    mutate(
      Label = case_when(
        Predicted == 1 & Actual == 1 ~ "True Positive",
        Predicted == 0 & Actual == 0 ~ "True Negative",
        Predicted == 1 & Actual == 0 ~ "False Positive",
        Predicted == 0 & Actual == 1 ~ "False Negative"
      )
    )
  
  ggplot(conf_df, aes(x = factor(Actual), y = factor(Predicted), fill = Label)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), color = "white", size = 3.5, fontface = "bold") +
    scale_fill_manual(values = category_colors) +
    labs(x = "Actual", y = "Predicted") +
    theme_minimal(base_size = 9) +
    theme(
      aspect.ratio = 1,
      legend.position = "none",
      panel.grid = element_blank(),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 9, face = "bold")
    )
}

# ---- Color palette for categories ----
category_colors <- c(
  "True Positive"  = "#0072B2",  # blue
  "False Negative" = "#D55E00",  # vermillion
  "False Positive" = "#E69F00",  # orange
  "True Negative"  = "#56B4E9"   # light blue
)

# ---- Top color key (label + coloured circle) ----
color_key_df <- data.frame(
  Category = factor(names(category_colors), levels = names(category_colors)),
  x = seq_along(category_colors)
)

color_key_plot <- ggplot(color_key_df, aes(y = 1)) +
  # Category text
  geom_text(aes(x = x, label = Category),
            hjust = 0.2,
            vjust = 0.5,
            fontface = "bold",
            size = 3) +
  # small colour circles to the right of the text
  geom_point(aes(x = x + 0.68, color = Category),
             size = 3) +
  scale_color_manual(values = category_colors) +
  # make sure nothing is clipped on left or right
  scale_x_continuous(
    limits = c(0.5, max(color_key_df$x) + 1.2),
    expand = expansion(mult = 0)
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = unit(c(0, 0, 5, 0), "pt")
  )

# ---- Create vertically stacked bar plots ----
bar_grobs <- list(
  make_weekly_plot(
    weekly_summary_rf_copernicus,
    "Copernicus satellite data: Baltic Sea. Time-lag 5",
    category_colors,
    show_x_axis = FALSE
  ),
  make_weekly_plot(
    weekly_summary_rf_16s,
    "16S RA dataset: Baltic Sea. Time-lag 7",
    category_colors,
    show_x_axis = FALSE
  ),
  make_weekly_plot(
    weekly_summary_rf_18s,
    "18S RA dataset: Baltic Sea. Time-lag 7",
    category_colors,
    show_x_axis = FALSE
  ),
  make_weekly_plot(
    weekly_summary_rf_16s_all,
    "16S RA dataset: Baltic Sea and Warnow Estuary. Time-lag 9",
    category_colors,
    show_x_axis = FALSE
  ),
  make_weekly_plot(
    weekly_summary_rf_18s_all,
    "18S RA dataset: Baltic Sea and Warnow Estuary. Time-lag 7",
    category_colors,
    show_x_axis = TRUE
  )
)

# ---- Create vertically stacked confusion matrices ----
conf_grobs <- list(
  make_conf_matrix_plot(results_manual_rf_copernicus, category_colors),
  make_conf_matrix_plot(results_manual_rf_16s, category_colors),
  make_conf_matrix_plot(results_manual_rf_18s, category_colors),
  make_conf_matrix_plot(results_manual_rf_16s_all, category_colors),
  make_conf_matrix_plot(results_manual_rf_18s_all, category_colors)
)

# ---- Bar plot rows with preserved custom heights ----
bar_rows <- list(
  bar_grobs[[1]],  # Copernicus
  bar_grobs[[2]],  # 16S
  bar_grobs[[3]],  # 18S
  bar_grobs[[4]],  # 16S All Locations
  bar_grobs[[5]]   # 18S All Locations
)

# Bar column without region group labels
bar_column <- arrangeGrob(
  grobs = bar_rows,
  ncol = 1,
  heights = unit(c(1, 1, 1, 1, 1.6), "null")  # custom plot heights
)

# ---- Confusion matrix column with consistent height ----
conf_column <- arrangeGrob(
  grobs = conf_grobs,
  ncol = 1,
  heights = unit(c(2, 2, 2, 2, 2), "null")
)

# ---- RF footer title ----
rf_footer <- textGrob(
  "RF model",
  gp   = gpar(fontsize = 10, fontface = "bold"),
  just = "center"
)

# Combine bar plots and RF label vertically
rf_column <- arrangeGrob(
  bar_column,
  #rf_footer,
  ncol    = 1,
  heights = unit.c(unit(1, "null"), unit(1.2, "lines"))
)

# ---- Final layout: top color key + main panel with shared y-axis ----
main_panel <- grid.arrange(
  textGrob(
    "Percentage correctly identified weekly [%]",
    rot = 90,
    gp  = gpar(fontsize = 10, fontface = "bold")
  ),
  rf_column,
  conf_column,
  ncol    = 4,
  widths  = unit.c(
    unit(1, "lines"),
    unit(3, "null"),
    unit(0.9, "null"),
    unit(0.1, "null")
  )
)

rf_90 <- arrangeGrob(
  color_key_plot,   # top part: colour code with labels
  main_panel,
  ncol    = 1,
  heights = c(0.6, 8)  # adjust if you want more/less space for the key
)

ggsave(
  "images/rf_90.tiff",
  rf_90,
  width = 7,     # inches
  height = 5.5,    # adjust if needed
  dpi  = 1000,   # resolution for line art
  compression = "lzw"
)


##### 100


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_copernicus$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_copernicus$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_copernicus$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 1) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

# Apply threshold and classify
results_manual_rf_copernicus <- results_manual_rf_copernicus %>%
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

# --- Weekly Summary Table for Plotting ---
weekly_summary_rf_copernicus <- results_manual_rf_copernicus %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")

# --- Confusion Matrix ---
confusion_matrix_rf_copernicus <- table(
  Predicted = results_manual_rf_copernicus$Predicted_Class,
  Actual = results_manual_rf_copernicus$Actual
)

cat("\n📌 Confusion Matrix (Random Forest):\n")
print(confusion_matrix_rf_copernicus)

# --- Sanity check output
head(weekly_summary_rf_copernicus)


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_16s$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_16s$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_16s$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 1) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

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


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_16s_all$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_16s_all$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_16s_all$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 1) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

# Apply threshold and classify
results_manual_rf_16s_all <- results_manual_rf_16s_all %>%
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

# --- Weekly Summary Table for Plotting ---
weekly_summary_rf_16s_all <- results_manual_rf_16s_all %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_18s$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_18s$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_18s$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 1) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

# Apply threshold and classify
results_manual_rf_18s <- results_manual_rf_18s %>%
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

# --- Weekly Summary Table for Plotting ---
weekly_summary_rf_18s <- results_manual_rf_18s %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")


# Threshold selection to ensure ≥50% recall
actual_positives <- sum(results_manual_rf_18s_all$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_rf_18s_all$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_rf_18s_all$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 1) {
    best_threshold_rf <- t
    break
  }
}

cat(sprintf("✅ Final manual threshold for RF (≥50%% recall): %.2f\n", best_threshold_rf))

# Apply threshold and classify
results_manual_rf_18s_all <- results_manual_rf_18s_all %>%
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

# --- Weekly Summary Table for Plotting ---
weekly_summary_rf_18s_all <- results_manual_rf_18s_all %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")



# 1. Get full date range
all_dates <- c(
  weekly_summary_rf_copernicus$week_start,
  weekly_summary_rf_16s$week_start,
  weekly_summary_rf_18s$week_start,
  weekly_summary_rf_16s_all$week_start,
  weekly_summary_rf_18s_all$week_start
)
min_date <- min(all_dates)
max_date <- max(all_dates)
full_weeks <- seq(min_date, max_date, by = "7 days")

# 2. Define all categories
all_categories <- c("True Positive", "False Negative", "False Positive", "True Negative")

# 3. Padding function
pad_weekly_data <- function(df) {
  expand_grid(week_start = full_weeks, Category = all_categories) %>%
    left_join(df, by = c("week_start", "Category")) %>%
    mutate(Count = replace_na(Count, 0))
}

# 4. Apply to each dataset
weekly_summary_rf_copernicus <- pad_weekly_data(weekly_summary_rf_copernicus)
weekly_summary_rf_16s        <- pad_weekly_data(weekly_summary_rf_16s)
weekly_summary_rf_18s        <- pad_weekly_data(weekly_summary_rf_18s)
weekly_summary_rf_16s_all    <- pad_weekly_data(weekly_summary_rf_16s_all)
weekly_summary_rf_18s_all    <- pad_weekly_data(weekly_summary_rf_18s_all)

# 5. Plotting function (updated)
make_weekly_plot <- function(df, model_title, category_colors, show_x_axis = FALSE) {
  df <- df %>%
    group_by(week_start) %>%
    mutate(Percent = Count / sum(Count) * 100) %>%
    ungroup()
  
  ggplot(df, aes(x = week_start, y = Percent, fill = Category)) +
    geom_bar(stat = "identity", position = "stack", width = 6) +
    scale_fill_manual(values = category_colors) +
    scale_x_date(
      date_labels = "%b %Y",
      date_breaks = "1 month",
      limits = c(min_date, max_date)  # Ensure shared limits
    ) +
    labs(x = NULL, y = NULL, title = model_title) +
    theme_minimal(base_size = 9) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
      axis.text.x = if (show_x_axis) element_text(angle = 45, hjust = 1, size = 8) else element_blank(),
      axis.ticks.x = if (show_x_axis) element_line() else element_blank(),
      panel.grid.minor = element_blank()
    )
}

# Function to create confusion matrix tile plot (updated)
make_conf_matrix_plot <- function(results_df, category_colors) {
  conf_df <- as.data.frame(table(
    Predicted = results_df$Predicted_Class,
    Actual    = results_df$Actual
  )) %>%
    mutate(
      Label = case_when(
        Predicted == 1 & Actual == 1 ~ "True Positive",
        Predicted == 0 & Actual == 0 ~ "True Negative",
        Predicted == 1 & Actual == 0 ~ "False Positive",
        Predicted == 0 & Actual == 1 ~ "False Negative"
      )
    )
  
  ggplot(conf_df, aes(x = factor(Actual), y = factor(Predicted), fill = Label)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), color = "white", size = 3.5, fontface = "bold") +
    scale_fill_manual(values = category_colors) +
    labs(x = "Actual", y = "Predicted") +
    theme_minimal(base_size = 9) +
    theme(
      aspect.ratio = 1,
      legend.position = "none",
      panel.grid = element_blank(),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 9, face = "bold")
    )
}

# ---- Color palette for categories ----
category_colors <- c(
  "True Positive"  = "#0072B2",  # blue
  "False Negative" = "#D55E00",  # vermillion
  "False Positive" = "#E69F00",  # orange
  "True Negative"  = "#56B4E9"   # light blue
)

# ---- Top color key (label + coloured circle) ----
color_key_df <- data.frame(
  Category = factor(names(category_colors), levels = names(category_colors)),
  x = seq_along(category_colors)
)

color_key_plot <- ggplot(color_key_df, aes(y = 1)) +
  # Category text
  geom_text(aes(x = x, label = Category),
            hjust = 0.2,
            vjust = 0.5,
            fontface = "bold",
            size = 3) +
  # small colour circles to the right of the text
  geom_point(aes(x = x + 0.68, color = Category),
             size = 3) +
  scale_color_manual(values = category_colors) +
  # make sure nothing is clipped on left or right
  scale_x_continuous(
    limits = c(0.5, max(color_key_df$x) + 1.2),
    expand = expansion(mult = 0)
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = unit(c(0, 0, 5, 0), "pt")
  )

# ---- Create vertically stacked bar plots ----
bar_grobs <- list(
  make_weekly_plot(
    weekly_summary_rf_copernicus,
    "Copernicus satellite data: Baltic Sea. Time-lag 5",
    category_colors,
    show_x_axis = FALSE
  ),
  make_weekly_plot(
    weekly_summary_rf_16s,
    "16S RA dataset: Baltic Sea. Time-lag 7",
    category_colors,
    show_x_axis = FALSE
  ),
  make_weekly_plot(
    weekly_summary_rf_18s,
    "18S RA dataset: Baltic Sea. Time-lag 7",
    category_colors,
    show_x_axis = FALSE
  ),
  make_weekly_plot(
    weekly_summary_rf_16s_all,
    "16S RA dataset: Baltic Sea and Warnow Estuary. Time-lag 9",
    category_colors,
    show_x_axis = FALSE
  ),
  make_weekly_plot(
    weekly_summary_rf_18s_all,
    "18S RA dataset: Baltic Sea and Warnow Estuary. Time-lag 7",
    category_colors,
    show_x_axis = TRUE
  )
)

# ---- Create vertically stacked confusion matrices ----
conf_grobs <- list(
  make_conf_matrix_plot(results_manual_rf_copernicus, category_colors),
  make_conf_matrix_plot(results_manual_rf_16s, category_colors),
  make_conf_matrix_plot(results_manual_rf_18s, category_colors),
  make_conf_matrix_plot(results_manual_rf_16s_all, category_colors),
  make_conf_matrix_plot(results_manual_rf_18s_all, category_colors)
)

# ---- Bar plot rows with preserved custom heights ----
bar_rows <- list(
  bar_grobs[[1]],  # Copernicus
  bar_grobs[[2]],  # 16S
  bar_grobs[[3]],  # 18S
  bar_grobs[[4]],  # 16S All Locations
  bar_grobs[[5]]   # 18S All Locations
)

# Bar column without region group labels
bar_column <- arrangeGrob(
  grobs = bar_rows,
  ncol = 1,
  heights = unit(c(1, 1, 1, 1, 1.6), "null")  # custom plot heights
)

# ---- Confusion matrix column with consistent height ----
conf_column <- arrangeGrob(
  grobs = conf_grobs,
  ncol = 1,
  heights = unit(c(2, 2, 2, 2, 2), "null")
)

# ---- RF footer title ----
rf_footer <- textGrob(
  "RF model",
  gp   = gpar(fontsize = 10, fontface = "bold"),
  just = "center"
)

# Combine bar plots and RF label vertically
rf_column <- arrangeGrob(
  bar_column,
  #rf_footer,
  ncol    = 1,
  heights = unit.c(unit(1, "null"), unit(1.2, "lines"))
)

# ---- Final layout: top color key + main panel with shared y-axis ----
main_panel <- grid.arrange(
  textGrob(
    "Percentage correctly identified weekly [%]",
    rot = 90,
    gp  = gpar(fontsize = 10, fontface = "bold")
  ),
  rf_column,
  conf_column,
  ncol    = 4,
  widths  = unit.c(
    unit(1, "lines"),
    unit(3, "null"),
    unit(0.9, "null"),
    unit(0.1, "null")
  )
)

rf_100 <- arrangeGrob(
  color_key_plot,   # top part: colour code with labels
  main_panel,
  ncol    = 1,
  heights = c(0.6, 8)  # adjust if you want more/less space for the key
)

ggsave(
  "images/rf_100.tiff",
  rf_100,
  width = 7,     # inches
  height = 5.5,    # adjust if needed
  dpi  = 1000,   # resolution for line art
  compression = "lzw"
)
