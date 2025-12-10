# Name: Conor Glackin
# Date: 28 July 2025
# Description: producing confusion matrices for 50%, 60%, 80%, 90% and 100%

#### 50

# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_copernicus$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_copernicus$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_copernicus$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.5) {
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

# Step: Weekly summary
weekly_summary_copernicus <- results_manual_copernicus %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")



# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_16s$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001


for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_16s$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_16s$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.5) {
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




# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_18s$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_18s$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_18s$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.5) {
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


# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_16s_all$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_16s_all$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_16s_all$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.5) {
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


# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_18s_all$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_18s_all$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_18s_all$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.5) {
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


########## lstm 

# 1. Get full date range
all_dates <- c(
  weekly_summary_copernicus$week_start,
  weekly_summary_16s$week_start,
  weekly_summary_18s$week_start,
  weekly_summary_16s_all$week_start,
  weekly_summary_18s_all$week_start
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
weekly_summary_copernicus <- pad_weekly_data(weekly_summary_copernicus)
weekly_summary_16s        <- pad_weekly_data(weekly_summary_16s)
weekly_summary_18s        <- pad_weekly_data(weekly_summary_18s)
weekly_summary_16s_all    <- pad_weekly_data(weekly_summary_16s_all)
weekly_summary_18s_all    <- pad_weekly_data(weekly_summary_18s_all)

# 5. Plotting function (FONT UPDATED)
make_weekly_plot <- function(df, model_title, category_colors, show_x_axis = FALSE) {
  df <- df %>%
    group_by(week_start) %>%
    mutate(Percent = Count / sum(Count) * 100) %>%
    ungroup()
  
  ggplot(df, aes(x = week_start, y = Percent, fill = Category)) +
    geom_bar(stat = "identity", position = "stack", width = 6) +
    scale_fill_manual(values = category_colors) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month",
                 limits = c(min_date, max_date)) +
    labs(x = NULL, y = NULL, title = model_title) +
    theme_minimal(base_size = 9) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
      axis.text.x = if (show_x_axis) {
        element_text(angle = 45, hjust = 1, size = 8)
      } else {
        element_blank()
      },
      axis.ticks.x = if (show_x_axis) element_line() else element_blank(),
      panel.grid.minor = element_blank()
    )
}

# Function to create confusion matrix tile plot (FONT UPDATED)
make_conf_matrix_plot <- function(results_df, category_colors) {
  conf_df <- as.data.frame(table(Predicted = results_df$Predicted_Class, Actual = results_df$Actual)) %>%
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
    geom_text(aes(label = Freq), color = "white", size = 3.5, fontface = "bold") +  # smaller text
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

# Define color palette (unchanged)
category_colors <- c(
  "True Positive"  = "#0072B2",  # blue
  "False Negative" = "#D55E00",  # vermillion
  "False Positive" = "#E69F00",  # orange
  "True Negative"  = "#56B4E9"   # light blue
)

# ---- Top color key (FONT UPDATED) ----
color_key_df <- data.frame(
  Category = factor(names(category_colors), levels = names(category_colors)),
  x = seq_along(category_colors)
)

color_key_plot <- ggplot(color_key_df, aes(y = 1)) +
  geom_text(aes(x = x, label = Category),
            hjust = 0.2,
            vjust = 0.5,
            fontface = "bold",
            size = 3) +        # was 4
  geom_point(aes(x = x + 0.68, color = Category),
             size = 3) +        # slightly smaller
  scale_color_manual(values = category_colors) +
  scale_x_continuous(
    limits = c(0.5, max(color_key_df$x) + 1.2),
    expand = expansion(mult = 0)
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = unit(c(0, 0, 5, 0), "pt")
  )

# Create vertically stacked bar plots
bar_grobs <- list(
  make_weekly_plot(weekly_summary_copernicus, "Copernicus satellite dataset: Baltic Sea. Time-step 1-10", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_16s,        "16S RA dataset: Baltic Sea. Time-step 1-2",              category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_18s,        "18S RA dataset: Baltic Sea. Time-step 1-4",              category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_16s_all,    "16S RA dataset: Baltic Sea and Warnow Estuary. Time-step 1-4", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_18s_all,    "18S RA dataset: Baltic Sea and Warnow Estuary. Time-step 1-4", category_colors, show_x_axis = TRUE)
)

# Create vertically stacked confusion matrices
conf_grobs <- list(
  make_conf_matrix_plot(results_manual_copernicus, category_colors),
  make_conf_matrix_plot(results_manual_16s,       category_colors),
  make_conf_matrix_plot(results_manual_18s,       category_colors),
  make_conf_matrix_plot(results_manual_16s_all,   category_colors),
  make_conf_matrix_plot(results_manual_18s_all,   category_colors)
)

# Bar plot rows with preserved custom heights
bar_rows <- list(
  bar_grobs[[1]],
  bar_grobs[[2]],
  bar_grobs[[3]],
  bar_grobs[[4]],
  bar_grobs[[5]]
)

bar_column <- arrangeGrob(
  grobs = bar_rows,
  ncol = 1,
  heights = unit(c(1, 1, 1, 1, 1.6), "null")
)

conf_column <- arrangeGrob(
  grobs = conf_grobs,
  ncol = 1,
  heights = unit(c(2, 2, 2, 2, 2), "null")
)

# Shared y-axis label (FONT UPDATED)
grid.arrange(
  textGrob("Percentage correctly identified weekly [%]",
           rot = 90,
           gp = gpar(fontsize = 10, fontface = "bold")),
  bar_column,
  conf_column,
  ncol = 4,
  widths = unit.c(unit(1, "lines"), unit(3, "null"), unit(0.9, "null"), unit(0.2, "null"))
)

recall_50_plot <- arrangeGrob(
  textGrob("Percentage correctly identified weekly [%]",
           rot = 90,
           gp = gpar(fontsize = 10, fontface = "bold")),
  bar_column,
  conf_column,
  ncol = 4,
  widths = unit.c(unit(1, "lines"), unit(3, "null"), unit(0.9, "null"), unit(0.1, "null"))
)

# ---- Final LSTM figure with top colour key + save to file ----
lstm_50 <- arrangeGrob(
  color_key_plot,
  recall_50_plot,
  ncol    = 1,
  heights = c(0.6, 8)
)

ggsave(
  "images/lstm_50.tiff",
  lstm_50,
  width  = 7,
  height = 5.5,
  dpi = 1000,
  compression = "lzw"
)

#### 60

# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_copernicus$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_copernicus$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_copernicus$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.6) {
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

# Step: Weekly summary
weekly_summary_copernicus <- results_manual_copernicus %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")



# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_16s$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001


for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_16s$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_16s$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.6) {
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




# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_18s$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_18s$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_18s$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.6) {
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


# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_16s_all$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_16s_all$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_16s_all$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.6) {
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


# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_18s_all$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_18s_all$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_18s_all$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.6) {
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


########## lstm 

# 1. Get full date range
all_dates <- c(
  weekly_summary_copernicus$week_start,
  weekly_summary_16s$week_start,
  weekly_summary_18s$week_start,
  weekly_summary_16s_all$week_start,
  weekly_summary_18s_all$week_start
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
weekly_summary_copernicus <- pad_weekly_data(weekly_summary_copernicus)
weekly_summary_16s        <- pad_weekly_data(weekly_summary_16s)
weekly_summary_18s        <- pad_weekly_data(weekly_summary_18s)
weekly_summary_16s_all    <- pad_weekly_data(weekly_summary_16s_all)
weekly_summary_18s_all    <- pad_weekly_data(weekly_summary_18s_all)

# 5. Plotting function (FONT UPDATED)
make_weekly_plot <- function(df, model_title, category_colors, show_x_axis = FALSE) {
  df <- df %>%
    group_by(week_start) %>%
    mutate(Percent = Count / sum(Count) * 100) %>%
    ungroup()
  
  ggplot(df, aes(x = week_start, y = Percent, fill = Category)) +
    geom_bar(stat = "identity", position = "stack", width = 6) +
    scale_fill_manual(values = category_colors) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month",
                 limits = c(min_date, max_date)) +  # Ensure shared limits
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

# Function to create confusion matrix tile plot (FONT UPDATED)
make_conf_matrix_plot <- function(results_df, category_colors) {
  conf_df <- as.data.frame(table(Predicted = results_df$Predicted_Class, Actual = results_df$Actual)) %>%
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

# Define color palette (updated)
category_colors <- c(
  "True Positive"  = "#0072B2",  # blue
  "False Negative" = "#D55E00",  # vermillion
  "False Positive" = "#E69F00",  # orange
  "True Negative"  = "#56B4E9"   # light blue
)

# ---- Top color key (label + coloured circle, FONT UPDATED) ----
color_key_df <- data.frame(
  Category = factor(names(category_colors), levels = names(category_colors)),
  x = seq_along(category_colors)
)

color_key_plot <- ggplot(color_key_df, aes(y = 1)) +
  geom_text(aes(x = x, label = Category),
            hjust = 0.2,
            vjust = 0.5,
            fontface = "bold",
            size = 3) +
  geom_point(aes(x = x + 0.68, color = Category),
             size = 3) +
  scale_color_manual(values = category_colors) +
  scale_x_continuous(
    limits = c(0.5, max(color_key_df$x) + 1.2),
    expand = expansion(mult = 0)
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = unit(c(0, 0, 5, 0), "pt")
  )

# Create vertically stacked bar plots
bar_grobs <- list(
  make_weekly_plot(weekly_summary_copernicus, "Copernicus satellite dataset: Baltic Sea. Time-step 1-10", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_16s, "16S RA dataset: Baltic Sea. Time-step 1-2", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_18s, "18S RA dataset: Baltic Sea. Time-step 1-4", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_16s_all, "16S RA dataset: Baltic Sea and Warnow Estuary. Time-step 1-4", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_18s_all, "18S RA dataset: Baltic Sea and Warnow Estuary. Time-step 1-4", category_colors, show_x_axis = TRUE)
)

# Create vertically stacked confusion matrices
conf_grobs <- list(
  make_conf_matrix_plot(results_manual_copernicus, category_colors),
  make_conf_matrix_plot(results_manual_16s, category_colors),
  make_conf_matrix_plot(results_manual_18s, category_colors),
  make_conf_matrix_plot(results_manual_16s_all, category_colors),
  make_conf_matrix_plot(results_manual_18s_all, category_colors)
)


# Bar plot rows with preserved custom heights
bar_rows <- list(
  bar_grobs[[1]],  # LSTM
  bar_grobs[[2]],  # 16S
  bar_grobs[[3]],  # 18S
  bar_grobs[[4]],  # 16S All Locations
  bar_grobs[[5]]   # 18S All Locations
)

# Bar column without region group labels
bar_column <- arrangeGrob(
  grobs = bar_rows,
  ncol = 1,
  heights = unit(c(1, 1, 1, 1, 1.6), "null")
)

# Confusion matrix column
conf_column <- arrangeGrob(
  grobs = conf_grobs,
  ncol = 1,
  heights = unit(c(2, 2, 2, 2, 2), "null")
)

# Combine side-by-side with shared y-axis label (FONT UPDATED)
grid.arrange(
  textGrob("Percentage correctly identified weekly [%]", rot = 90,
           gp = gpar(fontsize = 10, fontface = "bold")),
  bar_column,
  conf_column,
  ncol = 4,
  widths = unit.c(unit(1, "lines"), unit(3, "null"), unit(0.9, "null"), unit(0.2, "null"))
)

recall_60_plot <- arrangeGrob(
  textGrob("Percentage correctly identified weekly [%]", rot = 90,
           gp = gpar(fontsize = 10, fontface = "bold")),
  bar_column,
  conf_column,
  ncol = 4,
  widths = unit.c(unit(1, "lines"), unit(3, "null"), unit(0.9, "null"), unit(0.1, "null"))
)

# ---- Final LSTM figure with top colour key + save to file ----
lstm_60 <- arrangeGrob(
  color_key_plot,
  recall_60_plot,
  ncol    = 1,
  heights = c(0.6, 8)
)

ggsave(
  "images/lstm_60.tiff",
  lstm_60,
  width  = 7,
  height = 5.5,
  dpi    = 1000,
  compression = "lzw"
)

#### 70

# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_copernicus$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

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

# Step: Weekly summary
weekly_summary_copernicus <- results_manual_copernicus %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")



# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_16s$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001


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




# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_18s$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

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


# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_16s_all$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

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


# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_18s_all$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

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


########## lstm 

# 1. Get full date range
all_dates <- c(
  weekly_summary_copernicus$week_start,
  weekly_summary_16s$week_start,
  weekly_summary_18s$week_start,
  weekly_summary_16s_all$week_start,
  weekly_summary_18s_all$week_start
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
weekly_summary_copernicus <- pad_weekly_data(weekly_summary_copernicus)
weekly_summary_16s        <- pad_weekly_data(weekly_summary_16s)
weekly_summary_18s        <- pad_weekly_data(weekly_summary_18s)
weekly_summary_16s_all    <- pad_weekly_data(weekly_summary_16s_all)
weekly_summary_18s_all    <- pad_weekly_data(weekly_summary_18s_all)

# 5. Plotting function (FONT UPDATED)
make_weekly_plot <- function(df, model_title, category_colors, show_x_axis = FALSE) {
  df <- df %>%
    group_by(week_start) %>%
    mutate(Percent = Count / sum(Count) * 100) %>%
    ungroup()
  
  ggplot(df, aes(x = week_start, y = Percent, fill = Category)) +
    geom_bar(stat = "identity", position = "stack", width = 6) +
    scale_fill_manual(values = category_colors) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month",
                 limits = c(min_date, max_date)) +  # Ensure shared limits
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

# Function to create confusion matrix tile plot (FONT UPDATED)
make_conf_matrix_plot <- function(results_df, category_colors) {
  conf_df <- as.data.frame(table(Predicted = results_df$Predicted_Class, Actual = results_df$Actual)) %>%
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

# Define color palette (updated)
category_colors <- c(
  "True Positive"  = "#0072B2",  # blue
  "False Negative" = "#D55E00",  # vermillion
  "False Positive" = "#E69F00",  # orange
  "True Negative"  = "#56B4E9"   # light blue
)

# ---- Top color key (label + coloured circle, FONT UPDATED) ----
color_key_df <- data.frame(
  Category = factor(names(category_colors), levels = names(category_colors)),
  x = seq_along(category_colors)
)

color_key_plot <- ggplot(color_key_df, aes(y = 1)) +
  geom_text(aes(x = x, label = Category),
            hjust = 0.2,
            vjust = 0.5,
            fontface = "bold",
            size = 3) +
  geom_point(aes(x = x + 0.68, color = Category),
             size = 3) +
  scale_color_manual(values = category_colors) +
  scale_x_continuous(
    limits = c(0.5, max(color_key_df$x) + 1.2),
    expand = expansion(mult = 0)
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = unit(c(0, 0, 5, 0), "pt")
  )

# Create vertically stacked bar plots
bar_grobs <- list(
  make_weekly_plot(weekly_summary_copernicus, "Copernicus satellite dataset: Baltic Sea. Time-step 1-10", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_16s, "16S RA dataset: Baltic Sea. Time-step 1-2", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_18s, "18S RA dataset: Baltic Sea. Time-step 1-4", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_16s_all, "16S RA dataset: Baltic Sea and Warnow Estuary. Time-step 1-4", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_18s_all, "18S RA dataset: Baltic Sea and Warnow Estuary. Time-step 1-4", category_colors, show_x_axis = TRUE)
)

# Create vertically stacked confusion matrices
conf_grobs <- list(
  make_conf_matrix_plot(results_manual_copernicus, category_colors),
  make_conf_matrix_plot(results_manual_16s, category_colors),
  make_conf_matrix_plot(results_manual_18s, category_colors),
  make_conf_matrix_plot(results_manual_16s_all, category_colors),
  make_conf_matrix_plot(results_manual_18s_all, category_colors)
)

# Bar plot rows
bar_rows <- list(
  bar_grobs[[1]],
  bar_grobs[[2]],
  bar_grobs[[3]],
  bar_grobs[[4]],
  bar_grobs[[5]]
)

bar_column <- arrangeGrob(
  grobs = bar_rows,
  ncol = 1,
  heights = unit(c(1, 1, 1, 1, 1.6), "null")
)

conf_column <- arrangeGrob(
  grobs = conf_grobs,
  ncol = 1,
  heights = unit(c(2, 2, 2, 2, 2), "null")
)

grid.arrange(
  textGrob("Percentage correctly identified weekly [%]", rot = 90,
           gp = gpar(fontsize = 10, fontface = "bold")),
  bar_column,
  conf_column,
  ncol = 4,
  widths = unit.c(unit(1, "lines"), unit(3, "null"), unit(0.9, "null"), unit(0.2, "null"))
)

recall_70_plot <- arrangeGrob(
  textGrob("Percentage correctly identified weekly [%]", rot = 90,
           gp = gpar(fontsize = 10, fontface = "bold")),
  bar_column,
  conf_column,
  ncol = 4,
  widths = unit.c(unit(1, "lines"), unit(3, "null"), unit(0.9, "null"), unit(0.1, "null"))
)

lstm_70 <- arrangeGrob(
  color_key_plot,
  recall_70_plot,
  ncol    = 1,
  heights = c(0.6, 8)
)

ggsave(
  "images/lstm_70.tiff",
  lstm_70,
  width  = 7,
  height = 5.5,
  dpi    = 1000,
  compression = "lzw"
)

#### 80

# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_copernicus$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_copernicus$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_copernicus$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.8) {
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

# Step: Weekly summary
weekly_summary_copernicus <- results_manual_copernicus %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")



# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_16s$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001


for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_16s$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_16s$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.8) {
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




# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_18s$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_18s$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_18s$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.8) {
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


# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_16s_all$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_16s_all$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_16s_all$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.8) {
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


# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_18s_all$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_18s_all$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_18s_all$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.8) {
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


########## lstm 

# 1. Get full date range
all_dates <- c(
  weekly_summary_copernicus$week_start,
  weekly_summary_16s$week_start,
  weekly_summary_18s$week_start,
  weekly_summary_16s_all$week_start,
  weekly_summary_18s_all$week_start
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
weekly_summary_copernicus <- pad_weekly_data(weekly_summary_copernicus)
weekly_summary_16s        <- pad_weekly_data(weekly_summary_16s)
weekly_summary_18s        <- pad_weekly_data(weekly_summary_18s)
weekly_summary_16s_all    <- pad_weekly_data(weekly_summary_16s_all)
weekly_summary_18s_all    <- pad_weekly_data(weekly_summary_18s_all)

# 5. Plotting function (FONT UPDATED)
make_weekly_plot <- function(df, model_title, category_colors, show_x_axis = FALSE) {
  df <- df %>%
    group_by(week_start) %>%
    mutate(Percent = Count / sum(Count) * 100) %>%
    ungroup()
  
  ggplot(df, aes(x = week_start, y = Percent, fill = Category)) +
    geom_bar(stat = "identity", position = "stack", width = 6) +
    scale_fill_manual(values = category_colors) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month",
                 limits = c(min_date, max_date)) +
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

# Function to create confusion matrix tile plot (FONT UPDATED)
make_conf_matrix_plot <- function(results_df, category_colors) {
  conf_df <- as.data.frame(table(Predicted = results_df$Predicted_Class, Actual = results_df$Actual)) %>%
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

# Define color palette (updated)
category_colors <- c(
  "True Positive"  = "#0072B2",
  "False Negative" = "#D55E00",
  "False Positive" = "#E69F00",
  "True Negative"  = "#56B4E9"
)

# ---- Top color key (FONT UPDATED) ----
color_key_df <- data.frame(
  Category = factor(names(category_colors), levels = names(category_colors)),
  x = seq_along(category_colors)
)

color_key_plot <- ggplot(color_key_df, aes(y = 1)) +
  geom_text(aes(x = x, label = Category),
            hjust = 0.2,
            vjust = 0.5,
            fontface = "bold",
            size = 3) +
  geom_point(aes(x = x + 0.68, color = Category),
             size = 3) +
  scale_color_manual(values = category_colors) +
  scale_x_continuous(
    limits = c(0.5, max(color_key_df$x) + 1.2),
    expand = expansion(mult = 0)
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = unit(c(0, 0, 5, 0), "pt")
  )

bar_grobs <- list(
  make_weekly_plot(weekly_summary_copernicus, "Copernicus satellite dataset: Baltic Sea. Time-step 1-10", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_16s, "16S RA dataset: Baltic Sea. Time-step 1-2", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_18s, "18S RA dataset: Baltic Sea. Time-step 1-4", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_16s_all, "16S RA dataset: Baltic Sea and Warnow Estuary. Time-step 1-4", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_18s_all, "18S RA dataset: Baltic Sea and Warnow Estuary. Time-step 1-4", category_colors, show_x_axis = TRUE)
)

conf_grobs <- list(
  make_conf_matrix_plot(results_manual_copernicus, category_colors),
  make_conf_matrix_plot(results_manual_16s, category_colors),
  make_conf_matrix_plot(results_manual_18s, category_colors),
  make_conf_matrix_plot(results_manual_16s_all, category_colors),
  make_conf_matrix_plot(results_manual_18s_all, category_colors)
)

bar_rows <- list(
  bar_grobs[[1]],
  bar_grobs[[2]],
  bar_grobs[[3]],
  bar_grobs[[4]],
  bar_grobs[[5]]
)

bar_column <- arrangeGrob(
  grobs = bar_rows,
  ncol = 1,
  heights = unit(c(1, 1, 1, 1, 1.6), "null")
)

conf_column <- arrangeGrob(
  grobs = conf_grobs,
  ncol = 1,
  heights = unit(c(2, 2, 2, 2, 2), "null")
)

grid.arrange(
  textGrob("Percentage correctly identified weekly [%]", rot = 90,
           gp = gpar(fontsize = 10, fontface = "bold")),
  bar_column,
  conf_column,
  ncol = 4,
  widths = unit.c(unit(1, "lines"), unit(3, "null"), unit(0.9, "null"), unit(0.2, "null"))
)

recall_80_plot <- arrangeGrob(
  textGrob("Percentage correctly identified weekly [%]", rot = 90,
           gp = gpar(fontsize = 10, fontface = "bold")),
  bar_column,
  conf_column,
  ncol = 4,
  widths = unit.c(unit(1, "lines"), unit(3, "null"), unit(0.9, "null"), unit(0.1, "null"))
)

lstm_80 <- arrangeGrob(
  color_key_plot,
  recall_80_plot,
  ncol    = 1,
  heights = c(0.6, 8)
)

ggsave(
  "images/lstm_80.tiff",
  lstm_80,
  width  = 7,
  height = 5.5,
  dpi    = 1000,
  compression = "lzw"
)

#### 90

# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_copernicus$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_copernicus$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_copernicus$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.9) {
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

# Step: Weekly summary
weekly_summary_copernicus <- results_manual_copernicus %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")



# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_16s$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001


for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_16s$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_16s$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.9) {
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




# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_18s$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_18s$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_18s$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.9) {
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


# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_16s_all$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_16s_all$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_16s_all$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.9) {
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


# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_18s_all$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_18s_all$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_18s_all$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 0.9) {
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


########## lstm 

# 1. Get full date range
all_dates <- c(
  weekly_summary_copernicus$week_start,
  weekly_summary_16s$week_start,
  weekly_summary_18s$week_start,
  weekly_summary_16s_all$week_start,
  weekly_summary_18s_all$week_start
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
weekly_summary_copernicus <- pad_weekly_data(weekly_summary_copernicus)
weekly_summary_16s        <- pad_weekly_data(weekly_summary_16s)
weekly_summary_18s        <- pad_weekly_data(weekly_summary_18s)
weekly_summary_16s_all    <- pad_weekly_data(weekly_summary_16s_all)
weekly_summary_18s_all    <- pad_weekly_data(weekly_summary_18s_all)

# 5. Plotting function (FONT UPDATED)
make_weekly_plot <- function(df, model_title, category_colors, show_x_axis = FALSE) {
  df <- df %>%
    group_by(week_start) %>%
    mutate(Percent = Count / sum(Count) * 100) %>%
    ungroup()
  
  ggplot(df, aes(x = week_start, y = Percent, fill = Category)) +
    geom_bar(stat = "identity", position = "stack", width = 6) +
    scale_fill_manual(values = category_colors) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month",
                 limits = c(min_date, max_date)) +
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

# Function to create confusion matrix tile plot (FONT UPDATED)
make_conf_matrix_plot <- function(results_df, category_colors) {
  conf_df <- as.data.frame(table(Predicted = results_df$Predicted_Class, Actual = results_df$Actual)) %>%
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

# Define color palette (updated)
category_colors <- c(
  "True Positive"  = "#0072B2",
  "False Negative" = "#D55E00",
  "False Positive" = "#E69F00",
  "True Negative"  = "#56B4E9"
)

# ---- Top color key (FONT UPDATED) ----
color_key_df <- data.frame(
  Category = factor(names(category_colors), levels = names(category_colors)),
  x = seq_along(category_colors)
)

color_key_plot <- ggplot(color_key_df, aes(y = 1)) +
  geom_text(aes(x = x, label = Category),
            hjust = 0.2,
            vjust = 0.5,
            fontface = "bold",
            size = 3) +
  geom_point(aes(x = x + 0.68, color = Category),
             size = 3) +
  scale_color_manual(values = category_colors) +
  scale_x_continuous(
    limits = c(0.5, max(color_key_df$x) + 1.2),
    expand = expansion(mult = 0)
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = unit(c(0, 0, 5, 0), "pt")
  )

bar_grobs <- list(
  make_weekly_plot(weekly_summary_copernicus, "Copernicus satellite dataset: Baltic Sea. Time-step 1-10", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_16s, "16S RA dataset: Baltic Sea. Time-step 1-2", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_18s, "18S RA dataset: Baltic Sea. Time-step 1-4", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_16s_all, "16S RA dataset: Baltic Sea and Warnow Estuary. Time-step 1-4", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_18s_all, "18S RA dataset: Baltic Sea and Warnow Estuary. Time-step 1-4", category_colors, show_x_axis = TRUE)
)

conf_grobs <- list(
  make_conf_matrix_plot(results_manual_copernicus, category_colors),
  make_conf_matrix_plot(results_manual_16s, category_colors),
  make_conf_matrix_plot(results_manual_18s, category_colors),
  make_conf_matrix_plot(results_manual_16s_all, category_colors),
  make_conf_matrix_plot(results_manual_18s_all, category_colors)
)

bar_rows <- list(
  bar_grobs[[1]],
  bar_grobs[[2]],
  bar_grobs[[3]],
  bar_grobs[[4]],
  bar_grobs[[5]]
)

bar_column <- arrangeGrob(
  grobs = bar_rows,
  ncol = 1,
  heights = unit(c(1, 1, 1, 1, 1.6), "null")
)

conf_column <- arrangeGrob(
  grobs = conf_grobs,
  ncol = 1,
  heights = unit(c(2, 2, 2, 2, 2), "null")
)

grid.arrange(
  textGrob("Percentage correctly identified weekly [%]", rot = 90,
           gp = gpar(fontsize = 10, fontface = "bold")),
  bar_column,
  conf_column,
  ncol = 4,
  widths = unit.c(unit(1, "lines"), unit(3, "null"), unit(0.9, "null"), unit(0.2, "null"))
)

recall_90_plot <- arrangeGrob(
  textGrob("Percentage correctly identified weekly [%]", rot = 90,
           gp = gpar(fontsize = 10, fontface = "bold")),
  bar_column,
  conf_column,
  ncol = 4,
  widths = unit.c(unit(1, "lines"), unit(3, "null"), unit(0.9, "null"), unit(0.1, "null"))
)

lstm_90 <- arrangeGrob(
  color_key_plot,
  recall_90_plot,
  ncol    = 1,
  heights = c(0.6, 8)
)

ggsave(
  "images/lstm_90.tiff",
  lstm_90,
  width  = 7,
  height = 5.5,
  dpi    = 1000,
  compression = "lzw"
)


#### 100

# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_copernicus$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_copernicus$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_copernicus$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 1) {
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

# Step: Weekly summary
weekly_summary_copernicus <- results_manual_copernicus %>%
  group_by(week_start, Category) %>%
  dplyr::summarise(Count = n(), .groups = "drop")



# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_16s$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001


for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_16s$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_16s$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 1) {
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




# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_18s$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_18s$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_18s$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 1) {
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


# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_16s_all$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_16s_all$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_16s_all$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 1) {
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


# Threshold selection: ensure ≥70% recall
actual_positives <- sum(results_manual_18s_all$Actual == 1)
thresholds <- seq(1, 0.0001, -0.0001)
best_threshold <- 0.0001

for (t in thresholds) {
  predicted_classes_temp <- ifelse(results_manual_18s_all$Predicted_Probability > t, 1, 0)
  tp <- sum(predicted_classes_temp == 1 & results_manual_18s_all$Actual == 1)
  recall <- tp / actual_positives
  if (recall >= 1) {
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


########## lstm 

# 1. Get full date range
all_dates <- c(
  weekly_summary_copernicus$week_start,
  weekly_summary_16s$week_start,
  weekly_summary_18s$week_start,
  weekly_summary_16s_all$week_start,
  weekly_summary_18s_all$week_start
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
weekly_summary_copernicus <- pad_weekly_data(weekly_summary_copernicus)
weekly_summary_16s        <- pad_weekly_data(weekly_summary_16s)
weekly_summary_18s        <- pad_weekly_data(weekly_summary_18s)
weekly_summary_16s_all    <- pad_weekly_data(weekly_summary_16s_all)
weekly_summary_18s_all    <- pad_weekly_data(weekly_summary_18s_all)

# 5. Plotting function (FONT UPDATED)
make_weekly_plot <- function(df, model_title, category_colors, show_x_axis = FALSE) {
  df <- df %>%
    group_by(week_start) %>%
    mutate(Percent = Count / sum(Count) * 100) %>%
    ungroup()
  
  ggplot(df, aes(x = week_start, y = Percent, fill = Category)) +
    geom_bar(stat = "identity", position = "stack", width = 6) +
    scale_fill_manual(values = category_colors) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month",
                 limits = c(min_date, max_date)) +
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

# Function to create confusion matrix tile plot (FONT UPDATED)
make_conf_matrix_plot <- function(results_df, category_colors) {
  conf_df <- as.data.frame(table(Predicted = results_df$Predicted_Class, Actual = results_df$Actual)) %>%
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

# Define color palette (updated)
category_colors <- c(
  "True Positive"  = "#0072B2",
  "False Negative" = "#D55E00",
  "False Positive" = "#E69F00",
  "True Negative"  = "#56B4E9"
)

# ---- Top color key (FONT UPDATED) ----
color_key_df <- data.frame(
  Category = factor(names(category_colors), levels = names(category_colors)),
  x = seq_along(category_colors)
)

color_key_plot <- ggplot(color_key_df, aes(y = 1)) +
  geom_text(aes(x = x, label = Category),
            hjust = 0.2,
            vjust = 0.5,
            fontface = "bold",
            size = 3) +
  geom_point(aes(x = x + 0.68, color = Category),
             size = 3) +
  scale_color_manual(values = category_colors) +
  scale_x_continuous(
    limits = c(0.5, max(color_key_df$x) + 1.2),
    expand = expansion(mult = 0)
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = unit(c(0, 0, 5, 0), "pt")
  )

bar_grobs <- list(
  make_weekly_plot(weekly_summary_copernicus, "Copernicus satellite dataset: Baltic Sea. Time-step 1-10", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_16s, "16S RA dataset: Baltic Sea. Time-step 1-2", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_18s, "18S RA dataset: Baltic Sea. Time-step 1-4", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_16s_all, "16S RA dataset: Baltic Sea and Warnow Estuary. Time-step 1-4", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_18s_all, "18S RA dataset: Baltic Sea and Warnow Estuary. Time-step 1-4", category_colors, show_x_axis = TRUE)
)

conf_grobs <- list(
  make_conf_matrix_plot(results_manual_copernicus, category_colors),
  make_conf_matrix_plot(results_manual_16s, category_colors),
  make_conf_matrix_plot(results_manual_18s, category_colors),
  make_conf_matrix_plot(results_manual_16s_all, category_colors),
  make_conf_matrix_plot(results_manual_18s_all, category_colors)
)

bar_rows <- list(
  bar_grobs[[1]],
  bar_grobs[[2]],
  bar_grobs[[3]],
  bar_grobs[[4]],
  bar_grobs[[5]]
)

bar_column <- arrangeGrob(
  grobs = bar_rows,
  ncol = 1,
  heights = unit(c(1, 1, 1, 1, 1.6), "null")
)

conf_column <- arrangeGrob(
  grobs = conf_grobs,
  ncol = 1,
  heights = unit(c(2, 2, 2, 2, 2), "null")
)

grid.arrange(
  textGrob("Percentage correctly identified weekly [%]", rot = 90,
           gp = gpar(fontsize = 10, fontface = "bold")),
  bar_column,
  conf_column,
  ncol = 4,
  widths = unit.c(unit(1, "lines"), unit(3, "null"), unit(0.9, "null"), unit(0.2, "null"))
)

recall_100_plot <- arrangeGrob(
  textGrob("Percentage correctly identified weekly [%]", rot = 90,
           gp = gpar(fontsize = 10, fontface = "bold")),
  bar_column,
  conf_column,
  ncol = 4,
  widths = unit.c(unit(1, "lines"), unit(3, "null"), unit(0.9, "null"), unit(0.1, "null"))
)

lstm_100 <- arrangeGrob(
  color_key_plot,
  recall_100_plot,
  ncol    = 1,
  heights = c(0.6, 8)
)

ggsave(
  "images/lstm_100.tiff",
  lstm_100,
  width  = 7,
  height = 5.5,
  dpi    = 1000,
  compression = "lzw"
)


