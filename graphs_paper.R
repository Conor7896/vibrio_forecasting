
######### quantity graph

setwd("C:/Users/glack/Documents/vibrio_second_paper/")

# Read the data
join_all_current_discharge_hplc_temp_weather <- 
  read_csv("data_all/join_all_current_discharge_hplc_temp_weather_full_year.csv")

# Define the Baltic Sea locations
baltic_sea_locations <- c("Heiligendamm", "Jemnitzschleuse", "Borgerende", 
                          "Nienhagen", "Elmenhorst", "Diedrichshagen", "Warnemuende")

# Create new column 'Sampling_Region' in base R
join_all_current_discharge_hplc_temp_weather$Sampling_Region <- ifelse(
  join_all_current_discharge_hplc_temp_weather$location %in% baltic_sea_locations,
  "Baltic Sea",
  "Warnow Estuary"
)


join_all_current_discharge_hplc_temp_weather <- join_all_current_discharge_hplc_temp_weather %>%
  mutate(
    v_vulnificus = if_else(
      location %in% c("Borgerende", "Heiligendamm", "Warnemuende", "Nienhagen") &
        date >= as.Date("2022-07-03") & date <= as.Date("2022-09-16"),
      v_vulnificus,  # keep value
      NA_real_       # set to NA (real because v_vulnificus is numeric)
    )
  )

# Convert to long format
df_long <- join_all_current_discharge_hplc_temp_weather %>%
  pivot_longer(
    cols = c(v_vulnificus, ddpcr, v_vul_total_abundance),
    names_to = "Measurement",
    values_to = "Value"
  )

# Step 1: Recode Measurement to ensure correct facet order (V. vulnificus colonies on top)
df_long$Measurement <- factor(df_long$Measurement,
                              levels = c("v_vulnificus", "ddpcr", "v_vul_total_abundance"),
                              labels = c(
                                "Presumptive cultured<br><i>V. vulnificus</i> colonies",
                                "ddPCR <i>V. vulnificus</i><br>(vvha copies ml<sup>&minus;1</sup>)",
                                "Relative abundance<br>based on 16S<br>rRNA genes"
                              )
)

#region_colors <- c(
#  "Baltic Sea" = "#1f78b4",  # Deep blue (keep as is)
#  "Warnow Estuary" = "#B3A125"  # Forest green
#)

region_colors <- c(
  "Baltic Sea" = "#1f78b4",  # Deep blue (keep as is)
  "Warnow Estuary" = "#A0522D"  # Forest green
)

# Step 3: Create the plot without title and legend
abundance_plot <- ggplot(df_long, aes(x = date, y = Value, color = Sampling_Region)) +
  geom_point(alpha = 0.7, size = 1.5) +
  facet_wrap(
    ~Measurement,
    scales = "free_y",
    ncol = 1,
    strip.position = "left"
  ) +
  scale_color_manual(values = region_colors) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(
    x = "Date",
    y = NULL,
    color = "Sampling Region:"  # Add legend title here
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text.y.left = element_markdown(size = 10, angle = 90),
    strip.placement = "outside",
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    legend.key = element_rect(fill = NA)
  )



# Step 4: Display the plot
print(abundance_plot)

######### this is for temperature and salinity

# Define Baltic Sea region if not yet done
baltic_sea_locations <- c("Heiligendamm", "Jemnitzschleuse", "Borgerende", 
                          "Nienhagen", "Elmenhorst", "Diedrichshagen", "Warnemuende")

df <- join_all_current_discharge_hplc_temp_weather
df$Sampling_Region <- ifelse(df$location %in% baltic_sea_locations, "Baltic Sea", "Warnow Estuary")

df <- df %>%
  mutate(
    v_vulnificus = if_else(
      location %in% c("Borgerende", "Heiligendamm", "Warnemuende", "Nienhagen") &
        date >= as.Date("2022-07-03") & date <= as.Date("2022-09-16"),
      v_vulnificus,  # keep value
      NA_real_       # set to NA (real because v_vulnificus is numeric)
    )
  )


# Define labels
measurement_labels <- c(
  "v_vulnificus" = "Presumptive cultured<br><i>V. vulnificus</i> colonies",
  "ddpcr" = "ddPCR <i>V. vulnificus</i><br>(vvha copies ml<sup>&minus;1</sup>)",
  "v_vul_total_abundance" = "Relative abundance<br>based on 16S<br>rRNA genes"
)

# Prepare salinity data
df_sal <- df %>%
  pivot_longer(cols = c(v_vulnificus, ddpcr, v_vul_total_abundance),
               names_to = "Measurement", values_to = "Value") %>%
  filter(!is.na(Value), !is.na(salinity)) %>%
  mutate(
    Variable = "Salinity (PSU)",
    X_value = salinity
  )

# Prepare temperature data
df_temp <- df %>%
  pivot_longer(cols = c(v_vulnificus, ddpcr, v_vul_total_abundance),
               names_to = "Measurement", values_to = "Value") %>%
  filter(!is.na(Value), !is.na(temperature.x)) %>%
  mutate(
    Variable = "Temperature (°C)",
    X_value = temperature.x
  )

# Combine and clean
df_env <- bind_rows(df_sal, df_temp) %>%
  filter(!is.na(Value)) %>%  # ✅ Ensure NAs are removed from all values
  mutate(
    Measurement = factor(Measurement, levels = names(measurement_labels), labels = measurement_labels),
    Variable = factor(Variable, levels = c("Salinity (PSU)", "Temperature (°C)"))
  )

# Define region colors
region_colors <- c("Baltic Sea" = "#1f78b4", "Warnow Estuary" = "#A0522D")
#region_colors <- c("Baltic Sea" = "black", "Warnow Estuary" = "black")

ggplot(df_env, aes(x = X_value, y = Value, color = Sampling_Region)) +
  geom_point(alpha = 0.7, size = 1.45) +
  facet_grid(Measurement ~ Variable, switch = "y", scales = "free_y") +
  scale_color_manual(values = region_colors) +
  labs(x = NULL, y = NULL, color = "Sampling Region:") +  # Add legend title
  theme_minimal(base_size = 13) +
  theme(
    strip.text.x.bottom = element_text(size = 11, face = "bold"),
    strip.text.y.left = ggtext::element_markdown(size = 11),
    strip.placement = "outside",
    legend.position = "top",  # Legend at top center
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    legend.key = element_rect(fill = NA),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.spacing = unit(1, "lines")
  )

# Plot
ggplot() +
  # 🔥 Temperature points: light red → dark red
  geom_point(data = df_env %>% filter(Variable == "Temperature (°C)"),
             aes(x = X_value, y = Value, color = X_value),
             size = 1.5, alpha = 0.8) +
  scale_color_gradient(
    low = "#fcae91", high = "#cb181d",  # light red to dark red
    name = "Temperature (°C)"
  ) +
  ggnewscale::new_scale_color() +  # Allow second color scale
  
  # 🌊 Salinity points: light blue → dark blue
  geom_point(data = df_env %>% filter(Variable == "Salinity (PSU)"),
             aes(x = X_value, y = Value, color = X_value),
             size = 1.5, alpha = 0.8) +
  scale_color_gradient(
    low = "#deebf7", high = "#2171b5",  # light blue to dark blue
    name = "Salinity (PSU)"
  ) +
  
  # Layout
  facet_grid(Measurement ~ Variable, switch = "y", scales = "free_y") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text.x.bottom = element_text(size = 11, face = "bold"),
    strip.text.y.left = ggtext::element_markdown(size = 10),
    strip.placement = "outside",
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    legend.key = element_rect(fill = NA),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.spacing = unit(1, "lines")
  )

################### this is for rf aupr 


# AUPRC values for location subset (5 models)
pr_value_subset <- c(pr_value_all, pr_value_otc, pr_value_16s, pr_value_18s, pr_value_cop)
labels_subset <- c(
  "Combined\ndataset",           # instead of "All data"
  "Biological and\nPhysical parameters",   # instead of "Physical data"
  "16S rRNA\nrelative abundance",
  "18S rRNA\nrelative abundance",
  "Copernicus\nsatellite data"
)
# AUPRC values for all locations (4 models only — no Copernicus)
pr_value_all_loc <- c(pr_value_all_all_loc, pr_value_otc_all_loc, pr_value_16s_all_loc, pr_value_18s_all_loc)
labels_all_loc <- c(
  "Combined\ndataset",
  "Biological and\nPhysical parameters",
  "16S rRNA\nrelative abundance",
  "18S rRNA\nrelative abundance"
)
df_subset <- data.frame(
  Model = factor(labels_subset, levels = labels_subset),
  AUPRC = pr_value_subset,
  Dataset = "Location subset"
)

df_all_loc <- data.frame(
  Model = factor(labels_all_loc, levels = labels_subset),  # keep same overall levels
  AUPRC = pr_value_all_loc,
  Dataset = "All locations"
)

# Combine
pr_value_data <- bind_rows(df_subset, df_all_loc)


# Rename datasets for publication
pr_value_data$Dataset <- recode(
  pr_value_data$Dataset,
  "Location subset" = "Baltic Sea",
  "All locations" = "Baltic Sea and Warnow Estuary"
)

# Final polished plot
ggplot(pr_value_data, aes(x = Model, y = AUPRC)) +
  geom_point(size = 3, color = "black") +  # Black points for journal-ready print
  facet_wrap(~Dataset, ncol = 1, scales = "free_y") +
  xlab("Data used for model training") +
  ylab("Area Under the Precision-Recall Curve (AUPRC)") +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 13, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold")
  )


# Rename datasets
pr_value_data$Dataset <- recode(
  pr_value_data$Dataset,
  "Location subset" = "Baltic Sea",
  "All locations" = "Baltic Sea and Warnow Estuary"
)

# Plot
# Define custom mixed color
dataset_colors <- c(
  "Baltic Sea" = "#1f78b4",                     # dark blue
  "Baltic Sea and Warnow Estuary" = "#5ca76a"  # bluish olive mix
)

# Plot
ggplot(pr_value_data, aes(x = Model, y = AUPRC, color = Dataset)) +
  geom_point(size = 4) +
  scale_color_manual(values = dataset_colors) +
  ylim(0.3, 1) +
  xlab("Data used for model training") +
  ylab("Area Under the Precision-Recall Curve (AUPRC)") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    legend.key = element_rect(fill = NA),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 13),
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold")
  ) +
  guides(color = guide_legend(title = "Sampling Region:"))


############# this is for the lstm and rf time lag aupr results

# Define LSTM results
lstm_values <- data.frame(
  Dataset = c("16S", "18S", "Copernicus"),
  AUPR = c(pr_value_16s_lstm, pr_value_18s_lstm, pr_value_coper_lstm),
  Type = "LSTM",
  TimeLag = NA
)

# Define Random Forest results (example for time lags 0–9)
rf_values <- data.frame(
  Dataset = rep(c("16S", "18S", "Copernicus"), each = 10),
  AUPR = c(
    pr_value_16s_0, pr_value_16s_1, pr_value_16s_2, pr_value_16s_3, pr_value_16s_4,
    pr_value_16s_5, pr_value_16s_6, pr_value_16s_7, pr_value_16s_8, pr_value_16s_9,
    
    pr_value_18s_0, pr_value_18s_1, pr_value_18s_2, pr_value_18s_3, pr_value_18s_4,
    pr_value_18s_5, pr_value_18s_6, pr_value_18s_7, pr_value_18s_8, pr_value_18s_9,
    
    pr_value_cop_0, pr_value_cop_1, pr_value_cop_2, pr_value_cop_3, pr_value_cop_4,
    pr_value_cop_5, pr_value_cop_6, pr_value_cop_7, pr_value_cop_8, pr_value_cop_9
  ),
  Type = "Random Forest",
  TimeLag = rep(0:9, times = 3)
)

# Add x-axis labels
lstm_values$X_Label <- "LSTM"

rf_values$X_Label <- paste0("Lag ", rf_values$TimeLag)

# Combine
plot_df <- rbind(lstm_values, rf_values)
ggplot(plot_df, aes(x = X_Label, y = AUPR)) +
  geom_line(
    data = subset(plot_df, Type == "Random Forest"),
    aes(group = Dataset),
    color = "gray50"
  ) +
  geom_point(aes(color = Type, shape = Type), size = 3) +
  scale_shape_manual(values = c("LSTM" = 16, "Random Forest" = 17)) +
  scale_color_manual(values = c("LSTM" = "blue", "Random Forest" = "black")) +
  facet_wrap(~Dataset, nrow = 3, strip.position = "left") +
  labs(title = "AUPR Comparison: LSTM vs Random Forest with Time Lags",
       y = "AUPR", x = "Model / Time Lag") +
  theme_minimal() +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 12, face = "bold"),
        legend.position = "top")




############### lstm vs random forest time lag

# Baltic Sea only — LSTM
lstm_values <- data.frame(
  Dataset = c("16S", "18S", "Copernicus"),
  AUPR = c(pr_value_16s_lstm, pr_value_18s_lstm, pr_value_coper_lstm),
  X_Label = "LSTM",
  Model_Type = "LSTM",
  Region = "Baltic Sea"
)

# Baltic Sea only — RF
rf_values <- data.frame(
  Dataset = rep(c("16S", "18S", "Copernicus"), each = 10),
  AUPR = c(
    pr_value_16s_1, pr_value_16s_2, pr_value_16s_3, pr_value_16s_4,
    pr_value_16s_5, pr_value_16s_6, pr_value_16s_7, pr_value_16s_8, pr_value_16s_9, pr_value_16s_10,
    pr_value_18s_1, pr_value_18s_2, pr_value_18s_3, pr_value_18s_4,
    pr_value_18s_5, pr_value_18s_6, pr_value_18s_7, pr_value_18s_8, pr_value_18s_9, pr_value_18s_10,
    pr_value_cop_1, pr_value_cop_2, pr_value_cop_3, pr_value_cop_4,
    pr_value_cop_5, pr_value_cop_6, pr_value_cop_7, pr_value_cop_8, pr_value_cop_9, pr_value_cop_10
  ),
  X_Label = rep(paste0("Time-lag ", 1:10), times = 3),
  Model_Type = "Random Forest",
  Region = "Baltic Sea"
)

# All regions — LSTM
lstm_all <- data.frame(
  Dataset = c("16S", "18S"),
  AUPR = c(pr_value_16s_all_loc_lstm, pr_value_18s_all_loc_lstm),
  X_Label = "LSTM",
  Model_Type = "LSTM",
  Region = "Baltic Sea and Warnow Estuary"
)

# All regions — RF
rf_all <- data.frame(
  Dataset = rep(c("16S", "18S"), each = 10),
  AUPR = c(
    pr_value_16s_all_1, pr_value_16s_all_2, pr_value_16s_all_3, pr_value_16s_all_4,
    pr_value_16s_all_5, pr_value_16s_all_6, pr_value_16s_all_7, pr_value_16s_all_8, pr_value_16s_all_9, pr_value_16s_all_10,
    pr_value_18s_all_1, pr_value_18s_all_2, pr_value_18s_all_3, pr_value_18s_all_4,
    pr_value_18s_all_5, pr_value_18s_all_6, pr_value_18s_all_7, pr_value_18s_all_8, pr_value_18s_all_9, pr_value_18s_all_10
  ),
  X_Label = rep(paste0("Time-lag ", 1:10), times = 2),
  Model_Type = "Random Forest",
  Region = "Baltic Sea and Warnow Estuary"
)

# Combine all
plot_df <- bind_rows(lstm_values, rf_values, lstm_all, rf_all)

# Ensure proper order on x-axis
plot_df$X_Label <- factor(plot_df$X_Label, levels = c("LSTM", paste0("Time-lag ", 1:10)))
plot_df$Model_Type <- factor(plot_df$Model_Type, levels = c("LSTM", "Random Forest"))
plot_df$Dataset <- factor(plot_df$Dataset, levels = c("16S", "18S", "Copernicus"))





ggplot(plot_df, aes(x = X_Label, y = AUPR, color = Dataset, group = interaction(Dataset, Region))) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_line(
    data = subset(plot_df, Model_Type == "Random Forest"),
    aes(group = interaction(Dataset, Region)),
    position = position_dodge(width = 0.5),
    linetype = "dashed",
    linewidth = 1
  ) +
  scale_color_manual(values = c("16S" = "#1b9e77", "18S" = "#d95f02", "Copernicus" = "#7570b3")) +
  
  facet_wrap(~Region, nrow = 1, strip.position = "bottom") +
  
  # Vertical divider
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "black", linewidth = 0.6) +
  
  # Add section headers ABOVE the plot
  annotate("text", x = 0.70, y = 1.05, label = "LSTM", vjust = 0, size = 5, fontface = "bold") +
  annotate("text", x = 6, y = 1.05, label = "Random Forest", vjust = 0, size = 5, fontface = "bold") +
  
  # Axis labels and limits
  labs(
    x = NULL,
    y = "Area Under Precision-Recall Curve (AUPR)",
    color = "Dataset"
  ) +
  ylim(0, 1.1) +  # Extend upper y-limit to fit labels
  coord_cartesian(clip = "off") +  # Allow annotation to go outside plot area
  theme_minimal(base_size = 14) +
  theme(
    strip.placement = "outside",
    strip.text = element_text(face = "bold", size = 13),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 11),
    panel.spacing = unit(2, "lines"),
    plot.margin = ggplot2::margin(t = 50, r = 10, b = 20, l = 10),  # corrected line
    plot.background = element_rect(fill = "white", color = NA)
  )


############ now i want to create 5 graphs based on confusion matrices


# Ensure proper date parsing
results_manual_coper$date <- as.Date(results_manual_coper$Date)
results_manual_coper$week <- lubridate::week(results_manual_coper$date)

# Add category labels
results_manual <- results_manual_coper %>%
  mutate(
    Category = case_when(
      Actual == 1 & Predicted_Class == 1 ~ "True Positive",
      Actual == 0 & Predicted_Class == 0 ~ "True Negative",
      Actual == 0 & Predicted_Class == 1 ~ "False Positive",
      Actual == 1 & Predicted_Class == 0 ~ "False Negative"
    )
  )

# Step: Create proper weekly order using start of ISO week
results_manual$week_start <- lubridate::floor_date(results_manual$date, unit = "week", week_start = 1)

# Step 4: Color scheme for journal-friendly visualization
category_colors <- c(
  "True Positive"  = "#08306B",  # deep blue
  "False Negative" = "#9ECAE1",  # soft light blue
  "False Positive" = "#FCAE91",  # soft salmon pink
  "True Negative"  = "#ea4434"   # muted dark red
)

# Step 1: Calculate percentage per category per week
weekly_summary <- results_manual %>%
  group_by(week_start, Category) %>%
 dplyr::summarise(Count = n(), .groups = "drop") %>%
  group_by(week_start) %>%
  mutate(Percentage = 100 * Count / sum(Count)) %>%
  ungroup()

# Step 2: Update the plot to use percentage
performance_plot <- ggplot(weekly_summary, aes(x = week_start, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", position = "stack", width = 5) +
  scale_fill_manual(values = category_colors) +
  scale_x_date(
    date_breaks = "1 week",
    labels = scales::label_date("%b %d, %Y")
  ) +
  labs(
    x = "Week Start Date",
    y = "Percentage of Predictions",
    fill = "Prediction Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 13),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )


print(performance_plot)
# Step 6: Confusion matrix and prepare for plotting
conf_matrix_lstm <- table(Predicted = results_manual$Predicted_Class, Actual = results_manual$Actual)
conf_matrix_tbl <- as.data.frame(conf_matrix_lstm)

# Add readable label
conf_matrix_tbl$Label <- case_when(
  conf_matrix_tbl$Actual == 1 & conf_matrix_tbl$Predicted == 1 ~ "True Positive",
  conf_matrix_tbl$Actual == 0 & conf_matrix_tbl$Predicted == 0 ~ "True Negative",
  conf_matrix_tbl$Actual == 0 & conf_matrix_tbl$Predicted == 1 ~ "False Positive",
  conf_matrix_tbl$Actual == 1 & conf_matrix_tbl$Predicted == 0 ~ "False Negative"
)

# Ensure numeric values for axes
conf_matrix_tbl$Actual <- as.numeric(as.character(conf_matrix_tbl$Actual))
conf_matrix_tbl$Predicted <- as.numeric(as.character(conf_matrix_tbl$Predicted))

# Step 7: Create confusion matrix plot
conf_matrix_plot <- ggplot(conf_matrix_tbl, aes(x = Actual, y = Predicted, fill = Label)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "white", size = 6, fontface = "bold") +
  scale_fill_manual(values = category_colors) +
  scale_x_continuous(breaks = c(0, 1), labels = c("0", "1"), expand = c(0, 0)) +
  scale_y_continuous(breaks = c(0, 1), labels = c("0", "1"), expand = c(0, 0)) +
  coord_fixed() +
  labs(
    x = "Actual",
    y = "Predicted",
    fill = "Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(face = "bold", size = 13)
  )

# Step 8: Combine bar chart and confusion matrix
gridExtra::grid.arrange(
  performance_plot,
  conf_matrix_plot,
  ncol = 2,
  widths = c(2.5, 1)  # wider bar plot
)


######### this is another attempt at my graph

# Function to process a weekly summary into a percentage plot
make_weekly_plot <- function(df, model_title, category_colors, show_x_axis = FALSE) {
  df <- df %>%
    group_by(week_start) %>%
    mutate(Percent = Count / sum(Count) * 100) %>%
    ungroup()
  
  p <- ggplot(df, aes(x = week_start, y = Percent, fill = Category)) +
    geom_bar(stat = "identity", position = "stack", width = 6) +
    scale_fill_manual(values = category_colors) +
    scale_x_date(date_labels = "%b %d, %Y", date_breaks = "2 weeks") +
    labs(
      x = NULL, y = "Percentage of Predictions", title = model_title
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
      axis.text.x = if (show_x_axis) element_text(angle = 45, hjust = 1) else element_blank(),
      axis.ticks.x = if (show_x_axis) element_line() else element_blank()
    )
  return(p)
}

# Function to generate a ggplot-based confusion matrix from raw results
make_conf_matrix_plot <- function(results_df, category_colors) {
  conf_tbl <- table(Predicted = results_df$Predicted_Class, Actual = results_df$Actual)
  conf_df <- as.data.frame(conf_tbl)
  
  conf_df <- conf_df %>%
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
    geom_text(aes(label = Freq), color = "white", size = 5, fontface = "bold") +
    scale_fill_manual(values = category_colors) +
    labs(x = "Actual", y = "Predicted") +
    #coord_fixed() +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 12, face = "bold")
    )
}

# Define color scheme
category_colors <- c(
  "True Positive"  = "#08306B",  # deep blue
  "False Negative" = "#9ECAE1",  # soft light blue
  "False Positive" = "#FCAE91",  # soft salmon pink
  "True Negative"  = "#ea4434"   # muted dark red
)

# Generate each model's combined panel
gridExtra::grid.arrange(
  arrangeGrob(
    make_weekly_plot(weekly_summary_coper, "LSTM", category_colors, show_x_axis = FALSE),
    make_conf_matrix_plot(results_manual_coper, category_colors),
    ncol = 2,
    widths = c(2.5, 1)
  ),
  arrangeGrob(
    make_weekly_plot(weekly_summary_16s, "16S", category_colors, show_x_axis = FALSE),
    make_conf_matrix_plot(results_manual_16s, category_colors),
    ncol = 2,
    widths = c(2.5, 1)
  ),
  arrangeGrob(
    make_weekly_plot(weekly_summary_18s, "18S", category_colors, show_x_axis = FALSE),
    make_conf_matrix_plot(results_manual_18s, category_colors),
    ncol = 2,
    widths = c(2.5, 1)
  ),
  arrangeGrob(
    make_weekly_plot(weekly_summary_16s_all, "16S All Locations", category_colors, show_x_axis = FALSE),
    make_conf_matrix_plot(results_manual_16s_all, category_colors),
    ncol = 2,
    widths = c(2.5, 1)
  ),
  arrangeGrob(
    make_weekly_plot(weekly_summary_18s_all, "18S All Locations", category_colors, show_x_axis = TRUE),  # ✅ Only bottom shows x-axis
    make_conf_matrix_plot(results_manual_18s_all, category_colors),
    ncol = 2,
    widths = c(2.5, 1)
  ),
  nrow = 5
)


# Create each pair (bar plot + confusion matrix)
grobs <- list(
  arrangeGrob(
    make_weekly_plot(weekly_summary_coper, "LSTM", category_colors, show_x_axis = FALSE) +
      theme(axis.title.y = element_blank()),  # remove individual y-axis label
    make_conf_matrix_plot(results_manual_coper, category_colors),
    ncol = 2,
    widths = c(2.5, 1)
  ),
  arrangeGrob(
    make_weekly_plot(weekly_summary_16s, "16S", category_colors, show_x_axis = FALSE) +
      theme(axis.title.y = element_blank()),
    make_conf_matrix_plot(results_manual_16s, category_colors),
    ncol = 2,
    widths = c(2.5, 1)
  ),
  arrangeGrob(
    make_weekly_plot(weekly_summary_18s, "18S", category_colors, show_x_axis = FALSE) +
      theme(axis.title.y = element_blank()),
    make_conf_matrix_plot(results_manual_18s, category_colors),
    ncol = 2,
    widths = c(2.5, 1)
  ),
  arrangeGrob(
    make_weekly_plot(weekly_summary_16s_all, "16S All Locations", category_colors, show_x_axis = FALSE) +
      theme(axis.title.y = element_blank()),
    make_conf_matrix_plot(results_manual_16s_all, category_colors),
    ncol = 2,
    widths = c(2.5, 1)
  ),
  arrangeGrob(
    make_weekly_plot(weekly_summary_18s_all, "18S All Locations", category_colors, show_x_axis = TRUE) +
      theme(axis.title.y = element_blank()),
    make_conf_matrix_plot(results_manual_18s_all, category_colors),
    ncol = 2,
    widths = c(2.5, 1)
  )
)

# Arrange with a shared y-axis label
grid.arrange(
  textGrob("Percentage of Predictions", rot = 90, gp = gpar(fontsize = 13, fontface = "bold")),
  arrangeGrob(grobs = grobs, ncol = 1),
  widths = unit.c(unit(1, "lines"), unit(1, "null"))
)


# Ensure all combined grobs have the same height
grob_list <- grobs  # grobs already arranged

# Find the max heights (row heights)
max_height <- do.call(grid::unit.pmax, lapply(grob_list, function(g) g$heights))

# Apply max height to all grobs
for (i in seq_along(grob_list)) {
  grob_list[[i]]$heights <- max_height
}
# Final layout with shared y-axis label and custom row heights
grid.arrange(
  textGrob("Percentage of Predictions", rot = 90, gp = gpar(fontsize = 13, fontface = "bold")),
  arrangeGrob(
    grobs = grobs,
    ncol = 1,
    heights = unit.c(
      unit(1, "null"),  # LSTM
      unit(1, "null"),  # 16S
      unit(1, "null"),  # 18S
      unit(1, "null"),  # 16S All Locations
      unit(1.6, "null") # 18S All Locations — extra space for x labels
    )
  ),
  widths = unit.c(unit(1, "lines"), unit(1, "null"))
)

########### this is the real final graph 

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

# 5. Plotting function (unchanged)
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
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.text.x = if (show_x_axis) element_text(angle = 45, hjust = 1, size = 10) else element_blank(),
      axis.ticks.x = if (show_x_axis) element_line() else element_blank(),
      panel.grid.minor = element_blank()
    )
}

# Function to create confusion matrix tile plot
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
    geom_text(aes(label = Freq), color = "white", size = 5, fontface = "bold") +
    scale_fill_manual(values = category_colors) +
    labs(x = "Actual", y = "Predicted") +
    theme_minimal(base_size = 12) +
    theme(
      aspect.ratio = 1,
      legend.position = "none",
      panel.grid = element_blank(),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 12, face = "bold")
    )
}

# Define color palette
category_colors <- c(
  "True Positive"  = "#08306B",
  "False Negative" = "#9ECAE1",
  "False Positive" = "#FCAE91",
  "True Negative"  = "#ea4434"
)

# Create vertically stacked bar plots
bar_grobs <- list(
  make_weekly_plot(weekly_summary_copernicus, "Copernicus satellite data: Baltic Sea. Time-step 1-10", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_16s, "16S rRNA relative abundance: Baltic Sea. Time-step 1-2", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_18s, "18S rRNA relative abundance: Baltic Sea. Time-step 1-4", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_16s_all, "16S rRNA relative abundance: Baltic Sea and Warnow Estuary. Time-step 1-4", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_18s_all, "18S rRNA relative abundance: Baltic Sea and Warnow Estuary. Time-step 1-4", category_colors, show_x_axis = TRUE)
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

# Bar column with section headers and individual heights
# Bar column without region group labels
bar_column <- arrangeGrob(
  grobs = bar_rows,
  ncol = 1,
  heights = unit(c(1, 1, 1, 1, 1.6), "null")  # no region labels, just plot heights
)


# Confusion matrix column with consistent height
conf_column <- arrangeGrob(
  grobs = conf_grobs,
  ncol = 1,
  heights = unit(c(2, 2, 2, 2, 2), "null")
)


# Combine side-by-side with shared y-axis label
grid.arrange(
  textGrob("Percentage correctly identified weekly [%]", rot = 90, gp = gpar(fontsize = 13, fontface = "bold")),
  bar_column,
  conf_column,
  ncol = 4,
  widths = unit.c(unit(1, "lines"), unit(3, "null"), unit(0.9, "null"), unit(1.2, "null"))
)


############# rf time lag

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

# 5. Plotting function (unchanged)
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
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.text.x = if (show_x_axis) element_text(angle = 45, hjust = 1, size = 10) else element_blank(),
      axis.ticks.x = if (show_x_axis) element_line() else element_blank(),
      panel.grid.minor = element_blank()
    )
}

# Function to create confusion matrix tile plot
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
    geom_text(aes(label = Freq), color = "white", size = 5, fontface = "bold") +
    scale_fill_manual(values = category_colors) +
    labs(x = "Actual", y = "Predicted") +
    theme_minimal(base_size = 12) +
    theme(
      aspect.ratio = 1,
      legend.position = "none",
      panel.grid = element_blank(),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 12, face = "bold")
    )
}

# Define color palette
category_colors <- c(
  "True Positive"  = "#08306B",
  "False Negative" = "#9ECAE1",
  "False Positive" = "#FCAE91",
  "True Negative"  = "#ea4434"
)

# Create vertically stacked bar plots
bar_grobs <- list(
  make_weekly_plot(weekly_summary_rf_copernicus, "Copernicus satellite data: Baltic Sea", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_rf_16s, "16S rRNA relative abundance: Baltic Sea", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_rf_18s, "18S rRNA relative abundance: Baltic Sea", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_rf_16s_all, "16S rRNA relative abundance: Baltic Sea and Warnow Estuary", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_rf_18s_all, "18S rRNA relative abundance: Baltic Sea and Warnow Estuary", category_colors, show_x_axis = TRUE)
)

# Create vertically stacked confusion matrices
conf_grobs <- list(
  make_conf_matrix_plot(results_manual_rf_copernicus, category_colors),
  make_conf_matrix_plot(results_manual_rf_16s, category_colors),
  make_conf_matrix_plot(results_manual_rf_18s, category_colors),
  make_conf_matrix_plot(results_manual_rf_16s_all, category_colors),
  make_conf_matrix_plot(results_manual_rf_18s_all, category_colors)
)


# Bar plot rows with preserved custom heights
bar_rows <- list(
  bar_grobs[[1]],  # LSTM
  bar_grobs[[2]],  # 16S
  bar_grobs[[3]],  # 18S
  bar_grobs[[4]],  # 16S All Locations
  bar_grobs[[5]]   # 18S All Locations
)

# Bar column with section headers and individual heights
# Bar column without region group labels
bar_column <- arrangeGrob(
  grobs = bar_rows,
  ncol = 1,
  heights = unit(c(1, 1, 1, 1, 1.6), "null")  # no region labels, just plot heights
)


# Confusion matrix column with consistent height
conf_column <- arrangeGrob(
  grobs = conf_grobs,
  ncol = 1,
  heights = unit(c(2, 2, 2, 2, 2), "null")
)


# Combine side-by-side with shared y-axis label
grid.arrange(
  textGrob("Percentage correctly identified weekly [%]", rot = 90, gp = gpar(fontsize = 13, fontface = "bold")),
  bar_column,
  conf_column,
  ncol = 4,
  widths = unit.c(unit(1, "lines"), unit(3, "null"), unit(0.9, "null"), unit(1.2, "null"))
)

############## this is potentially if i have to put the graphs together

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

# 5. Plotting function (unchanged)
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
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.text.x = if (show_x_axis) element_text(angle = 45, hjust = 1, size = 10) else element_blank(),
      axis.ticks.x = if (show_x_axis) element_line() else element_blank(),
      panel.grid.minor = element_blank()
    )
}

# Function to create confusion matrix tile plot
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
    geom_text(aes(label = Freq), color = "white", size = 5, fontface = "bold") +
    scale_fill_manual(values = category_colors) +
    labs(x = "Actual", y = "Predicted") +
    theme_minimal(base_size = 12) +
    theme(
      aspect.ratio = 1,
      legend.position = "none",
      panel.grid = element_blank(),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 12, face = "bold")
    )
}

# Define color palette
category_colors <- c(
  "True Positive"  = "#08306B",
  "False Negative" = "#9ECAE1",
  "False Positive" = "#FCAE91",
  "True Negative"  = "#ea4434"
)

# Create vertically stacked bar plots
bar_grobs <- list(
  make_weekly_plot(weekly_summary_copernicus, "Copernicus satellite data: Baltic Sea", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_16s, "16S rRNA relative abundance: Baltic Sea", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_18s, "18S rRNA relative abundance: Baltic Sea", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_16s_all, "16S rRNA relative abundance: Baltic Sea and Warnow Estuary", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_18s_all, "18S rRNA relative abundance: Baltic Sea and Warnow Estuary", category_colors, show_x_axis = TRUE)
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

# Bar column with section headers and individual heights
# Bar column without region group labels
bar_column <- arrangeGrob(
  grobs = bar_rows,
  ncol = 1,
  heights = unit(c(1, 1, 1, 1, 1.6), "null")  # no region labels, just plot heights
)


# Confusion matrix column with consistent height
conf_column <- arrangeGrob(
  grobs = conf_grobs,
  ncol = 1,
  heights = unit(c(1.8, 1.8, 1.8, 1.8, 1.8), "null")
)


# Create LSTM footer title grob
lstm_footer <- textGrob("LSTM", gp = gpar(fontsize = 14, fontface = "bold"), just = "center")

# Combine bar plots and LSTM label vertically
lstm_column <- arrangeGrob(
  bar_column,
  lstm_footer,
  ncol = 1,
  heights = unit.c(unit(1, "null"), unit(1.2, "lines"))
)

# Combine all with shared y-axis
grid.arrange(
  textGrob("Percentage correctly identified weekly [%]", rot = 90, gp = gpar(fontsize = 13, fontface = "bold")),
  lstm_column,
  conf_column,
  ncol = 4,
  widths = unit.c(unit(1, "lines"), unit(3, "null"), unit(0.9, "null"), unit(1.2, "null"))
)

############## for RF time lag 



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

# 5. Plotting function (unchanged)
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
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.text.x = if (show_x_axis) element_text(angle = 45, hjust = 1, size = 10) else element_blank(),
      axis.ticks.x = if (show_x_axis) element_line() else element_blank(),
      panel.grid.minor = element_blank()
    )
}

# Function to create confusion matrix tile plot
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
    geom_text(aes(label = Freq), color = "white", size = 5, fontface = "bold") +
    scale_fill_manual(values = category_colors) +
    labs(x = "Actual", y = "Predicted") +
    theme_minimal(base_size = 12) +
    theme(
      aspect.ratio = 1,
      legend.position = "none",
      panel.grid = element_blank(),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 12, face = "bold")
    )
}

# Define color palette
category_colors <- c(
  "True Positive"  = "#08306B",
  "False Negative" = "#9ECAE1",
  "False Positive" = "#FCAE91",
  "True Negative"  = "#ea4434"
)

# Create vertically stacked bar plots
bar_grobs <- list(
  make_weekly_plot(weekly_summary_rf_copernicus, "Copernicus satellite data: Baltic Sea", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_rf_16s, "16S rRNA relative abundance: Baltic Sea", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_rf_18s, "18S rRNA relative abundance: Baltic Sea", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_rf_16s_all, "16S rRNA relative abundance: Baltic Sea and Warnow Estuary", category_colors, show_x_axis = FALSE),
  make_weekly_plot(weekly_summary_rf_18s_all, "18S rRNA relative abundance: Baltic Sea and Warnow Estuary", category_colors, show_x_axis = TRUE)
)

# Create vertically stacked confusion matrices
conf_grobs <- list(
  make_conf_matrix_plot(results_manual_rf_copernicus, category_colors),
  make_conf_matrix_plot(results_manual_rf_16s, category_colors),
  make_conf_matrix_plot(results_manual_rf_18s, category_colors),
  make_conf_matrix_plot(results_manual_rf_16s_all, category_colors),
  make_conf_matrix_plot(results_manual_rf_18s_all, category_colors)
)


# Bar plot rows with preserved custom heights
bar_rows <- list(
  bar_grobs[[1]],  # LSTM
  bar_grobs[[2]],  # 16S
  bar_grobs[[3]],  # 18S
  bar_grobs[[4]],  # 16S All Locations
  bar_grobs[[5]]   # 18S All Locations
)

# Bar column with section headers and individual heights
# Bar column without region group labels
bar_column <- arrangeGrob(
  grobs = bar_rows,
  ncol = 1,
  heights = unit(c(1, 1, 1, 1, 1.6), "null")  # no region labels, just plot heights
)


# Confusion matrix column with consistent height
conf_column <- arrangeGrob(
  grobs = conf_grobs,
  ncol = 1,
  heights = unit(c(2, 2, 2, 2, 2), "null")
)


# Create LSTM footer title grob
rf_footer <- textGrob("RF model", gp = gpar(fontsize = 14, fontface = "bold"), just = "center")

# Combine bar plots and LSTM label vertically
rf_column <- arrangeGrob(
  bar_column,
  rf_footer,
  ncol = 1,
  heights = unit.c(unit(1, "null"), unit(1.2, "lines"))
)

# Combine all with shared y-axis
grid.arrange(
  textGrob("Percentage correctly identified weekly [%]", rot = 90, gp = gpar(fontsize = 13, fontface = "bold")),
  rf_column,
  conf_column,
  ncol = 4,
  widths = unit.c(unit(1, "lines"), unit(3, "null"), unit(0.9, "null"), unit(1.2, "null"))
)







################## this is for lstm model




# Store AUC values
pr_value_values <- c( 0.417, 0.107, 0.269)

# Define custom labels for the x-axis
custom_labels <- c("16S data", "18S data", "Copernicus data")  # Adjust labels as needed

# Create a data frame
pr_value_data <- data.frame(
  Model = factor(custom_labels, levels = custom_labels),  # Custom x-axis labels
  AUC = pr_value_values  # Y-axis (AUC values)
)

# Create AUC plot
pr_value_plot <- ggplot(pr_value_data, aes(x = Model, y = AUC)) +
  geom_point(size = 3, color = "blue") +   
  geom_line(group = 1, color = "blue") +   
  ggtitle("LSTM Model Performance different datasets") +
  xlab("Data used") +
  ylab("AUPRC") +
  ylim(0, 0.5) +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Print the plot
print(pr_value_plot)


# Store AUC values
auc_value_values <- c( 0.9327, 0.9043, 0.8922)

# Define custom labels for the x-axis
custom_labels <- c("16S data", "18S data", "Copernicus data")  # Adjust labels as needed

# Create a data frame
auc_value_data <- data.frame(
  Model = factor(custom_labels, levels = custom_labels),  # Custom x-axis labels
  AUC = auc_value_values  # Y-axis (AUC values)
)

auc_value_plot <- ggplot(auc_value_data, aes(x = Model, y = AUC)) +
  geom_point(size = 3, color = "red") +   
  geom_line(group = 1, color = "red") +   
  ggtitle("") +
  xlab("Data used") +
  ylab("ROC-AUC") +
  ylim(0.89, 1) +  
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


############# this is updated rf time lag vs lstm



# ---- LSTM ----

# Baltic Sea only — LSTM (from summary tables)
lstm_bs <- bind_rows(
  test_summary_all_timesteps_16s %>% mutate(Dataset = "16S"),
  test_summary_all_timesteps_18s %>% mutate(Dataset = "18S"),
  test_summary_all_timesteps_copernicus %>% mutate(Dataset = "Copernicus")
) %>%
  mutate(
    Model_Type = "LSTM",
    Region = "Baltic Sea",
    X_Label = paste0("Time-step ", TimeStep),
    AUPR = Avg_Test_AUPRC
  )

# All regions — LSTM
lstm_all <- bind_rows(
  test_summary_all_timesteps_16s_all %>% mutate(Dataset = "16S"),
  test_summary_all_timesteps_18s_all %>% mutate(Dataset = "18S")
) %>%
  mutate(
    Model_Type = "LSTM",
    Region = "Baltic Sea and Warnow Estuary",
    X_Label = paste0("Time-step ", TimeStep),
    AUPR = Avg_Test_AUPRC
  )

# ---- RF (leave as-is, using your hardcoded values) ----

# # Baltic Sea — RF
# rf_values <- data.frame(
#   Dataset = rep(c("16S", "18S", "Copernicus"), each = 10),
#   AUPR = c(
#     pr_value_16s_1, pr_value_16s_2, pr_value_16s_3, pr_value_16s_4,
#     pr_value_16s_5, pr_value_16s_6, pr_value_16s_7, pr_value_16s_8, pr_value_16s_9, pr_value_16s_10,
#     pr_value_18s_1, pr_value_18s_2, pr_value_18s_3, pr_value_18s_4,
#     pr_value_18s_5, pr_value_18s_6, pr_value_18s_7, pr_value_18s_8, pr_value_18s_9, pr_value_18s_10,
#     pr_value_cop_1, pr_value_cop_2, pr_value_cop_3, pr_value_cop_4,
#     pr_value_cop_5, pr_value_cop_6, pr_value_cop_7, pr_value_cop_8, pr_value_cop_9, pr_value_cop_10
#   ),
#   X_Label = rep(paste0("Time-lag ", 1:10), times = 3),
#   Model_Type = "Random Forest",
#   Region = "Baltic Sea"
# )

# Baltic Sea — RF
rf_values <- data.frame(
  Dataset = rep(c("16S", "18S", "Copernicus"), each = 10),
  AUPR = c(
    avg_test_auprc_16s_1, avg_test_auprc_16s_2, avg_test_auprc_16s_3, avg_test_auprc_16s_4,
    avg_test_auprc_16s_5, avg_test_auprc_16s_6, avg_test_auprc_16s_7, avg_test_auprc_16s_8, avg_test_auprc_16s_9, avg_test_auprc_16s_10,
    avg_test_auprc_18s_1, avg_test_auprc_18s_2, avg_test_auprc_18s_3, avg_test_auprc_18s_4,
    avg_test_auprc_18s_5, avg_test_auprc_18s_6, avg_test_auprc_18s_7, avg_test_auprc_18s_8, avg_test_auprc_18s_9, avg_test_auprc_18s_10,
    avg_test_auprc_1, avg_test_auprc_2, avg_test_auprc_3, avg_test_auprc_4,
    avg_test_auprc_5, avg_test_auprc_6, avg_test_auprc_7, avg_test_auprc_8, avg_test_auprc_9, avg_test_auprc_10
  ),
  X_Label = rep(paste0("Time-lag ", 1:10), times = 3),
  Model_Type = "Random Forest",
  Region = "Baltic Sea"
)

# All regions — RF
rf_all <- data.frame(
  Dataset = rep(c("16S", "18S"), each = 10),
  AUPR = c(
    avg_test_auprc_16s_all_1, avg_test_auprc_16s_all_2, avg_test_auprc_16s_all_3, avg_test_auprc_16s_all_4,
    avg_test_auprc_16s_all_5, avg_test_auprc_16s_all_6, avg_test_auprc_16s_all_7, avg_test_auprc_16s_all_8, avg_test_auprc_16s_all_9, avg_test_auprc_16s_all_10,
    avg_test_auprc_18s_all_1, avg_test_auprc_18s_all_2, avg_test_auprc_18s_all_3, avg_test_auprc_18s_all_4,
    avg_test_auprc_18s_all_5, avg_test_auprc_18s_all_6, avg_test_auprc_18s_all_7, avg_test_auprc_18s_all_8, avg_test_auprc_18s_all_9, avg_test_auprc_18s_all_10
  ),
  X_Label = rep(paste0("Time-lag ", 1:10), times = 2),
  Model_Type = "Random Forest",
  Region = "Baltic Sea and Warnow Estuary"
)





# Define numeric Time column shared by both models
lstm_df <- bind_rows(
  test_summary_all_timesteps_16s        %>% mutate(Dataset = "16S", Region = "Baltic Sea"),
  test_summary_all_timesteps_16s_all    %>% mutate(Dataset = "16S", Region = "Baltic Sea and Warnow Estuary"),
  test_summary_all_timesteps_18s        %>% mutate(Dataset = "18S", Region = "Baltic Sea"),
  test_summary_all_timesteps_18s_all    %>% mutate(Dataset = "18S", Region = "Baltic Sea and Warnow Estuary"),
  test_summary_all_timesteps_copernicus %>% mutate(Dataset = "Copernicus", Region = "Baltic Sea")
) %>%
  mutate(
    Model_Type = "LSTM",
    Time = TimeStep,
    AUPR = Avg_Test_AUPRC
  )

rf_df <- bind_rows(
  rf_values %>% mutate(Region = "Baltic Sea"),
  rf_all %>% mutate(Region = "Baltic Sea and Warnow Estuary")
) %>%
  mutate(
    Model_Type = "Random Forest",
    Time = rep(1:10, times = nrow(.) / 10)
  )

# Shared data setup
plot_df <- bind_rows(lstm_df, rf_df) %>%
  mutate(
    Dataset = factor(Dataset, levels = c("16S", "18S", "Copernicus")),
    Region = factor(Region, levels = c("Baltic Sea", "Baltic Sea and Warnow Estuary"))
  )

# Define RF plot (no title now)
rf_plot <- plot_df %>%
  filter(Model_Type == "Random Forest") %>%
  ggplot(aes(x = Time, y = AUPR, color = Dataset, group = Dataset)) +
  geom_point(size = 3) +
  geom_line() +
  facet_wrap(~Region, nrow = 1) +
  scale_x_continuous(breaks = 1:10, labels = paste0("", 1:10)) +
  scale_color_manual(values = c("16S" = "#1b9e77", "18S" = "#d95f02", "Copernicus" = "#7570b3")) +
  ylim(0.1, 0.8) +
  labs(y = "AUPRC", x = NULL, color = "Dataset") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

# Define LSTM plot (no title now)
lstm_plot <- plot_df %>%
  filter(Model_Type == "LSTM") %>%
  ggplot(aes(x = Time, y = AUPR, color = Dataset, group = Dataset)) +
  geom_point(size = 3) +
  geom_line() +
  facet_wrap(~Region, nrow = 1) +
  scale_x_continuous(breaks = 1:10, labels = paste0("1 to ", 1:10)) +
  scale_color_manual(values = c("16S" = "#1b9e77", "18S" = "#d95f02", "Copernicus" = "#7570b3")) +
  ylim(0.1, 0.8) +
  labs(y = "AUPRC", x = "Time-lag") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

# Create vertical labels using textGrob wrapped in an empty ggplot
rf_label <- ggplot() + 
  theme_void() + 
  annotation_custom(textGrob("Random Forest", rot = 90, gp = gpar(fontsize = 14, fontface = "bold")))

lstm_label <- ggplot() + 
  theme_void() + 
  annotation_custom(textGrob("LSTM", rot = 90, gp = gpar(fontsize = 14, fontface = "bold")))

# Combine with patchwork layout
(rf_label + rf_plot) / (lstm_label + lstm_plot) + 
  plot_layout(widths = c(1, 1), heights = c(2, 1))

# Combine vertically
rf_plot / lstm_plot + plot_layout(heights = c(1, 1))


# Define rotated label grobs (for both sides)
rf_left_label <- ggplot() + theme_void() +
  annotation_custom(grid::textGrob("Random Forest", rot = 90, gp = gpar(fontsize = 13, fontface = "bold")))

rf_right_label <- ggplot() + theme_void() +
  annotation_custom(grid::textGrob("Random Forest", rot = -90, gp = gpar(fontsize = 13, fontface = "bold")))

lstm_left_label <- ggplot() + theme_void() +
  annotation_custom(grid::textGrob("LSTM", rot = 90, gp = gpar(fontsize = 13, fontface = "bold")))

lstm_right_label <- ggplot() + theme_void() +
  annotation_custom(grid::textGrob("LSTM", rot = -90, gp = gpar(fontsize = 13, fontface = "bold")))

# Combine using patchwork layout
layout <- "
AB
DE
"

# A = RF left label, B = RF plot, C = RF right label
# D = LSTM left label, E = LSTM plot, F = LSTM right label
(
   
    lstm_left_label + lstm_plot +
  rf_left_label + rf_plot 
) +
  plot_layout(design = layout, widths = c(0.05, 1, 0.05), heights = c(1, 1))




##################### this is for supplementary information confusion matrices






