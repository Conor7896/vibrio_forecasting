######### quantity graph

setwd("C:/Users/glack/Documents/vibrio_second_paper/")

# Read the data
join_all_current_discharge_hplc_temp_weather <- 
  readr::read_csv("data_all/join_all_current_discharge_hplc_temp_weather_full_year.csv")

# Define the Baltic Sea locations
baltic_sea_locations <- c(
  "Heiligendamm", "Jemnitzschleuse", "Borgerende", 
  "Nienhagen", "Elmenhorst", "Diedrichshagen", "Warnemuende"
)

# Create new column 'Sampling_Region'
join_all_current_discharge_hplc_temp_weather$Sampling_Region <- ifelse(
  join_all_current_discharge_hplc_temp_weather$location %in% baltic_sea_locations,
  "Baltic Sea",
  "Warnow Estuary"
)

# Limit v_vulnificus to core summer window at selected locations
join_all_current_discharge_hplc_temp_weather <- 
  join_all_current_discharge_hplc_temp_weather %>%
  dplyr::mutate(
    v_vulnificus = dplyr::if_else(
      location %in% c("Borgerende", "Heiligendamm", "Warnemuende", "Nienhagen") &
        date >= as.Date("2022-07-03") & date <= as.Date("2022-09-16"),
      v_vulnificus,
      NA_real_
    )
  )

# Long format
df_long <- join_all_current_discharge_hplc_temp_weather %>%
  tidyr::pivot_longer(
    cols = c(v_vulnificus, ddpcr, v_vul_total_abundance),
    names_to = "Measurement",
    values_to = "Value"
  )

# Facet labels
df_long$Measurement <- factor(
  df_long$Measurement,
  levels = c("v_vulnificus", "ddpcr", "v_vul_total_abundance"),
  labels = c(
    "Presumptive cultured<br><i>V. vulnificus</i> colonies",
    "ddPCR <i>V. vulnificus</i><br>(vvha copies ml<sup>&minus;1</sup>)",
    "Relative abundance<br>based on 16S<br>rRNA genes"
  )
)

# Colours
region_colors <- c(
  "Baltic Sea"     = "#1f78b4",
  "Warnow Estuary" = "#A0522D"
)

# Plot
abundance_plot <- ggplot(
  df_long,
  aes(x = date, y = Value,
      color = Sampling_Region,
      shape = Sampling_Region)
) +
  geom_point(alpha = 0.7, size = 1.4, na.rm = TRUE) +   # slightly larger points
  facet_wrap(
    ~Measurement,
    scales = "free_y",
    ncol = 1,
    strip.position = "left"
  ) +
  scale_color_manual(values = region_colors) +
  scale_shape_manual(values = c(
    "Baltic Sea"     = 16,
    "Warnow Estuary" = 15
  )) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(
    x     = "Date",
    y     = NULL,
    color = "Sampling Region:",
    shape = "Sampling Region:"
  ) +
  theme_minimal(base_size = 9, base_family = "sans") +  # << main text size bump
  theme(
    strip.text.y.left = ggtext::element_markdown(size = 9, angle = 90),
    strip.placement   = "outside",
    axis.text.x       = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y       = element_text(size = 8),
    axis.title.x      = element_text(size = 9),
    legend.position   = "top",
    legend.title      = element_text(face = "bold", size = 9),
    legend.text       = element_text(size = 8),
    legend.key        = element_rect(fill = NA),
    plot.background   = element_rect(fill = "white", colour = NA),
    panel.background  = element_rect(fill = "white", colour = NA),
    plot.margin       = ggplot2::margin(t = 4, r = 4, b = 12, l = 4, unit = "mm")
  )

agg_tiff(
  "images/abundance_quantity.tiff",
  width       = 7,
  height      = 6.5,
  units       = "in",
  res         = 1000,          # still publication-grade
  compression = "lzw",
  background  = "white"
)

print(abundance_plot)
dev.off()

######### next plot 


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
######### Temperature & Salinity vs Vibrio Measures — Publication Format #########

# Plot
env_plot <- ggplot(
  df_env,
  aes(x = X_value, y = Value,
      color = Sampling_Region,
      shape = Sampling_Region)
) +
  geom_point(alpha = 0.7, size = 1.4, na.rm = TRUE) +     # point size slightly increased (visual balance)
  facet_grid(
    Measurement ~ Variable,
    switch = "y",
    scales = "free_y"
  ) +
  scale_color_manual(values = region_colors) +
  scale_shape_manual(values = c(
    "Baltic Sea"     = 16,   # ● circle
    "Warnow Estuary" = 15    # ■ square
  )) +
  labs(
    x = NULL,
    y = NULL,
    color = "Sampling Region:",
    shape = "Sampling Region:"
  ) +
  theme_minimal(base_size = 9, base_family = "sans") +   # slightly larger, journal-friendly
  theme(
    strip.text.x.bottom = element_text(size = 9, face = "bold"),
    strip.text.y.left   = ggtext::element_markdown(size = 9),
    strip.placement     = "outside",
    legend.position     = "top",
    legend.title        = element_text(face = "bold", size = 9),
    legend.text         = element_text(size = 8),
    axis.text.x         = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y         = element_text(size = 8),
    legend.key          = element_rect(fill = NA),
    panel.background    = element_rect(fill = "white", colour = NA),
    plot.background     = element_rect(fill = "white", colour = NA),
    panel.spacing       = unit(1, "lines"),
    plot.margin         = ggplot2::margin(t = 4, r = 4, b = 10, l = 4, unit = "mm")
  )

env_plot <- env_plot +
  scale_x_continuous() +  # (forces axis to be drawn even in multi-facet grid)
  theme(
    axis.text.x  = element_text(size = 8, angle = 45, hjust = 1),
    axis.title.x = element_text(size = 9),
    plot.margin  = ggplot2::margin(t = 4, r = 4, b = 18, l = 4, unit = "mm")  # increased bottom margin
  )

print(env_plot)

agg_tiff(
  "images/env_temp_salinity_vvul.tiff",
  width       = 7,        # suitable for ~2-column layout
  height      = 6.5,      # adjust slightly if needed
  units       = "in",
  res         = 1000,      # 800–1000 dpi OK for line art
  compression = "lzw",
  background  = "white"
)

print(env_plot)
dev.off()

measurement_labels <- c(
  "v_vulnificus"          = "Presumptive cultured<br><i>V. vulnificus</i> colonies",
  "ddpcr"                 = "ddPCR <i>V. vulnificus</i><br>(vvha copies ml<sup>&minus;1</sup>)",
  "v_vul_total_abundance" = "Relative abundance<br>based on 16S<br>rRNA genes"
)

# Prepare salinity data
df_sal <- df %>%
  pivot_longer(
    cols      = c(v_vulnificus, ddpcr, v_vul_total_abundance),
    names_to  = "Measurement",
    values_to = "Value"
  ) %>%
  filter(
    !is.na(Value),
    !is.na(salinity)
  ) %>%
  mutate(
    Variable = "Salinity (PSU)",
    X_value  = salinity
  )

# Prepare temperature data
df_temp <- df %>%
  pivot_longer(
    cols      = c(v_vulnificus, ddpcr, v_vul_total_abundance),
    names_to  = "Measurement",
    values_to = "Value"
  ) %>%
  filter(
    !is.na(Value),
    !is.na(temperature.x)
  ) %>%
  mutate(
    Variable = "Temperature (°C)",
    X_value  = temperature.x
  )

# Combine and clean
df_env <- bind_rows(df_sal, df_temp) %>%
  filter(!is.na(Value)) %>%  # Ensure NAs are removed from all values
  mutate(
    Measurement = factor(
      Measurement,
      levels = names(measurement_labels),
      labels = measurement_labels
    ),
    Variable = factor(
      Variable,
      levels = c("Salinity (PSU)", "Temperature (°C)")
    )
  )

# Define region colors
region_colors <- c(
  "Baltic Sea"      = "#1f78b4",
  "Warnow Estuary"  = "#A0522D"
)
# region_colors <- c("Baltic Sea" = "black", "Warnow Estuary" = "black")

ggplot(df_env, aes(x = X_value, y = Value, color = Sampling_Region)) +
  geom_point(alpha = 0.7, size = 1.45) +
  facet_grid(
    Measurement ~ Variable,
    switch = "y",
    scales = "free_y"
  ) +
  scale_color_manual(values = region_colors) +
  labs(
    x     = NULL,
    y     = NULL,
    color = "Sampling Region:"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text.x.bottom = element_text(size = 11, face = "bold"),
    strip.text.y.left   = ggtext::element_markdown(size = 11),
    strip.placement     = "outside",
    legend.position     = "top",  # Legend at top center
    legend.title        = element_text(face = "bold", size = 12),
    legend.text         = element_text(size = 11),
    legend.key          = element_rect(fill = NA),
    axis.text.x         = element_text(angle = 45, hjust = 1),
    panel.spacing       = unit(1, "lines")
  )


################### RF AUPRC PLOT ###################

# AUPRC values for location subset (5 models)
pr_value_subset <- c(pr_value_all, pr_value_otc, pr_value_16s, pr_value_18s, pr_value_cop)
labels_subset <- c(
  "Combined\ndataset",
  "Biological and\nPhysical parameters",
  "16S rRNA\nrelative abundance",
  "18S rRNA\nrelative abundance",
  "Copernicus\nsatellite data"
)

# AUPRC values for all locations (4 models only — no Copernicus)
pr_value_all_loc <- c(
  pr_value_all_all_loc,
  pr_value_otc_all_loc,
  pr_value_16s_all_loc,
  pr_value_18s_all_loc
)
labels_all_loc <- c(
  "Combined\ndataset",
  "Biological and\nPhysical parameters",
  "16S rRNA\nrelative abundance",
  "18S rRNA\nrelative abundance"
)

# Data frames
df_subset <- data.frame(
  Model   = factor(labels_subset, levels = labels_subset),
  AUPRC   = pr_value_subset,
  Dataset = "Location subset"
)

df_all_loc <- data.frame(
  Model   = factor(labels_all_loc, levels = labels_subset),  # keep same overall levels
  AUPRC   = pr_value_all_loc,
  Dataset = "All locations"
)

# Combine
pr_value_data <- dplyr::bind_rows(df_subset, df_all_loc)

# Rename datasets for publication
pr_value_data$Dataset <- dplyr::recode(
  pr_value_data$Dataset,
  "Location subset" = "Baltic Sea",
  "All locations"   = "Baltic Sea and Warnow Estuary"
)

# Colours
dataset_colors <- c(
  "Baltic Sea"                   = "#1f78b4",  # dark blue
  "Baltic Sea and Warnow Estuary" = "#5ca76a"  # bluish olive mix
)

# Shapes: Baltic Sea = circle, Baltic+Warnow = triangle
dataset_shapes <- c(
  "Baltic Sea"                    = 16,  # filled circle
  "Baltic Sea and Warnow Estuary" = 17   # filled triangle
)

# Plot
rf_aupr_plot <- ggplot(
  pr_value_data,
  aes(
    x     = Model,
    y     = AUPRC,
    color = Dataset,
    shape = Dataset
  )
) +
  geom_point(size = 2.5) +
  scale_color_manual(values = dataset_colors) +
  scale_shape_manual(values = dataset_shapes) +
  ylim(0.3, 1) +
  xlab("Data used for model training") +
  ylab("Area Under the Precision-Recall Curve (AUPRC)") +
  theme_minimal(base_size = 9, base_family = "sans") +
  theme(
    legend.position  = "top",
    legend.title     = element_text(face = "bold", size = 9),
    legend.text      = element_text(size = 8),
    legend.key       = element_rect(fill = NA),
    axis.title.x     = element_text(size = 9),
    axis.title.y     = element_text(size = 9),
    axis.text.x      = element_text(size = 8, angle = 0, hjust = 0.5),
    axis.text.y      = element_text(size = 8),
    plot.title       = element_text(hjust = 0.5, size = 10, face = "bold"),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.margin      = ggplot2::margin(t = 4, r = 4, b = 12, l = 4, unit = "mm")
  ) +
  guides(
    color = guide_legend(title = "Sampling Region:"),
    shape = guide_legend(title = "Sampling Region:")
  )

print(rf_aupr_plot)
# Save to TIFF (similar format to abundance_plot)
agg_tiff(
  "images/rf_aupr.tiff",
  width       = 7,
  height      = 4,
  units       = "in",
  res         = 1000,
  compression = "lzw",
  background  = "white"
)

print(rf_aupr_plot)
dev.off()


#### time lag lstm


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

# ---- RF ----

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


# Shared df
plot_df <- bind_rows(
  lstm_df <- bind_rows(
    test_summary_all_timesteps_16s        %>% mutate(Dataset = "16S", Region = "Baltic Sea"),
    test_summary_all_timesteps_16s_all    %>% mutate(Dataset = "16S", Region = "Baltic Sea and Warnow Estuary"),
    test_summary_all_timesteps_18s        %>% mutate(Dataset = "18S", Region = "Baltic Sea"),
    test_summary_all_timesteps_18s_all    %>% mutate(Dataset = "18S", Region = "Baltic Sea and Warnow Estuary"),
    test_summary_all_timesteps_copernicus %>% mutate(Dataset = "Copernicus", Region = "Baltic Sea")
  ) %>% mutate(Model_Type = "LSTM", Time = TimeStep, AUPR = Avg_Test_AUPRC),
  
  rf_df <- bind_rows(
    rf_values %>% mutate(Region = "Baltic Sea"),
    rf_all %>% mutate(Region = "Baltic Sea and Warnow Estuary")
  ) %>% mutate(Model_Type = "Random Forest", Time = rep(1:10, times = nrow(.)/10))
) %>%
  mutate(
    Dataset = factor(Dataset, levels = c("16S", "18S", "Copernicus")),
    Region  = factor(Region, levels = c("Baltic Sea", "Baltic Sea and Warnow Estuary"))
  )

# ------------------------
#  APPLY SHAPE MAPPING
# ------------------------
rf_plot <- plot_df %>%
  filter(Model_Type == "Random Forest") %>%
  ggplot(aes(x = Time, y = AUPR, color = Dataset, shape = Region, group = Dataset)) +
  geom_point(size = 3) +
  geom_line() +
  facet_wrap(~Region, nrow = 1) +
  scale_shape_manual(values = c("Baltic Sea" = 16, "Baltic Sea and Warnow Estuary" = 17)) +
  scale_x_continuous(breaks = 1:10, labels = paste0("", 1:10)) +
  scale_color_manual(values = c("16S" = "#1b9e77", "18S" = "#d95f02", "Copernicus" = "#7570b3")) +
  ylim(0.1, 0.8) +
  labs(y = "AUPRC", x = NULL, color = "Dataset") +
  theme_minimal(base_size = 9) +                                        # ← changed
  theme(
    legend.position = "top",
    legend.title = element_text(size = 9, face = "bold"),              # ← changed
    legend.text  = element_text(size = 8),                             # ← changed
    axis.text.x  = element_text(angle = 30, hjust = 1, size = 8),      # ← changed
    axis.text.y  = element_text(size = 8),                             # ← changed
    strip.text   = element_text(size = 9, face = "bold")               # ← changed
  )

lstm_plot <- plot_df %>%
  filter(Model_Type == "LSTM") %>%
  ggplot(aes(x = Time, y = AUPR, color = Dataset, shape = Region, group = Dataset)) +
  geom_point(size = 3) +
  geom_line() +
  facet_wrap(~Region, nrow = 1) +
  scale_shape_manual(values = c("Baltic Sea" = 16, "Baltic Sea and Warnow Estuary" = 17)) +
  scale_x_continuous(breaks = 1:10, labels = paste0("1 to ", 1:10)) +
  scale_color_manual(values = c("16S" = "#1b9e77", "18S" = "#d95f02", "Copernicus" = "#7570b3")) +
  ylim(0.1, 0.8) +
  labs(y = "AUPRC", x = "Time-lag") +
  theme_minimal(base_size = 9) +                                         # ← changed
  theme(
    legend.position="none",
    axis.text.x = element_text(angle = 30, hjust = 1, size = 8),         # ← changed
    axis.text.y = element_text(size = 8),                                # ← changed
    strip.text  = element_text(size = 9, face = "bold")                  # ← changed
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

# Save to TIFF (similar format to abundance_plot)
agg_tiff(
  "images/rf_lstm_timelag.tiff",
  width       = 7,
  height      = 5,
  units       = "in",
  res         = 1000,      # <-- exactly as you asked
  compression = "lzw",
  background  = "white"
)

print(rf_plot / lstm_plot)   # saves combined RF + LSTM panel
dev.off()


############# updated graphs

######### quantity graph

setwd("C:/Users/glack/Documents/vibrio_second_paper/")

# Read the data
join_all_current_discharge_hplc_temp_weather <- 
  readr::read_csv("data_all/join_all_current_discharge_hplc_temp_weather_full_year.csv")

# Define the Baltic Sea locations
baltic_sea_locations <- c(
  "Heiligendamm", "Jemnitzschleuse", "Borgerende", 
  "Nienhagen", "Elmenhorst", "Diedrichshagen", "Warnemuende"
)

# Create new column 'Sampling_Region'
join_all_current_discharge_hplc_temp_weather$Sampling_Region <- ifelse(
  join_all_current_discharge_hplc_temp_weather$location %in% baltic_sea_locations,
  "Baltic Sea",
  "Warnow Estuary"
)

# Limit v_vulnificus to core summer window at selected locations
join_all_current_discharge_hplc_temp_weather <- 
  join_all_current_discharge_hplc_temp_weather %>%
  dplyr::mutate(
    v_vulnificus = dplyr::if_else(
      location %in% c("Borgerende", "Heiligendamm", "Warnemuende", "Nienhagen") &
        date >= as.Date("2022-07-03") & date <= as.Date("2022-09-16"),
      v_vulnificus,
      NA_real_
    )
  )

# Long format
df_long <- join_all_current_discharge_hplc_temp_weather %>%
  tidyr::pivot_longer(
    cols = c(v_vulnificus, ddpcr, v_vul_total_abundance),
    names_to = "Measurement",
    values_to = "Value"
  )
# Facet labels
df_long$Measurement <- factor(
  df_long$Measurement,
  levels = c("v_vulnificus", "ddpcr", "v_vul_total_abundance"),
  labels = c(
    "Presumptive cultured<br><i>V. vulnificus</i> colonies",
    "ddPCR <i>V. vulnificus</i><br>(vvha copies ml<sup>&minus;1</sup>)",
    "Relative abundance<br>based on 16S<br>rRNA genes"
  )
)

# Colours
region_colors <- c(
  "Baltic Sea"     = "#1f78b4",
  "Warnow Estuary" = "#A0522D"
)

# ------------------------------------------------------------------
# Panel labels (A, B, C) – one per facet, in the upper-left area
# ------------------------------------------------------------------

label_df <- df_long %>%
  dplyr::group_by(Measurement) %>%
  dplyr::summarise(
    date  = min(date, na.rm = TRUE),
    Value = max(Value, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  dplyr::mutate(
    date  = date + 350,
    Value = Value * 0.95,
    label = dplyr::case_when(
      Measurement == "Presumptive cultured<br><i>V. vulnificus</i> colonies" ~ "A",
      Measurement == "ddPCR <i>V. vulnificus</i><br>(vvha copies ml<sup>&minus;1</sup>)" ~ "B",
      Measurement == "Relative abundance<br>based on 16S<br>rRNA genes" ~ "C"
    )
  )



# Plot
abundance_plot <- ggplot(
  df_long,
  aes(x = date, y = Value,
      color = Sampling_Region,
      shape = Sampling_Region)
) +
  geom_point(alpha = 0.7, size = 1.4, na.rm = TRUE) +   # slightly larger points
  facet_wrap(
    ~Measurement,
    scales = "free_y",
    ncol = 1,
    strip.position = "left"
  ) +
  # A, B, C labels inside each panel
  geom_text(
    data = label_df,
    aes(x = date, y = Value, label = label),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 4
  ) +
  scale_color_manual(values = region_colors) +
  scale_shape_manual(values = c(
    "Baltic Sea"     = 16,
    "Warnow Estuary" = 15
  )) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(
    x     = "Date",
    y     = NULL,
    color = "Sampling Region:",
    shape = "Sampling Region:"
  ) +
  theme_minimal(base_size = 9, base_family = "sans") +  # << main text size bump
  theme(
    strip.text.y.left = ggtext::element_markdown(size = 9, angle = 90),
    strip.placement   = "outside",
    axis.text.x       = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y       = element_text(size = 8),
    axis.title.x      = element_text(size = 9),
    legend.position   = "top",
    legend.title      = element_text(face = "bold", size = 9),
    legend.text       = element_text(size = 8),
    legend.key        = element_rect(fill = NA),
    plot.background   = element_rect(fill = "white", colour = NA),
    panel.background  = element_rect(fill = "white", colour = NA),
    plot.margin       = ggplot2::margin(t = 4, r = 4, b = 12, l = 4, unit = "mm")
  )

agg_tiff(
  "images/abundance_quantity.tiff",
  width       = 7,
  height      = 6.5,
  units       = "in",
  res         = 1000,          # still publication-grade
  compression = "lzw",
  background  = "white"
)

print(abundance_plot)
dev.off()

##### graph 2


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

# ------------------------------------------------------------------
# Panel labels (A, B, C) – one per Vibrio measure, in each row
# ------------------------------------------------------------------
label_df <- df_env %>%
  dplyr::group_by(Measurement, Variable) %>%
  dplyr::summarise(
    X_value = min(X_value, na.rm = TRUE),
    Value   = max(Value,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(Measurement, Variable) %>%  # ensures consistent panel order
  dplyr::mutate(
    X_value = X_value * 0.001,   # small horizontal nudge (as you had)
    Value   = Value * 0.95,      # small vertical nudge
    label   = c("A", "B", "C", "D", "E", "F")[dplyr::row_number()]
  )


# Define region colors
region_colors <- c("Baltic Sea" = "#1f78b4", "Warnow Estuary" = "#A0522D")
#region_colors <- c("Baltic Sea" = "black", "Warnow Estuary" = "black")
######### Temperature & Salinity vs Vibrio Measures — Publication Format #########

# Plot
env_plot <- ggplot(
  df_env,
  aes(x = X_value, y = Value,
      color = Sampling_Region,
      shape = Sampling_Region)
) +
  geom_point(alpha = 0.7, size = 1.4, na.rm = TRUE) +     # point size slightly increased (visual balance)
  geom_text(
    data = label_df,
    aes(x = X_value, y = Value, label = label),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 4
  ) +
  facet_grid(
    Measurement ~ Variable,
    switch = "y",
    scales = "free_y"
  ) +
  scale_color_manual(values = region_colors) +
  scale_shape_manual(values = c(
    "Baltic Sea"     = 16,   # ● circle
    "Warnow Estuary" = 15    # ■ square
  )) +
  labs(
    x = NULL,
    y = NULL,
    color = "Sampling Region:",
    shape = "Sampling Region:"
  ) +
  theme_minimal(base_size = 9, base_family = "sans") +   # slightly larger, journal-friendly
  theme(
    strip.text.x.bottom = element_text(size = 9, face = "bold"),
    strip.text.y.left   = ggtext::element_markdown(size = 9),
    strip.placement     = "outside",
    legend.position     = "top",
    legend.title        = element_text(face = "bold", size = 9),
    legend.text         = element_text(size = 8),
    axis.text.x         = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y         = element_text(size = 8),
    legend.key          = element_rect(fill = NA),
    panel.background    = element_rect(fill = "white", colour = NA),
    plot.background     = element_rect(fill = "white", colour = NA),
    panel.spacing       = unit(1, "lines"),
    plot.margin         = ggplot2::margin(t = 4, r = 4, b = 10, l = 4, unit = "mm")
  )

env_plot <- env_plot +
  scale_x_continuous() +  # (forces axis to be drawn even in multi-facet grid)
  theme(
    axis.text.x  = element_text(size = 8, angle = 45, hjust = 1),
    axis.title.x = element_text(size = 9),
    plot.margin  = ggplot2::margin(t = 4, r = 4, b = 18, l = 4, unit = "mm")  # increased bottom margin
  )

print(env_plot)

agg_tiff(
  "images/env_temp_salinity_vvul.tiff",
  width       = 7,        # suitable for ~2-column layout
  height      = 6.5,      # adjust slightly if needed
  units       = "in",
  res         = 1000,      # 800–1000 dpi OK for line art
  compression = "lzw",
  background  = "white"
)

print(env_plot)
dev.off()


#### time lag lstm


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

# ---- RF ----

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


# Shared df
plot_df <- bind_rows(
  lstm_df <- bind_rows(
    test_summary_all_timesteps_16s        %>% mutate(Dataset = "16S", Region = "Baltic Sea"),
    test_summary_all_timesteps_16s_all    %>% mutate(Dataset = "16S", Region = "Baltic Sea and Warnow Estuary"),
    test_summary_all_timesteps_18s        %>% mutate(Dataset = "18S", Region = "Baltic Sea"),
    test_summary_all_timesteps_18s_all    %>% mutate(Dataset = "18S", Region = "Baltic Sea and Warnow Estuary"),
    test_summary_all_timesteps_copernicus %>% mutate(Dataset = "Copernicus", Region = "Baltic Sea")
  ) %>% mutate(Model_Type = "LSTM", Time = TimeStep, AUPR = Avg_Test_AUPRC),
  
  rf_df <- bind_rows(
    rf_values %>% mutate(Region = "Baltic Sea"),
    rf_all %>% mutate(Region = "Baltic Sea and Warnow Estuary")
  ) %>% mutate(Model_Type = "Random Forest", Time = rep(1:10, times = nrow(.)/10))
) %>%
  mutate(
    Dataset = factor(Dataset, levels = c("16S", "18S", "Copernicus")),
    Region  = factor(Region, levels = c("Baltic Sea", "Baltic Sea and Warnow Estuary"))
  )

# ------------------------
#  APPLY SHAPE MAPPING
# ------------------------
# ------------------------
#  APPLY SHAPE MAPPING
# ------------------------
rf_plot <- plot_df %>%
  filter(Model_Type == "Random Forest") %>%
  dplyr::mutate(
    Region_panel = factor(
      dplyr::case_when(
        Region == "Baltic Sea" ~ "C: Baltic Sea",
        Region == "Baltic Sea and Warnow Estuary" ~ "D: Baltic Sea and Warnow Estuary"
      ),
      levels = c("C: Baltic Sea", "D: Baltic Sea and Warnow Estuary")
    )
  ) %>%
  ggplot(aes(x = Time, y = AUPR, color = Dataset, shape = Region, group = Dataset)) +
  geom_point(size = 3) +
  geom_line() +
  facet_wrap(~Region_panel, nrow = 1) +
  scale_shape_manual(values = c("Baltic Sea" = 16, "Baltic Sea and Warnow Estuary" = 17)) +
  scale_x_continuous(breaks = 1:10, labels = paste0("", 1:10)) +
  scale_color_manual(values = c("16S" = "#1b9e77", "18S" = "#d95f02", "Copernicus" = "#7570b3")) +
  ylim(0.1, 0.8) +
  labs(y = "AUPRC", x = NULL, color = "Dataset") +
  theme_minimal(base_size = 9) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text  = element_text(size = 8),
    axis.text.x  = element_text(angle = 30, hjust = 1, size = 8),
    axis.text.y  = element_text(size = 8),
    strip.text   = element_text(size = 9, face = "bold")
  )

lstm_plot <- plot_df %>%
  filter(Model_Type == "LSTM") %>%
  dplyr::mutate(
    Region_panel = factor(
      dplyr::case_when(
        Region == "Baltic Sea" ~ "A: Baltic Sea",
        Region == "Baltic Sea and Warnow Estuary" ~ "B: Baltic Sea and Warnow Estuary"
      ),
      levels = c("A: Baltic Sea", "B: Baltic Sea and Warnow Estuary")
    )
  ) %>%
  ggplot(aes(x = Time, y = AUPR, color = Dataset, shape = Region, group = Dataset)) +
  geom_point(size = 3) +
  geom_line() +
  facet_wrap(~Region_panel, nrow = 1) +
  scale_shape_manual(values = c("Baltic Sea" = 16, "Baltic Sea and Warnow Estuary" = 17)) +
  scale_x_continuous(breaks = 1:10, labels = paste0("1 to ", 1:10)) +
  scale_color_manual(values = c("16S" = "#1b9e77", "18S" = "#d95f02", "Copernicus" = "#7570b3")) +
  ylim(0.1, 0.8) +
  labs(y = "AUPRC", x = "Time-lag") +
  theme_minimal(base_size = 9) +
  theme(
    legend.position="none",
    axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    strip.text  = element_text(size = 9, face = "bold")
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
full_plot <- (
  lstm_left_label + lstm_plot +
    rf_left_label + rf_plot 
) +
  plot_layout(design = layout,
              widths  = c(0.05, 1),
              heights = c(1, 1))

# Save to TIFF (similar format to abundance_plot)
agg_tiff(
  "images/rf_lstm_timelag.tiff",
  width       = 7,
  height      = 5,
  units       = "in",
  res         = 1000,
  compression = "lzw",
  background  = "white"
)

print(full_plot)   # <- this now includes the vertical LSTM / RF labels
dev.off()
