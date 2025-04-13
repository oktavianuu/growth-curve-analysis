# ================================================================
# WHO-Style Growth Curve Modeling (Weight-for-Age, Boys)
# Study Location: Desa Jungkat, Kecamatan Jongkat, Pontianak, West Kalimantan
# Model: BCPE Distribution using GAMLSS
# ================================================================
# NOTE:
# Three candidate models were tested: BCCG (LMS), BCPE, and BCT.
# Model selection was based on AIC, GAIC (BIC), and worm plot diagnostics.
# Although BCCG had the lowest AIC/GAIC, the worm plot revealed tail misfit.
# BCPE had a negligible AIC difference but provided the best residual fit.
# Therefore, BCPE was selected as the final model for this growth curve.

# 📦 Load Required Libraries
library(gamlss)
library(dplyr)
library(tidyr)
library(ggplot2)
library(writexl)
library(readxl)

# 📂 Load the Data
data_boys_60 <- read.csv("data/data_boys_60.csv")

# ================================================================
# Model Fitting using GAMLSS with BCPE distribution
# ================================================================

boys_bcpe <- gamlss(
  bw ~ pb(age), 
  sigma.fo = ~ pb(age), 
  nu.fo = ~ pb(age), 
  tau.fo = ~ pb(age), 
  data = data_boys_60, 
  family = BCPE
)

# ================================================================
# Predict Z-score Curves (-3, -2, 0, +2, +3)
# ================================================================

Z_scores <- c(-3, -2, 0, 2, 3)
zscore_pred <- centiles.pred(
  boys_bcpe,
  xname = "age",
  xvalues = 1:60,
  type = "standard-centiles",
  dev = Z_scores,
  plot = FALSE
)

# Convert to long-form for plotting
zscore_df <- as.data.frame(zscore_pred)
colnames(zscore_df) <- c("Age", paste0("Z", Z_scores))

plot_df <- zscore_df %>%
  pivot_longer(cols = -Age, names_to = "Z", values_to = "Value") %>%
  mutate(Label = Z)

label_df <- plot_df %>%
  filter(Age == max(Age))  # Tip labels at Age = 60

# ================================================================
# Grid Setup (1 line per month, thicker every 12 months)
# ================================================================

monthly_lines <- data.frame(x = 1:60)
yearly_lines <- data.frame(x = seq(12, 60, 12))

# ================================================================
# WHO-style Plot
# ================================================================

ggplot(plot_df, aes(x = Age, y = Value, color = Z, linetype = Z)) +
  geom_line(size = 1) +
  geom_vline(data = monthly_lines, aes(xintercept = x), color = "grey85", size = 0.3) +
  geom_vline(data = yearly_lines, aes(xintercept = x), color = "grey50", size = 0.8) +
  geom_text(data = label_df, aes(label = Label), 
            hjust = -0.1, vjust = 0.5, size = 3) +
  scale_x_continuous(
    breaks = seq(12, 60, 12),
    labels = 1:5,
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_color_manual(values = c(
    "Z-3" = "#DC143C",   # Crimson
    "Z-2" = "#1E90FF",   # Dodger Blue
    "Z0"  = "#228B22",   # Forest Green
    "Z2"  = "#FF8C00",   # Dark Orange
    "Z3"  = "#323ea8"    # Deep Blue
  )) +
  scale_linetype_manual(values = rep("solid", 5)) +
  labs(
    x = "Age (years)",
    y = "Weight (kg)",
    #title = "Weight-for-Age Z-Score Reference Curves (BCPE Model)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.5),
    panel.grid.minor.y = element_line(size = 0.2, color = "grey90")
  )

# ================================================================
# 💾 Save the Z-score Table to Excel
# ================================================================

write_xlsx(zscore_df, "outputs/weight_zscore_table.xlsx")


# ================================================================
# Combine local data with WHO reeference table
# ================================================================

# === Grid setup ===
monthly_lines <- data.frame(x = 1:60)
yearly_lines <- data.frame(x = seq(12, 60, 12))

# === Combine WHO + Local data ===
combined_plot_df <- bind_rows(
  who_bw_long %>% mutate(Source = "WHO"),
  local_bw_long %>% mutate(Source = "Local")
)

combined_label_df <- bind_rows(
  who_labels %>% mutate(Source = "WHO"),
  local_labels %>% mutate(Source = "Local")
)

# === Month labels: every 2 months, drop year labels a bit
month_labels <- as.character(1:60)
month_labels[1:60 %% 2 != 0] <- ""  # remove odd numbers
month_labels[seq(12, 60, 12)] <- paste0("\n", 1:5, " years")  # drop years lower

# === PLOT ===
ggplot() +
  # Grid lines
  geom_vline(data = monthly_lines, aes(xintercept = x), color = "grey85", size = 0.3) +
  geom_vline(data = yearly_lines, aes(xintercept = x), color = "grey50", size = 0.8) +

  # WHO curves: dashed black
  geom_line(
    data = subset(combined_plot_df, Source == "WHO"),
    aes(x = Age, y = Value, group = Z, linetype = "WHO reference"),
    color = "black", size = 0.8
  ) +

  # Local curves: colored solid
  geom_line(
    data = subset(combined_plot_df, Source == "Local"),
    aes(x = Age, y = Value, color = Z, group = Z, linetype = "Local model"),
    size = 1.2
  ) +

  # Tip labels (Local = colored, WHO = black)
  geom_text(
    data = subset(combined_label_df, Source == "Local"),
    aes(x = Age, y = Value, label = Z, color = Z),
    hjust = -0.1, vjust = 0.5, size = 3.5, show.legend = FALSE
  ) +
  geom_text(
    data = subset(combined_label_df, Source == "WHO"),
    aes(x = Age, y = Value, label = Z),
    hjust = -0.1, vjust = 0.5, size = 3.5,
    color = "black", show.legend = FALSE
  ) +

  # Custom X-axis
  scale_x_continuous(
    breaks = 1:60,
    labels = month_labels,
    expand = expansion(mult = c(0, 0.1))
  ) +

  # Custom Y-axis
  scale_y_continuous(
    breaks = seq(0, 30, 1),
    labels = ifelse(seq(0, 30, 1) %% 2 == 0, seq(0, 30, 1), ""),
    expand = expansion(mult = c(0, 0.05))
  ) +

  # Manual Z-score colors for local curves
  scale_color_manual(values = c(
    "Z-3" = "#DC143C",
    "Z-2" = "#1E90FF",
    "Z0"  = "#228B22",
    "Z2"  = "#FF8C00",
    "Z3"  = "#323ea8"
  )) +

  # Manual linetype legend (only two items)
  scale_linetype_manual(
    name = "Curve Type",
    values = c("Local model" = "solid", "WHO reference" = "dashed")
  ) +

  # Labels and theme
  labs(
    x = "Age (months)",
    y = "Weight (kg)",
    color = NULL  # Hides the color legend box
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.5),
    panel.grid.minor.y = element_line(size = 0.2, color = "grey90"),
    axis.text.x = element_text(size = 10, lineheight = 0.9),
    axis.text.y = element_text(size = 10),
    plot.margin = margin(20, 20, 40, 20),
    legend.title = element_blank()
  )