# ================================================================
# WHO-Style Growth Curve Modeling (Height-for-Age, Boys)
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

boys_bh_bcpe <- gamlss(
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
zscore_pred_bh <- centiles.pred(
  boys_bh_bcpe,
  xname = "age",
  xvalues = 1:60,
  type = "standard-centiles",
  dev = Z_scores,
  plot = FALSE
)

# Convert to long-form for plotting
zscore_df_bh <- as.data.frame(zscore_pred_bh)
colnames(zscore_df_bh) <- c("Age", paste0("Z", Z_scores))

plot_df_bh <- zscore_df_bh %>%
  pivot_longer(cols = -Age, names_to = "Z", values_to = "Value") %>%
  mutate(Label = Z)

label_df_bh <- plot_df_bh %>%
  filter(Age == max(Age))  # Tip labels at Age = 60

# ================================================================
# Grid Setup (1 line per month, thicker every 12 months)
# ================================================================

monthly_lines <- data.frame(x = 1:60)
yearly_lines <- data.frame(x = seq(12, 60, 12))

# ================================================================
# WHO-style Plot
# ================================================================

ggplot(plot_df_bh, aes(x = Age, y = Value, color = Z, linetype = Z)) +
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
    y = "Height (cm)",
    title = "Weight-for-Age Z-Score Reference Curves (BCPE Model)"
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

write_xlsx(zscore_df, "outputs/height_zscore_table.xlsx")