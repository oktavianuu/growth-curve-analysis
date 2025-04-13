library(ggplot2)
library(dplyr)
library(readxl)

# === Load and prepare WHO BMI reference data ===
who_bmi_raw <- read_excel("data/who/who_bmi.xlsx")

who_bmi <- who_bmi_raw %>%
  select(Month, SD3neg, SD2neg, SD0, SD2, SD3) %>%
  rename(
    Age = Month,
    `Z-3` = SD3neg,
    `Z-2` = SD2neg,
    `Z0`  = SD0,
    `Z2`  = SD2,
    `Z3`  = SD3
  )

who_bmi_long <- who_bmi %>%
  pivot_longer(cols = -Age, names_to = "Z", values_to = "Value") %>%
  mutate(Source = "WHO_bmi")

# === Prepare local BMI data ===
local_bmi_long <- df_bmi_boys %>%
  pivot_longer(cols = -Age, names_to = "Z", values_to = "Value") %>%
  mutate(Source = "Local_bmi")

# === Create label positions at last age (tip of each line) ===
who_labels_bmi <- who_bmi_long %>% filter(Age == max(Age))
local_labels_bmi <- local_bmi_long %>% filter(Age == max(Age))
combined_label_bmi <- bind_rows(who_labels_bmi, local_labels_bmi)

# === Combine WHO + Local data ===
combined_plot_bmi <- bind_rows(who_bmi_long, local_bmi_long)

# === Grid setup ===
monthly_lines <- data.frame(x = 1:60)
yearly_lines <- data.frame(x = seq(12, 60, 12))

# === Month labels: every 2 months, drop year labels a bit
month_labels <- as.character(1:60)
month_labels[1:60 %% 2 != 0] <- ""  # blank out odd numbers
month_labels[seq(12, 60, 12)] <- paste0("\n", 1:5, " years")

# === Manual color map
z_colors <- c(
  "Z-3" = "#DC143C",
  "Z-2" = "#1E90FF",
  "Z0"  = "#228B22",
  "Z2"  = "#FF8C00",
  "Z3"  = "#323ea8"
)

# === FINAL BMI PLOT ===
ggplot() +
  # Grid lines
  geom_vline(data = monthly_lines, aes(xintercept = x), color = "grey85", size = 0.3) +
  geom_vline(data = yearly_lines, aes(xintercept = x), color = "grey50", size = 0.8) +

  # WHO dashed black curves
  geom_line(
    data = subset(combined_plot_bmi, Source == "WHO_bmi"),
    aes(x = Age, y = Value, group = Z, linetype = "WHO reference"),
    color = "black", size = 0.8
  ) +

  # Local colored solid curves
  geom_line(
    data = subset(combined_plot_bmi, Source == "Local_bmi"),
    aes(x = Age, y = Value, group = Z, linetype = "Local model"),
    color = NA,
    size = 1.2
  ) +
  geom_line(
    data = subset(combined_plot_bmi, Source == "Local_bmi"),
    aes(x = Age, y = Value, group = Z),
    color = z_colors[subset(combined_plot_bmi, Source == "Local_bmi")$Z],
    size = 1.2,
    inherit.aes = FALSE
  ) +

  # Tip labels
  geom_text(
    data = subset(combined_label_bmi, Source == "Local_bmi"),
    aes(x = Age, y = Value, label = Z),
    color = z_colors[subset(combined_label_bmi, Source == "Local_bmi")$Z],
    hjust = -0.1, vjust = 0.5, size = 3.5, show.legend = FALSE
  ) +
  geom_text(
    data = subset(combined_label_bmi, Source == "WHO_bmi"),
    aes(x = Age, y = Value, label = Z),
    color = "black", hjust = -0.1, vjust = 0.5, size = 3.5, show.legend = FALSE
  ) +

  # X-axis (1–60 months, label every 2 months, highlight year)
  scale_x_continuous(
    breaks = 1:60,
    labels = month_labels,
    expand = expansion(mult = c(0, 0.1))
  ) +

  # Y-axis (based on actual BMI range: 7 to 27)
  scale_y_continuous(
    breaks = seq(7, 27, 1),
    labels = ifelse(seq(7, 27, 1) %% 2 == 0, seq(7, 27, 1), ""),
    expand = expansion(mult = c(0, 0.05))
  ) +

  # Legend (only 2 lines: solid = local, dashed = WHO)
  scale_linetype_manual(
    name = NULL,
    values = c("Local model" = "solid", "WHO reference" = "dashed")
  ) +

  # Final polish
  labs(
    x = "Age (months)",
    y = "BMI"
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