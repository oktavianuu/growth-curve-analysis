who_bh_raw <- read_excel("data/who/who_height.xlsx")

# select z-scores only
who_bh <- who_bh %>%
  select(Month, SD3neg, SD2neg, SD0, SD2, SD3) %>%
  rename(
    Age = Month,
    `Z-3` = SD3neg,
    `Z-2` = SD2neg,
    `Z0`  = SD0,
    `Z2`  = SD2,
    `Z3`  = SD3
  )

# Reshape into long format
who_bh_long <- who_bh %>%
  pivot_longer(cols = -Age, names_to = "Z", values_to = "Value") %>%
  mutate(Source = "WHO_height")

# Reshape local data into long format
local_bh_long <- df_bh_boys %>%
  pivot_longer(cols = -Age, names_to = "Z", values_to = "Value") %>%
  mutate(Source = "Local_height")

who_labels_bh <- who_bh_long %>% 
  filter(Age == max(Age))

local_labels_bh <- local_bh_long %>% 
  filter(Age == max(Age))

combined_label_bh <- bind_rows(
  who_labels_bh %>% mutate(Source = "WHO_height"),
  local_labels_bh %>% mutate(Source = "Local_height")
)

# combine both data
combined_bh <- bind_rows(who_bh_long, local_bh_long)


# ================================================================
# Combine local data with WHO reeference table
# ================================================================

# === Grid setup ===
monthly_lines <- data.frame(x = 1:60)
yearly_lines <- data.frame(x = seq(12, 60, 12))

# === Combine WHO + Local data ===
combined_plot_bh <- bind_rows(
  who_bh_long %>% mutate(Source = "WHO_height"),
  local_bh_long %>% mutate(Source = "Local_height")
)

combined_label_bh <- bind_rows(
  who_labels_bh %>% mutate(Source = "WHO_height"),
  local_labels_bh %>% mutate(Source = "Local_height")
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
    data = subset(combined_plot_bh, Source == "WHO_height"),
    aes(x = Age, y = Value, group = Z, linetype = "WHO reference"),
    color = "black", size = 0.8
  ) +

  # Local curves: colored solid
  geom_line(
    data = subset(combined_plot_bh, Source == "Local_height"),
    aes(x = Age, y = Value, color = Z, group = Z, linetype = "Local model"),
    size = 1.2
  ) +

  # Tip labels (Local = colored, WHO = black)
  geom_text(
    data = subset(combined_label_bh, Source == "Local_height"),
    aes(x = Age, y = Value, label = Z, color = Z),
    hjust = -0.1, vjust = 0.5, size = 3.5, show.legend = FALSE
  ) +
  geom_text(
    data = subset(combined_label_bh, Source == "WHO_height"),
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
  breaks = seq(40, 130, 1),
  labels = ifelse(seq(40, 130, 1) %% 2 == 0, seq(40, 130, 1), ""),
  expand = expansion(mult = c(0, 0.05))
)+

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
    y = "Height (cm)",
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
