# Load required data and libraries
source("Analysis/prepare_data.R")
library(ggplot2)
library(dplyr)
library(patchwork)

# 1. Predictions Data
base_preds <- bind_rows(
  # Baseline
  tibble(treatment = "Baseline", cost_param = "HH", predicted_price = 2.6),
  tibble(treatment = "Baseline", cost_param = "HL", predicted_price = 3.6),
  tibble(treatment = "Baseline", cost_param = "LH", predicted_price = 3.2),
  tibble(treatment = "Baseline", cost_param = "LL", predicted_price = 4.4),
  # Pure
  tibble(treatment = "Pure", cost_param = "HH", predicted_price = 12.1),
  tibble(treatment = "Pure", cost_param = "HL", predicted_price = 13.0),
  tibble(treatment = "Pure", cost_param = "LH", predicted_price = 8.1),
  tibble(treatment = "Pure", cost_param = "LL", predicted_price = 10.2),
  # Menu
  tibble(treatment = "Menu", cost_param = "HH", predicted_price = 3.3),
  tibble(treatment = "Menu", cost_param = "HL", predicted_price = 4.3),
  tibble(treatment = "Menu", cost_param = "LH", predicted_price = 4.9),
  tibble(treatment = "Menu", cost_param = "LL", predicted_price = 6.6),
  # Sub-menu
  tibble(treatment = "Sub-menu", cost_param = "HH", predicted_price = 4.1),
  tibble(treatment = "Sub-menu", cost_param = "HL", predicted_price = 4.4),
  tibble(treatment = "Sub-menu", cost_param = "LH", predicted_price = 5.8),
  tibble(treatment = "Sub-menu", cost_param = "LL", predicted_price = 6.1)
)

predictions_lines <- base_preds |>
  mutate(
    treatment = factor(
      treatment,
      levels = c("Baseline", "Pure", "Menu", "Sub-menu"),
      labels = c(
        "S: Stacking", "P: Pure bundling",
        "M: Menu (optimal)", "U: Menu (suboptimal)"
      )
    ),
    cost_param = factor(cost_param, levels = c("HH", "HL", "LH", "LL")),
    cp_num = as.numeric(cost_param),
    x_start = (cp_num - 1) * 3 + 0.5,
    x_end = (cp_num - 1) * 3 + 3.5
  )

# 2. Experimental Data
plot_data <- trades_only |>
  mutate(
    treatment = factor(
      treatment,
      levels = c("Baseline", "Pure", "Menu", "Sub-menu"),
      labels = c(
        "S: Stacking", "P: Pure bundling",
        "M: Menu (optimal)", "U: Menu (suboptimal)"
      )
    )
  ) |>
  group_by(treatment) |>
  mutate(treatment_group_idx = dense_rank(interaction(session_code, group_id))) |>
  group_by(treatment, treatment_group_idx, cost_param) |>
  mutate(repetition = dense_rank(round_number)) |>
  group_by(treatment, treatment_group_idx, cost_param, repetition, product_type) |>
  summarise(avg_price = median(price, na.rm = TRUE), .groups = "drop") |>
  mutate(
    cost_param = factor(cost_param, levels = c("HH", "HL", "LH", "LL")),
    cp_num = as.numeric(cost_param),
    x_pos = (cp_num - 1) * 3 + repetition,
    line_group = interaction(treatment_group_idx, cost_param, product_type)
  )

# 3. Aesthetics & Shapes
product_shapes <- c(
  "Product A" = 21,
  "Product B" = 24,
  "Package"   = 23,
  "Package 1" = 22,
  "Package 2" = 25
)

product_colors <- c(
  "Product A" = "#e60000",
  "Product B" = "#0066cc",
  "Package"   = "#00b300",
  "Package 1" = "#7300e6",
  "Package 2" = "#ff6600"
)

# Shrunk plot point sizes to keep multi-series lines clear
product_sizes <- c(
  "Product A" = 0.85,
  "Product B" = 0.85,
  "Package"   = 1.0,
  "Package 1" = 0.85,
  "Package 2" = 0.85
)

group_colors <- c(
  "1" = "#e60000",
  "2" = "#0066cc",
  "3" = "#00b300",
  "4" = "#7300e6",
  "5" = "#ff6600"
)

# 4. Main Plot Construction
equilibrium_plot <- ggplot() +
  geom_vline(xintercept = c(3.5, 6.5, 9.5), color = "grey85", linetype = "dotted") +
  geom_segment(
    data = predictions_lines,
    aes(x = x_start, xend = x_end, y = predicted_price, yend = predicted_price),
    # linetype = "dashed",
    color = "black",
    linewidth = 0.5,
    alpha = 0.8
  ) +
  geom_line(
    data = plot_data,
    aes(x = x_pos, y = avg_price, color = as.factor(treatment_group_idx), group = line_group),
    alpha = 0.4,
    linewidth = 0.5
  ) +
  geom_point(
    data = plot_data,
    aes(
      x = x_pos, y = avg_price,
      color = as.factor(treatment_group_idx),
      fill = as.factor(treatment_group_idx),
      shape = product_type,
      size = product_type
    ),
    stroke = 0.3,
    alpha = 0.8
  ) +
  facet_wrap(~treatment, ncol = 2) +
  scale_x_continuous(
    breaks = c(2, 5, 8, 11),
    labels = c("HH", "HL", "LH", "LL")
  ) +
  scale_colour_manual(values = group_colors, guide = "none") +
  scale_fill_manual(values = group_colors, guide = "none") +
  scale_shape_manual(values = product_shapes, drop = FALSE, name = "Product type ") +
  scale_size_manual(values = product_sizes, guide = "none") +
  labs(
    x = "Firm heterogeneity levels",
    y = "Median price"
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 9), # Axis text set to 9pt
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),

    # Legend at bottom
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 10),
    legend.margin = margin(t = 0, b = -2),
    legend.key.size = unit(0.4, "cm")
  ) +

  # Override shape legend aesthetics so key boxes are filled and outlined clearly
  guides(
    shape = guide_legend(
      nrow = 1,
      override.aes = list(
        shape  = product_shapes,
        fill   = "black",
        color  = "black",
        stroke = 0,
        size   = 2
      )
    )
  ) +
  ylim(0, 16)

# 5. Centered & Evenly Spaced Custom Group Legend
legend_df <- data.frame(
  group = 1:5
)

# Position centers evenly at 13, 32, 51, 70, 89 across a 0-100 x-scale
legend_df$x_pos <- legend_df$group * 18 + 5

group_labels_df <- data.frame(
  group = 1:5,
  x = (1:5 * 18) + 5 - 2.5 # Position text cleanly to the left of each box
)

custom_legend <- ggplot(legend_df, aes(x = x_pos, y = 1, color = as.factor(group))) +
  geom_point(size = 3.2, shape = 15) +
  geom_text(
    data = group_labels_df,
    aes(x = x, y = 1, label = paste("Group", group)),
    inherit.aes = FALSE,
    hjust = 1,
    vjust = 0.5,
    fontface = "bold",
    size = 3.5,
    color = "black"
  ) +
  scale_color_manual(values = group_colors, guide = "none") +
  scale_x_continuous(limits = c(0, 100)) +
  theme_void() +
  theme(
    plot.margin = margin(t = 2, b = 2, l = 0, r = 0)
  )

# 6. Combine and Export via patchwork
final_plot <- wrap_elements(equilibrium_plot) / custom_legend +
  plot_layout(heights = c(12, 0.5))

ggsave(
  filename = "Charts/Equilibrium prices.pdf",
  plot = final_plot,
  width = 15,
  height = 15,
  units = "cm",
  device = cairo_pdf
)

print(final_plot)
