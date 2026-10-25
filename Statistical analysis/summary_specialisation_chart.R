# ==============================================================================
# SELLER SPECIALISATION SUMMARY CHART (AVERAGED ACROSS COST PARAMETERS)
# ==============================================================================
# Ensure underlying data objects are available
if (!exists("trades_with_seller_type")) {
    source("graphing_vars.R")
}

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# ------------------------------------------------------------------------------
# 1. COMPUTE AVERAGE TRADE SHARE ACROSS ALL COST PARAMETERS
# ------------------------------------------------------------------------------
# Calculate share per cost parameter first, then average across cost parameters
seller_spec_avg <- trades_with_seller_type |>
    filter(treatment != "Pure") |>
    group_by(treatment, cost_param, seller_type, product_type) |>
    summarise(
        quant_raw = n(),
        .groups = "drop_last"
    ) |>
    mutate(
        total_quant = sum(quant_raw),
        share = quant_raw / total_quant
    ) |>
    group_by(treatment, seller_type, product_type) |>
    summarise(
        share = mean(share, na.rm = TRUE),
        .groups = "drop"
    ) |>
    mutate(
        treatment = factor(
            treatment,
            levels = c("Baseline", "Menu", "Sub-menu"),
            labels = c("Stacking", "Optimal menu", "Suboptimal menu")
        ),
        product_type = factor(
            product_type,
            levels = c("Product A", "Product B", "Package 1", "Package 2")
        )
    )

# Calculate segment label positioning (centered within stacked bar sections)
seller_spec_avg <- seller_spec_avg |>
    mutate(
        label_text = ifelse(share >= 0.05, sprintf("%.0f%%", share * 100), "")
    )

# ------------------------------------------------------------------------------
# 2. THEME & COLOR DEFINITIONS
# ------------------------------------------------------------------------------
seller_fill_colors <- c(
    "Product A" = "#e60000",
    "Product B" = "#0066cc",
    "Package 1" = "#7300e6",
    "Package 2" = "#ff6600"
)

summary_seller_theme <- theme_bw(base_size = 12) +
    theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.title = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(size = 11),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12),
        # legend.key.size = unit(0.35, "cm"),
        # legend.spacing.x = unit(0.15, "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
    )

# ------------------------------------------------------------------------------
# 3. BUILD & EXPORT SUMMARY PLOT
# ------------------------------------------------------------------------------
seller_summary_plot <- ggplot(seller_spec_avg, aes(x = seller_type, y = share, fill = product_type)) +
    geom_col(position = "fill", width = 0.6, color = "black", linewidth = 0.3) +
    # geom_text(
    #     aes(label = label_text),
    #     position = position_stack(vjust = 0.5),
    #     color = "white",
    #     fontface = "bold",
    #     size = 3
    # ) +
    facet_wrap(~treatment, ncol = 3) +
    scale_y_continuous(labels = percent_format(), name = "Share of total trades") +
    scale_fill_manual(values = seller_fill_colors, name = "Product type") +
    labs(
        title = "Seller specialisation",
        x = "Seller type"
    ) +
    summary_seller_theme

# Export high-quality image/PDF
ggsave(
    filename = "Charts/Seller specialisation summary.png",
    plot = seller_summary_plot,
    width = 18,
    height = 10,
    units = "cm",
    dpi = 300
)
