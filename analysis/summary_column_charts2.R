# ==============================================================================
# SUMMARY STATISTICS COLUMN CHARTS (PRODUCT-LEVEL BREAKDOWN)
# ==============================================================================
# Sourcing graphing_vars.R ensures all underlying data and packages are loaded
if (!exists("group_prod") || !exists("group_table_data")) {
    source("graphing_vars.R")
}

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# ------------------------------------------------------------------------------
# 1. PREPARE PRODUCT-LEVEL & MARKET-LEVEL SUMMARY STATISTICS
# ------------------------------------------------------------------------------
# A. Average across independent sessions first, then calculate treatment product means
prod_summary <- group_prod |>
    group_by(treatment, config_session_number, product_type) |>
    summarise(
        price_adj = mean(price_adj, na.rm = TRUE),
        quant_adj = mean(quant_adj, na.rm = TRUE),
        .groups = "drop"
    ) |>
    group_by(treatment, product_type) |>
    summarise(
        price_adj = mean(price_adj, na.rm = TRUE),
        quant_adj = mean(quant_adj, na.rm = TRUE),
        .groups = "drop"
    ) |>
    mutate(
        treatment_label = factor(
            treatment,
            levels = c("Baseline", "Pure", "Menu", "Sub-menu"),
            labels = c("Stacking", "Pure\nbundling", "Optimal\nmenu", "Suboptimal\nmenu")
        ),
        product_type = factor(
            product_type,
            levels = c("Product A", "Product B", "Package", "Package 1", "Package 2")
        )
    )

# B. Market-level profit data for Chart 2
mkt_summary <- group_table_data |>
    group_by(treatment) |>
    summarise(
        buyer_profit = mean(Buyer, na.rm = TRUE),
        seller_profit = mean(Seller, na.rm = TRUE),
        total_profit = mean(Total, na.rm = TRUE),
        .groups = "drop"
    ) |>
    mutate(
        treatment_label = factor(
            treatment,
            levels = c("Baseline", "Pure", "Menu", "Sub-menu"),
            labels = c("Stacking", "Pure\nbundling", "Optimal\nmenu", "Suboptimal\nmenu")
        )
    )

# ------------------------------------------------------------------------------
# 2. SHARED THEME & COLOR DEFINITIONS
# ------------------------------------------------------------------------------
column_chart_theme <- theme_bw(base_size = 12) +
    theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 11),
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
    )

# Exact color mapping matching final_plot & seller_fill_colors
product_colors <- c(
    "Product A" = "#e60000", # Red
    "Product B" = "#0066cc", # Blue
    "Package"   = "#00b300", # Green
    "Package 1" = "#7300e6", # Purple
    "Package 2" = "#ff6600" # Orange
)

profit_role_fills <- c(
    "Buyer"  = "black",
    "Seller" = "white"
)

# ------------------------------------------------------------------------------
# 3. CHART 1: PRODUCT-LEVEL PRICES & QUANTITIES (SIDE-BY-SIDE PANELS)
# ------------------------------------------------------------------------------
# Dodging rule with preserve = "single" keeps column width consistent across single & double product markets
dodge_setting <- position_dodge2(width = 0.7, preserve = "single")

# Panel A: Price (adj.)
p_price_prod <- ggplot(prod_summary, aes(x = treatment_label, y = price_adj, fill = product_type)) +
    geom_col(position = dodge_setting, color = "black", linewidth = 0.3) +
    #   geom_text(
    #     aes(label = sprintf("%.2f", price_adj)),
    #     position = dodge_setting,
    #     vjust = -0.5,
    #     size = 2.7,
    #     fontface = "bold"
    #   ) +
    scale_fill_manual(values = product_colors, name = "Product type") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(title = "Trade prices (adj.)", x = "Treatment", y = "Median price") +
    column_chart_theme

# Panel B: Quantity (adj.)
p_quant_prod <- ggplot(prod_summary, aes(x = treatment_label, y = quant_adj, fill = product_type)) +
    geom_col(position = dodge_setting, color = "black", linewidth = 0.3) +
    #   geom_text(
    #     aes(label = sprintf("%.2f", quant_adj)),
    #     position = dodge_setting,
    #     vjust = -0.5,
    #     size = 2.7,
    #     fontface = "bold"
    #   ) +
    scale_fill_manual(values = product_colors, name = "Product type") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(title = "Trade volumes (adj.)", x = "Treatment", y = "Quantity traded") +
    column_chart_theme

# Combine panels with a unified collected legend
chart_price_quant_prod <- (p_price_prod | p_quant_prod) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

# Export Chart 1
ggsave(
    filename = "Charts/Product prices and quantities (avg).png",
    plot = chart_price_quant_prod,
    width = 20,
    height = 11,
    units = "cm",
    dpi = 300
)

# ------------------------------------------------------------------------------
# 4. CHART 2: STACKED MARKET PROFITS (BUYER + SELLER PROFITS)
# ------------------------------------------------------------------------------
profit_stacked_data <- mkt_summary |>
    select(treatment_label, buyer_profit, seller_profit, total_profit) |>
    pivot_longer(
        cols = c(buyer_profit, seller_profit),
        names_to = "Role",
        values_to = "Profit"
    ) |>
    mutate(
        Role = factor(
            case_when(
                Role == "buyer_profit" ~ "Buyer",
                Role == "seller_profit" ~ "Seller"
            ),
            levels = c("Seller", "Buyer")
        )
    )

chart_profits <- ggplot(profit_stacked_data, aes(x = treatment_label, y = Profit, fill = Role)) +
    geom_col(position = "stack", width = 0.55, color = "black", linewidth = 0.35) +
    #   geom_text(
    #     aes(label = sprintf("%.2f", Profit)),
    #     position = position_stack(vjust = 0.5),
    #     color = ifelse(profit_stacked_data$Role == "Buyer", "white", "black"),
    #     size = 2.8,
    #     fontface = "bold"
    #   ) +
    #   geom_text(
    #     data = mkt_summary,
    #     aes(x = treatment_label, y = total_profit, label = sprintf("Total: %.2f", total_profit), fill = NULL),
    #     vjust = -0.6,
    #     size = 3,
    #     fontface = "bold"
    #   ) +
    scale_fill_manual(values = profit_role_fills, name = "Role") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
        title = "Market profits",
        x = "Treatment",
        y = "Total profit"
    ) +
    column_chart_theme

# Export Chart 2
ggsave(
    filename = "Charts/Market profits (stacked columns).png",
    plot = chart_profits,
    width = 12,
    height = 10,
    units = "cm",
    dpi = 300
)
