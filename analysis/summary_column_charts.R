# ==============================================================================
# SUMMARY STATISTICS COLUMN CHARTS
# ==============================================================================
# Sourcing graphing_vars.R ensures all underlying data and packages are loaded
if (!exists("group_table_data")) {
    source("graphing_vars.R")
}

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# ------------------------------------------------------------------------------
# 1. PREPARE SUMMARY STATISTICS FROM DATA
# ------------------------------------------------------------------------------
# Compute treatment means matching final_table
mkt_summary <- group_table_data |>
    group_by(treatment) |>
    summarise(
        price_adj = mean(group_adj_price, na.rm = TRUE),
        quant_adj = mean(group_adj_quantity, na.rm = TRUE),
        buyer_profit = mean(Buyer, na.rm = TRUE),
        seller_profit = mean(Seller, na.rm = TRUE),
        total_profit = mean(Total, na.rm = TRUE),
        .groups = "drop"
    ) |>
    mutate(
        # Treatment abbreviations matching final_plot
        treatment_label = factor(
            treatment,
            levels = c("Baseline", "Pure", "Menu", "Sub-menu"),
            labels = c("S", "P", "M", "U")
        )
    )

# ------------------------------------------------------------------------------
# 2. SHARED THEME & COLOR DEFINITIONS
# ------------------------------------------------------------------------------
# Match theme_bw styling and typography from graphing_vars.R
column_chart_theme <- theme_bw(base_size = 10) +
    theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 10),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 9),
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
    )

# Treatment color palette aligned with product/treatment colors in final_plot
treatment_colors <- c(
    "S" = "#0066cc", # Blue (Baseline)
    "P" = "#00b300", # Green (Pure)
    "M" = "#7300e6", # Purple (Menu)
    "U" = "#ff6600" # Orange (Sub-menu)
)

# Role fills matching market profits in final_plot (Buyer = Black, Seller = White)
profit_role_fills <- c(
    "Buyer" = "black",
    "Seller" = "white"
)

# ------------------------------------------------------------------------------
# 3. CHART 1: ADJUSTED PRICES AND QUANTITIES (SEPARATE PANELS)
# ------------------------------------------------------------------------------
# Panel A: Price (adj.)
p_price_col <- ggplot(mkt_summary, aes(x = treatment_label, y = price_adj, fill = treatment_label)) +
    geom_col(width = 0.55, color = "black", linewidth = 0.3, show.legend = FALSE) +
    geom_text(
        aes(label = sprintf("%.2f", price_adj)),
        vjust = -0.5,
        size = 3,
        fontface = "bold"
    ) +
    scale_fill_manual(values = treatment_colors) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = "Trade prices (adj.)", x = "Treatment", y = "Median price") +
    column_chart_theme

# Panel B: Quantity (adj.)
p_quant_col <- ggplot(mkt_summary, aes(x = treatment_label, y = quant_adj, fill = treatment_label)) +
    geom_col(width = 0.55, color = "black", linewidth = 0.3, show.legend = FALSE) +
    geom_text(
        aes(label = sprintf("%.2f", quant_adj)),
        vjust = -0.5,
        size = 3,
        fontface = "bold"
    ) +
    scale_fill_manual(values = treatment_colors) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = "Trade volumes (adj.)", x = "Treatment", y = "Quantity traded") +
    column_chart_theme

# Combine into side-by-side panels
chart_price_quant <- p_price_col | p_quant_col

# Export Chart 1
ggsave(
    filename = "Charts/Price_and_Quantity_Columns.pdf",
    plot = chart_price_quant,
    width = 16,
    height = 10,
    units = "cm",
    dpi = 300
)

# ------------------------------------------------------------------------------
# 4. CHART 2: STACKED MARKET PROFITS (BUYER + SELLER PROFITS)
# ------------------------------------------------------------------------------
# Reshape buyer and seller profits into long format for stacking
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
            levels = c("Seller", "Buyer") # Stack Buyer on bottom, Seller on top
        )
    )

chart_profits <- ggplot(profit_stacked_data, aes(x = treatment_label, y = Profit, fill = Role)) +
    geom_col(position = "stack", width = 0.55, color = "black", linewidth = 0.35) +
    # In-bar segment labels
    geom_text(
        aes(label = sprintf("%.2f", Profit)),
        position = position_stack(vjust = 0.5),
        color = ifelse(profit_stacked_data$Role == "Buyer", "white", "black"),
        size = 2.8,
        fontface = "bold"
    ) +
    # Total profit label placed on top of stacked bar
    geom_text(
        data = mkt_summary,
        aes(x = treatment_label, y = total_profit, label = sprintf("Total: %.2f", total_profit), fill = NULL),
        vjust = -0.6,
        size = 3,
        fontface = "bold"
    ) +
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
    filename = "Charts/Market_Profits_Stacked_Columns.pdf",
    plot = chart_profits,
    width = 12,
    height = 10,
    units = "cm",
    dpi = 300
)
