source("Analysis/prepare_data.R")

# Load graphing libraries
library(ggplot2)
library(patchwork)
library(dplyr)
library(cowplot)
library(showtext)

# Define visual parameters
product_shapes <- c(
    "Product A" = 21,
    "Product B" = 24,
    "Package"   = 23,
    "Package 1" = 22,
    "Package 2" = 25
)

# SHRUNK point sizes to clear panel clutter
product_sizes <- c(
    "Product A" = 0.4,
    "Product B" = 0.4,
    "Package"   = 0.5,
    "Package 1" = 0.4,
    "Package 2" = 0.4
)

group_shades <- c(
    # Group 1: Reds
    "1.1" = "#ff9999", "1.2" = "#e60000", "1.3" = "#800000",
    # Group 2: Blues
    "2.1" = "#99ccff", "2.2" = "#0066cc", "2.3" = "#003366",
    # Group 3: Greens
    "3.1" = "#99e699", "3.2" = "#00b300", "3.3" = "#004d00",
    # Group 4: Purples
    "4.1" = "#d9b3ff", "4.2" = "#7300e6", "4.3" = "#330066",
    # Group 5: Oranges
    "5.1" = "#ffcc99", "5.2" = "#ff6600", "5.3" = "#993300"
)


# FIXED CUSTOM LEGEND FOR GROUPS

# Spread the group identifiers horizontally so they don't clip
legend_df <- data.frame(
    group = rep(1:5, each = 3),
    repetition = rep(1:3, times = 5)
)
legend_df$interaction_code <- paste(legend_df$group, legend_df$repetition, sep = ".")

# Increased multiplier from 0.15 to 0.4 to space out the 3 points in each group
legend_df$x_pos <- legend_df$group * 3 + (legend_df$repetition * 0.4)

legend_df <- data.frame(
    group = rep(1:5, each = 3),
    repetition = rep(1:3, times = 5)
)
legend_df$interaction_code <- paste(legend_df$group, legend_df$repetition, sep = ".")

# Group centers placed evenly at 10, 30, 50, 70, 90 across 0-100 x-limits
# Individual square repetitions offset slightly (-1.5, 0, +1.5) around each group center
legend_df$x_pos <- (legend_df$group * 21 - 8) + ((legend_df$repetition - 2) * 2.5)

# Center labels directly above/left of each 3-square cluster
group_labels_df <- data.frame(
    group = 1:5,
    x = (1:5 * 21) - 8 - 5 # Positioned cleanly to the left of each cluster
)

custom_legend <- ggplot(legend_df, aes(x = x_pos, y = 1, color = interaction_code)) +
    geom_point(size = 3.2, shape = 15) +
    geom_text(
        data = group_labels_df,
        aes(x = x, y = 1, label = paste("Group", group)),
        inherit.aes = FALSE,
        hjust = 1,
        vjust = 0.5,
        fontface = "bold",
        size = 3.5
    ) +
    scale_color_manual(values = group_shades, guide = "none") +
    # Fixed 0-100 scale guarantees 100% margin padding on left and right edges
    scale_x_continuous(limits = c(0, 100)) +
    theme_void() +
    theme(
        plot.margin = margin(t = 2, b = 2, l = 0, r = 0)
    )


# Prepare trade data and factors
trades_only <- trades_only |>
    mutate(
        treatment = factor(
            treatment,
            levels = c("Baseline", "Pure", "Menu", "Sub-menu"),
            labels = c("S", "P", "M", "U")
        ),
        cost_param = factor(cost_param, levels = c("HH", "HL", "LH", "LL"))
    ) |>
    group_by(treatment) |>
    mutate(treatment_group_idx = dense_rank(interaction(session_code, group_id))) |>
    group_by(treatment, treatment_group_idx, cost_param) |>
    mutate(repetition = dense_rank(round_number)) |>
    ungroup()


# MAIN GRID PLOT

dynamics_grid_plot <- ggplot(trades_only, aes(x = elapsed_time_seconds, y = price)) +
    geom_path(
        data = trades_only |> arrange(elapsed_time_seconds),
        aes(
            group = interaction(session_code, group_id, round_number, product_type),
            color = as.factor(interaction(treatment_group_idx, repetition))
        ),
        linewidth = 0.2,
        alpha = 0.35,
        show.legend = FALSE
    ) +
    geom_point(
        aes(
            shape = product_type,
            size  = product_type,
            color = as.factor(interaction(treatment_group_idx, repetition)),
            fill  = as.factor(interaction(treatment_group_idx, repetition))
        ),
        stroke = 0,
        alpha = 0.8
    ) +
    facet_grid(cost_param ~ treatment) +
    coord_cartesian(ylim = c(0, 16)) +

    # Ensure the shape scale shows up as the Product Type legend
    scale_shape_manual(
        values = product_shapes,
        drop = FALSE,
        name = "Product type "
    ) +
    scale_color_manual(values = group_shades, guide = "none") +
    scale_fill_manual(values = group_shades, guide = "none") +
    scale_size_manual(values = product_sizes, guide = "none") +
    labs(
        x = "Time (s)",
        y = "Price"
    ) +
    theme_bw(base_size = 10) +
    theme(
        plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 10),
        axis.text = element_text(size = 9),
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(face = "bold", size = 10),
        panel.grid.minor = element_blank(),
        # panel.grid.major = element_line(linewidth = 0.4, color = "grey92"),

        # RE-ENABLE PRODUCT TYPE LEGEND AT BOTTOM OF GRID
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 10),
        legend.margin = margin(t = 0, b = -5),
        # Override size in legend so points are visible, despite being tiny in the plot
        legend.key.size = unit(0.4, "cm")
    ) +
    guides(
        shape = guide_legend(
            nrow = 1,
            override.aes = list(size = 2, color = "black", fill = "black", shape = product_shapes) # Force legend points to be black and visible
        )
    )

# Combine main grid plot with the custom group legend at the very bottom
# Heights updated so legends don't take up excessive vertical space
final_plot <- wrap_elements(dynamics_grid_plot) / custom_legend +
    plot_layout(heights = c(20, 1))

# Export PDF
ggsave(
    filename = "Charts/Trading dynamics.pdf",
    plot = final_plot,
    width = 15,
    height = 20, # Reduced slightly from 22 for better portrait proportion
    unit = "cm",
    device = cairo_pdf # Ensures vector shapes export cleanly
)

print(final_plot)
