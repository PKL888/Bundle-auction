# ==============================================================================
# UNIFIED MASTER SCRIPT: TABLE, PLOTS, AND BALANCE TESTS
# ==============================================================================
source("Analysis/prepare_data.R")

library(ggplot2)
library(patchwork)
library(tidyr)
library(dplyr)
library(cowplot)
library(stargazer)

# ------------------------------------------------------------------------------
# 1. STAGE 1 & 2: TRADE-LEVEL ADJUSTMENTS & ROUND-LEVEL AGGREGATION
# ------------------------------------------------------------------------------
trades_adj <- trades_only |>
  select(-c(order_id, is_bid, player_id)) |>
  mutate(
    is_package = grepl("Package", product_type),
    attribute_multiplier = case_when(
      !is_package ~ 1,
      treatment %in% c("P", "Pure") ~ 2,
      treatment %in% c("U", "Sub-menu") & cost_param == "HH" ~ 1 + 1 / 16,
      treatment %in% c("U", "Sub-menu") & cost_param == "HL" ~ 1 + 1 / 20,
      treatment %in% c("U", "Sub-menu") & cost_param == "LH" ~ 1 + 1 / 8,
      treatment %in% c("U", "Sub-menu") & cost_param == "LL" ~ 1 + 1 / 8,
      treatment %in% c("M", "Menu") & cost_param == "HH" ~ 1 + 1 / 4,
      treatment %in% c("M", "Menu") & cost_param == "HL" ~ 1 + 1 / 5,
      treatment %in% c("M", "Menu") & cost_param == "LH" ~ 1 + 1 / 2,
      treatment %in% c("M", "Menu") & cost_param == "LL" ~ 1 + 1 / 2,
      TRUE ~ 1
    ),
    attribute_price = price / attribute_multiplier
  )

# A. Product-Level Round Summaries (For Plots & Balance Tests)
round_prod <- trades_adj |>
  group_by(cost_param, treatment, config_session_number, round_number, product_type) |>
  summarise(
    quant_raw = n(),
    quant_adj = sum(attribute_multiplier),
    price_raw = median(price),
    price_adj = median(attribute_price),
    .groups = "drop"
  )

# B. Market-Level Round Summaries (For the Table)
round_mkt <- trades_adj |>
  group_by(config_session_number, round_number, treatment) |>
  summarise(
    total_mkt_quant_adj = sum(attribute_multiplier),
    num_products = if_else(first(treatment) %in% c("P", "Pure"), 1, 2),
    mkt_quant_adj = total_mkt_quant_adj / num_products, mkt_price_adj = median(attribute_price),
    total_trades_round = n(),
    round_time = max(elapsed_time_seconds),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 2. STAGE 3: GROUP-LEVEL AGGREGATION (THE INDEPENDENT UNIT OF ANALYSIS)
# ------------------------------------------------------------------------------
# A. Group Product Averages (For Plots & Balance Tests)
group_prod <- round_prod |>
  group_by(cost_param, treatment, config_session_number, product_type) |>
  summarise(
    quant_raw = mean(quant_raw),
    quant_adj = mean(quant_adj),
    price_raw = mean(price_raw),
    price_adj = mean(price_adj),
    .groups = "drop"
  )

# B. Group Market Averages (For the Table)
group_mkt <- round_mkt |>
  group_by(config_session_number, treatment) |>
  summarise(
    group_adj_quantity = mean(mkt_quant_adj),
    group_adj_price = mean(mkt_price_adj), group_raw_trades = mean(total_trades_round),
    group_time = mean(round_time),
    .groups = "drop"
  )

# C. Group Profit Averages (For Table and Plots)
group_profits <- market_profits_data |>
  mutate(total_market_profit = total_market_buyer_profit + total_market_seller_profit) |>
  group_by(treatment, config_session_number) |>
  summarise(
    Buyer = mean(as.numeric(total_market_buyer_profit)),
    Seller = mean(as.numeric(total_market_seller_profit)),
    Total = mean(as.numeric(total_market_profit)),
    .groups = "drop"
  )

group_profits_cp <- market_profits_data |>
  mutate(total_market_profit = as.numeric(total_market_buyer_profit) + as.numeric(total_market_seller_profit)) |>
  group_by(cost_param, treatment, config_session_number) |>
  summarise(
    Buyer = mean(as.numeric(total_market_buyer_profit), na.rm = TRUE),
    Seller = mean(as.numeric(total_market_seller_profit), na.rm = TRUE),
    Total = mean(total_market_profit, na.rm = TRUE),
    .groups = "drop"
  )

# Merge Profits into Market Data for the Table
group_table_data <- group_mkt |>
  left_join(group_profits,
    by = c("config_session_number", "treatment")
  ) |>
  mutate(treatment = factor(treatment, levels = c("Baseline", "Pure", "Menu", "Sub-menu")))

# ------------------------------------------------------------------------------
# 3. LATEX SUMMARY TABLE WITH MANN-WHITNEY TESTS (OVERALL AGGREGATE) PANELs A & B
# ------------------------------------------------------------------------------
calc_table_row <- function(df, metric_col, label, run_test = TRUE) {
  base_vec <- df |>
    filter(treatment == "Baseline") |>
    pull(.data[[metric_col]])

  summary_df <- df |>
    group_by(treatment) |>
    summarise(
      m_val = mean(.data[[metric_col]], na.rm = TRUE),
      s_val = sd(.data[[metric_col]], na.rm = TRUE),
      p_val = if (first(treatment) == "Baseline" || !run_test) {
        NA_real_
      } else {
        wilcox.test(.data[[metric_col]], base_vec, exact = FALSE)$p.value
      },
      .groups = "drop"
    ) |>
    mutate(
      stars = case_when(
        is.na(p_val) ~ "",
        p_val < 0.01 ~ "***",
        p_val < 0.05 ~ "**",
        p_val < 0.10 ~ "*",
        TRUE ~ ""
      ),
      mean_formatted = if_else(
        stars == "",
        sprintf("%.2f", m_val),
        sprintf("%.2f%s", m_val, stars)
      ),
      sd_formatted = sprintf("(%.2f)", s_val)
    )

  row_mean <- summary_df |>
    select(treatment, formatted = mean_formatted) |>
    pivot_wider(names_from = treatment, values_from = formatted) |>
    mutate(Metric = label)

  row_sd <- summary_df |>
    select(treatment, formatted = sd_formatted) |>
    pivot_wider(names_from = treatment, values_from = formatted) |>
    mutate(Metric = "")

  bind_rows(row_mean, row_sd) |>
    rename(S = Baseline, P = Pure, M = Menu, U = `Sub-menu`) |>
    select(Metric, S, P, M, U)
}

metrics_list <- list(
  "Quantity (num.)" = "group_raw_trades",
  "Quantity (adj.)" = "group_adj_quantity",
  "Median price (adj.)" = "group_adj_price",
  "Buyer profits" = "Buyer",
  "Seller profits" = "Seller",
  "Total profits" = "Total",
  "Trading time (s)" = "group_time"
)

# Generate table rows
table_rows <- lapply(names(metrics_list), function(name) {
  do_test <- !(name %in% c("Trading time (s)"))
  calc_table_row(group_table_data, metrics_list[[name]], name, run_test = do_test)
}) |> bind_rows()

# Append N
n_row <- group_table_data |>
  group_by(treatment) |>
  summarise(val = as.character(n()), .groups = "drop") |>
  pivot_wider(names_from = treatment, values_from = val) |>
  mutate(Metric = "Observations") |>
  rename(S = Baseline, P = Pure, M = Menu, U = `Sub-menu`) |>
  select(Metric, S, P, M, U)

final_table <- bind_rows(table_rows, n_row)

# Print LaTeX Table
stargazer(
  final_table,
  out = "Tables/summary_statistics.tex",
  type = "latex", summary = FALSE, rownames = FALSE,
  float = FALSE
)
tex_file_path <- "Tables/summary_statistics.tex"
tex_lines <- readLines(tex_file_path)
tex_lines <- gsub("\\\\textasteriskcentered \\\\textasteriskcentered \\\\textasteriskcentered", "$^{***}$", tex_lines)
tex_lines <- gsub("\\\\textasteriskcentered \\\\textasteriskcentered", "$^{**}$", tex_lines)
tex_lines <- gsub("\\\\textasteriskcentered", "$^{*}$", tex_lines)
tex_lines <- gsub("ccccc", "lcccc", tex_lines)
tex_lines <- gsub("Trading", " \\\\\\\\[-1.8ex] \\\\hline \\\\\\\\[-1.8ex] Trading", tex_lines)
writeLines(tex_lines, tex_file_path)

# ------------------------------------------------------------------------------
# PANEL A: DESCRIPTIVE STATISTICS (MEANS & STANDARD DEVIATIONS)
# ------------------------------------------------------------------------------
calc_panel_a_row <- function(df, metric_col, label) {
  df |>
    group_by(treatment) |>
    summarise(
      m_val = mean(.data[[metric_col]], na.rm = TRUE),
      s_val = sd(.data[[metric_col]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      mean_formatted = sprintf("%.2f", m_val),
      sd_formatted   = sprintf("(%.2f)", s_val)
    ) |>
    select(treatment, mean_formatted, sd_formatted) |>
    pivot_longer(cols = c(mean_formatted, sd_formatted), names_to = "stat", values_to = "val") |>
    pivot_wider(names_from = treatment, values_from = val) |>
    mutate(
      Metric = if_else(stat == "mean_formatted", label, "")
    ) |>
    rename(S = Baseline, P = Pure, M = Menu, U = `Sub-menu`) |>
    select(Metric, S, P, M, U)
}

metrics_list <- list(
  "Quantity (num.)"     = "group_raw_trades",
  "Quantity (adj.)"     = "group_adj_quantity",
  "Median price (adj.)" = "group_adj_price",
  "Buyer profits"       = "Buyer",
  "Seller profits"      = "Seller",
  "Total profits"       = "Total",
  "Trading time (s)"    = "group_time"
)

# Build Panel A rows
panel_a_rows <- lapply(names(metrics_list), function(name) {
  calc_panel_a_row(group_table_data, metrics_list[[name]], name)
}) |> bind_rows()

# Append Observation counts to Panel A
n_row_a <- group_table_data |>
  group_by(treatment) |>
  summarise(val = as.character(n()), .groups = "drop") |>
  pivot_wider(names_from = treatment, values_from = val) |>
  mutate(Metric = "Observations") |>
  rename(S = Baseline, P = Pure, M = Menu, U = `Sub-menu`) |>
  select(Metric, S, P, M, U)

final_panel_a <- bind_rows(panel_a_rows, n_row_a)

# Export Panel A Table
stargazer(
  final_panel_a,
  out = "Tables/summary_statistics_panelA.tex",
  type = "latex", summary = FALSE, rownames = FALSE, float = FALSE
)

# Format LaTeX spacing for Panel A
tex_lines_a <- readLines("Tables/summary_statistics_panelA.tex")
tex_lines_a <- gsub("ccccc", "lcccc", tex_lines_a)
tex_lines_a <- gsub("Trading", " \\\\\\\\[-1.8ex] \\\\hline \\\\\\\\[-1.8ex] Trading", tex_lines_a)
writeLines(tex_lines_a, "Tables/summary_statistics_panelA.tex")



# ------------------------------------------------------------------------------
# 4. PAIRED PRODUCT BALANCE TESTS
# ------------------------------------------------------------------------------
cat("\n--- PAIRED WILCOXON TESTS (PRODUCT BALANCE) ---\n")

get_balance_row <- function(t_name, prod1, prod2, display_label) {
  df <- group_prod |>
    filter(treatment == t_name & product_type %in% c(prod1, prod2))

  if (nrow(df) == 0) {
    return(NULL)
  }

  # Collapse across cost parameters to group level
  group_level <- df |>
    group_by(config_session_number, product_type) |>
    summarise(
      quant_adj = mean(quant_adj, na.rm = TRUE),
      price_adj = mean(price_adj, na.rm = TRUE),
      .groups = "drop"
    )

  # Pivot quantities and prices
  wide_quant <- group_level |>
    select(config_session_number, product_type, quant_adj) |>
    pivot_wider(names_from = product_type, values_from = quant_adj)

  wide_price <- group_level |>
    select(config_session_number, product_type, price_adj) |>
    pivot_wider(names_from = product_type, values_from = price_adj)

  # Ensure complete pairs
  clean_quant <- wide_quant |> filter(!is.na(.data[[prod1]]) & !is.na(.data[[prod2]]))
  clean_price <- wide_price |> filter(!is.na(.data[[prod1]]) & !is.na(.data[[prod2]]))

  # Paired Wilcoxon Tests
  q_test <- wilcox.test(clean_quant[[prod1]], clean_quant[[prod2]], paired = TRUE, exact = FALSE)
  p_test <- wilcox.test(clean_price[[prod1]], clean_price[[prod2]], paired = TRUE, exact = FALSE)

  # Calculate mean differences (Product 1 - Product 2)
  q_diff_mean <- mean(clean_quant[[prod1]] - clean_quant[[prod2]], na.rm = TRUE)
  p_diff_mean <- mean(clean_price[[prod1]] - clean_price[[prod2]], na.rm = TRUE)

  # Format output row combining Treatment code and product comparison
  data.frame(
    Treatment = sprintf("%s: %s vs %s", display_label, prod1, prod2),
    N = as.character(nrow(clean_quant)),
    Quant_Diff = sprintf("%.2f", q_diff_mean),
    Quant_P = sprintf("%.3f", q_test$p.value),
    Price_Diff = sprintf("%.2f", p_diff_mean),
    Price_P = sprintf("%.3f", p_test$p.value)
  )
}

# Compile all three tests into one data frame
balance_summary <- bind_rows(
  get_balance_row("Baseline", "Product A", "Product B", "S"),
  get_balance_row("Menu", "Package 1", "Package 2", "M"),
  get_balance_row("Sub-menu", "Package 1", "Package 2", "U")
)

# Rename columns for LaTeX export (now 6 columns total instead of 7)
colnames(balance_summary) <- c("Treatment", "N", "Diff.", "p-value", "Diff.", "p-value")

# Generate raw LaTeX via stargazer
raw_latex <- capture.output(
  stargazer(
    as.data.frame(balance_summary),
    out = "Tables/product_balance_tests.tex",
    type = "latex",
    summary = FALSE,
    rownames = FALSE,
    float = FALSE
  )
)

raw_latex <- gsub("cccccc", "lccccc", raw_latex)

raw_latex <- gsub("Diff\\.\\.1", "Diff.", raw_latex)
raw_latex <- gsub("p-value\\.1", "p-value", raw_latex)
raw_latex <- gsub("p-value", "$p$-value", raw_latex)

header_idx <- grep("Treatment & N &", raw_latex)

# Adjusted LaTeX column span headers (spanning columns 3-4 for Quantity, 5-6 for Price)
top_headers <- c(
  " & & \\multicolumn{2}{c}{Quantity} & \\multicolumn{2}{c}{Median price} \\\\",
  "\\cline{3-4} \\cline{5-6} \\\\[-1.8ex]"
)

formatted_latex <- c(
  raw_latex[1:(header_idx - 1)],
  top_headers,
  raw_latex[header_idx:length(raw_latex)]
)

# Save output
writeLines(formatted_latex, "Tables/product_balance_tests.tex")

profit_lookup <- profit_summary |>
  mutate(
    treatment = case_when(
      treatment == "baseline" ~ "Baseline",
      treatment == "single_package" ~ "Pure",
      treatment == "package_menu" ~ "Menu",
      treatment == "suboptimal_menu" ~ "Sub-menu",
      TRUE ~ as.character(treatment)
    )
  ) |>
  select(treatment, config_session_number, player_id_in_group, seller_type) |>
  distinct()

trades_with_seller_type <- trades_only |>
  left_join(
    profit_lookup,
    by = c(
      "treatment" = "treatment",
      "config_session_number" = "config_session_number",
      "trade_seller_id" = "player_id_in_group"
    )
  )

# 2. Check that seller_type merged successfully without missing values
cat("Missing seller_types after join:", sum(is.na(trades_with_seller_type$seller_type)), "\n")
table(trades_with_seller_type$seller_type, useNA = "ifany")

# 1. Aggregate specialization metrics at the Seller Type level
seller_spec <- trades_with_seller_type |>
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
  ungroup() |>
  mutate(
    treatment = factor(treatment, levels = c("Baseline", "Pure", "Menu", "Sub-menu"), labels = c("S: Stacking", "P: Pure bundling", "M: Menu (optimal)", "U: Menu (suboptimal)")),
    cost_param = factor(cost_param, levels = c("HH", "HL", "LH", "LL"))
  )

primary_labels <- seller_spec |>
  group_by(treatment, cost_param, seller_type) |>
  slice_max(share, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    label_text = sprintf("%.0f%%", share * 100),
    y_fixed = if_else(product_type %in% c("Product A", "Package 1"), 0.90, 0.10)
  )

seller_fill_colors <- c(
  "Product A" = "#e60000",
  "Product B" = "#0066cc",
  "Package 1" = "#7300e6",
  "Package 2" = "#ff6600"
)

seller_plot_theme <- theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 10),
    axis.title = element_text(face = "bold"),
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    axis.text = element_text(size = 9),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.35, "cm"),
    legend.spacing.x = unit(0.15, "cm"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# 4. Build the plot
seller_plot <- ggplot(seller_spec, aes(x = seller_type, y = share, fill = product_type)) +
  geom_col(position = "fill", width = 0.6, color = "black", linewidth = 0.3) +
  geom_text(
    data = primary_labels,
    aes(x = seller_type, y = y_fixed, label = label_text),
    color = "white",
    fontface = "bold",
    size = 3
  ) +
  facet_grid(treatment ~ cost_param) +
  scale_y_continuous(labels = scales::percent_format(), name = "Share of total trades") +
  scale_fill_manual(values = seller_fill_colors, name = "Product type") +
  labs(
    # title = "Seller specialisation",
    x = "Seller type"
  ) +
  seller_plot_theme

ggsave(
  filename = "Charts/Seller specialisation.pdf",
  plot = seller_plot,
  width = 15,
  height = 14,
  units = "cm"
)

# ------------------------------------------------------------------------------
# SELLER SPECIALISATION NON-PARAMETRIC TESTS (METHOD 2)
# ------------------------------------------------------------------------------
cat("\n--- SELLER SPECIALISATION TESTS (NON-PARAMETRIC) ---\n")

# 1. Identify trades matching comparative advantage at individual seller level
seller_trades <- trades_with_seller_type |>
  filter(treatment != "Pure") |>
  mutate(
    # Check if trade matches comparative advantage
    is_aligned = case_when(
      seller_type == "A" & product_type %in% c("Product A", "Package 1") ~ TRUE,
      seller_type == "B" & product_type %in% c("Product B", "Package 2") ~ TRUE,
      TRUE ~ FALSE
    ),
    is_option_1 = product_type %in% c("Product A", "Package 1")
  )

# 2. Aggregate at individual seller level (seller ID within session)
seller_level_spec <- seller_trades |>
  group_by(treatment, config_session_number, trade_seller_id, seller_type) |>
  summarise(
    total_trades    = n(),
    aligned_trades  = sum(is_aligned),
    option1_trades  = sum(is_option_1),
    spec_share      = aligned_trades / total_trades,
    option1_share   = option1_trades / total_trades,
    .groups = "drop"
  )

# 3. Calculate statistical tests by treatment
run_spec_test <- function(t_name, display_label) {
  df <- seller_level_spec |> filter(treatment == t_name)
  
  if (nrow(df) == 0) return(NULL)
  
  # A. One-sample Wilcoxon signed-rank test against 0.50 (random choice)
  # Tests whether sellers trade aligned products at a rate significantly > 50%
  wilcox_one_sample <- wilcox.test(df$spec_share, mu = 0.50, alternative = "greater", exact = FALSE)
  
  # B. Two-sample Mann-Whitney U test between Seller A and Seller B
  # Tests whether Type A sellers trade significantly more Option 1 than Type B sellers
  type_a_opt1 <- df |> filter(seller_type == "A") |> pull(option1_share)
  type_b_opt1 <- df |> filter(seller_type == "B") |> pull(option1_share)
  
  wilcox_two_sample <- wilcox.test(type_a_opt1, type_b_opt1, alternative = "greater", exact = FALSE)
  
  mean_spec_share <- mean(df$spec_share, na.rm = TRUE)
  mean_a_opt1     <- mean(type_a_opt1, na.rm = TRUE)
  mean_b_opt1     <- mean(type_b_opt1, na.rm = TRUE)
  
  data.frame(
    Treatment   = display_label,
    N_Sellers   = as.character(nrow(df)),
    Mean_Spec   = sprintf("%.1f%%", mean_spec_share * 100),
    p_vs_random = sprintf("%.3f", wilcox_one_sample$p.value),
    Opt1_TypeA  = sprintf("%.1f%%", mean_a_opt1 * 100),
    Opt1_TypeB  = sprintf("%.1f%%", mean_b_opt1 * 100),
    p_between   = sprintf("%.3f", wilcox_two_sample$p.value)
  )
}

spec_summary <- bind_rows(
  run_spec_test("Baseline", "S: Stacking"),
  run_spec_test("Menu", "M: Menu (optimal)"),
  run_spec_test("Sub-menu", "U: Menu (suboptimal)")
)

# 4. Format and export to LaTeX via stargazer
colnames(spec_summary) <- c(
  "Treatment", "N", "Avg. share", "p-value (> 0.50)",
  "Type A", "Type B", "p-value (A > B)"
)

raw_spec_latex <- capture.output(
  stargazer(
    as.data.frame(spec_summary),
    out = "Tables/seller_specialisation_tests.tex",
    type = "latex",
    summary = FALSE,
    rownames = FALSE,
    float = FALSE
  )
)

raw_spec_latex <- gsub("ccccccc", "lccc|ccc", raw_spec_latex)
raw_spec_latex <- gsub("p-value \\(\\\\textgreater  0\\.50\\)", "$p$-value ($> 0.50$)", raw_spec_latex)
raw_spec_latex <- gsub("p-value \\(A \\\\textgreater  B\\)", "$p$-value ($A > B$)", raw_spec_latex)

writeLines(raw_spec_latex, "Tables/seller_specialisation_tests.tex")

# ------------------------------------------------------------------------------
# 5. GENERATE PLOTS (MEAN & 95% CI AT PRODUCT LEVEL)
# ------------------------------------------------------------------------------
calc_ci <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 2) {
    return(list(mean = mean(x), ymin = mean(x), ymax = mean(x)))
  }
  m <- mean(x)
  se <- sd(x) / sqrt(n)
  error <- qt(0.975, df = n - 1) * se
  list(mean = m, ymin = m - error, ymax = m + error)
}

# Legend structures matching graphing_vars.R
legend_levels <- c(
  "Product A", "Product B",
  "Package_filled", "Package 1_filled", "Package 2_filled",
  "Text1", "Text2",
  "Package_outline", "Package 1_outline", "Package 2_outline"
)

legend_labels <- c(
  "Product A", "Product B",
  "Package", "Package 1", "Package 2",
  " ", "  ",
  "Package", "Package 1", "Package 2"
)

product_shapes <- c(
  "Product A" = 21, "Product B" = 24,
  "Package_filled" = 23, "Package 1_filled" = 22, "Package 2_filled" = 25,
  "Text1" = NA, "Text2" = NA,
  "Package_outline" = 23, "Package 1_outline" = 22, "Package 2_outline" = 25
)

product_colours <- c(
  "Product A" = "#e60000", "Product B" = "#0066cc",
  "Package_filled" = "#00b300", "Package 1_filled" = "#7300e6", "Package 2_filled" = "#ff6600",
  "Text1" = "transparent", "Text2" = "transparent",
  "Package_outline" = "#99e699", "Package 1_outline" = "#d9b3ff", "Package 2_outline" = "#ffcc99"
)

product_fills <- c(
  "Product A" = "#e60000", "Product B" = "#0066cc",
  "Package_filled" = "#00b300", "Package 1_filled" = "#7300e6", "Package 2_filled" = "#ff6600",
  "Text1" = "transparent", "Text2" = "transparent",
  "Package_outline" = "white", "Package 1_outline" = "white", "Package 2_outline" = "white"
)

product_sizes <- c(
  "Product A" = 0.6, "Product B" = 0.6,
  "Package_filled" = 0.7, "Package 1_filled" = 0.6, "Package 2_filled" = 0.6,
  "Text1" = 0, "Text2" = 0,
  "Package_outline" = 0.5, "Package 1_outline" = 0.4, "Package 2_outline" = 0.4
)

product_strokes <- c(
  "Product A" = 0.2, "Product B" = 0.2,
  "Package_filled" = 0.2, "Package 1_filled" = 0.2, "Package 2_filled" = 0.2,
  "Text1" = 0, "Text2" = 0,
  "Package_outline" = 0.8, "Package 1_outline" = 0.8, "Package 2_outline" = 0.8
)

pointrange_theme <- theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 10),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 9),
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

unified_guide <- guide_legend(nrow = 2, byrow = TRUE)

# Mapping function (INVERTED: RAW quantities map to outlines, ADJUSTED quantities map to filled)
apply_legend_mapping <- function(df, is_adj = FALSE) {
  df |> mutate(
    legend_type = case_when(
      # RAW packages get lighter OUTLINE shapes
      !is_adj & product_type == "Package" ~ "Package_outline",
      !is_adj & product_type == "Package 1" ~ "Package 1_outline",
      !is_adj & product_type == "Package 2" ~ "Package 2_outline",
      # ADJUSTED packages get darker FILLED shapes
      is_adj & product_type == "Package" ~ "Package_filled",
      is_adj & product_type == "Package 1" ~ "Package 1_filled",
      is_adj & product_type == "Package 2" ~ "Package 2_filled",
      # Standalone products or text spacers
      is_adj & product_type %in% c("Product A", "Product B") ~ "Text1",
      TRUE ~ product_type
    ),
    legend_type = factor(legend_type, levels = legend_levels),
    treatment = factor(treatment, levels = c("Baseline", "Pure", "Menu", "Sub-menu"), labels = c("S", "P", "M", "U"))
  )
}

# A. Raw Data (Mapped to Outlines for Packages)
plot_data_raw <- group_prod |>
  group_by(cost_param, treatment, product_type) |>
  summarise(
    quant = list(calc_ci(quant_raw)),
    price = list(calc_ci(price_raw)),
    .groups = "drop"
  ) |>
  unnest_wider(quant, names_sep = "_") |>
  unnest_wider(price, names_sep = "_") |>
  apply_legend_mapping(is_adj = FALSE)

# B. Adjusted Data (Mapped to Filled Shapes for Packages)
plot_data_adjusted <- group_prod |>
  filter(!(product_type %in% c("Product A", "Product B"))) |>
  group_by(cost_param, treatment, product_type) |>
  summarise(
    quant = list(calc_ci(quant_adj)),
    price = list(calc_ci(price_adj)),
    .groups = "drop"
  ) |>
  unnest_wider(quant, names_sep = "_") |>
  unnest_wider(price, names_sep = "_") |>
  apply_legend_mapping(is_adj = TRUE)

# C. Profit Data (Mean & 95% CI)
plot_data_profits <- group_mkt |>
  left_join(group_profits_cp, by = c("config_session_number", "treatment")) |>
  pivot_longer(cols = c(Buyer, Seller, Total), names_to = "Role", values_to = "Profit") |>
  mutate(Role = factor(Role, levels = c("Total", "Buyer", "Seller"))) |>
  group_by(cost_param, treatment, Role) |>
  summarise(
    prof = list(calc_ci(Profit)),
    .groups = "drop"
  ) |>
  unnest_wider(prof, names_sep = "_") |>
  mutate(treatment = factor(treatment, levels = c("Baseline", "Pure", "Menu", "Sub-menu"), labels = c("S", "P", "M", "U")))

# --- PLOT CONSTRUCTION ---

# 1. Price Plot
p_prices <- ggplot(mapping = aes(x = treatment, group = product_type)) +
  geom_pointrange(
    data = plot_data_raw,
    aes(
      y = price_mean, ymin = price_ymin, ymax = price_ymax,
      colour = legend_type, shape = legend_type, fill = legend_type, size = legend_type, stroke = legend_type
    ),
    position = position_dodge(width = 0.5), show.legend = FALSE
  ) +
  geom_pointrange(
    data = plot_data_adjusted,
    aes(
      y = price_mean, ymin = price_ymin, ymax = price_ymax,
      colour = legend_type, shape = legend_type, fill = legend_type, size = legend_type, stroke = legend_type
    ),
    position = position_dodge(width = 0.5), show.legend = FALSE
  ) +
  facet_wrap(~cost_param, ncol = 2) +
  scale_colour_manual(values = product_colours, labels = legend_labels, drop = FALSE) +
  scale_fill_manual(values = product_fills, labels = legend_labels, drop = FALSE) +
  scale_shape_manual(values = product_shapes, labels = legend_labels, drop = FALSE) +
  scale_size_manual(values = product_sizes, labels = legend_labels, drop = FALSE) +
  scale_discrete_manual(aesthetics = "stroke", values = product_strokes, labels = legend_labels, drop = FALSE) +
  labs(title = "Trade prices", x = "Treatment", y = "Median price") +
  pointrange_theme +
  ylim(0, 16)

# 2. Quantity Plot
p_quant <- ggplot(mapping = aes(x = treatment, group = product_type)) +
  geom_pointrange(
    data = plot_data_raw,
    aes(
      y = quant_mean, ymin = quant_ymin, ymax = quant_ymax,
      colour = legend_type, shape = legend_type, fill = legend_type, size = legend_type, stroke = legend_type
    ),
    position = position_dodge(width = 0.5)
  ) +
  geom_pointrange(
    data = plot_data_adjusted,
    aes(
      y = quant_mean, ymin = quant_ymin, ymax = quant_ymax,
      colour = legend_type, shape = legend_type, fill = legend_type, size = legend_type, stroke = legend_type
    ),
    position = position_dodge(width = 0.5)
  ) +
  facet_wrap(~cost_param, ncol = 2) +
  scale_colour_manual(values = product_colours, labels = legend_labels, drop = FALSE) +
  scale_fill_manual(values = product_fills, labels = legend_labels, drop = FALSE) +
  scale_shape_manual(values = product_shapes, labels = legend_labels, drop = FALSE) +
  scale_size_manual(values = product_sizes, labels = legend_labels, drop = FALSE) +
  scale_discrete_manual(aesthetics = "stroke", values = product_strokes, labels = legend_labels, drop = FALSE) +
  guides(
    colour = unified_guide, shape = unified_guide, fill = unified_guide,
    size = unified_guide, stroke = unified_guide
  ) +
  labs(
    title = "Trade volumes", x = "Treatment", y = "Quantity traded",
    color = "Product type", shape = "Product type", fill = "Product type",
    size = "Product type", stroke = "Product type"
  ) +
  pointrange_theme +
  ylim(0, 72)

# 3. Market Profits Plot
p_profit <- ggplot(
  plot_data_profits,
  aes(
    x = treatment, y = prof_mean,
    group = Role, fill = Role, color = Role,
    size = Role, stroke = Role, shape = Role
  )
) +
  geom_pointrange(aes(ymin = prof_ymin, ymax = prof_ymax),
    position = position_dodge(width = 0.5)
  ) +
  facet_wrap(~cost_param, ncol = 2) +
  scale_shape_manual(values = c("Total" = 23, "Buyer" = 21, "Seller" = 21)) +
  scale_size_manual(values = c("Total" = 0.7, "Buyer" = 0.6, "Seller" = 0.5)) +
  scale_discrete_manual(aesthetics = "stroke", values = c("Total" = 0, "Buyer" = 0, "Seller" = 0.8)) +
  scale_fill_manual(values = c("Total" = "#7300e6", "Buyer" = "black", "Seller" = "white")) +
  scale_color_manual(values = c("Total" = "#8A2BE2", "Buyer" = "black", "Seller" = "black")) +
  labs(
    title = "Market profits", x = "Treatment", y = "Total profit",
    fill = "Role", shape = "Role", size = "Role", stroke = "Role"
  ) +
  pointrange_theme

# --- COMBINE & EXPORT ---
leg_products <- get_legend(p_quant)
leg_profits <- get_legend(p_profit)

p_prices_nl <- p_prices + theme(legend.position = "none")
p_quant_nl <- p_quant + theme(legend.position = "none")
p_profit_nl <- p_profit + theme(legend.position = "none")

p_top <- p_prices_nl | p_quant_nl | p_profit_nl
p_bottom <- wrap_elements(leg_products) | wrap_elements(leg_profits)
p_bottom <- p_bottom + plot_layout(widths = c(2, 1))

combined_summary_plot <- (p_top / p_bottom) +
  plot_layout(heights = c(8, 1)) +
  plot_annotation(
    # title = "Market statistics summary (means with 95% CI)",
    theme = theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10))
  )

final_plot <- ggdraw(combined_summary_plot) +
  draw_label(
    "Price/volume of packages:",
    x = 0.35, y = 0.044,
    hjust = 1, size = 10, fontface = "italic"
  )

ggsave(
  filename = "Charts/Summary statistics.pdf",
  plot = final_plot,
  width = 24,
  height = 14,
  units = "cm"
)

print(final_plot)
