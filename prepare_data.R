# Load libraries
library(tidyverse)
library(lubridate)

# Set working directory
setwd("C:/Users/peleg/Desktop/Chocolate/University/Honours/Thesis/Experiment")
file_name <- "Data/combined_experiment_data.csv"

# Define parsing function for custom oTree export
parse_otree_custom_export <- function(filepath) {
  lines <- readLines(filepath)
  header_idx <- grep("^record_type", lines)
  section_names <- c(
    "PROFIT_SUMMARY", "EVENT_LOG", "MARKET_CONVERGENCE",
    "PLAYER_ROUND_PANEL", "MARKET_SUMMARY", "QUESTIONNAIRE"
  )
  df_list <- list()

  for (i in seq_along(header_idx)) {
    start_line <- header_idx[i]
    end_line <- ifelse(i < length(header_idx), header_idx[i + 1] - 1,
      length(lines)
    )

    block_lines <- lines[start_line:end_line]
    df_list[[section_names[i]]] <- read.csv(
      text = paste(block_lines, collapse = "\n"),
      stringsAsFactors = FALSE,
      na.strings = c("", "N/A")
    )
  }
  return(df_list)
}

# Define cost parameter mapping function
get_cost_param <- function(config_session, round_num) {
  session_orders <- list(
    "1" = c("HL", "LH", "LL", "HH"),
    "2" = c("LL", "HL", "HH", "LH"),
    "3" = c("LH", "LL", "HH", "HL"),
    "4" = c("HL", "HH", "LL", "LH"),
    "5" = c("HH", "LH", "HL", "LL")
  )
  if (round_num <= 2) {
    return("PRACTICE")
  }

  block_idx <- ((round_num - 3) %/% 3) + 1
  sess_str <- as.character(config_session)
  return(session_orders[[sess_str]][block_idx])
}
get_cost_param_vec <- Vectorize(get_cost_param)

# Load data
lab_data <- parse_otree_custom_export(file_name)

event_log <- lab_data$EVENT_LOG
market_summary <- lab_data$MARKET_SUMMARY
profit_summary <- lab_data$PROFIT_SUMMARY

# Prepare trade data
trades_only <- event_log |>
  filter(event_type == "Trade", is_practice == 0) |>
  mutate(
    price = as.numeric(price),
    elapsed_time_seconds = as.numeric(elapsed_time_seconds),
    phase = factor(((round_number - 3) %/% 3) + 1),
    cost_param = get_cost_param_vec(config_session_number, round_number),
    treatment = case_when(
      treatment == "baseline" ~ "Baseline",
      treatment == "single_package" ~ "Pure",
      treatment == "package_menu" ~ "Menu",
      treatment == "suboptimal_menu" ~ "Sub-menu",
      TRUE ~ treatment
    ),
    cost_param = factor(cost_param, levels = c("HH", "HL", "LH", "LL")),
    treatment = factor(treatment, levels = c("Baseline", "Pure", "Menu", "Sub-menu")),
    product_type = factor(product_type, levels = c(
      "Product A", "Product B", "Package", "Package 1", "Package 2"
    ))
  ) |>
  filter(cost_param != "PRACTICE")

# Prepare market profits data
market_profits_data <- market_summary |>
  filter(is_practice == 0, total_market_buyer_profit != 0) |>
  mutate(
    cost_param = get_cost_param_vec(config_session_number, round_number),
    treatment = case_when(
      treatment == "baseline" ~ "Baseline",
      treatment == "single_package" ~ "Pure",
      treatment == "package_menu" ~ "Menu",
      treatment == "suboptimal_menu" ~ "Sub-menu",
      TRUE ~ treatment
    ),
    cost_param = factor(cost_param, levels = c("HH", "HL", "LH", "LL")),
    treatment = factor(treatment, levels = c("Baseline", "Pure", "Menu", "Sub-menu"))
  ) |>
  filter(cost_param != "PRACTICE")

# Prepare questionnaire data
questionnaire_data <- lab_data$QUESTIONNAIRE |>
  mutate(
    # Cast continuous metrics to numeric for summary calculations
    age = as.numeric(age),
    risk_preference = as.numeric(risk_preference),

    # Standardize treatment names to match trade and profit data
    treatment = case_when(
      treatment == "baseline" ~ "Baseline",
      treatment == "single_package" ~ "Pure",
      treatment == "package_menu" ~ "Menu",
      treatment == "suboptimal_menu" ~ "Sub-menu",
      TRUE ~ treatment
    ),
    treatment = factor(treatment,
      levels =
        c("Baseline", "Pure", "Menu", "Sub-menu")
    )
  ) |>
  select(-ends_with("_attempts"))
