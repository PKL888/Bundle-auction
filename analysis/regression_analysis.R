library(car)
library(lme4)
library(lmtest)
library(plm)
library(sandwich)
library(stargazer)
library(tidyverse)

source("Analysis/prepare_data.R")

# 1. DATA PREPARATION & MERGING
market_summary_data <- trades_only |>
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
  ) |>
  group_by(
    config_session_number, round_number, phase, treatment, cost_param, product_type
  ) |>
  summarise(
    total_qty_per_product_adj = sum(attribute_multiplier),
    median_price_per_product_adj = median(attribute_price, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(config_session_number, round_number, phase, treatment, cost_param) |>
  summarise(
    quantity = mean(total_qty_per_product_adj, na.rm = TRUE),
    price = mean(median_price_per_product_adj, na.rm = TRUE),
    .groups = "drop"
  )

profit_summary_data <- market_profits_data |>
  mutate(
    total_profits = as.numeric(total_market_buyer_profit) + as.numeric(total_market_seller_profit),
    buyer_profits = as.numeric(total_market_buyer_profit),
    seller_profits = as.numeric(total_market_seller_profit)
  )

# Master Regression Dataset
reg_df <- market_summary_data |>
  left_join(
    profit_summary_data,
    by = c("config_session_number", "round_number", "treatment", "cost_param")
  ) |>
  mutate(
    treatment = factor(treatment, levels = c("Baseline", "Pure", "Menu", "Sub-menu"), labels = c("S", "P", "M", "U")),
    cost_param = factor(cost_param, levels = c("HH", "HL", "LH", "LL")),
    round_fe = factor(round_number)
  )

# 2. MODULAR REGRESSION FITTER FUNCTION
outcomes_config <- list(
  quantity = list(col = "quantity", label = "Quantity (adj.)"),
  price = list(col = "price", label = "Median price (adj.)"),
  buyer_profits = list(col = "buyer_profits", label = "Buyer profits"),
  seller_profits = list(col = "seller_profits", label = "Seller profits"),
  total_profits = list(col = "total_profits", label = "Total profits")
)

fit_outcome_models <- function(dep_var, df = reg_df) {
  f_m1 <- as.formula(paste(dep_var, "~ treatment"))
  f_m2 <- as.formula(paste(dep_var, "~ treatment + cost_param"))
  f_m3 <- as.formula(paste(dep_var, "~ treatment * cost_param"))
  
  m1 <- lm(f_m1, data = df)
  m2 <- lm(f_m2, data = df)
  m3 <- lm(f_m3, data = df)
  
  # Clustered Standard Errors by Independent Session/Group
  se1 <- sqrt(diag(vcovCL(m1, cluster = ~config_session_number)))
  se2 <- sqrt(diag(vcovCL(m2, cluster = ~config_session_number)))
  se3 <- sqrt(diag(vcovCL(m3, cluster = ~config_session_number)))
  
  list(
    models = list(m1, m2, m3),
    ses = list(se1, se2, se3)
  )
}

# Estimate all models dynamically
model_fits <- lapply(names(outcomes_config), function(key) {
  fit_outcome_models(outcomes_config[[key]]$col)
})
names(model_fits) <- names(outcomes_config)

cov_labels_1 <- c(
  "$P$", "$M$", "$U$"
)

cov_labels_2 <- c(
  "$P$", "$M$", "$U$",
  "HL", "LH", "LL"
)

cov_labels_3 <- c(
  "$P$", "$M$", "$U$",
  "HL", "LH", "LL",
  "$P$ x HL", "$P$ x LH", "$P$ x LL",
  "$M$ x HL", "$M$ x LH", "$M$ x LL",
  "$U$ x HL", "$U$ x LH", "$U$ x LL"
)

interaction_order <- c(
  "^treatmentP$", "^treatmentM$", "^treatmentU$",
  "^cost_paramHL$", "^cost_paramLH$", "^cost_paramLL$",
  "treatmentP:cost_paramHL", "treatmentP:cost_paramLH", "treatmentP:cost_paramLL",
  "treatmentM:cost_paramHL", "treatmentM:cost_paramLH", "treatmentM:cost_paramLL",
  "treatmentU:cost_paramHL", "treatmentU:cost_paramLH", "treatmentU:cost_paramLL"
)

# HELPER FUNCTIONS FOR POST-ESTIMATION HYPOTHESIS TESTS & LATEX CLEANUP
get_diff_stars_only <- function(model, vcov_mat, spec) {
  lh <- linearHypothesis(model, spec, vcov = vcov_mat)
  
  # Calculate difference in point estimates
  cf <- coef(model)
  vars <- trimws(unlist(strsplit(spec, "=")))
  diff_val <- cf[vars[1]] - cf[vars[2]]
  
  # Extract p-value safely for both lm (Pr(>F)) and plm (Pr(>Chisq))
  p_val <- if ("Pr(>F)" %in% names(lh)) {
    lh$`Pr(>F)`[2]
  } else if ("Pr(>Chisq)" %in% names(lh)) {
    lh$`Pr(>Chisq)`[2]
  } else {
    lh[[ncol(lh)]][2] # Fallback to the last column
  }
  
  # Format stars
  stars <- if (!is.na(p_val) && p_val < 0.01) "$^{***}$" else if (!is.na(p_val) && p_val < 0.05) "$^{**}$" else if (!is.na(p_val) && p_val < 0.1) "$^{*}$" else ""
  
  sprintf("%.2f%s", diff_val, stars)
}

clean_stargazer_tex <- function(file_path, remove_ex = FALSE) {
  tex <- readLines(file_path)
  
  # 1. Column alignment fixes
  tex <- gsub("{@{\\extracolsep{5pt}}llccc}", "{lccccc}", tex, fixed = TRUE)
  tex <- gsub("{@{\\extracolsep{5pt}}lccccc}", "{lccccc}", tex, fixed = TRUE)
  
  # 2. Convert text multiplication to LaTeX times symbol
  tex <- gsub(" x ", " $\\times$ ", tex, fixed = TRUE)
  
  # 3. Format p-values and remove stargazer note lines
  tex <- gsub("p-value", "$p$-value", tex, fixed = TRUE)
  tex <- gsub("\\textit{Note:}  & \\multicolumn{5}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}", "", tex, fixed = TRUE)
  tex <- gsub("\\textit{Note:}  & \\multicolumn{6}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}", "", tex, fixed = TRUE)
  tex <- gsub("\\textit{Note:}  & \\multicolumn{3}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}", "", tex, fixed = TRUE)
  
  # 4. Conditional removal of \\[-1.8ex]
  if (is.logical(remove_ex) && remove_ex) {
    tex <- gsub("\\\\[-1.8ex]", "\\\\", tex, fixed = TRUE)
  }
  
  writeLines(tex, file_path)
}

# TABLE 1: MAIN TEXT — TOTAL PROFIT (3 MODELS)
stargazer(
  model_fits$total_profits$models,
  se = model_fits$total_profits$ses,
  out = "Tables/reg_total_profits_main.tex",
  type = "latex",
  dep.var.labels = "Total profit",
  covariate.labels = cov_labels_3,
  order = interaction_order,
  digits = 2, digits.extra = 2,
  keep.stat = c("n", "rsq"),
  float = FALSE
)
clean_stargazer_tex("Tables/reg_total_profits_main.tex", remove_ex = TRUE)

# TABLE 2: MAIN TEXT — CONSOLIDATED OVERVIEW (5 COLUMNS: MODEL 1 FOR ALL OUTCOMES)
m1_models <- lapply(model_fits, function(x) x$models[[1]])
m1_ses <- lapply(model_fits, function(x) x$ses[[1]]) 
dep_labels_5 <- sapply(outcomes_config, function(x) x$label)

diff_P_M <- sapply(seq_along(m1_models), function(i) {
  vcov_i <- vcovCL(m1_models[[i]], cluster = ~config_session_number)
  get_diff_stars_only(m1_models[[i]], vcov_i, "treatmentP = treatmentM")
})

diff_P_U <- sapply(seq_along(m1_models), function(i) {
  vcov_i <- vcovCL(m1_models[[i]], cluster = ~config_session_number)
  get_diff_stars_only(m1_models[[i]], vcov_i, "treatmentP = treatmentU")
})

diff_M_U <- sapply(seq_along(m1_models), function(i) {
  vcov_i <- vcovCL(m1_models[[i]], cluster = ~config_session_number)
  get_diff_stars_only(m1_models[[i]], vcov_i, "treatmentM = treatmentU")
})

stargazer(
  m1_models,
  se = m1_ses,
  out = "Tables/reg_consolidated_main.tex",
  type = "latex",
  column.labels = dep_labels_5,
  dep.var.labels.include = FALSE,
  covariate.labels = cov_labels_1,
  order = interaction_order,
  digits = 2, digits.extra = 2,
  keep.stat = c("n", "rsq", "adj.rsq", "f"),
  add.lines = list(
    c("$P - M$", diff_P_M),
    c("$P - U$", diff_P_U),
    c("$M - U$", diff_M_U)
  ),
  float = FALSE
)
clean_stargazer_tex("Tables/reg_consolidated_main.tex", remove_ex = TRUE)

# TABLE 3: MAIN TEXT — MARKET PROFIT (3 MODELS)
stargazer(
  c(model_fits$buyer_profits$models, model_fits$seller_profits$models),
  se = c(model_fits$buyer_profits$ses, model_fits$seller_profits$ses),
  out = "Tables/reg_market_profits_main.tex",
  type = "latex",
  dep.var.labels = c("Buyer profit", "Seller profit"),
  column.separate = c(3, 3),
  covariate.labels = cov_labels_3,
  order = interaction_order,
  digits = 2, digits.extra = 2,
  keep.stat = c("n", "rsq"),
  float = FALSE
)
clean_stargazer_tex("Tables/reg_market_profits_main.tex", remove_ex = TRUE)

# TABLE 4: MAIN TEXT — MARKET SUMMARY (3 MODELS)
stargazer(
  c(model_fits$quantity$models, model_fits$price$models),
  se = c(model_fits$quantity$ses, model_fits$price$ses),
  out = "Tables/reg_market_summary_main.tex",
  type = "latex",
  dep.var.labels = c("Quantity (adj.)", "Median price (adj.)"),
  column.separate = c(3, 3),
  covariate.labels = cov_labels_3,
  order = interaction_order,
  digits = 2, digits.extra = 2,
  keep.stat = c("n", "rsq"),
  float = FALSE
)
clean_stargazer_tex("Tables/reg_market_summary_main.tex", remove_ex = TRUE)

# 3. ROUND FIXED EFFECTS MODEL WITH INLINE 3-TREATMENT TESTS

fit_outcome_models_fe <- function(dep_var, df = reg_df) {
  f_m1_fe <- as.formula(paste(dep_var, "~ treatment + round_fe"))
  m1_fe <- lm(f_m1_fe, data = df)
  se1_fe <- sqrt(diag(vcovCL(m1_fe, cluster = ~config_session_number)))
  list(model = m1_fe, se = se1_fe)
}

model_fits_fe <- lapply(names(outcomes_config), function(key) {
  fit_outcome_models_fe(outcomes_config[[key]]$col)
})
names(model_fits_fe) <- names(outcomes_config)

m1_fe_models <- lapply(model_fits_fe, function(x) x$model)
m1_fe_ses <- lapply(model_fits_fe, function(x) x$se)

diff_P_M_fe <- sapply(seq_along(m1_fe_models), function(i) {
  vcov_i <- vcovCL(m1_fe_models[[i]], cluster = ~config_session_number)
  get_diff_stars_only(m1_fe_models[[i]], vcov_i, "treatmentP = treatmentM")
})

diff_P_U_fe <- sapply(seq_along(m1_fe_models), function(i) {
  vcov_i <- vcovCL(m1_fe_models[[i]], cluster = ~config_session_number)
  get_diff_stars_only(m1_fe_models[[i]], vcov_i, "treatmentP = treatmentU")
})

diff_M_U_fe <- sapply(seq_along(m1_fe_models), function(i) {
  vcov_i <- vcovCL(m1_fe_models[[i]], cluster = ~config_session_number)
  get_diff_stars_only(m1_fe_models[[i]], vcov_i, "treatmentM = treatmentU")
})

stargazer(
  m1_fe_models,
  se = m1_fe_ses,
  out = "Tables/reg_consolidated_fe_main.tex",
  type = "latex",
  column.labels = dep_labels_5,
  dep.var.labels.include = FALSE,
  covariate.labels = cov_labels_1,
  order = interaction_order,
  omit = "round_fe",
  add.lines = list(
    c("Round fixed effects", "Yes", "Yes", "Yes", "Yes", "Yes"),
    c("$P - M$", diff_P_M_fe),
    c("$P - U$", diff_P_U_fe),
    c("$M - U$", diff_M_U_fe)
  ),
  digits = 2, digits.extra = 2,
  keep.stat = c("n", "rsq", "adj.rsq", "f"),
  float = FALSE
)
clean_stargazer_tex("Tables/reg_consolidated_fe_main.tex")

# 4. RANDOM EFFECTS MODEL WITH INLINE 3-TREATMENT TESTS

pdata <- pdata.frame(reg_df, index = c("config_session_number", "round_number"))

re_models <- list()
re_ses <- list()

for (key in names(outcomes_config)) {
  dep_var <- outcomes_config[[key]]$col
  f_re <- as.formula(paste(dep_var, "~ treatment"))
  m_re <- plm(f_re, data = pdata, model = "random")
  se_re <- sqrt(diag(vcovG(m_re, cluster = "group")))
  
  re_models[[key]] <- m_re
  re_ses[[key]] <- se_re
}

diff_P_M_re <- sapply(seq_along(re_models), function(i) {
  vcov_i <- vcovG(re_models[[i]], cluster = "group")
  get_diff_stars_only(re_models[[i]], vcov_i, "treatmentP = treatmentM")
})

diff_P_U_re <- sapply(seq_along(re_models), function(i) {
  vcov_i <- vcovG(re_models[[i]], cluster = "group")
  get_diff_stars_only(re_models[[i]], vcov_i, "treatmentP = treatmentU")
})

diff_M_U_re <- sapply(seq_along(re_models), function(i) {
  vcov_i <- vcovG(re_models[[i]], cluster = "group")
  get_diff_stars_only(re_models[[i]], vcov_i, "treatmentM = treatmentU")
})

stargazer(
  re_models,
  se = re_ses,
  out = "Tables/reg_consolidated_re_main.tex",
  type = "latex",
  column.labels = dep_labels_5,
  dep.var.labels.include = FALSE,
  covariate.labels = cov_labels_1,
  order = interaction_order,
  add.lines = list(
    c("$P - M$", diff_P_M_re),
    c("$P - U$", diff_P_U_re),
    c("$M - U$", diff_M_U_re)
  ),
  digits = 2, digits.extra = 2,
  keep.stat = c("n", "rsq"),
  float = FALSE
)
clean_stargazer_tex("Tables/reg_consolidated_re_main.tex")

# 5. DEDICATED 12-INTERACTION HYPOTHESIS TEST TABLE (RAW TABULAR ONLY)

costs <- c("HH", "HL", "LH", "LL")
policy_pairs <- list(
  c("P", "M"),
  c("P", "U"),
  c("M", "U")
)

calc_interaction_hypothesis <- function(model, vcov_mat, cost, p1, p2) {
  if (cost == "HH") {
    spec <- sprintf("treatment%s = treatment%s", p1, p2)
  } else {
    spec <- sprintf(
      "treatment%s + treatment%s:cost_param%s = treatment%s + treatment%s:cost_param%s",
      p1, p1, cost, p2, p2, cost
    )
  }
  
  lh <- linearHypothesis(model, spec, vcov = vcov_mat)
  
  cf <- coef(model)
  if (cost == "HH") {
    diff_val <- cf[paste0("treatment", p1)] - cf[paste0("treatment", p2)]
  } else {
    eff1 <- cf[paste0("treatment", p1)] + cf[paste0("treatment", p1, ":cost_param", cost)]
    eff2 <- cf[paste0("treatment", p2)] + cf[paste0("treatment", p2, ":cost_param", cost)]
    diff_val <- eff1 - eff2
  }
  
  p_val <- lh$`Pr(>F)`[2]
  stars <- if (p_val < 0.01) "$^{***}$" else if (p_val < 0.05) "$^{**}$" else if (p_val < 0.1) "$^{*}$" else ""
  
  sprintf("%.2f%s", diff_val, stars)
}

# Construct matrix of results across 12 tests (rows) and 5 outcomes (columns)
m3_models <- lapply(model_fits, function(x) x$models[[3]])
m3_vcovs <- lapply(m3_models, function(m) vcovCL(m, cluster = ~config_session_number))

table_rows <- list()

for (cost in costs) {
  for (pair in policy_pairs) {
    p1 <- pair[1]
    p2 <- pair[2]
    row_label <- sprintf("$%s - %s$", p1, p2)
    
    row_vals <- sapply(seq_along(m3_models), function(k) {
      calc_interaction_hypothesis(m3_models[[k]], m3_vcovs[[k]], cost, p1, p2)
    })
    
    table_rows[[length(table_rows) + 1]] <- c(row_label, row_vals, cost)
  }
}

# Generate raw tabular LaTeX environment string
latex_headers <- c("Quantity (adj.)", "Median price (adj.)", "Buyer profits", "Seller profits", "Total profits")

tex_tabular_only <- c(
  "\\begin{tabular}{lccccc}",
  "\\hline\\hline",
  "[-1.8ex]",
  paste("Comparison & ", paste(latex_headers, collapse = " & "), " \\\\"),
  "\\hline",
  "[-1.8ex]"
)

current_cost <- ""
for (r in table_rows) {
  cost_label <- r[length(r)]
  row_data <- r[1:(length(r) - 1)]
  
  if (cost_label != current_cost) {
    current_cost <- cost_label
    tex_tabular_only <- c(tex_tabular_only, paste0("\\multicolumn{6}{l}{\\textbf{Cost Parameter Environment: ", current_cost, "}} \\\\"))
  }
  tex_tabular_only <- c(tex_tabular_only, paste(row_data, collapse = " & ") %>% paste0(" \\\\"))
}

tex_tabular_only <- c(
  tex_tabular_only,
  "\\hline\\hline",
  "\\end{tabular}"
)

writeLines(tex_tabular_only, "Tables/reg_interactions_tests.tex")
