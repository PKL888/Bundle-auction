# ==============================================================================
# Questionnaire Summary Statistics
# ==============================================================================

# 1. Summary for Continuous Variables (Age, Risk Preference)
continuous_demographics <- questionnaire_data |>
  summarise(
    Total_Participants = n(),
    Mean_Age = mean(as.numeric(age), na.rm = TRUE),
    SD_Age = sd(as.numeric(age), na.rm = TRUE),
    Mean_Risk_Preference = mean(as.numeric(risk_preference), na.rm = TRUE),
    SD_Risk_Preference = sd(as.numeric(risk_preference), na.rm = TRUE)
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "Metric",
    values_to = "Value"
  ) |>
  mutate(Value = round(Value, 2))

# 2. Summary for Categorical Variables
categorical_demographics <- questionnaire_data |>
  select(
    gender, birth_place, years_in_australia, field_of_study,
    prev_experiments, academic_level, gpa
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "Category",
    values_to = "Response"
  ) |>
  group_by(Category, Response) |>
  summarise(Count = n(), .groups = "drop") |>
  group_by(Category) |>
  mutate(Percentage = round((Count / sum(Count)) * 100, 1)) |>
  arrange(Category, desc(Count))

# View the resulting tables
View(continuous_demographics)
View(categorical_demographics)

library(gtsummary)
library(kableExtra)

clean_labels <- list(
  age = "Age",
  risk_preference = "Risk preference",
  gender = "Gender",
  birth_place = "Birth place",
  years_in_australia = "Years in Australia",
  field_of_study = "Field of study",
  prev_experiments = "Experience",
  academic_level = "Academic level",
  gpa = "GPA"
)

# Create a publication-style balance table
balance_table <- questionnaire_data |>
  mutate(
    gender = fct_recode(
      factor(gender),
      "Other" = "Prefer not to say"
    ),
    birth_place = fct_recode(
      factor(birth_place),
      "US / Canada" = "United States or Canada"
    ),
    years_in_australia = fct_recode(
      factor(years_in_australia),
      "1--2 years" = "1-2 years",
      "2--5 years" = "2-5 years",
      "<1 year" = "Less than 1 year",
      ">5 years" = "More than 5 years",
      "N/A" = "Not applicable (born in Australia)"
    ),
    field_of_study = fct_recode(
      factor(field_of_study),
      "Business" = "Management / Business / Commerce",
      "Health Sciences" = "Medicine / Nursing / Health Sciences",
      "IT" = "Information Technology",
      "Science / Maths" = "Science / Mathematics"
    ),
    prev_experiments = fct_recode(
      factor(prev_experiments),
      "1--2 experiments" = "1-2 previous experiments",
      "3--5 experiments" = "3-5 previous experiments",
      ">5 experiments " = "More than 5 previous experiments"
    ),
    academic_level = fct_recode(
      factor(academic_level),
      "1st year" = "1st year undergraduate",
      "2nd year" = "2nd year undergraduate",
      "3rd year" = "3rd year undergraduate",
      ">3rd year" = "4th year undergraduate or above",
      "Masters" = "Graduate student"
    ),
    gpa = fct_recode(
      factor(gpa),
      "<4.0" = "Below 4.0",
      "4.0--4.99" = "Between 4.0 and 4.99",
      "5.0--5.99" = "Between 5.0 and 5.99",
      "6.0--6.49" = "Between 6.0 and 6.49",
      "6.5--7.0" = "Between 6.5 and 7.0",
      "N/A" = "Not applicable (this is my first semester at the University)"
    )
  ) |>
  select(
    treatment, age, risk_preference, gender, birth_place,
    years_in_australia, field_of_study, prev_experiments,
    academic_level, gpa
  ) |>
  tbl_summary(
    by = treatment,
    label = clean_labels,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2
  ) |>
  add_p(
    test = all_categorical() ~ "fisher.test",
    test.args = all_categorical() ~ list(simulate.p.value = TRUE, B = 2000)
  ) |>
  modify_header(
    label = "**Characteristic**",
    stat_1 = "**Baseline**",
    stat_2 = "**Pure**",
    stat_3 = "**Menu**",
    stat_4 = "**Sub-menu**",
    p.value = "**$p$-value**"
  ) |>
  modify_footnote(everything() ~ NA)

# Convert the gtsummary object to a gt object
# Extract the raw LaTeX code natively using gt
raw_latex <- balance_table |>
  as_gt() |>
  gt::as_latex() |>
  as.character()

# 1. Un-escape the dollar signs in the p-value header
raw_latex <- gsub("\\$p\\$-value", "$p$-value", raw_latex, fixed = TRUE)

# 2. Wrap inequalities in math mode
raw_latex <- gsub(">", "$>$", raw_latex, fixed = TRUE)
raw_latex <- gsub("<", "$<$", raw_latex, fixed = TRUE)

# 3. Remove outer floating table wrapper
raw_latex <- gsub("\\begin{table}[t]", "", raw_latex, fixed = TRUE)
raw_latex <- gsub("\\end{table}", "", raw_latex, fixed = TRUE)

# 4. Convert tabular* to longtable
raw_latex <- gsub("\\begin{tabular*}{\\linewidth}{@{\\extracolsep{\\fill}}lccccc}",
  "\\setlength{\\tabcolsep}{4pt} \\begin{longtable}{lccccc}", raw_latex,
  fixed = TRUE
)
raw_latex <- gsub("\\end{tabular*}", "\\end{longtable}", raw_latex, fixed = TRUE)

# 5. Remove minipage around footnotes (This prevents document scoping errors)
raw_latex <- gsub("\\begin{minipage}{\\linewidth}", "\\noindent", raw_latex, fixed = TRUE)
raw_latex <- gsub("\\end{minipage}", "", raw_latex, fixed = TRUE)

# 6. Inject Caption, Label, and Multi-page Headers (Run this ONLY once)
header_target <- "\\toprule\n\\textbf{Characteristic} & \\textbf{Baseline} & \\textbf{Pure} & \\textbf{Menu} & \\textbf{Sub-menu} & \\textbf{$p$-value} \\\\ \n\\midrule\\addlinespace[2.5pt]"

header_replacement <- paste0(
  "% Page 1 Header with Automatic Table Numbering & Title\n",
  "\\caption{Demographic balance across market treatments} \\label{tab:demographic_balance} \\\\\n",
  "\\toprule\n",
  "Characteristic & {S} & {P} & {M} & {U} & {$p$-value} \\\\ \n",
  "\\midrule\\addlinespace[2.5pt]\n",
  "\\endfirsthead % End of first page header\n\n",
  "% Subsequent Pages Header\n",
  "\\multicolumn{6}{c}{{Demographic balance (continued)}} \\\\ \\addlinespace[0.5em]\n",
  "\\toprule\n",
  "Characteristic & {S} & {P} & {M} & {U} & {$p$-value} \\\\ \n",
  "\\midrule\\addlinespace[2.5pt]\n",
  "\\endhead % End of header for extra pages"
)

n_counts <- questionnaire_data |>
  group_by(treatment) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(treatment = factor(treatment, levels = c("Baseline", "Pure", "Menu", "Sub-menu"))) |>
  arrange(treatment) |>
  pull(n)

# Format the observations row
obs_row <- sprintf(
  "\\midrule\nObservations & %d & %d & %d & %d & \\\\ \n",
  n_counts[1], n_counts[2], n_counts[3], n_counts[4]
)

# 2. Inject right above \bottomrule in raw_latex
raw_latex <- gsub("\\bottomrule", paste0(obs_row, "\\bottomrule"), raw_latex, fixed = TRUE)

raw_latex <- gsub(header_target, header_replacement, raw_latex, fixed = TRUE)

# Write clean LaTeX output directly to file
writeLines(raw_latex, "Tables/demographic_balance.tex")

# Print the table to the R console/viewer (optional)
balance_table
