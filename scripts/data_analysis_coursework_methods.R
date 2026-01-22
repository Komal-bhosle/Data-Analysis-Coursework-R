# Data Analysis Coursework (R) – Representative Methods
# Author: Komal Bhosle
#
# Purpose:
# This script demonstrates core methods used across coursework labs:
# - data wrangling (tidyverse)
# - exploratory data analysis (EDA)
# - visualization (ggplot2)
# - summary tables (gtsummary/gt)
# - contingency tables + association (odds ratio)
#
# Data sources:
# - MASS::Melanoma (built-in dataset)

# ---- Libraries ----
suppressPackageStartupMessages({
  library(tidyverse)
  library(gtsummary)
  library(gt)
  library(MASS)       # Melanoma dataset
  library(easystats)  # oddsratio()
})

# ---- 1) Data import (reproducible) ----
data("Melanoma", package = "MASS")
melanoma <- as_tibble(Melanoma)

# ---- 2) Data cleaning / recoding ----
melanoma_clean <- melanoma |>
  mutate(
    sex = factor(ifelse(sex == 1, "Male", "Female")),
    ulcer = factor(ifelse(ulcer == 1, "Ulcer Present", "Ulcer Absent")),
    status = factor(status, levels = c(1, 2, 3),
                    labels = c("Melanoma Death", "Not Dead", "Non-Melanoma Death"))
  ) |>
  select(time, status, sex, age, year, thickness, ulcer)

# Quick sanity checks
glimpse(melanoma_clean)
summary(melanoma_clean)

# ---- 3) EDA: missingness + basic summaries ----
melanoma_clean |>
  summarise(across(everything(), ~ sum(is.na(.x)))) |>
  print()

melanoma_clean |>
  summarise(
    n = n(),
    mean_age = mean(age, na.rm = TRUE),
    mean_thickness = mean(thickness, na.rm = TRUE)
  ) |>
  print()

# ---- 4) Visualization ----
# Thickness distribution overall
melanoma_clean |>
  ggplot(aes(x = thickness)) +
  geom_histogram(bins = 25) +
  theme_classic() +
  labs(title = "Tumor Thickness Distribution", x = "Thickness (mm)", y = "Count")

# Ulcer status by sex
melanoma_clean |>
  ggplot(aes(x = sex, fill = ulcer)) +
  geom_bar(width = 0.75) +
  coord_flip() +
  theme_classic() +
  labs(title = "Ulcer Status by Sex", x = NULL, y = "Count", fill = "Ulcer")

# ---- 5) Summary table (clean and recruiter-friendly) ----
tbl1 <- melanoma_clean |>
  select(sex, ulcer, age, thickness) |>
  tbl_summary(
    by = sex,
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    digits = all_continuous() ~ 2,
    label = list(
      ulcer ~ "Ulcer status",
      age ~ "Age",
      thickness ~ "Tumor thickness (mm)"
    )
  ) |>
  add_overall() |>
  bold_labels()

tbl1 |>
  as_gt() |>
  tab_header(title = md("**Summary Table: Melanoma Dataset**")) |>
  tab_source_note(md("Data: `MASS::Melanoma` (reproducible built-in dataset)"))

# ---- 6) Contingency table + Odds Ratio (association) ----
ct <- melanoma_clean |>
  tbl_cross(
    row = sex,
    col = ulcer,
    percent = "row",
    label = list(sex ~ "Sex", ulcer ~ "Ulcer status")
  ) |>
  bold_labels()

ct |>
  as_gt() |>
  tab_header(title = md("**2x2 Table: Ulcer status by Sex**"))

# Odds ratio (if any cells are zero, you may need a continuity correction)
or_res <- oddsratio(melanoma_clean$sex, melanoma_clean$ulcer)
print(or_res)
