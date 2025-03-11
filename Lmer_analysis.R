# ---- Load data ----
# Load required libraries
library(readxl)
library(tidyverse)
library(dplyr)
library(lme4)
library(ggplot2)

# Load data
df <- read_excel("merged_data.xlsx", sheet = "merged_data")
# ------------------------

# ---- Data Cleaning ----
# Remove rows where `afwijkers` is NA (optional, as you suggested)
df <- df |> filter(!is.na(t1))

# Standardize Ancestry column: Keep only the first part before any colon or space
df <- df %>%
  mutate(Ancestry = str_extract(Ancestry, "^[^: ]+"))  # Extract first part

# Convert `t1-t4` and `r1-r4` into long format
data_long <- df |> 
  pivot_longer(
    cols = c(t1:t4, r1:r4),  # Select only the tray and runner columns
    names_to = c("type", "tray"),  # Split the column name into "type" (t/r) and "tray" (1-4)
    names_pattern = "(t|r)([1-4])"  # Use regex to extract `t` or `r` and the tray number
  ) |> 
  pivot_wider(
    names_from = "type",  # Spread "t" (trays) and "r" (runners) into separate columns
    values_from = "value"  # Use the melted values
  )

# Normalize data: Add runners per plant metric
data_long_normalized <- data_long %>%
  mutate(runners_per_plant = r / t) %>%
  filter(!is.na(runners_per_plant), t > 0)  # Exclude invalid or zero rows

# Extract Stage (St1, St2, St3, St4) from Group_code, assign "Unknown" if missing
data_long_normalized <- data_long_normalized %>%
  mutate(stage = str_extract(Group_code, "St[1-4]"),
         stage = factor(stage, levels = c("St1", "St2", "St3", "St4", "Unknown"), 
                        labels = c("St1", "St2", "St3", "St4", "Unknown"))) %>%
  replace_na(list(stage = "Unknown"))  # Fill missing stages with "Unknown"

# Extract planting date (month + week) from anywhere in Group_code
data_long_normalized <- data_long_normalized %>%
  mutate(planting_date = str_extract(Group_code, "[a-zA-Z]+\\(wk\\d{2}\\)"),  # Extract planting date from anywhere in the string
         planting_date = replace_na(planting_date, "Unknown"),  # Replace NA with "Unknown"
         planting_date = factor(planting_date, levels = c("sept(wk38)", "sept(wk36)", "nov(wk46)", "Unknown")))  # Convert to factor

# column renaming

rename_column <- function(df, old_name, new_name) {
  df %>% rename(!!new_name := all_of(old_name))
}

data_long_normalized <- rename_column(data_long_normalized, "afwijkers", "deviants_in_total_plants")

data_long_normalized <- rename_column(data_long_normalized, "Group_code", "group_code")

data_long_normalized <- rename_column(data_long_normalized, "Ancestry", "ancestry")

data_long_normalized <- rename_column(data_long_normalized, "totaal_plant", "total_plants")

data_long_normalized <- rename_column(data_long_normalized, "tray", "tray_nr")

data_long_normalized <- rename_column(data_long_normalized, "t", "plants_in_tray")

data_long_normalized <- rename_column(data_long_normalized, "r", "runner_count")

# remove trays column (redundant)
data_long_normalized <- data_long_normalized %>% select(-trays)

# tray_type to factors
data_long_normalized <- data_long_normalized %>% mutate(tray_type = factor(tray_type))

#tray_nr to factors
data_long_normalized <- data_long_normalized %>% mutate(tray_nr = factor(tray_nr))

# change date week_8
data_long_normalized <- data_long_normalized %>% mutate(count_date = ifelse(count_date == "week_8", "10_February_2025", count_date))

data_long_normalized <- data_long_normalized %>%
  mutate(count_date = case_when(
    count_date == "6_Januari_2025" ~ as.Date("2025-01-06"),
    count_date == "19_December_2024" ~ as.Date("2024-12-19"),
    count_date == "10_February_2025" ~ as.Date("2025-02-10"),
    TRUE ~ NA_Date_  # Ensure other values become NA if they exist
  ))

data_long_normalized <- data_long_normalized %>%
  mutate(tray_type = fct_recode(tray_type, "T16" = "T16s", "T16" = "mini T16", "T9" = "T9s", "T9" = "norm T9"))

data_long_normalized <- data_long_normalized %>% mutate(deviants_in_total_plants = replace_na(deviants_in_total_plants, 0))

# ------------------------

# ============================================
#  INVESTIGATING THE EFFECT OF TRAY TYPE ON 
#  RUNNER PRODUCTION IN STRAWBERRY PLANTS
# ============================================

# Load necessary libraries
library(dplyr)
library(lme4)

# --------------------------------------------
# 1️⃣ DATA EXPLORATION & NORMALITY CHECKS
# --------------------------------------------

# Check normality of the response variable (runners_per_plant)
shapiro.test(data_long_normalized$runners_per_plant)  # Shapiro-Wilk test
hist(data_long_normalized$runners_per_plant, breaks = 20)  # Histogram
qqnorm(data_long_normalized$runners_per_plant); qqline(data_long_normalized$runners_per_plant)  # Q-Q Plot

# --------------------------------------------
# 2️⃣ CHECK FOR CONFOUNDERS
# --------------------------------------------

# Check if ancestry is unevenly distributed across tray types (Chi-square test)
chisq.test(table(data_long_normalized$tray_type, data_long_normalized$ancestry))

# Display ancestry distribution per tray type in percentages
table_ancestry <- prop.table(table(data_long_normalized$tray_type, data_long_normalized$ancestry), margin = 1)
round(table_ancestry * 100, 1)  # Show percentages

# Check if ancestry influences runners_per_plant (ANOVA)
anova(lm(runners_per_plant ~ ancestry, data = data_long_normalized))

# ============================================
# 3️⃣ FULL DATASET ANALYSIS (600+ SAMPLES)
# ============================================

# Linear Mixed Model (LMM) including all potential confounders
model <- lmer(runners_per_plant ~ tray_type + ancestry + planting_date + count_date + (1 | plot), 
              data = data_long_normalized)

# View model summary
summary(model)

# Check residuals for normality & homoscedasticity
plot(model)  # Residuals vs. Fitted Values
qqnorm(resid(model)); qqline(resid(model))  # Q-Q Plot

# --------------------------------------------
# MODEL COMPARISON: SIMPLE vs FULL
# --------------------------------------------

# Remove missing values to ensure both models use the same dataset
data_complete <- na.omit(data_long_normalized)  

# Simple model: Only tray_type with random effect for plot
model_simple <- lmer(runners_per_plant ~ tray_type + (1 | plot), data = data_complete)

# Full model: Includes confounders
model_full <- lmer(runners_per_plant ~ tray_type + ancestry + planting_date + count_date + (1 | plot), 
                   data = data_complete)

# Compare models (Likelihood Ratio Test)
anova(model_simple, model_full)  

# Check variance of random effects
summary(model_full)$varcor  

# ============================================
# 4️⃣ PAIRED ANALYSIS (FILTERED DATASET, 200 SAMPLES)
# ============================================

# --------------------------------------------
# STEP 1: FILTERING FOR REPEATED PLOTS (.001 & .002)
# --------------------------------------------

# Extract base plot ID (removes .001, .002 suffix)
filtered_data <- data_long_normalized %>%
  mutate(base_plot = gsub("\\.00[12]$", "", plot))

# Find plots that have both T9 and T16 tray types
valid_plots <- filtered_data %>%
  group_by(base_plot) %>%
  filter(n_distinct(tray_type) > 1) %>%
  pull(base_plot) %>%
  unique()

# Keep only those plots
filtered_data <- filtered_data %>%
  filter(base_plot %in% valid_plots)

# --------------------------------------------
# STEP 2: RUN LMM ON FILTERED DATA
# --------------------------------------------

# Linear Mixed Model on paired dataset (controls for planting_date & count_date)
model_paired <- lmer(runners_per_plant ~ tray_type + planting_date + count_date + (1 | base_plot), 
                     data = filtered_data)

# View model summary
summary(model_paired)

# ============================================
# 5️⃣ FINAL CONCLUSION:
# - Both models (full & paired) show **no significant effect** of tray type on runners_per_plant.
# - **Planting date has a strong effect**, suggesting initial differences were due to plant age.
# - The results are robust, as one model considers full genetic diversity, and the other strictly controls for genetics.
# ============================================





