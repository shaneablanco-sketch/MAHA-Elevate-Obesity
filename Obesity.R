# Medicare population only | Survey years: 2020 & 2022
# Note: MEPS BMI (ADBMI42) is only collected in even years (from 2018 onward)

# %%

library(tidyverse)
library(tidymodels)
library(ggthemes)
library(skimr)
library(srvyr)
library(gtsummary)

source("functions.R")

# ---------------------------------------------------------------------------
# CCSR codes for the 10 target clinical conditions
# ---------------------------------------------------------------------------

# %%

target_ccsr <- c(
    # Diabetes
    "END002",
    "END003",
    "END004",
    "END005",
    "END006",
    # Hyperlipidemia
    "END010",
    # Hypertension
    "CIR007",
    "CIR008",
    # Mental health
    "MBD001",
    "MBD002",
    "MBD003",
    "MBD004",
    "MBD005",
    "MBD006",
    "MBD007",
    "MBD008",
    "MBD009",
    "MBD010",
    "MBD011",
    "MBD012",
    "MBD013",
    "MBD014",
    "MBD017",
    "MBD018",
    "MBD019",
    "MBD020",
    "MBD021",
    "MBD022",
    "MBD023",
    "MBD024",
    "MBD025",
    "MBD026",
    "MBD027",
    "MBD028",
    "MBD029",
    "MBD030",
    "MBD031",
    "MBD032",
    "MBD033",
    "MBD034",
    # Pulmonary disease
    "RSP006",
    "RSP007",
    "RSP008",
    "RSP010",
    "RSP011",
    "RSP012",
    "RSP013",
    "RSP014",
    "RSP016",
    # Arthritis
    "MUS001",
    "MUS002",
    "MUS003",
    "MUS004",
    "MUS005",
    "MUS006",
    "MUS007",
    # Back problems
    "MUS011",
    "MUS038",
    # Heart disease
    "CIR001",
    "CIR002",
    "CIR003",
    "CIR004",
    "CIR005",
    "CIR006",
    "CIR010",
    "CIR011",
    "CIR012",
    "CIR014",
    "CIR015",
    "CIR016",
    "CIR017",
    "CIR018",
    # Cerebrovascular disease
    "NVS012",
    "CIR020",
    "CIR021",
    "CIR022",
    "CIR023",
    "CIR024",
    "CIR025",
    # Asthma
    "RSP009"
)

# ---------------------------------------------------------------------------
# Conditions data  (2020 & 2022 only — BMI not collected in odd years)
# ---------------------------------------------------------------------------

# %%

h222 <- load_meps_conditions(
    dat_path = "data/conditions_data/2020/h222.dat",
    spec_path = "setup/h222_spec.csv",
    rename_path = "setup/h222_renames.csv",
    target_ccsr = target_ccsr,
    ccsr_cols = c("ccsr_code1", "ccsr_code2", "ccsr_code3"),
    survey_year = 2020
)

h241 <- load_meps_conditions(
    dat_path = "data/conditions_data/2022/h241.dat",
    spec_path = "setup/h241_spec.csv",
    rename_path = "setup/h241_renames.csv",
    target_ccsr = target_ccsr,
    ccsr_cols = c("ccsr_code1", "ccsr_code2", "ccsr_code3", "ccsr_code4"),
    survey_year = 2022
)

conditions_stacked <- bind_rows(h222, h241)
rm(h222, h241)
gc()

# ---------------------------------------------------------------------------
# Consolidated data column map
# Columns shared across both years are defined once; year-specific names
# (age, region, marital status, Medicare coverage, expenditures, weights)
# are overridden per year so both files stack with identical column names.
# ---------------------------------------------------------------------------

# %%

consolidated_cols_shared <- c(
    panel_number = "panel_number",
    family_id = "annual_family_identifier",
    sex = "sex",
    race = "race_edited_imputed_racev1x",
    hispanic = "hispanic_ethnicity_edited_imputed_hispanx",
    education = "years_of_educ_when_first_entered_meps",
    bmi = "adult_body_mass_index_17_rd_4_2",
    child_bmi = "child_s_body_mass_index_6_17_r4_2"
)

consolidated_cols_2020 <- c(
    person_id = "person_id_duid_pid",
    consolidated_cols_shared,
    age = "age_as_of_12_31_20_edited_imputed",
    region = "census_region_as_of_12_31_20",
    marital_status = "marital_status_12_31_20_edited_imputed",
    ever_medicare = "ever_have_medicare_during_2020_ed",
    mcr_jan = "covered_by_medicare_in_jan20_ed",
    mcr_feb = "covered_by_medicare_in_feb20_ed",
    mcr_mar = "covered_by_medicare_in_mar20_ed",
    mcr_apr = "covered_by_medicare_in_apr20_ed",
    mcr_may = "covered_by_medicare_in_may20_ed",
    mcr_jun = "covered_by_medicare_in_jun20_ed",
    mcr_jul = "covered_by_medicare_in_jul20_ed",
    mcr_aug = "covered_by_medicare_in_aug20_ed",
    mcr_sep = "covered_by_medicare_in_sep20_ed",
    mcr_oct = "covered_by_medicare_in_oct20_ed",
    mcr_nov = "covered_by_medicare_in_nov20_ed",
    mcr_dec = "covered_by_medicare_in_dec20_ed",
    total_expenditure = "total_health_care_exp_20",
    person_weight = "final_person_weight_2020",
    saq_weight = "final_saq_person_weight_2020",
    variance_stratum = "variance_estimation_stratum_2020",
    variance_psu = "variance_estimation_psu_2020"
)

consolidated_cols_2022 <- c(
    person_id = "person_id_duid_pid",
    consolidated_cols_shared,
    age = "age_as_of_12_31_22_edited_imputed",
    region = "census_region_as_of_12_31_22",
    marital_status = "marital_status_12_31_22_edited_imputed",
    ever_medicare = "ever_have_medicare_during_2022_ed",
    mcr_jan = "covered_by_medicare_in_jan22_ed",
    mcr_feb = "covered_by_medicare_in_feb22_ed",
    mcr_mar = "covered_by_medicare_in_mar22_ed",
    mcr_apr = "covered_by_medicare_in_apr22_ed",
    mcr_may = "covered_by_medicare_in_may22_ed",
    mcr_jun = "covered_by_medicare_in_jun22_ed",
    mcr_jul = "covered_by_medicare_in_jul22_ed",
    mcr_aug = "covered_by_medicare_in_aug22_ed",
    mcr_sep = "covered_by_medicare_in_sep22_ed",
    mcr_oct = "covered_by_medicare_in_oct22_ed",
    mcr_nov = "covered_by_medicare_in_nov22_ed",
    mcr_dec = "covered_by_medicare_in_dec22_ed",
    total_expenditure = "total_health_care_exp_22",
    person_weight = "final_person_weight_2022",
    saq_weight = "final_saq_person_weight_2022",
    variance_stratum = "variance_estimation_stratum_2022",
    variance_psu = "variance_estimation_psu_2022"
)

# ---------------------------------------------------------------------------
# Consolidated data  (2020 & 2022 only)
# ---------------------------------------------------------------------------

# %%

h224 <- load_meps_consolidated(
    dat_path = "data/consolidated_data/2020/h224.dat",
    spec_path = "setup/h224_spec.csv",
    rename_path = "setup/h224_renames.csv",
    survey_year = 2020,
    col_map = consolidated_cols_2020
)

h243 <- load_meps_consolidated(
    dat_path = "data/consolidated_data/2022/h243.dat",
    spec_path = "setup/h243_spec.csv",
    rename_path = "setup/h243_renames.csv",
    survey_year = 2022,
    col_map = consolidated_cols_2022
)

consolidated_stacked <- bind_rows(h224, h243)
rm(h224, h243)
gc()

# ---------------------------------------------------------------------------
# Join: keep only persons with at least one of the 10 target conditions
# Result: one row per condition per person per year
# ---------------------------------------------------------------------------

# %%

data_joined <- consolidated_stacked |>
    inner_join(
        conditions_stacked,
        by = c("person_id", "panel_number", "survey_year")
    )

rm(consolidated_stacked, conditions_stacked)
invisible(gc())

# ---------------------------------------------------------------------------
# Recode CCSR codes to human-readable condition labels in place
# ---------------------------------------------------------------------------
ccsr_to_condition <- c(
    # Diabetes
    END002 = "diabetes",
    END003 = "diabetes",
    END004 = "diabetes",
    END005 = "diabetes",
    END006 = "diabetes",
    # Hyperlipidemia
    END010 = "hyperlipidemia",
    # Hypertension
    CIR007 = "hypertension",
    CIR008 = "hypertension",
    # Mental health
    MBD001 = "mental_health",
    MBD002 = "mental_health",
    MBD003 = "mental_health",
    MBD004 = "mental_health",
    MBD005 = "mental_health",
    MBD006 = "mental_health",
    MBD007 = "mental_health",
    MBD008 = "mental_health",
    MBD009 = "mental_health",
    MBD010 = "mental_health",
    MBD011 = "mental_health",
    MBD012 = "mental_health",
    MBD013 = "mental_health",
    MBD014 = "mental_health",
    MBD017 = "mental_health",
    MBD018 = "mental_health",
    MBD019 = "mental_health",
    MBD020 = "mental_health",
    MBD021 = "mental_health",
    MBD022 = "mental_health",
    MBD023 = "mental_health",
    MBD024 = "mental_health",
    MBD025 = "mental_health",
    MBD026 = "mental_health",
    MBD027 = "mental_health",
    MBD028 = "mental_health",
    MBD029 = "mental_health",
    MBD030 = "mental_health",
    MBD031 = "mental_health",
    MBD032 = "mental_health",
    MBD033 = "mental_health",
    MBD034 = "mental_health",
    # Pulmonary disease
    RSP006 = "pulmonary_disease",
    RSP007 = "pulmonary_disease",
    RSP008 = "pulmonary_disease",
    RSP010 = "pulmonary_disease",
    RSP011 = "pulmonary_disease",
    RSP012 = "pulmonary_disease",
    RSP013 = "pulmonary_disease",
    RSP014 = "pulmonary_disease",
    RSP016 = "pulmonary_disease",
    # Arthritis
    MUS001 = "arthritis",
    MUS002 = "arthritis",
    MUS003 = "arthritis",
    MUS004 = "arthritis",
    MUS005 = "arthritis",
    MUS006 = "arthritis",
    MUS007 = "arthritis",
    # Back problems
    MUS011 = "back_problems",
    MUS038 = "back_problems",
    # Heart disease
    CIR001 = "heart_disease",
    CIR002 = "heart_disease",
    CIR003 = "heart_disease",
    CIR004 = "heart_disease",
    CIR005 = "heart_disease",
    CIR006 = "heart_disease",
    CIR010 = "heart_disease",
    CIR011 = "heart_disease",
    CIR012 = "heart_disease",
    CIR014 = "heart_disease",
    CIR015 = "heart_disease",
    CIR016 = "heart_disease",
    CIR017 = "heart_disease",
    CIR018 = "heart_disease",
    # Cerebrovascular disease
    NVS012 = "cerebrovascular_disease",
    CIR020 = "cerebrovascular_disease",
    CIR021 = "cerebrovascular_disease",
    CIR022 = "cerebrovascular_disease",
    CIR023 = "cerebrovascular_disease",
    CIR024 = "cerebrovascular_disease",
    CIR025 = "cerebrovascular_disease",
    # Asthma
    RSP009 = "asthma"
)

data_joined <- data_joined |>
    mutate(across(starts_with("ccsr_code"), ~ ccsr_to_condition[.]))

# ---------------------------------------------------------------------------
# Medicare sample: continuous 12-month coverage
# Keep only persons covered in all 12 months of the survey year
# ---------------------------------------------------------------------------
mcr_months <- c(
    "mcr_jan",
    "mcr_feb",
    "mcr_mar",
    "mcr_apr",
    "mcr_may",
    "mcr_jun",
    "mcr_jul",
    "mcr_aug",
    "mcr_sep",
    "mcr_oct",
    "mcr_nov",
    "mcr_dec"
)

data_medicare <- data_joined |>
    filter(if_all(all_of(mcr_months), ~ . == 1))

# ---------------------------------------------------------------------------
# Reshape to one row per person + clean
# ---------------------------------------------------------------------------

# Person-level columns (from consolidated) — used to drop condition columns
person_cols <- c(
    "person_id",
    "panel_number",
    "family_id",
    "sex",
    "race",
    "hispanic",
    "education",
    "bmi",
    "age",
    "region",
    "marital_status",
    "ever_medicare",
    mcr_months,
    "total_expenditure",
    "person_weight",
    "saq_weight",
    "variance_stratum",
    "variance_psu",
    "survey_year"
)

# Condition flags: one binary column per condition per person-year
condition_flags <- data_medicare |>
    select(person_id, survey_year, starts_with("ccsr_code")) |>
    pivot_longer(
        starts_with("ccsr_code"),
        values_to = "condition",
        names_to = NULL
    ) |>
    filter(!is.na(condition)) |>
    distinct() |>
    mutate(present = 1L) |>
    pivot_wider(names_from = condition, values_from = present, values_fill = 0L)

# Clean person-level data and join condition flags
data_clean <- data_medicare |>
    select(all_of(person_cols)) |>
    distinct(person_id, survey_year, .keep_all = TRUE) |>
    # 1. BMI: adult only, non-missing, plausible range
    filter(!is.na(bmi), between(bmi, 10, 100)) |>
    # 2. Expenditures: non-missing, strictly positive (Gamma family requires > 0)
    filter(!is.na(total_expenditure), total_expenditure > 0) |>
    # 3. Survey weights: non-missing, positive
    filter(!is.na(person_weight), person_weight > 0) |>
    # 5. Covariates: complete cases
    filter(
        !is.na(sex),
        !is.na(race),
        !is.na(hispanic),
        !is.na(education),
        !is.na(marital_status),
        !is.na(region)
    ) |>
    # Drop redundant monthly Medicare columns (all == 1 by construction)
    select(-all_of(mcr_months), -ever_medicare) |>
    inner_join(condition_flags, by = c("person_id", "survey_year")) |>
    # BMI categories — underweight collapsed into normal due to sparse counts
    # in the Medicare population (n < 200); not a target group for this study
    mutate(
        bmi_category = case_when(
            bmi < 25.0 ~ "normal_or_below",
            bmi < 30.0 ~ "overweight",
            bmi < 35.0 ~ "obese_class_i",
            bmi < 40.0 ~ "obese_class_ii",
            bmi >= 40.0 ~ "obese_class_iii"
        ),
        bmi_category = factor(
            bmi_category,
            levels = c(
                "normal_or_below",
                "overweight",
                "obese_class_i",
                "obese_class_ii",
                "obese_class_iii"
            )
        )
    )

rm(data_joined, data_medicare, condition_flags)
invisible(gc())

# Convert condition flag columns to logical so gtsummary treats them as
# binary categorical variables rather than continuous integers
condition_vars <- c(
    "diabetes",
    "hypertension",
    "hyperlipidemia",
    "mental_health",
    "pulmonary_disease",
    "arthritis",
    "back_problems",
    "heart_disease",
    "cerebrovascular_disease",
    "asthma"
)

data_clean <- data_clean |>
    mutate(across(any_of(condition_vars), as.logical))

data_clean <- data_clean |>
    mutate(
        sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female")),
        race = factor(
            race,
            levels = c(1, 2, 3, 4, 5, 6),
            labels = c(
                "White",
                "Black",
                "Am. Indian / Alaska Native",
                "Asian",
                "Nat. Hawaiian / Pacific Islander",
                "Multiple races"
            )
        ),
        hispanic = factor(hispanic, levels = c(1, 2), labels = c("Hispanic", "Not Hispanic")),
        marital_status = factor(
            marital_status,
            levels = c(1, 2, 3, 4, 5),
            labels = c("Married", "Widowed", "Divorced", "Separated", "Never married")
        ),
        region = factor(
            region,
            levels = c(1, 2, 3, 4),
            labels = c("Northeast", "Midwest", "South", "West")
        )
    )

# ---------------------------------------------------------------------------
# Survey design object
# MEPS uses a stratified clustered design — must account for this in all
# estimates and tests, otherwise standard errors will be wrong
# ---------------------------------------------------------------------------
svy_clean <- data_clean |>
    as_survey_design(
        ids = variance_psu,
        strata = variance_stratum,
        weights = person_weight,
        nest = TRUE
    )

# ---------------------------------------------------------------------------
# Table 1: descriptive statistics by BMI category with p-values
# Continuous: survey-weighted mean (SD)
# Categorical/binary: survey-weighted % (n)
# P-values: Rao-Scott chi-square (categorical), Wald F-test (continuous)
# ---------------------------------------------------------------------------
tbl_descriptive <- svy_clean |>
    tbl_svysummary(
        by = bmi_category,
        include = c(
            age,
            sex,
            race,
            hispanic,
            education,
            marital_status,
            region,
            total_expenditure,
            all_of(condition_vars)
        ),
        statistic = list(
            all_continuous() ~ "{mean} ({sd})",
            all_categorical() ~ "{p}% ({n})"
        ),
        digits = list(
            all_continuous() ~ 1,
            all_categorical() ~ c(1, 0)
        ),
        missing = "no",
        label = list(
            age ~ "Age (years)",
            sex ~ "Sex",
            race ~ "Race",
            hispanic ~ "Hispanic ethnicity",
            education ~ "Education (years)",
            marital_status ~ "Marital status",
            region ~ "Census region",
            total_expenditure ~ "Total expenditure ($)",
            diabetes ~ "Diabetes",
            hypertension ~ "Hypertension",
            hyperlipidemia ~ "Hyperlipidemia",
            mental_health ~ "Mental health condition",
            pulmonary_disease ~ "Pulmonary disease",
            arthritis ~ "Arthritis",
            back_problems ~ "Back problems",
            heart_disease ~ "Heart disease",
            cerebrovascular_disease ~ "Cerebrovascular disease",
            asthma ~ "Asthma"
        )
    ) |>
    add_p(
        test = list(
            all_continuous() ~ "svy.kruskal.test",
            all_categorical() ~ "svy.chisq.test"
        )
    ) |>
    add_overall() |>
    bold_labels() |>
    bold_p(t = 0.05)

tbl_descriptive

# ---------------------------------------------------------------------------
# Heatmap: survey-weighted condition prevalence by BMI category
# ---------------------------------------------------------------------------

# Calculate weighted prevalence (%) for each condition × BMI group
heatmap_data <- condition_vars |>
    map(\(cond) {
        svy_clean |>
            group_by(bmi_category) |>
            summarise(
                prevalence = survey_mean(
                    as.numeric(.data[[cond]]),
                    na.rm = TRUE
                ) *
                    100
            ) |>
            mutate(condition = cond)
    }) |>
    list_rbind() |>
    mutate(
        condition = str_replace_all(condition, "_", " ") |> str_to_title()
    )

# Plot
ggplot(heatmap_data, aes(x = bmi_category, y = condition, fill = prevalence)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(
        aes(label = sprintf("%.1f%%", prevalence)),
        size = 3,
        color = "black"
    ) +
    scale_fill_gradient(
        low = "#f7fbff",
        high = "#2166ac",
        name = "Prevalence (%)"
    ) +
    labs(
        title = "Condition Prevalence by BMI Category",
        subtitle = "Medicare beneficiaries, 2020 & 2022 | Survey-weighted estimates",
        x = "BMI Category",
        y = NULL
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "right"
    )

# ===========================================================================
# My analysis
# Survey-weighted spending gradient, adjusted Gamma GLM, and counterfactual
# savings simulation for obesity → spending reduction
# ===========================================================================

# %%

bmi_labels <- c(
    normal_or_below = "Normal /\nUnderweight",
    overweight      = "Overweight",
    obese_class_i   = "Obese\nClass I",
    obese_class_ii  = "Obese\nClass II",
    obese_class_iii = "Obese\nClass III"
)

# ---------------------------------------------------------------------------
# 1. Descriptive: survey-weighted mean expenditure by BMI category
# ---------------------------------------------------------------------------

# %%

spend_by_bmi <- svy_clean |>
    group_by(bmi_category) |>
    summarise(
        mean = survey_mean(total_expenditure, vartype = "ci", na.rm = TRUE)
    )

ggplot(spend_by_bmi, aes(x = bmi_category, y = mean)) +
    geom_col(fill = "#0F3F6D", width = 0.65) +
    geom_errorbar(
        aes(ymin = mean_low, ymax = mean_upp),
        width = 0.2, linewidth = 0.7, color = "gray40"
    ) +
    geom_text(
        aes(label = scales::dollar(round(mean, -2))),
        vjust = -0.9, size = 3.5, fontface = "bold"
    ) +
    scale_x_discrete(labels = bmi_labels) +
    scale_y_continuous(
        labels = scales::dollar_format(),
        expand = expansion(mult = c(0, 0.18))
    ) +
    labs(
        title    = "Healthcare Spending Rises With Obesity",
        subtitle = "Survey-weighted mean annual expenditure | Medicare beneficiaries, 2020 & 2022",
        x = NULL,
        y = "Mean annual expenditure"
    ) +
    theme_wsj(base_size = 11) +
    theme(
        axis.title.y  = element_text(size = 9),
        plot.subtitle = element_text(size = 9, color = "gray40")
    )

# ---------------------------------------------------------------------------
# 2. Gamma GLM: adjusted spending ratio by BMI category
#    Family: Gamma(link = "log") — standard for non-negative right-skewed
#    expenditure data; exponentiated coefficients = spending multipliers
#    Reference category: normal_or_below
# ---------------------------------------------------------------------------

# %%

model_spend <- survey::svyglm(
    total_expenditure ~ bmi_category + age + sex + race + hispanic +
        education + marital_status + region + as.factor(survey_year) +
        diabetes + hypertension + hyperlipidemia + mental_health +
        pulmonary_disease + arthritis + back_problems + heart_disease +
        cerebrovascular_disease + asthma,
    design = svy_clean,
    family  = Gamma(link = "log")
)

summary(model_spend)

# Exponentiated coefficients → spending ratio vs. normal_or_below
bmi_coefs <- tibble(
    term      = names(coef(model_spend)),
    estimate  = coef(model_spend),
    conf.low  = confint(model_spend)[, 1],
    conf.high = confint(model_spend)[, 2]
) |>
    filter(str_detect(term, "bmi_category")) |>
    mutate(
        across(c(estimate, conf.low, conf.high), exp),
        label = str_remove(term, "bmi_category") |>
            str_replace_all("_", " ") |>
            str_to_title()
    )

ggplot(bmi_coefs, aes(x = estimate, y = fct_reorder(label, estimate))) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.7) +
    geom_errorbarh(
        aes(xmin = conf.low, xmax = conf.high),
        height = 0.2, linewidth = 0.8, color = "gray40"
    ) +
    geom_point(size = 4, color = "#C41E3D") +
    geom_text(
        aes(label = sprintf("×%.2f", estimate)),
        hjust = -0.35, size = 3.5, fontface = "bold"
    ) +
    scale_x_continuous(
        limits = c(0.9, max(bmi_coefs$conf.high) * 1.18),
        labels = \(x) sprintf("×%.1f", x)
    ) +
    labs(
        title    = "Obesity Linked to Higher Spending After Adjustment",
        subtitle = "Adjusted spending ratio vs. Normal/Underweight | Gamma GLM, survey-weighted",
        x        = "Adjusted spending ratio (reference: Normal / Underweight)",
        y        = NULL
    ) +
    theme_wsj(base_size = 11) +
    theme(
        axis.title.x  = element_text(size = 9),
        plot.subtitle = element_text(size = 9, color = "gray40")
    )

# ---------------------------------------------------------------------------
# 3. Counterfactual simulation: predicted savings from stepping down one BMI class
#    Each obese person is assigned the next lower BMI category; all other
#    covariates held fixed. Difference in predicted spending = estimated savings.
# ---------------------------------------------------------------------------

# %%

bmi_levels <- levels(data_clean$bmi_category)

data_cf <- data_clean |>
    mutate(
        bmi_category = case_when(
            bmi_category == "obese_class_iii" ~ "obese_class_ii",
            bmi_category == "obese_class_ii"  ~ "obese_class_i",
            bmi_category == "obese_class_i"   ~ "overweight",
            TRUE ~ as.character(bmi_category)
        ) |>
            factor(levels = bmi_levels)
    )

pred_orig <- as.numeric(predict(model_spend, newdata = data_clean, type = "response"))
pred_cf   <- as.numeric(predict(model_spend, newdata = data_cf,    type = "response"))

savings_data <- data_clean |>
    mutate(
        spend_pred   = pred_orig,
        spend_cf     = pred_cf,
        spend_saving = spend_pred - spend_cf
    ) |>
    filter(bmi_category %in% c("obese_class_i", "obese_class_ii", "obese_class_iii")) |>
    group_by(bmi_category) |>
    summarise(
        mean_saving_pp = weighted.mean(spend_saving, w = person_weight),
        total_saving   = sum(spend_saving * person_weight),
        n_weighted     = sum(person_weight),
        .groups        = "drop"
    )

savings_labels <- c(
    obese_class_i   = "Obese Class I\n→ Overweight",
    obese_class_ii  = "Obese Class II\n→ Class I",
    obese_class_iii = "Obese Class III\n→ Class II"
)

# Per-person savings
ggplot(savings_data, aes(x = bmi_category, y = mean_saving_pp)) +
    geom_col(fill = "#C41E3D", width = 0.6) +
    geom_text(
        aes(label = scales::dollar(round(mean_saving_pp, -1))),
        vjust = -0.6, size = 4, fontface = "bold"
    ) +
    scale_x_discrete(labels = savings_labels) +
    scale_y_continuous(
        labels = scales::dollar_format(),
        expand = expansion(mult = c(0, 0.2))
    ) +
    labs(
        title    = "Predicted Annual Savings Per Person",
        subtitle = "Spending reduction from stepping down one BMI class | Medicare, 2020 & 2022",
        x        = NULL,
        y        = "Predicted savings per person (annual)"
    ) +
    theme_wsj(base_size = 11) +
    theme(
        axis.title.y  = element_text(size = 9),
        plot.subtitle = element_text(size = 9, color = "gray40")
    )

# Population-level total savings
ggplot(savings_data, aes(x = bmi_category, y = total_saving / 1e9)) +
    geom_col(fill = "#0F3F6D", width = 0.6) +
    geom_text(
        aes(label = sprintf("$%.1fB", total_saving / 1e9)),
        vjust = -0.6, size = 4, fontface = "bold"
    ) +
    scale_x_discrete(labels = savings_labels) +
    scale_y_continuous(
        labels = \(x) sprintf("$%.0fB", x),
        expand = expansion(mult = c(0, 0.2))
    ) +
    labs(
        title    = "Estimated Total Medicare Savings",
        subtitle = "Population-weighted annual savings | One BMI class reduction",
        x        = NULL,
        y        = "Total annual savings (billions)"
    ) +
    theme_wsj(base_size = 11) +
    theme(
        axis.title.y  = element_text(size = 9),
        plot.subtitle = element_text(size = 9, color = "gray40")
    )

# ===========================================================================
# Article analysis replication
# 2-Stage Residual Inclusion (2SRI) IV model following Cawley et al.
# Instrument: BMI of oldest child aged 11–45 in the same MEPS household
# Spending simulation: 5 / 10 / 15 / 20 / 25% BMI reductions
# data_clean / svy_clean are NOT modified — all IV objects are separate
# ===========================================================================

# %%

# ---------------------------------------------------------------------------
# Step 1: Column maps for broad family load (all persons, both years)
# These are different from consolidated_cols_2020/2022 — no Medicare filter
# is applied here; children and non-Medicare adults are kept for linking.
# ---------------------------------------------------------------------------

family_cols_2020 <- c(
    person_id    = "person_id_duid_pid",
    family_id    = "annual_family_identifier",
    panel_number = "panel_number",
    age          = "age_as_of_12_31_20_edited_imputed",
    bmi_adult    = "adult_body_mass_index_17_rd_4_2",
    bmi_child    = "child_s_body_mass_index_6_17_r4_2",
    family_size  = "ru_size_including_student_as_of_12_31_20",
    proxy        = "was_respondent_a_proxy_in_r4_2",
    dobmm        = "date_of_birth_month",
    dobyy        = "date_of_birth_year"
)

family_cols_2022 <- c(
    person_id    = "person_id_duid_pid",
    family_id    = "annual_family_identifier",
    panel_number = "panel_number",
    age          = "age_as_of_12_31_22_edited_imputed",
    bmi_adult    = "adult_body_mass_index_17_rd_4_2",
    bmi_child    = "child_s_body_mass_index_6_17_r4_2",
    family_size  = "ru_size_including_student_as_of_12_31_22",
    proxy        = "was_respondent_a_proxy_in_r4_2",
    dobmm        = "date_of_birth_month",
    dobyy        = "date_of_birth_year"
)

# ---------------------------------------------------------------------------
# Step 2: Load all persons from both consolidated files (no filtering)
# ---------------------------------------------------------------------------

# %%

family_2020 <- load_meps_consolidated(
    dat_path    = "data/consolidated_data/2020/H224.DAT",
    spec_path   = "setup/h224_spec.csv",
    rename_path = "setup/h224_renames.csv",
    survey_year = 2020,
    col_map     = family_cols_2020
)

family_2022 <- load_meps_consolidated(
    dat_path    = "data/consolidated_data/2022/h243.dat",
    spec_path   = "setup/h243_spec.csv",
    rename_path = "setup/h243_renames.csv",
    survey_year = 2022,
    col_map     = family_cols_2022
)

family_all <- bind_rows(family_2020, family_2022)
rm(family_2020, family_2022)
gc()

# ---------------------------------------------------------------------------
# Step 3: Build instrument table
# For each Medicare adult in data_clean, find the oldest family member aged
# 11–45 (excluding self) with a valid BMI. Children 11–17 use child BMI
# (CHBMIX42); adults 18–45 use adult BMI (ADBMI42).
# Child age in months = (survey_year − birth_year) × 12 − (birth_month − 12)
# ---------------------------------------------------------------------------

# %%

instrument_candidates <- family_all |>
    mutate(
        bmi_combined = case_when(
            age >= 11 & age <= 17 & !is.na(bmi_child) ~ bmi_child,
            age >= 18 & age <= 45 & !is.na(bmi_adult) ~ bmi_adult,
            TRUE ~ NA_real_
        ),
        child_age_months = (survey_year - dobyy) * 12 - (dobmm - 12)
    ) |>
    filter(!is.na(bmi_combined), age >= 11, age <= 45, !is.na(child_age_months)) |>
    select(
        family_id, panel_number, survey_year,
        child_person_id      = person_id,
        child_bmi_instrument = bmi_combined,
        child_age_months,
        child_age            = age
    )

instrument_table <- data_clean |>
    select(person_id, family_id, panel_number, survey_year) |>
    inner_join(instrument_candidates,
               by = c("family_id", "panel_number", "survey_year")) |>
    filter(person_id != child_person_id) |>
    group_by(person_id, survey_year) |>
    slice_max(child_age, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(person_id, survey_year, child_bmi_instrument, child_age_months)

# ---------------------------------------------------------------------------
# Step 4: Pull parent-level extra controls (family_size, proxy) from family_all
# These were not included in the original data_clean pipeline
# ---------------------------------------------------------------------------

parent_extra <- family_all |>
    semi_join(data_clean, by = c("person_id", "survey_year")) |>
    select(person_id, survey_year, family_size, proxy)

rm(family_all)
gc()

# ---------------------------------------------------------------------------
# Step 5: Build data_iv — completely separate from data_clean
# Inner join on instrument_table reduces n: only adults with a qualifying
# household child are retained (expected; noted for comparison with article)
# ---------------------------------------------------------------------------

# %%

data_iv <- data_clean |>
    inner_join(instrument_table, by = c("person_id", "survey_year")) |>
    left_join(parent_extra,      by = c("person_id", "survey_year")) |>
    filter(!is.na(family_size), !is.na(proxy)) |>
    mutate(
        race_eth = case_when(
            hispanic == "Hispanic"                       ~ "Hispanic",
            race == "White" & hispanic == "Not Hispanic" ~ "Non-Hispanic White",
            race == "Black" & hispanic == "Not Hispanic" ~ "Non-Hispanic Black",
            race == "Asian"                              ~ "Asian",
            TRUE                                         ~ "Other"
        ) |>
            factor(levels = c(
                "Non-Hispanic White", "Non-Hispanic Black",
                "Hispanic", "Asian", "Other"
            )),
        age_group = cut(
            age,
            breaks = c(0, 64, 69, 74, 79, 84, Inf),
            labels = c("<65", "65-69", "70-74", "75-79", "80-84", "85+"),
            right  = TRUE
        ),
        proxy_reported = factor(
            proxy,
            levels = c(1, 2),
            labels = c("Proxy", "Self")
        )
    )

cat("data_clean n =", nrow(data_clean), "\n")
cat("data_iv n    =", nrow(data_iv),    "(adults with qualifying child in-household)\n")

# ---------------------------------------------------------------------------
# Step 6: Survey design for IV sample
# ---------------------------------------------------------------------------

svy_iv <- data_iv |>
    as_survey_design(
        ids     = variance_psu,
        strata  = variance_stratum,
        weights = person_weight,
        nest    = TRUE
    )

# ---------------------------------------------------------------------------
# Step 7: 2SRI Stage 1 — survey-weighted linear regression of BMI on instrument
# F-statistic on child_bmi_instrument should exceed 10 (strong instrument rule)
# ---------------------------------------------------------------------------

# %%

stage1_model <- survey::svyglm(
    bmi ~ child_bmi_instrument + age_group + sex + race_eth + education +
        marital_status + region + family_size + proxy_reported +
        child_age_months + as.factor(survey_year),
    design = svy_iv,
    family = gaussian()
)

summary(stage1_model)

stage1_ftest <- survey::regTermTest(stage1_model, ~ child_bmi_instrument)
cat("Stage 1 F-statistic (instrument):", as.numeric(stage1_ftest$Ftest), "\n")

# Add residuals to data_iv and rebuild survey design
data_iv <- data_iv |>
    mutate(resid_stage1 = residuals(stage1_model))

svy_iv <- data_iv |>
    as_survey_design(
        ids     = variance_psu,
        strata  = variance_stratum,
        weights = person_weight,
        nest    = TRUE
    )

# ---------------------------------------------------------------------------
# Step 8: 2SRI Stage 2 — Gamma GLM with residual inclusion
# resid_stage1 absorbs the endogenous component of BMI (Terza et al. 2008)
# ---------------------------------------------------------------------------

# %%

model_2sri <- survey::svyglm(
    total_expenditure ~ bmi + resid_stage1 +
        age_group + sex + race_eth + education +
        marital_status + region + family_size + proxy_reported +
        child_age_months + as.factor(survey_year) +
        diabetes + hypertension + hyperlipidemia + mental_health +
        pulmonary_disease + arthritis + back_problems + heart_disease +
        cerebrovascular_disease + asthma,
    design = svy_iv,
    family = Gamma(link = "log")
)

summary(model_2sri)

# ---------------------------------------------------------------------------
# Step 9: BMI reduction simulation — 5 / 10 / 15 / 20 / 25%
# Residuals are held fixed (correct 2SRI counterfactual practice — the
# residual captures individual-level endogeneity, not the BMI level itself)
# ---------------------------------------------------------------------------

# %%

baseline_pred_iv <- as.numeric(
    predict(model_2sri, newdata = data_iv, type = "response")
)

bmi_pcts <- c(0.05, 0.10, 0.15, 0.20, 0.25)

sim_results <- map(bmi_pcts, \(pct) {
    data_sim <- data_iv |>
        mutate(bmi = bmi * (1 - pct))

    sim_pred <- as.numeric(predict(model_2sri, newdata = data_sim, type = "response"))

    tibble(
        bmi_reduction_pct = pct * 100,
        mean_saving_pp    = weighted.mean(baseline_pred_iv - sim_pred,
                                          w = data_iv$person_weight),
        total_saving_bil  = sum((baseline_pred_iv - sim_pred) *
                                    data_iv$person_weight) / 1e9
    )
}) |> list_rbind()

sim_results

# ===========================================================================
# Article replication visualizations
# ===========================================================================

# %%

# ---------------------------------------------------------------------------
# Plot 1: Instrument strength — child BMI vs. parent BMI scatter
# ---------------------------------------------------------------------------

ggplot(
    data_iv |> slice_sample(n = min(3000, nrow(data_iv))),
    aes(x = child_bmi_instrument, y = bmi)
) +
    geom_jitter(alpha = 0.25, size = 0.9, color = "#0F3F6D",
                width = 0.3, height = 0.3) +
    geom_smooth(method = "lm", color = "#C41E3D", linewidth = 1.2, se = TRUE) +
    labs(
        title    = "Child BMI Predicts Parent BMI",
        subtitle = "Instrument validity check | Medicare beneficiaries with child in household",
        x        = "Oldest child BMI (instrument)",
        y        = "Parent (Medicare adult) BMI"
    ) +
    theme_wsj(base_size = 11) +
    theme(
        axis.title    = element_text(size = 9),
        plot.subtitle = element_text(size = 9, color = "gray40")
    )

# ---------------------------------------------------------------------------
# Plot 2: Stage 1 instrument coefficient
# ---------------------------------------------------------------------------

stage1_coef <- tibble(
    term      = names(coef(stage1_model)),
    estimate  = coef(stage1_model),
    conf.low  = confint(stage1_model)[, 1],
    conf.high = confint(stage1_model)[, 2]
) |>
    filter(term == "child_bmi_instrument")

ggplot(stage1_coef, aes(x = estimate, y = "Child BMI → Parent BMI")) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.7) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.15, linewidth = 1, color = "gray40") +
    geom_point(size = 5, color = "#C41E3D") +
    geom_text(aes(label = sprintf("beta = %.3f", estimate)),
              vjust = -1.3, size = 3.5, fontface = "bold") +
    annotate("text",
             x     = stage1_coef$conf.high * 1.05,
             y     = 1,
             label = sprintf("F = %.1f", as.numeric(stage1_ftest$Ftest)),
             size  = 3.5, color = "#0F3F6D", fontface = "bold", hjust = 0) +
    labs(
        title    = "Stage 1: Child BMI Is a Strong Instrument",
        subtitle = "Unit increase in child BMI predicts unit increase in parent BMI",
        x        = "Stage 1 coefficient",
        y        = NULL
    ) +
    theme_wsj(base_size = 11) +
    theme(
        axis.title.x  = element_text(size = 9),
        plot.subtitle = element_text(size = 9, color = "gray40")
    )

# ---------------------------------------------------------------------------
# Plot 3 (renumbered): Per-person savings by BMI reduction level (line chart)
# ---------------------------------------------------------------------------

ggplot(sim_results, aes(x = bmi_reduction_pct, y = mean_saving_pp)) +
    geom_line(color = "#0F3F6D", linewidth = 1.2) +
    geom_point(size = 4, color = "#C41E3D") +
    geom_text(
        aes(label = scales::dollar(round(mean_saving_pp, -1))),
        vjust = -0.9, size = 3.5, fontface = "bold"
    ) +
    scale_x_continuous(
        breaks = bmi_pcts * 100,
        labels = \(x) paste0(x, "%")
    ) +
    scale_y_continuous(
        labels = scales::dollar_format(),
        expand = expansion(mult = c(0.05, 0.2))
    ) +
    labs(
        title    = "Greater BMI Reduction Yields Greater Savings",
        subtitle = "Predicted annual savings per person | 2SRI IV model, Medicare 2020 & 2022",
        x        = "BMI reduction",
        y        = "Mean savings per person (annual)"
    ) +
    theme_wsj(base_size = 11) +
    theme(
        axis.title    = element_text(size = 9),
        plot.subtitle = element_text(size = 9, color = "gray40")
    )

# ---------------------------------------------------------------------------
# Plot 5: Total Medicare population savings by reduction level (bar chart)
# ---------------------------------------------------------------------------

ggplot(sim_results, aes(x = factor(bmi_reduction_pct), y = total_saving_bil)) +
    geom_col(fill = "#0F3F6D", width = 0.65) +
    geom_text(
        aes(label = sprintf("$%.1fB", total_saving_bil)),
        vjust = -0.6, size = 4, fontface = "bold"
    ) +
    scale_x_discrete(labels = \(x) paste0(x, "%")) +
    scale_y_continuous(
        labels = \(x) sprintf("$%.0fB", x),
        expand = expansion(mult = c(0, 0.2))
    ) +
    labs(
        title    = "Estimated Total Medicare Savings From Obesity Reduction",
        subtitle = "Population-weighted annual savings | 2SRI IV model",
        x        = "BMI reduction",
        y        = "Total annual savings (billions)"
    ) +
    theme_wsj(base_size = 11) +
    theme(
        axis.title    = element_text(size = 9),
        plot.subtitle = element_text(size = 9, color = "gray40")
    )

# ===========================================================================
# Comparison
# Side-by-side results from "My analysis" (naive Gamma GLM, categorical
# BMI step-downs) vs. "Article analysis replication" (2SRI IV, % reductions)
# ===========================================================================

# %%

# ---------------------------------------------------------------------------
# Reshape both results to a shared long format for faceted plotting
# ---------------------------------------------------------------------------

comparison_savings <- bind_rows(
    savings_data |>
        mutate(
            method   = "My Analysis\n(Naive GLM)",
            scenario = case_match(
                as.character(bmi_category),
                "obese_class_i"   ~ "Class I\n-> Overweight",
                "obese_class_ii"  ~ "Class II\n-> Class I",
                "obese_class_iii" ~ "Class III\n-> Class II"
            ),
            savings_pp  = mean_saving_pp,
            savings_tot = total_saving / 1e9
        ) |>
        select(method, scenario, savings_pp, savings_tot),
    sim_results |>
        mutate(
            method      = "2SRI Replication\n(IV-Adjusted)",
            scenario    = paste0(bmi_reduction_pct, "%"),
            savings_pp  = mean_saving_pp,
            savings_tot = total_saving_bil
        ) |>
        select(method, scenario, savings_pp, savings_tot)
) |>
    mutate(
        method   = factor(method,
                          levels = c("My Analysis\n(Naive GLM)",
                                     "2SRI Replication\n(IV-Adjusted)")),
        scenario = factor(scenario, levels = unique(scenario))
    )

# ---------------------------------------------------------------------------
# Plot 1: Per-person savings — faceted, shared y-axis
# ---------------------------------------------------------------------------

# %%

ggplot(comparison_savings, aes(x = scenario, y = savings_pp, fill = method)) +
    geom_col(width = 0.65, show.legend = FALSE) +
    geom_text(
        aes(label = scales::dollar(round(savings_pp, -1))),
        vjust = -0.6, size = 3.2, fontface = "bold"
    ) +
    scale_fill_manual(values = c(
        "My Analysis\n(Naive GLM)"        = "#0F3F6D",
        "2SRI Replication\n(IV-Adjusted)" = "#C41E3D"
    )) +
    scale_y_continuous(
        labels = scales::dollar_format(),
        expand = expansion(mult = c(0, 0.22))
    ) +
    facet_wrap(~ method, scales = "free_x", nrow = 1) +
    labs(
        title    = "Predicted Per-Person Savings: Two Methods Compared",
        subtitle = "Left: categorical BMI step-downs (naive GLM) | Right: % BMI reductions (2SRI IV)",
        x        = NULL,
        y        = "Mean annual savings per person"
    ) +
    theme_wsj(base_size = 10) +
    theme(
        axis.title.y  = element_text(size = 9),
        plot.subtitle = element_text(size = 8, color = "gray40"),
        strip.text    = element_text(face = "bold", size = 9)
    )

# ---------------------------------------------------------------------------
# Plot 2: Population-level savings — faceted, shared y-axis
# ---------------------------------------------------------------------------

# %%

ggplot(comparison_savings, aes(x = scenario, y = savings_tot, fill = method)) +
    geom_col(width = 0.65, show.legend = FALSE) +
    geom_text(
        aes(label = sprintf("$%.1fB", savings_tot)),
        vjust = -0.6, size = 3.2, fontface = "bold"
    ) +
    scale_fill_manual(values = c(
        "My Analysis\n(Naive GLM)"        = "#0F3F6D",
        "2SRI Replication\n(IV-Adjusted)" = "#C41E3D"
    )) +
    scale_y_continuous(
        labels = \(x) sprintf("$%.0fB", x),
        expand = expansion(mult = c(0, 0.22))
    ) +
    facet_wrap(~ method, scales = "free_x", nrow = 1) +
    labs(
        title    = "Estimated Total Medicare Savings: Two Methods Compared",
        subtitle = "Left: categorical BMI step-downs (naive GLM) | Right: % BMI reductions (2SRI IV)",
        x        = NULL,
        y        = "Total annual savings (billions)"
    ) +
    theme_wsj(base_size = 10) +
    theme(
        axis.title.y  = element_text(size = 9),
        plot.subtitle = element_text(size = 8, color = "gray40"),
        strip.text    = element_text(face = "bold", size = 9)
    )

# ---------------------------------------------------------------------------
# Plot 3: 2SRI BMI coefficient — spending multiplier per unit BMI increase
# (model_spend uses categorical bmi_category so a single-coefficient
#  comparison across models is not applicable here)
# ---------------------------------------------------------------------------

# %%

iv_bmi_coef <- tibble(
    term      = names(coef(model_2sri)),
    estimate  = coef(model_2sri),
    conf.low  = confint(model_2sri)[, 1],
    conf.high = confint(model_2sri)[, 2]
) |>
    filter(term == "bmi") |>
    mutate(across(c(estimate, conf.low, conf.high), exp))

ggplot(iv_bmi_coef, aes(x = estimate, y = "2SRI (IV-Adjusted)")) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.7) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.15, linewidth = 1.1, color = "gray40") +
    geom_point(size = 5, color = "#C41E3D") +
    geom_text(
        aes(label = sprintf("x%.4f", estimate)),
        vjust = -1.3, size = 3.5, fontface = "bold"
    ) +
    labs(
        title    = "2SRI: Spending Multiplier Per Unit BMI Increase",
        subtitle = "Exponentiated continuous BMI coefficient | Gamma GLM, IV-adjusted",
        x        = "Spending ratio per unit BMI (exponentiated; reference = 1.0)",
        y        = NULL
    ) +
    theme_wsj(base_size = 11) +
    theme(
        axis.title.x  = element_text(size = 9),
        plot.subtitle = element_text(size = 9, color = "gray40")
    )
