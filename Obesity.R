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
    # 2. Expenditures: non-missing, non-negative
    filter(!is.na(total_expenditure), total_expenditure >= 0) |>
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
data_clean <- data_clean |>
    mutate(
        bmi_category = fct_collapse(
            bmi_category,
            "normal_or_below" = c("underweight", "normal")
        )
    )
