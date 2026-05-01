# Obesity.R
# Replication of: "Health Care Spending Associated with Weight Loss"
# Medicare population only | Survey years: 2020 & 2022
# Note: MEPS BMI (ADBMI42) is only collected in even years (from 2018 onward)

library(tidyverse)
library(tidymodels)
library(ggthemes)
library(skimr)
library(srvyr)

source("functions.R")

# ---------------------------------------------------------------------------
# CCSR codes for the 10 target clinical conditions
# ---------------------------------------------------------------------------
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
data_joined <- consolidated_stacked |>
    inner_join(
        conditions_stacked,
        by = c("person_id", "survey_year")
    )

rm(consolidated_stacked, conditions_stacked)
gc()
