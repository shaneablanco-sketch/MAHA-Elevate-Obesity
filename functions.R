# functions.R
# Requires: tidyverse, here
# Loaded by Obesity.R via source("functions.R")

# ---------------------------------------------------------------------------
# load_meps_file()
#   Reads a MEPS fixed-width .dat file using a spec CSV (start, end, name)
#   and optionally renames columns using a rename CSV (old_name, new_name).
# ---------------------------------------------------------------------------
load_meps_file <- function(dat_path, spec_path, rename_path = NULL) {
    stopifnot(
        "dat_path does not exist" = file.exists(dat_path),
        "spec_path does not exist" = file.exists(spec_path)
    )

    spec <- read_csv(spec_path, col_types = "iic", show_col_types = FALSE)

    df <- read_fwf(
        dat_path,
        col_positions = fwf_positions(
            start = spec$start,
            end = spec$end,
            col_names = spec$name
        ),
        col_types = cols(.default = col_character()),
        na = c("", "-1", "-7", "-8", "-9", "-15"),
        show_col_types = FALSE
    )

    if (!is.null(rename_path)) {
        stopifnot("rename_path does not exist" = file.exists(rename_path))
        renames <- read_csv(
            rename_path,
            col_types = "cc",
            show_col_types = FALSE
        )
        # rename() expects c(new_name = "old_name") — setNames builds that vector
        rename_vec <- setNames(renames$old_name, renames$new_name)
        rename_vec <- rename_vec[rename_vec %in% names(df)]
        df <- rename(df, !!!rename_vec)
    }

    df
}

# ---------------------------------------------------------------------------
# load_meps_conditions()
#   Wraps load_meps_file() for a MEPS conditions data file:
#     - filters rows to target CCSR codes
#     - tags each row with survey_year
# ---------------------------------------------------------------------------
load_meps_conditions <- function(
    dat_path,
    spec_path,
    rename_path,
    target_ccsr,
    ccsr_cols,
    survey_year
) {
    stopifnot(
        "ccsr_cols must be a character vector" = is.character(ccsr_cols),
        "survey_year must be a single integer" = length(survey_year) == 1
    )

    load_meps_file(dat_path, spec_path, rename_path) |>
        filter(if_any(all_of(ccsr_cols), ~ . %in% target_ccsr)) |>
        mutate(survey_year = as.integer(survey_year))
}

# ---------------------------------------------------------------------------
# load_meps_consolidated()
#   Wraps load_meps_file() for a MEPS consolidated data file:
#     - selects and renames to year-agnostic names in one step via col_map
#     - converts all columns to numeric
#     - tags each row with survey_year
#
#   col_map: named character vector where
#     names()  = desired year-agnostic column name  (e.g. "age")
#     values   = post-rename column name             (e.g. "age_as_of_12_31_20_edited_imputed")
#
#   select(!!!col_map) both selects and renames in a single dplyr step.
# ---------------------------------------------------------------------------
load_meps_consolidated <- function(
    dat_path,
    spec_path,
    rename_path,
    survey_year,
    col_map
) {
    stopifnot(
        "col_map must be a named character vector" = !is.null(names(col_map)),
        "survey_year must be a single integer" = length(survey_year) == 1
    )

    load_meps_file(dat_path, spec_path, rename_path) |>
        select(!!!col_map) |>
        mutate(across(-c(person_id, family_id), as.numeric)) |>
        mutate(survey_year = as.integer(survey_year))
}
