# 01_county_monthly_abridged.R
# Example 1: Monthly county x sex x abridged age (85+) table with complete grid
# and sequential redistribution of missing values (reproduces the starter script).

library(nvssread)

# --- User configuration (edit these paths for real restricted server data) ---
DATA_DIR <- file.path("inst", "examples", "synthetic_data")
YEARS    <- c(2019L, 2022L)
OUT_DIR  <- file.path("inst", "examples", "outputs", "example_01_county_monthly")

# --- Run tabulation ---
saved_files <- nvss_tabulate(
  data_dir       = DATA_DIR,
  years          = YEARS,
  vars           = c("month", "state", "county", "sex", "age"),
  age_type       = "abridged",
  age_top        = 85L,
  redistribute   = TRUE,
  complete_grid  = TRUE,
  geo_filter     = "residence",
  output_dir     = OUT_DIR,
  output_pattern = "01_mort_table_{year}.csv"
)

# --- Preview the first saved CSV ---
preview <- data.table::fread(saved_files[[1L]], colClasses = list(character = c("state_FIPS", "county_FIPS", "FIPS_code")))
message("\nPreview of ", basename(saved_files[[1L]]), " (first 10 non-zero rows):")
print(head(preview[nDx > 0], 10L))