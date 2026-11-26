# 02_state_single_age_race_nativity_cod.R
# Example 2: State-level table by sex, single-year age (top-coded at 100+),
# race/ethnicity (single/multiple race), nativity (US-born / US Territory /
# Foreign-born), and 27 cause-of-death groups (incl. ADRD & COVID-19).

library(nvssread)

# --- User configuration (edit these paths for real restricted server data) ---
DATA_DIR <- file.path("inst", "examples", "synthetic_data")
YEARS    <- c(2019L, 2022L)
OUT_DIR  <- file.path("inst", "examples", "outputs", "example_02_state_race_nativity_cod")

# --- Run tabulation ---
saved_files <- nvss_tabulate(
  data_dir       = DATA_DIR,
  years          = YEARS,
  vars           = c("state", "sex", "age", "race_eth", "nativity", "cod"),
  age_type       = "single",
  age_top        = 100L,
  race_type      = "single_multi",
  redistribute   = TRUE,
  complete_grid  = FALSE,
  geo_filter     = "residence",
  output_dir     = OUT_DIR,
  output_pattern = "02_state_single_age_race_nativity_cod_{year}.csv"
)

# --- Preview 2022 output (includes COVID-19 and NH Multiple/Other) ---
preview <- data.table::fread(saved_files[[length(saved_files)]], colClasses = list(character = "state_FIPS"))
message("\nPreview of ", basename(saved_files[[length(saved_files)]]), " (first 10 rows):")
print(head(preview, 10L))