# 03_state_bridged_raw.R
# Example 3: Un-redistributed bridged-race table (pre-2020) with abridged age
# top-coded at 95+. Demonstrates inspecting raw missingness (NA rows) before
# choosing whether to redistribute.

library(nvssread)

# --- User configuration (edit these paths for real restricted server data) ---
DATA_DIR <- file.path("inst", "examples", "synthetic_data")
YEARS    <- 2019L
OUT_DIR  <- file.path("inst", "examples", "outputs", "example_03_state_bridged_raw")

# --- Run tabulation (also return data.table in memory for inspection) ---
dt_raw <- nvss_tabulate(
  data_dir       = DATA_DIR,
  years          = YEARS,
  vars           = c("state", "sex", "age", "race_eth", "cod"),
  age_type       = "abridged",
  age_top        = 95L,
  race_type      = "bridged",
  redistribute   = FALSE,
  complete_grid  = FALSE,
  geo_filter     = "residence",
  output_dir     = OUT_DIR,
  output_pattern = "03_state_bridged_raw_{year}.csv",
  return_data    = TRUE
)

# --- Inspect rows with missing values retained ---
message("\nSample rows with NA values (because redistribute = FALSE):")
print(head(dt_raw[is.na(age) | is.na(race_eth)], 10L))