# run_all_examples.R
# Convenience runner: generates synthetic NVSS files and executes all examples,
# storing sample CSV results under inst/examples/outputs/.

PACKAGE_DIR <- "C:/Users/epaglino/OneDrive - University of Helsinki/projects/nvssread/"

source(file.path(PACKAGE_DIR, "inst", "examples", "00_generate_synthetic_nvss.R"))
source(file.path(PACKAGE_DIR, "inst", "examples", "01_county_monthly_abridged.R"))
source(file.path(PACKAGE_DIR, "inst", "examples", "02_state_single_age_race_nativity_cod.R"))
source(file.path(PACKAGE_DIR, "inst", "examples", "03_state_bridged_raw.R"))

message("\nAll examples completed. CSV outputs saved under: inst/examples/outputs/")
