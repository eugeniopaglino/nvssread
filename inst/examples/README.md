# nvssread Examples

These scripts demonstrate how to call `nvss_tabulate()` for common demographic
and cause-of-death tabulations and show what the resulting CSV tables look like.

## Quick Start (Synthetic Data)

You do **not** need access to restricted NVSS files to run these examples. From
the package root directory, run:

```r
source("inst/examples/run_all_examples.R")
```

This creates tiny, synthetic fixed-width NVSS files in inst/examples/synthetic_data/ (for years 2019 and 2022), runs all three example scripts, and saves the resulting CSVs inside inst/examples/outputs/.

## Running on Restricted Server Data

To run any script on real NVSS data, edit DATA_DIR, YEARS, and OUT_DIR at the top of the script. Never save or commit outputs from real restricted NVSS files inside this repository.

##Running on Restricted Server Data

To run any script on real NVSS data, edit DATA_DIR, YEARS, and OUT_DIR at the top of the script. Never save or commit outputs from real restricted NVSS files inside this repository.

## Scripts Included

| Script | Table Specification | Output Folder |
| --- | --- | --- |
| `00_generate_synthetic_nvss.R` | Builds synthetic 2019 & 2022 fixed-width files with realistic edge cases (missing values, foreign residents, territories, ADRD, COVID-19, alcohol-induced `X65`, multiracial codes). | `synthetic_data/` |
| `01_county_monthly_abridged.R` | Monthly county × sex × abridged age (`85+`), complete grid, redistributed. | `outputs/example_01_county_monthly/` |
| `02_state_single_age_race_nativity_cod.R` | State × sex × single-year age (`100+`) × race/ethnicity (`single_multi`) × nativity × cause of death, redistributed. | `outputs/example_02_state_race_nativity_cod/` |
| `03_state_bridged_raw.R` | State × sex × abridged age (`95+`) × bridged race × cause of death, **un-redistributed** (`redistribute = FALSE`) to inspect missingness. | `outputs/example_03_state_bridged_raw/` |