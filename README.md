<!-- badges: start -->
[![R-CMD-check](https://github.com/eugeniopaglino/nvssread/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/eugeniopaglino/nvssread/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# nvssread

`nvssread` reads fixed-width National Vital Statistics System (NVSS) All-County mortality microdata files (**1989 onwards**), cleans and harmonizes demographic, geographic, and cause-of-death (ICD-9 and ICD-10) variables, optionally harmonizes longitudinal county FIPS boundaries and performs sequential proportional redistribution of missing values, and exports one tabulated CSV file per year.

The package is built around a single main function `nvss_tabulate()` so researchers without direct server access can send an exact function call to someone who can access the server to generate reproducible death tables.

---

## Installation

Install directly from the local package folder:

```r
install.packages("path/to/nvssread", repos = NULL, type = "source")

# Or using devtools (recommended during development):
devtools::install("path/to/nvssread")
devtools::test("path/to/nvssread")
```

**Dependencies:** `data.table`, `glue`, `readr` (all installed automatically if missing).

---

## Expected Input Directory Layout

By default, `nvss_tabulate()` expects annual fixed-width files arranged inside `data_dir` using the following naming pattern:

```text
data_dir/
├── mort1989/
│   └── MULT1989USAllCnty.txt
├── mort1990/
│   └── MULT1990USAllCnty.txt
...
└── mort2024/
    └── MULT2024USAllCnty.txt
```

Some renaming might be needed for this to work or you can override `file_pattern` using `{year}` as a placeholder (e.g., `file_pattern = "MULT{year}USAllCnty.txt"`). This can become tedious though, so doing the renaming once might be easier in the long-run.

---

## Quick Start

```r
library(nvssread)

nvss_tabulate(
  data_dir           = "/path/to/restricted/nvss",
  years              = 2012:2024,
  vars               = c("state", "sex", "age", "race_eth", "nativity", "cod"),
  age_type           = "abridged",
  age_top            = 85,
  race_type          = "single_multi",
  race_groups        = "detailed",
  harmonize_counties = FALSE,
  redistribute       = TRUE,
  output_dir         = "/path/to/outputs/my_table"
)
```

**Exported functions:**
- `nvss_tabulate()` — main reader, cleaner, redistributor, and annual CSV exporter.
- `nvss_cod_groups()` — returns the 27 harmonized cause-of-death category names.
- `nvss_county_crosswalk()` — returns the longitudinal US county FIPS crosswalk (`1989–present`) used when `harmonize_counties = TRUE`, so researchers can apply identical county groupings to Census/population denominator files.

---

## `nvss_tabulate()` Arguments

| Argument | Default | Description |
|---|---|---|
| `data_dir` | *(required)* | Base directory containing NVSS fixed-width files. |
| `years` | *(required)* | Integer vector of years to process (`>= 1989`). |
| `vars` | `c("state", "county", "sex", "age")` | Subset of variables to tabulate by (see table below). `year` is always included. |
| `age_type` | `"abridged"` | `"abridged"` (`00-00`, `01-04`, 5-year groups) or `"single"` (single years of age). |
| `age_top` | `NULL` | Open-ended age threshold. Defaults to `85` for `"abridged"` (must be a multiple of 5) and `100` for `"single"`. Pass `Inf` or `NA` to disable top-coding for single-year age. |
| `race_type` | `"single_multi"` | `"single_multi"` (keeps `"NH Multiple/Other"` separate via Race Recode 40 for 2012+) or `"bridged"` (bridged/single-race categories for 1989–2020). |
| `race_groups` | `"detailed"` | Grouping scheme when `"race_eth"` is requested: `"detailed"` (all 5–6 groups), `"wbh"` (`NH White`, `NH Black`, `Hispanic`, `NH Other`), or `"bw"` (`White`, `Black`, `Other`, including Hispanics). |
| `harmonize_counties` | `FALSE` | If `TRUE`, applies `nvss_county_crosswalk()` to harmonize historical county FIPS changes across 1989–present (including combining all Connecticut counties and 2022+ planning regions into `"09900"`). |
| `redistribute` | `TRUE` | If `TRUE`, sequentially redistributes missing values proportionally with coarser-stratum fallback. Missing counts are always printed to the console regardless of this setting. |
| `complete_grid` | `FALSE` | If `TRUE`, expands the table to include zero-death rows (`nDx = 0`) for all combinations of observed levels (preserving only observed state–county pairs). Keep `FALSE` for high-dimensional tables to avoid huge CSVs. |
| `geo_filter` | `"residence"` | Restricts records to the 50 US states + DC by `"residence"` (default), `"occurrence"`, or `"both"`. |
| `output_dir` | `NULL` | Directory where annual CSVs are saved (created automatically if needed). If `NULL`, no files are written. |
| `output_pattern` | `"nvss_deaths_{year}.csv"` | Filename template inside `output_dir`. |
| `file_pattern` | `"mort{year}/MULT{year}USAllCnty.txt"` | Relative input file template inside `data_dir`. |
| `return_data` | `is.null(output_dir)` | Whether to return the combined `data.table` in memory. Defaults to `FALSE` when writing CSVs to keep memory low. |

---

## Available Tabulation Variables (`vars`)

Pass any combination of these names to `vars`:

| `vars` value | Output column(s) | Categories / Coding | Missing (`NA`) handling |
|---|---|---|---|
| *(always on)* | `year` | 4-digit integer data year (2-digit years in 1989–1995 files are normalized to `19xx`) | — |
| `"month"` | `month` | Integer `1`–`12` | Out-of-range / blank → `NA` |
| `"state"` | `state_FIPS` | 2-digit character FIPS (`"01"`–`"56"`, 50 states + DC; read from numeric FIPS in 1989–2002 and postal abbreviations in 2003+) | Non-US / unknown → `NA` (excluded when `geo_filter = "residence"`) |
| `"county"` | `state_FIPS`, `county_FIPS`, `FIPS_code` | Automatically adds `"state"`. `county_FIPS` is 3-digit within state; `FIPS_code` is 5-digit `state_FIPS + county_FIPS`. | `"000"`, `"999"`, blank → `NA` |
| `"sex"` | `sex` | `"Male"`, `"Female"` (handles `1`/`2` in 1989–2002 and `M`/`F` in 2003+) | Anything else → `NA` |
| `"age"` | `age` | **Abridged:** `"00-00"`, `"01-04"`, `"05-09"`, ..., `"{age_top}+"` (default `"85+"`). **Single:** `"0"`, `"1"`, ..., `"{age_top}+"` (default `"100+"`). Handles 3-digit Detail Age (1989–2002, incl. centenarians `1`+`00–99`) and 4-digit Detail Age (2003+); infant units map to `0`. | Unit `9` or value `999` with unit `1` → `NA` |
| `"race_eth"` | `race_eth` | `"Hispanic"`, `"NH White"`, `"NH Black"`, `"NH API"`, `"NH AIAN"`, and (under `single_multi` in 2012+ or code `09` in 1989–1991) `"NH Multiple/Other"` (or collapsed per `race_groups`). | Unknown Hispanic origin (`99` in 1989–2002; `996`–`999` in 2003+) or missing race → `NA` |
| `"nativity"` | `nativity` | `"US-born"` (50 states + DC), `"US Territory"` (`PR`, `VI`, `GU`, `AS`, `MP`), `"Foreign-born"` (all other countries). Handles numeric NCHS birth-state codes (1989–2002) and postal recodes (2003+). | `"99"` (1989–2002), `"ZZ"` (2003+), or blank → `NA` |
| `"cod"` | `cod` | 27 mutually exclusive cause-of-death groups harmonized across ICD-9 (1989–1998) and ICD-10 (1999+). Run `nvss_cod_groups()` to list them. | Blank or invalid ICD code → `NA` |

The death count column is always named **`nDx`** (numeric; may contain fractional values when `redistribute = TRUE`).

---

## Methodological Notes

### 1. Race/Ethnicity Harmonization (1989–2024+)

- **Hispanic precedence:** Under `race_groups = "detailed"` and `"wbh"`, anyone with Hispanic origin (`01`–`05` in 1989–2002; `200`–`299` in 2003+) is classified as `"Hispanic"`, regardless of race. All other groups are strictly Non-Hispanic (`00` in 1989–2002; `100`–`199` in 2003+). Records with unknown Hispanic origin (`99` or `996`–`999`) are coded as `NA`. Under `race_groups = "bw"`, Hispanic origin is ignored (`White`, `Black`, `Other`).
- **Asian & Pacific Islander combined:** Asian and Native Hawaiian/Other Pacific Islander (NHOPI) are combined into `"NH API"`, including multiracial Asian + NHOPI (`Race Recode 40` code `23`). Note that CDC WONDER's `D158` (`Single Race 6`) places code `23` into `"More than one race"`, so pooling `NH API` and `NH Multiple/Other` reconciles counts to zero difference.
- **Choosing `race_type`:**
  - `"single_multi"` *(default)* uses **Race Recode 40** (available from **2012 onwards**; nationally complete across all states from **2018 onwards**) to separate `"NH Multiple/Other"`. If years `< 2012` are included, the function emits a warning and falls back to single/bridged race for those years.
  - `"bridged"` uses NCHS bridged/single race (`1989`–`2020`). Because NCHS discontinued bridged race after 2020, requesting `race_type = "bridged"` for any year `>= 2020` emits a warning and falls back to Race Recode 40 whenever bridged fields are blank.
- **Pre-1997 Hispanic reporting & 1989–1991 race code `09`:** Prior to 1997, a few states did not report Hispanic origin on death certificates (e.g., Oklahoma through 1996, New Hampshire through 1992, Louisiana through 1990), so decedents from those states have missing `race_eth` (`NA`) under `"detailed"` or `"wbh"`. `nvss_tabulate()` warns automatically when `< 1997` is requested with Hispanic-stratified groups; use `race_groups = "bw"` for complete pre-1997 coverage. In 1989–1991 (prior to NCHS imputation in 1992), race code `09` (*All other races*) maps to `"NH Multiple/Other"` (or `"Other"` under `"bw"`).

### 2. Missing-Value Reporting & Sequential Redistribution

- Every run prints the exact count and percentage of missing records per requested variable before aggregation.
- When `redistribute = TRUE`, missing values are redistributed sequentially in the order:
  `month` → `sex` → `county_FIPS` → `cod` → `age` → `race_eth` → `nativity`.
- **Zero-denominator fallback:** If a stratum with missing values has zero known deaths for the variable being redistributed (`D == 0`), weights are automatically borrowed from progressively coarser strata (dropping `county_FIPS` → `month` → `cod` → `nativity` → `race_eth` → `age` → `sex` → `state_FIPS`) while keeping the redistributed deaths in their original non-missing stratum. When redistributing `county_FIPS`, `state_FIPS` is never dropped so unknown-county deaths stay within their state of residence. Total deaths are strictly conserved.

### 3. Longitudinal County FIPS Harmonization (`1989–present`)

When `harmonize_counties = TRUE`, `nvss_tabulate()` applies `nvss_county_crosswalk()` to reconcile historical US county FIPS renames, merges, splits, and reorganizations:
- **1-to-1 renames:** Dade FL (`12025` → `12086`), Shannon SD (`46113` → `46102`), Wade Hampton AK (`02270` → `02158`).
- **Absorbed cities/counties:** Virginia independent cities reverted to surrounding counties (`51780` → `51083`, `51560` → `51005`, `51515` → `51019`), Yellowstone National Park MT (`30113` → `30031`), and Kalawao HI (`15005` → `15009`).
- **Multi-county clusters:** All 8 legacy Connecticut counties and all 9 post-2022 planning regions are combined statewide into `09900`; Adams/Boulder/Broomfield/Jefferson/Weld CO are combined into `08901`; and Alaska census-area reorganizations are collapsed into consistent longitudinal units (including Southeast Alaska `02901`).

### 4. Cause-of-Death Classification (ICD-9 & ICD-10, 27 Groups)

Underlying cause of death (`ICD-9` at positions 142–145 for 1989–1998; `ICD-10` at 142–145 for 1999–2002 and 146–149 for 2003+) is classified into 27 mutually exclusive categories with strict precedence:

1. **Alcohol-induced** is assigned **first** and excluded from all other groups (including alcohol-related suicide `X65` in ICD-10).
2. **COVID-19** (`U07.1`) is broken out as its own category (2020+).
3. **ADRD** replaces Alzheimer's disease and is removed from *Mental disorders* and *Nervous system*.
4. **Drug overdose** is excluded from *Homicide*, *Suicide*, and *Other external causes*.
5. **Other external causes** is the strict residual of external causes (`E800–E999` in ICD-9; `V00–Y89` in ICD-10) after excluding *Alcohol-induced*, *Drug overdose*, *Homicide*, *Suicide*, and *Transport accidents*.

*(In 1989–1998 NCHS tapes, leading `"E"` characters on external causes and leading `"*"` characters on HIV `042–044` are omitted on tape; `nvssread` handles both raw tape strings and standard E-prefixed/dotted codes. When requested years span 1998–1999, `nvss_tabulate()` notes the ICD-9 to ICD-10 transition.)*

| # | Category (`cod`) | ICD-9 Codes (1989–1998) | ICD-10 Codes (1999+) |
|---|---|---|---|
| 1 | `HIV/AIDS` | `042–044` | `B20–B24` |
| 2 | `Other infectious and parasitic diseases` | `001–139`, excl. `042–044` | `A00–B99`, excl. `B20–B24` |
| 3 | `Liver cancer` | `155` | `C22` |
| 4 | `Lung cancer` | `162` | `C33–C34` |
| 5 | `Breast, prostate, colorectal, and cervical cancer` | `153–154`, `174–175`, `180`, `185` | `C18–C21`, `C50`, `C53`, `C61` |
| 6 | `All other cancers` | `140–239`, excl. above cancer groups | `C00–D48`, excl. above cancer groups |
| 7 | `Mental disorders` | `290–319`, excl. `290`, `291`, `294.1`, `303`, `305.0` | `F00–F99`, excl. `F01–F03`, `F10` |
| 8 | `Nervous system, excl. ADRD` | `320–389`, excl. `331.0–331.2`, `331.8`, `357.5` | `G00–G98`, excl. `G30`, `G31.0`, `G31.1`, `G31.2`, `G31.8`, `G62.1`, `G72.1` |
| 9 | `ADRD` | `290`, `294.1`, `331.0–331.2`, `331.8` | `F01–F03`, `G30`, `G31.0`, `G31.1`, `G31.8` |
| 10 | `Hypertensive heart disease` | `401–405` | `I10–I15` |
| 11 | `Circulatory diseases, excl. Hypertensive heart disease` | `390–459`, excl. `401–405`, `425.5` | `I00–I99`, excl. `I10–I15`, `I42.6` |
| 12 | `Respiratory diseases excl. Influenza and pneumonia` | `460–519`, excl. `480–487` | `J00–J98`, excl. `J09–J18` |
| 13 | `Influenza and pneumonia` | `480–487` | `J09–J18` |
| 14 | `Diseases of the digestive system` | `520–579`, excl. `535.3`, `571.0–571.3` | `K00–K92`, excl. `K29.2`, `K70`, `K85.2`, `K86.0` |
| 15 | `Diabetes` | `250` | `E10–E14` |
| 16 | `Endocrine, nutritional, and metabolic diseases excl. Diabetes` | `240–279`, excl. `250` | `E00–E88`, excl. `E10–E14`, `E24.4` |
| 17 | `Diseases of the genitourinary system` | `580–629` | `N00–N98` |
| 18 | `Perinatal conditions and congenital anomalies` | `740–779` | `P00–P96`, `Q00–Q99` |
| 19 | `Homicide` | `E960–E969`, excl. `E962.0` | `X86–Y09`, `Y87.1` (excl. drug homicide `X85`) |
| 20 | `Suicide` | `E950–E959`, excl. `E950.0–E950.5` | `X66–X84`, `Y87.0` (excl. `X60–X64`, `X65`) |
| 21 | `Drug overdose` | `E850–E858`, `E950.0–E950.5`, `E962.0`, `E980.0–E980.5` | `X40–X44`, `X60–X64`, `X85`, `Y10–Y14` |
| 22 | `Alcohol-induced` | `291`, `303`, `305.0`, `357.5`, `425.5`, `535.3`, `571.0–571.3`, `790.3`, `E860` | `E24.4`, `F10`, `G31.2`, `G62.1`, `G72.1`, `I42.6`, `K29.2`, `K70`, `K85.2`, `K86.0`, `R78.0`, `X45`, `X65`, `Y15` |
| 23 | `Transport accidents` | `E800–E848`, `E929.0–E929.1` | `V01–V99`, `Y85` |
| 24 | `Other external causes` | Residual of `E800–E999` | Residual of `V00–Y89` |
| 25 | `COVID-19` | *(not applicable)* | `U07.1` |
| 26 | `Symptoms, signs, and ill-defined conditions` | `780–799`, excl. `790.3` | `R00–R99`, excl. `R78.0` |
| 27 | `All other causes` | `280–289`, `630–739`, and other valid residual codes | `D50–D89`, `G99`, `H00–H99`, `J99`, `L00–L99`, `M00–M99`, `N99`, `O00–O99`, `U00–U99` (excl. `U07.1`), and other valid residual codes |

### 5. Reading Output CSVs Back Into R

Always read FIPS columns as **character** so leading zeros (e.g., `"01"`, `"06037"`) are preserved:

```r
library(data.table)

dt <- fread(
  "nvss_deaths_2022.csv",
  colClasses = list(character = c("state_FIPS", "county_FIPS", "FIPS_code"))
)
```

---

## Testing Locally With Synthetic Examples

To see how `nvss_tabulate()` works and inspect sample CSV outputs without needing server access, run the synthetic examples from the package root:

```r
source("inst/examples/run_all_examples.R")
```

This generates small synthetic fixed-width files in `inst/examples/synthetic_data/` and writes sample outputs to `inst/examples/outputs/`. See `inst/examples/README.md` for details on each script.

---

## Researcher Table Request Template

Researchers who need custom tables from the restricted server can copy, edit, and send this snippet to someone who has access to the server:

```r
# --- Table Request ---
# Project / Requester: <Your / Name Project>
# Output folder name:  <short_descriptive_name>

library(nvssread)

nvss_tabulate(
  data_dir           = SERVER_DATA_DIR, # location of the restricted data on the server
  years              = 1989:2024,
  vars               = c("state", "county", "sex", "age", "race_eth", "cod"),
  age_type           = "abridged",      # "abridged" or "single"
  age_top            = 85,              # e.g., 85, 95, 100 (or Inf for un-top-coded single)
  race_type          = "single_multi",  # "single_multi" or "bridged"
  race_groups        = "detailed",      # "detailed", "wbh", or "bw"
  harmonize_counties = TRUE,            # TRUE (consistent 1989+ boundaries) or FALSE (raw FIPS)
  redistribute       = TRUE,            # TRUE (proportional w/ fallback) or FALSE (keep NAs)
  complete_grid      = FALSE,           # TRUE only for lower-dimensional tables
  geo_filter         = "residence",     # "residence", "occurrence", or "both"
  output_dir         = SERVER_OUT_DIR   # filled in by server operator
)
```

---

## Companion Package & Status

- **Supported now:** 1989–2024+ NVSS All-County files (1989–1995, 1996–1998, 1999–2002, and 2003+ layouts), ICD-9 and ICD-10 cause-of-death harmonization, and longitudinal county FIPS harmonization.
- **Population denominators:** Public population denominators in a matching schema (SEER, Census PEP, and IPUMS) are maintained separately in the companion `nvsspop` package so researchers can generate denominators locally without server access (this is in progress).

