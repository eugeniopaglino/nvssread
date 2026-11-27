# validation/validate_public_nvss.R
# End-to-end validation of nvssread against public-use NCHS mortality files
# and committed national CDC WONDER benchmark tables:
#   1) age x sex x race_eth (1990, 2000, 2020, 2024)
#   2) sex x cod            (1990, 2020)

library(data.table)
library(nvssread)

WONDER_DIR <- file.path("validation", "wonder")
CACHE_DIR  <- file.path("validation", "cache")
OUT_DIR    <- file.path("validation", "results")

dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIR,   recursive = TRUE, showWarnings = FALSE)

# Exact case-sensitive NCHS FTP zip filenames
NCHS_BASE_URL <- "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/DVS/mortality"
NCHS_ZIPS <- c(
  "1990" = "Mort1990us.zip",
  "2000" = "Mort2000us.zip",
  "2020" = "mort2020us.zip",
  "2024" = "mort2024us.zip"
)

# ==============================================================================
# 1. DOWNLOAD & UNZIP PUBLIC-USE NCHS FILES (WITH CACHING)
# ==============================================================================
ensure_nchs_public_file <- function(year, cache_dir = CACHE_DIR) {
  yr_chr   <- as.character(year)
  txt_path <- file.path(cache_dir, sprintf("public_mort_%d.txt", year))
  if (file.exists(txt_path) && file.info(txt_path)$size > 1e6) {
    return(txt_path)
  }
  
  zip_name <- NCHS_ZIPS[[yr_chr]]
  if (is.null(zip_name)) stop("No NCHS URL configured for year ", year, call. = FALSE)
  
  zip_path <- file.path(cache_dir, zip_name)
  url      <- paste0(NCHS_BASE_URL, "/", zip_name)
  
  if (!file.exists(zip_path) || file.info(zip_path)$size < 1e6) {
    message(sprintf("Downloading %s ...", url))
    old_to <- getOption("timeout")
    options(timeout = max(600, old_to))
    on.exit(options(timeout = old_to), add = TRUE)
    tmp_zip <- paste0(zip_path, ".tmp")
    status <- download.file(url, destfile = tmp_zip, mode = "wb", quiet = FALSE)
    if (status != 0L) stop("Download failed for ", url, call. = FALSE)
    file.rename(tmp_zip, zip_path)
  }
  
  tmp_ex <- file.path(cache_dir, sprintf("unzip_%d", year))
  unlink(tmp_ex, recursive = TRUE)
  dir.create(tmp_ex, recursive = TRUE, showWarnings = FALSE)
  
  # 1) Try base R unzip (works for standard Deflate zips like 1990/2000/2010)
  extracted <- suppressWarnings(unzip(zip_path, exdir = tmp_ex))
  
  # 2) Fallback for Deflate64 zips (2020/2024): try 7z, system unzip, or Windows Shell
  if (length(extracted) == 0L) {
    unlink(list.files(tmp_ex, full.names = TRUE), recursive = TRUE)
    if (nzchar(Sys.which("7z"))) {
      st <- system2("7z", c("x", "-y", paste0("-o", tmp_ex), zip_path))
    } else if (nzchar(Sys.which("unzip"))) {
      st <- system2("unzip", c("-o", zip_path, "-d", tmp_ex))
    } else if (.Platform$OS.type == "windows") {
      z_abs <- normalizePath(zip_path, winslash = "\\", mustWork = TRUE)
      d_abs <- normalizePath(tmp_ex,   winslash = "\\", mustWork = TRUE)
      ps_cmd <- sprintf(
        "$s = New-Object -ComObject Shell.Application; $s.NameSpace('%s').CopyHere($s.NameSpace('%s').Items(), 16)",
        d_abs, z_abs
      )
      st <- system2("powershell", c("-NoProfile", "-Command", shQuote(ps_cmd)))
    } else {
      st <- 1L
    }
    if (identical(st, 0L)) {
      extracted <- list.files(tmp_ex, full.names = TRUE, recursive = TRUE)
    }
  }
  
  if (length(extracted) == 0L) {
    stop("Failed to extract ", zip_path, ". Extract it manually to ", txt_path, " and re-run.", call. = FALSE)
  }
  
  sizes     <- file.info(extracted)$size
  data_file <- extracted[which.max(sizes)]
  if (is.na(max(sizes)) || max(sizes) < 1e6) {
    stop("Extracted file from ", zip_path, " is unexpectedly small/corrupt.", call. = FALSE)
  }
  
  ok <- file.rename(data_file, txt_path)
  if (!ok) {
    file.copy(data_file, txt_path, overwrite = TRUE)
  }
  unlink(tmp_ex, recursive = TRUE)
  txt_path
}

# ==============================================================================
# 2. WONDER PARSERS (AGE x SEX x RACE_ETH, SEX x COD, AND COUNTY)
# ==============================================================================
map_wonder_age_to_abridged <- function(age_code, age_label) {
  code <- trimws(age_code)
  lbl  <- tolower(trimws(age_label))
  fcase(
    code == "1"     | grepl("^<\\s*1", lbl),            "00-00",
    code == "1-4"   | grepl("^1-4", lbl),               "01-04",
    code == "5-9"   | grepl("^5-9", lbl),               "05-09",
    code %chin% c("10-14","15-19","20-24","25-29","30-34","35-39",
                  "40-44","45-49","50-54","55-59","60-64","65-69",
                  "70-74","75-79","80-84",
                  "25-34","35-44","45-54","55-64","65-74","75-84"),             code,
    code %chin% c("85-89","90-94","95-99","100+","85+") |
      grepl("^(85|90|95|100)", lbl),                    "85+",
    code == "NS"    | grepl("not stated", lbl),         NA_character_,
    default = NA_character_
  )
}

read_wonder_age_sex_race <- function(path, year) {
  dt <- fread(path, colClasses = "character", fill = TRUE, showProgress = FALSE)
  setnames(dt, trimws(names(dt)))
  if ("Notes" %in% names(dt)) dt <- dt[is.na(Notes) | trimws(Notes) == ""]
  
  # Age column (Five-Year Age Groups or Age Group)
  age_code_col <- intersect(c("Five-Year Age Groups Code", "Age Group Code"), names(dt))[1L]
  age_lbl_col  <- intersect(c("Five-Year Age Groups", "Age Group"), names(dt))[1L]
  dt[, age := map_wonder_age_to_abridged(get(age_code_col), get(age_lbl_col))]
  
  # Sex
  sex_col <- intersect(c("Gender", "Sex"), names(dt))[1L]
  dt[, sex := trimws(get(sex_col))]
  dt <- dt[sex %chin% c("Male", "Female")]
  
  # Race/Ethnicity
  if (year == 1990L) {
    race_col <- intersect(c("Race"), names(dt))[1L]
    r_val    <- tolower(trimws(dt[[race_col]]))
    dt[, race_eth := fcase(
      grepl("white", r_val), "White",
      grepl("black", r_val), "Black",
      default = "Other"
    )]
  } else {
    hisp_col <- intersect(c("Hispanic Origin"), names(dt))[1L]
    race_col <- intersect(c("Single Race 6", "Race"), names(dt))[1L]
    h_val    <- tolower(trimws(dt[[hisp_col]]))
    r_val    <- tolower(trimws(dt[[race_col]]))
    
    dt[, race_eth := fcase(
      grepl("^hispanic", h_val),                                      "Hispanic",
      grepl("not stated", h_val),                                     NA_character_,
      grepl("white", r_val),                                          "NH White",
      grepl("black", r_val),                                          "NH Black",
      grepl("american indian|alaska native", r_val),                  "NH AIAN",
      grepl("asian|pacific islander|native hawaiian", r_val),         "NH API",
      grepl("more than one", r_val),                                  "NH Multiple/Other",
      default = NA_character_
    )]
    
    # For 2020/2024 Single Race 6, pool NH API + NH Multiple/Other (Race Recode 40 code 23)
    if (year >= 2020L) {
      dt[race_eth %chin% c("NH API", "NH Multiple/Other"),
         race_eth := "NH API + NH Multiple/Other"]
    }
  }
  
  dt[, is_supp := tolower(trimws(Deaths)) == "suppressed"]
  dt[, wonder_deaths := suppressWarnings(as.numeric(gsub(",", "", trimws(Deaths))))]
  dt <- dt[!is.na(wonder_deaths) | is_supp]
  dt[is_supp == TRUE, wonder_deaths := 0]
  
  # Aggregate (combines 85+ bands, Hispanic across races, etc.) and track suppressed sub-cells
  dt[, .(
    wonder_deaths = sum(wonder_deaths),
    n_supp        = sum(is_supp)
  ), by = .(age, sex, race_eth)]
}

read_wonder_county <- function(path) {
  dt <- fread(path, colClasses = "character", fill = TRUE, showProgress = FALSE)
  setnames(dt, trimws(names(dt)))
  if ("Notes" %in% names(dt)) dt <- dt[is.na(Notes) | trimws(Notes) == ""]
  
  dt[, FIPS_code := trimws(`County Code`)]
  dt <- dt[grepl("^[0-9]{5}$", FIPS_code)]
  dt[FIPS_code == "12025", FIPS_code := "12086"] # Dade -> Miami-Dade
  dt[, state_FIPS := substr(FIPS_code, 1L, 2L)]
  
  d_low <- tolower(trimws(dt$Deaths))
  dt[, is_supp := d_low == "suppressed"]
  dt[, wonder_deaths := suppressWarnings(as.numeric(gsub(",", "", trimws(Deaths))))]
  dt[d_low %chin% c("suppressed", "missing", "not applicable"), wonder_deaths := 0]
  dt <- dt[!is.na(wonder_deaths)]
  
  dt[, .(
    wonder_deaths = sum(wonder_deaths),
    n_supp        = sum(is_supp)
  ), by = .(state_FIPS, FIPS_code)]
}

# ==============================================================================
# 3. RUN BENCHMARK COMPARISONS
# ==============================================================================
YEARS <- c(1990L, 2000L, 2020L, 2024L)
all_failures <- list()

for (yr in YEARS) {
  asr_file <- file.path(WONDER_DIR, sprintf("age_sex_race_%d.csv", yr))
  cod_file <- file.path(WONDER_DIR, sprintf("sex_cod_%d.csv", yr))
  
  cty_file <- file.path(WONDER_DIR, sprintf("county_%d.csv", yr))
  
  if (!file.exists(asr_file) && !file.exists(cod_file) && !file.exists(cty_file)) {
    message(sprintf("[%d] No WONDER benchmark CSVs found in %s; skipping.", yr, WONDER_DIR))
    next
  }
  
  ensure_nchs_public_file(yr)
  
  # --- Check 1: Age (abridged 85+) x Sex x Race/Ethnicity ---
  if (file.exists(asr_file)) {
    message(sprintf("\n[%d] Checking age x sex x race_eth against %s ...", yr, basename(asr_file)))
    
    r_type   <- if (yr >= 2020L) "single_multi" else "bridged"
    r_groups <- if (yr == 1990L) "bw" else "detailed"
    
    nvss_asr <- nvss_tabulate(
      data_dir     = CACHE_DIR,
      years        = yr,
      vars         = c("age", "sex", "race_eth"),
      age_type     = "abridged",
      age_top      = 85L,
      race_type    = r_type,
      race_groups  = r_groups,
      redistribute = FALSE,
      geo_filter   = "residence",
      file_pattern = "public_mort_{year}.txt",
      return_data  = TRUE
    )
    
    if (yr == 1990L) {
      nvss_asr[, age := fcase(
        age %chin% c("25-29", "30-34"), "25-34",
        age %chin% c("35-39", "40-44"), "35-44",
        age %chin% c("45-49", "50-54"), "45-54",
        age %chin% c("55-59", "60-64"), "55-64",
        age %chin% c("65-69", "70-74"), "65-74",
        age %chin% c("75-79", "80-84"), "75-84",
        default = age
      )]
      nvss_asr <- nvss_asr[, .(nDx = sum(nDx)), by = .(age, sex, race_eth)]
    } else if (yr >= 2020L) {
      nvss_asr[race_eth %chin% c("NH API", "NH Multiple/Other"),
               race_eth := "NH API + NH Multiple/Other"]
      nvss_asr <- nvss_asr[, .(nDx = sum(nDx)), by = .(age, sex, race_eth)]
    } else {
      nvss_asr <- nvss_asr[, .(nDx = sum(nDx)), by = .(age, sex, race_eth)]
    }
    
    wonder_asr <- read_wonder_age_sex_race(asr_file, yr)
    
    cmp_asr <- merge(wonder_asr, nvss_asr, by = c("age", "sex", "race_eth"), all = TRUE)
    cmp_asr[is.na(wonder_deaths), wonder_deaths := 0]
    cmp_asr[is.na(n_supp),        n_supp := 0L]
    cmp_asr[is.na(nDx),           nDx := 0]
    cmp_asr[, diff := nDx - wonder_deaths]
    
    # Exact match for unsuppressed cells; [1, 9] per suppressed sub-cell otherwise
    mm_asr <- cmp_asr[
      (n_supp == 0L & abs(diff) > 1e-6) |
        (n_supp >  0L & (diff < n_supp | diff > 9L * n_supp))
    ]
    if (nrow(mm_asr) > 0L) {
      message(sprintf("  FAIL [%d age x sex x race_eth]: %d mismatched cell(s)!", yr, nrow(mm_asr)))
      print(mm_asr)
      fwrite(mm_asr, file.path(OUT_DIR, sprintf("mismatch_age_sex_race_%d.csv", yr)))
      all_failures[[paste0("asr_", yr)]] <- mm_asr
    } else {
      n_exact <- sum(cmp_asr$n_supp == 0L)
      n_bound <- sum(cmp_asr$n_supp > 0L)
      message(sprintf("  PASS [%d age x sex x race_eth]: %d exact cells + %d suppression-bounded cells, 0 discrepancies (total = %s).",
                      yr, n_exact, n_bound, format(sum(cmp_asr$nDx), big.mark = ",")))
    }
  }
  
  # --- Check 2: Sex x Cause of Death ---
  if (file.exists(cod_file)) {
    message(sprintf("\n[%d] Checking sex x cod against %s ...", yr, basename(cod_file)))
    
    nvss_cod <- nvss_tabulate(
      data_dir     = CACHE_DIR,
      years        = yr,
      vars         = c("sex", "cod"),
      redistribute = FALSE,
      geo_filter   = "residence",
      file_pattern = "public_mort_{year}.txt",
      return_data  = TRUE
    )[, .(nDx = sum(nDx)), by = .(sex, cod)]
    
    wonder_cod <- fread(cod_file, showProgress = FALSE)
    setnames(wonder_cod, trimws(names(wonder_cod)))
    wonder_cod[, wonder_deaths := as.numeric(gsub(",", "", as.character(wonder_deaths)))]
    
    # Left-join onto wonder_cod so you can validate either all 27 groups or a curated subset
    cmp_cod <- merge(wonder_cod, nvss_cod, by = c("sex", "cod"), all.x = TRUE)
    cmp_cod[is.na(nDx), nDx := 0]
    cmp_cod[, diff := nDx - wonder_deaths]
    
    mm_cod <- cmp_cod[abs(diff) > 1e-6]
    if (nrow(mm_cod) > 0L) {
      message(sprintf("  FAIL [%d sex x cod]: %d mismatched cell(s)!", yr, nrow(mm_cod)))
      print(mm_cod)
      fwrite(mm_cod, file.path(OUT_DIR, sprintf("mismatch_sex_cod_%d.csv", yr)))
      all_failures[[paste0("cod_", yr)]] <- mm_cod
    } else {
      message(sprintf("  PASS [%d sex x cod]: %d benchmark cells, 0 discrepancies.", yr, nrow(cmp_cod)))
    }
  }
  
  # --- Check 3: State (all 50 + DC) & Unmasked County (>=100k pop) Totals ---
  if (file.exists(cty_file)) {
    message(sprintf("\n[%d] Checking state & unmasked county totals against %s ...", yr, basename(cty_file)))
    
    nvss_geo <- nvss_tabulate(
      data_dir           = CACHE_DIR,
      years              = yr,
      vars               = c("state", "county"),
      harmonize_counties = FALSE,
      redistribute       = FALSE,
      geo_filter         = "residence",
      file_pattern       = "public_mort_{year}.txt",
      return_data        = TRUE
    )
    nvss_geo[FIPS_code == "12025", FIPS_code := "12086"]
    
    wonder_cty <- read_wonder_county(cty_file)
    
    # 3A: Exact match on every unmasked (>=100k) county in public-use NVSS
    nvss_cty <- nvss_geo[!is.na(FIPS_code), .(nDx = sum(nDx)), by = .(FIPS_code)]
    cmp_cty  <- merge(nvss_cty, wonder_cty[, .(FIPS_code, wonder_deaths, n_supp)],
                      by = "FIPS_code", all.x = TRUE)
    cmp_cty[is.na(wonder_deaths), wonder_deaths := 0]
    cmp_cty[, diff := nDx - wonder_deaths]
    
    mm_cty <- cmp_cty[abs(diff) > 1e-6]
    if (nrow(mm_cty) > 0L) {
      message(sprintf("  FAIL [%d county]: %d mismatched unmasked county cell(s)!", yr, nrow(mm_cty)))
      print(mm_cty)
      fwrite(mm_cty, file.path(OUT_DIR, sprintf("mismatch_county_%d.csv", yr)))
      all_failures[[paste0("cty_", yr)]] <- mm_cty
    } else {
      message(sprintf("  PASS [%d county]: %d unmasked (>=100k) counties, 0 discrepancies (total = %s).",
                      yr, nrow(cmp_cty), format(sum(cmp_cty$nDx), big.mark = ",")))
    }
    
    # 3B: All 50 states + DC (includes masked <100k counties)
    nvss_st   <- nvss_geo[, .(nDx = sum(nDx)), by = .(state_FIPS)]
    wonder_st <- wonder_cty[, .(wonder_deaths = sum(wonder_deaths), n_supp = sum(n_supp)), by = .(state_FIPS)]
    cmp_st    <- merge(wonder_st, nvss_st, by = "state_FIPS", all = TRUE)
    cmp_st[is.na(wonder_deaths), wonder_deaths := 0]
    cmp_st[is.na(n_supp),        n_supp := 0L]
    cmp_st[is.na(nDx),           nDx := 0]
    cmp_st[, diff := nDx - wonder_deaths]
    
    mm_st <- cmp_st[
      (n_supp == 0L & abs(diff) > 1e-6) |
        (n_supp >  0L & (diff < 0 | diff > 9L * n_supp))
    ]
    if (nrow(mm_st) > 0L) {
      message(sprintf("  FAIL [%d state]: %d mismatched state(s)!", yr, nrow(mm_st)))
      print(mm_st)
      fwrite(mm_st, file.path(OUT_DIR, sprintf("mismatch_state_%d.csv", yr)))
      all_failures[[paste0("st_", yr)]] <- mm_st
    } else {
      n_exact <- sum(cmp_st$n_supp == 0L)
      n_bound <- sum(cmp_st$n_supp > 0L)
      message(sprintf("  PASS [%d state]: %d exact states + %d suppression-bounded states, 0 discrepancies (total = %s).",
                      yr, n_exact, n_bound, format(sum(cmp_st$nDx), big.mark = ",")))
    }
  }
  
}

if (length(all_failures) > 0L) {
  stop(sprintf("Validation failed on %d benchmark table(s). See %s for details.",
               length(all_failures), OUT_DIR), call. = FALSE)
}

message("\nAll public NVSS vs. CDC WONDER validation checks passed!")
