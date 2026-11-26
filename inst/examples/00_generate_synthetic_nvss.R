# 00_generate_synthetic_nvss.R
# Creates small synthetic NVSS fixed-width files (2019 and 2022) in
# inst/examples/synthetic_data/ so users can run the examples without
# restricted server access.

build_nvss_line <- function(year, month, state_occ, state_res, county_res,
                            birth_place, sex, age_type, age_val, ucod,
                            race_detail = "  ", race_rec5 = " ",
                            hisp_orig = "100", race_rec40 = "  ") {
  line <- strrep(" ", 495L)
  
  place <- function(str, start, end, val) {
    w <- end - start + 1L
    v <- formatC(as.character(val), width = w, flag = "-")
    v <- substr(v, 1L, w)
    substr(str, start, end) <- v
    str
  }
  
  line <- place(line,  21L,  22L, state_occ)
  line <- place(line,  29L,  30L, state_res)
  line <- place(line,  35L,  37L, county_res)
  line <- place(line,  59L,  60L, birth_place)
  line <- place(line,  65L,  66L, sprintf("%02d", as.integer(month)))
  line <- place(line,  69L,  69L, sex)
  line <- place(line,  70L,  70L, as.character(age_type))
  line <- place(line,  71L,  73L, sprintf("%03d", as.integer(age_val)))
  line <- place(line, 102L, 105L, sprintf("%04d", as.integer(year)))
  line <- place(line, 146L, 149L, ucod)
  line <- place(line, 445L, 446L, race_detail)
  line <- place(line, 450L, 450L, race_rec5)
  line <- place(line, 484L, 486L, sprintf("%03d", as.integer(hisp_orig)))
  line <- place(line, 489L, 490L, race_rec40)
  line
}

generate_synthetic_year <- function(year, n = 400L, out_base) {
  set.seed(1000L + year)
  
  # State-county pool (plus occasional ZZ foreign resident & 999 missing county)
  geo_pool <- list(
    c("CA", "037"), c("CA", "073"),
    c("NY", "061"), c("NY", "047"),
    c("TX", "201"), c("TX", "113"),
    c("IL", "031"), c("IL", "043")
  )
  
  # Sample ICD-10 codes spanning key project categories & edge cases
  cod_pool <- c(
    "I219", # Circulatory excl. HHD
    "I119", # Hypertensive heart disease
    "C349", # Lung cancer
    "C509", # Breast/prostate/colorectal/cervical
    "G309", # ADRD (Alzheimer's)
    "F019", # ADRD (Vascular dementia - removed from Mental)
    "G318", # ADRD (Removed from Nervous)
    "G20 ", # Nervous system excl. ADRD
    "J189", # Influenza and pneumonia
    "J449", # Respiratory excl. flu/pneumonia
    "E119", # Diabetes
    "X42 ", # Drug overdose
    "X65 ", # Alcohol-induced (takes priority over Suicide)
    "K703", # Alcohol-induced
    "X72 ", # Suicide
    "X95 ", # Homicide
    "V435", # Transport accidents
    "W19 ", # Other external causes
    if (year >= 2020L) "U071" else "R99 " # COVID-19 in 2022, ill-defined in 2019
  )
  
  birth_pool <- c("CA", "NY", "TX", "IL", "PR", "MX", "CC", "YY", "ZZ")
  birth_prob <- c(0.22, 0.18, 0.18, 0.14, 0.04, 0.08, 0.04, 0.09, 0.03)
  
  lines <- character(n)
  for (i in seq_len(n)) {
    g <- geo_pool[[sample.int(length(geo_pool), 1L)]]
    st_res <- g[[1L]]
    ct_res <- g[[2L]]
    st_occ <- st_res
    
    # Inject ~2% foreign residents (excluded by geo_filter = "residence")
    if (runif(1L) < 0.02) {
      st_res <- "ZZ"
      ct_res <- "000"
    } else if (runif(1L) < 0.015) {
      # Inject ~1.5% missing county within US state
      ct_res <- "999"
    }
    
    month <- sample.int(12L, 1L)
    sex   <- sample(c("M", "F"), 1L)
    
    # Age: mix of infant (age_type = 2), regular years (1), 100+, and ~2% missing (9/999)
    u_age <- runif(1L)
    if (u_age < 0.02) {
      age_type <- 9L; age_val <- 999L
    } else if (u_age < 0.05) {
      age_type <- 2L; age_val <- sample.int(11L, 1L)
    } else if (u_age < 0.08) {
      age_type <- 1L; age_val <- sample(100L:104L, 1L)
    } else {
      age_type <- 1L; age_val <- sample(1L:99L, 1L)
    }
    
    bp   <- sample(birth_pool, 1L, prob = birth_prob)
    ucod <- sample(cod_pool, 1L)
    
    # Hispanic origin: ~76% Non-Hispanic (100), ~20% Hispanic (210/260), ~4% Unknown (999)
    hisp <- sample(c(100L, 210L, 260L, 999L), 1L, prob = c(0.76, 0.14, 0.06, 0.04))
    
    # Race Recode 40 (including code 23 = Asian+NHOPI -> NH API, code 15 = Black+White -> NH Multiple/Other)
    r40 <- sample(c("01", "02", "03", "05", "11", "23", "15"), 1L,
                  prob = c(0.55, 0.20, 0.03, 0.10, 0.02, 0.02, 0.08))
    
    # Bridged race populated through 2020; blank filler in 2021+
    if (year <= 2020L) {
      r_det <- switch(r40,
                      "01" = "01", "02" = "02", "03" = "03",
                      "05" = "04", "11" = "06", "23" = "78", "15" = "02"
      )
      r5 <- switch(r40,
                   "01" = "1", "02" = "2", "03" = "3",
                   "05" = "4", "11" = "5", "23" = "4", "15" = "2"
      )
    } else {
      r_det <- "  "
      r5 <- switch(r40,
                   "01" = "1", "02" = "2", "03" = "3",
                   "05" = "4", "11" = "5", "23" = "4", "15" = "2"
      )
    }
    
    lines[[i]] <- build_nvss_line(
      year = year, month = month, state_occ = st_occ, state_res = st_res,
      county_res = ct_res, birth_place = bp, sex = sex,
      age_type = age_type, age_val = age_val, ucod = ucod,
      race_detail = r_det, race_rec5 = r5, hisp_orig = hisp, race_rec40 = r40
    )
  }
  
  yr_dir <- file.path(out_base, sprintf("mort%d", year))
  dir.create(yr_dir, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(yr_dir, sprintf("MULT%dUSAllCnty.txt", year))
  writeLines(lines, out_file)
  message("Wrote synthetic file: ", out_file)
  invisible(out_file)
}

# Run when sourced directly
if (!exists("SYNTH_BASE_DIR")) {
  SYNTH_BASE_DIR <- file.path("inst", "examples", "synthetic_data")
}
generate_synthetic_year(2019L, n = 400L, out_base = SYNTH_BASE_DIR)
generate_synthetic_year(2022L, n = 400L, out_base = SYNTH_BASE_DIR)