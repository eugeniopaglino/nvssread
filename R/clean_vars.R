#' Clean sex variable (supports 1989-2002 numeric 1/2 and 2003+ M/F)
#' @keywords internal
clean_sex <- function(x) {
  x <- toupper(trimws(x))
  data.table::fcase(
    x %chin% c("M", "1"), "Male",
    x %chin% c("F", "2"), "Female",
    default = NA_character_
  )
}

#' Build ordered abridged age labels up to age_top+
#' @keywords internal
abridged_age_levels <- function(age_top = 85L) {
  age_top <- as.integer(age_top)
  if (is.na(age_top) || age_top < 5L || (age_top %% 5L) != 0L) {
    stop("For age_type = 'abridged', 'age_top' must be a multiple of 5 (at least 5).", call. = FALSE)
  }
  starts <- seq.int(5L, age_top - 5L, by = 5L)
  mid_labels <- sprintf("%02d-%02d", starts, starts + 4L)
  c("00-00", "01-04", mid_labels, sprintf("%02d+", age_top))
}

#' Clean NVSS Detail Age (1989-2002 3-digit or 2003+ 4-digit) into abridged or single years
#' @keywords internal
clean_age <- function(age_type, age_val, year = 2003L,
                      mode = c("abridged", "single"), age_top = NULL) {
  mode <- match.arg(mode)
  
  if (year <= 2002L) {
    # 1989-2002 3-digit Detail Age (locations 64-66):
    #   0 + 01-99 -> age in years (<100)
    #   1 + 00-99 -> 100 + age_val (years 100+)
    #   2..6      -> infant units (months/weeks/days/hours/minutes) -> 0
    #   9 / NA    -> Age not stated -> NA
    age_yrs <- data.table::fcase(
      age_type %between% c(2L, 6L), 0L,
      age_type == 0L & !is.na(age_val) & age_val %between% c(0L, 99L), as.integer(age_val),
      age_type == 1L & !is.na(age_val) & age_val %between% c(0L, 99L), 100L + as.integer(age_val),
      default = NA_integer_
    )
  } else {
    # 2003+ 4-digit Detail Age (locations 70-73):
    #   1 + 000-135 -> years (1 + 999 -> NA)
    #   2..6        -> infant units -> 0
    #   9 / NA      -> Age not stated -> NA
    age_yrs <- data.table::fcase(
      age_type %between% c(2L, 6L), 0L,
      age_type == 1L & !is.na(age_val) & age_val < 999L, as.integer(age_val),
      default = NA_integer_
    )
  }
  
  if (mode == "abridged") {
    if (is.null(age_top)) age_top <- 85L
    age_top <- as.integer(age_top)
    levels_abr <- abridged_age_levels(age_top)
    
    out <- rep(NA_character_, length(age_yrs))
    ok <- !is.na(age_yrs)
    ay <- age_yrs[ok]
    
    out[ok] <- data.table::fifelse(
      ay == 0L, "00-00",
      data.table::fifelse(
        ay <= 4L, "01-04",
        data.table::fifelse(
          ay >= age_top, sprintf("%02d+", age_top),
          sprintf("%02d-%02d", (ay %/% 5L) * 5L, (ay %/% 5L) * 5L + 4L)
        )
      )
    )
    attr(out, "age_levels") <- levels_abr
    return(out)
  }
  
  # mode == "single"
  if (is.null(age_top)) age_top <- 100L
  if (is.infinite(age_top) || is.na(age_top)) {
    out <- ifelse(is.na(age_yrs), NA_character_, as.character(age_yrs))
    max_obs <- suppressWarnings(max(age_yrs, na.rm = TRUE))
    if (is.finite(max_obs)) {
      attr(out, "age_levels") <- as.character(seq.int(0L, max_obs))
    }
    return(out)
  }
  
  age_top <- as.integer(age_top)
  if (age_top < 1L) {
    stop("For age_type = 'single', 'age_top' must be >= 1, Inf, or NA.", call. = FALSE)
  }
  out <- ifelse(
    is.na(age_yrs), NA_character_,
    ifelse(age_yrs >= age_top, paste0(age_top, "+"), as.character(age_yrs))
  )
  attr(out, "age_levels") <- c(as.character(seq.int(0L, age_top - 1L)), paste0(age_top, "+"))
  out
}

#' Map Race Recode 40 (positions 489-490, 2012+) to base race labels
#' @keywords internal
map_race_rec40 <- function(r40) {
  r40 <- trimws(r40)
  api_codes <- c("04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "23")
  multi_other_codes <- c(
    "15", "16", "17", "18", "19", "20", "21", "22",
    "24", "25", "26", "27", "28", "29", "30", "31",
    "32", "33", "34", "35", "36", "37", "38", "39",
    "40", "99"
  )
  
  data.table::fcase(
    r40 == "01", "White",
    r40 == "02", "Black",
    r40 == "03", "AIAN",
    r40 %chin% api_codes, "API",
    r40 %chin% multi_other_codes, "Multiple/Other",
    default = NA_character_
  )
}

#' Map Detail / Bridged Race (1989-2002 pos 60-61; 2003-2020 pos 445-446 & 450) to base race labels
#' @keywords internal
map_race_bridged <- function(r_det, r5 = NULL) {
  r_det <- trimws(r_det)
  api_det <- c("04", "05", "06", "07", "08", "18", "28", "38", "48", "58", "68", "78")
  
  from_det <- data.table::fcase(
    r_det == "01", "White",
    r_det == "02", "Black",
    r_det == "03", "AIAN",
    r_det %chin% api_det, "API",
    r_det == "09", "Multiple/Other", # 1989-1991 "All other races" before 1992 imputation
    default = NA_character_
  )
  
  if (is.null(r5)) return(from_det)
  
  r5 <- trimws(r5)
  from_r5 <- data.table::fcase(
    r5 == "1", "White",
    r5 == "2", "Black",
    r5 == "3", "AIAN",
    r5 %chin% c("4", "5"), "API",
    default = NA_character_
  )
  
  data.table::fifelse(!is.na(from_det), from_det, from_r5)
}

#' Clean race/ethnicity with configurable grouping scheme (1989 onwards)
#' @keywords internal
clean_race_eth <- function(hispanic_origin = NULL, race_detail, race_rec5 = NULL,
                           race_rec40 = NULL, year,
                           race_type = c("single_multi", "bridged"),
                           race_groups = c("detailed", "wbh", "bw")) {
  race_type   <- match.arg(race_type)
  race_groups <- match.arg(race_groups)
  n <- length(race_detail)
  
  r_bridged <- map_race_bridged(race_detail, race_rec5)
  r_40 <- if (!is.null(race_rec40)) map_race_rec40(race_rec40) else rep(NA_character_, n)
  
  base_race <- if (race_type == "single_multi") {
    data.table::fifelse(!is.na(r_40), r_40, r_bridged)
  } else {
    data.table::fifelse(!is.na(r_bridged), r_bridged, r_40)
  }
  
  # Black / White / Other (including Hispanics; ignores Hispanic origin)
  if (race_groups == "bw") {
    return(data.table::fcase(
      base_race == "White", "White",
      base_race == "Black", "Black",
      !is.na(base_race),    "Other",
      default = NA_character_
    ))
  }
  
  if (is.null(hispanic_origin)) {
    stop("'hispanic_origin' is required when race_groups is 'detailed' or 'wbh'.", call. = FALSE)
  }
  
  if (year <= 2002L) {
    # 1989-2002 2-digit Hispanic Origin (locations 80-81):
    #   00 = Non-Hispanic, 01-05 (1-98) = Hispanic, 99 or blank = Unknown (NA)
    is_hisp <- !is.na(hispanic_origin) & hispanic_origin %between% c(1L, 98L)
    is_nh   <- !is.na(hispanic_origin) & hispanic_origin == 0L
  } else {
    # 2003+ 3-digit Hispanic Origin (locations 484-486):
    #   100-199 = Non-Hispanic, 200-299 = Hispanic, 996-999 or blank = Unknown (NA)
    is_hisp <- !is.na(hispanic_origin) & hispanic_origin %between% c(200L, 299L)
    is_nh   <- !is.na(hispanic_origin) & hispanic_origin %between% c(100L, 199L)
  }
  
  # NH White / NH Black / Hispanic / NH Other
  if (race_groups == "wbh") {
    return(data.table::fcase(
      is_hisp, "Hispanic",
      is_nh & base_race == "White", "NH White",
      is_nh & base_race == "Black", "NH Black",
      is_nh & !is.na(base_race),    "NH Other",
      default = NA_character_
    ))
  }
  
  # Detailed: Hispanic, NH White, NH Black, NH API, NH AIAN (+ NH Multiple/Other)
  nh_label <- data.table::fifelse(!is.na(base_race), paste0("NH ", base_race), NA_character_)
  data.table::fcase(
    is_hisp, "Hispanic",
    is_nh,   nh_label,
    default = NA_character_
  )
}

#' Clean nativity (1989-2002 NCHS numeric 78-79; 2003+ postal recode 59-60)
#' @keywords internal
clean_nativity <- function(birth_place, year = 2003L) {
  bp <- toupper(trimws(birth_place))
  
  if (year <= 2002L) {
    bp_num <- suppressWarnings(as.integer(bp))
    return(data.table::fcase(
      is.na(bp_num) | bp_num == 99L, NA_character_,
      bp_num %between% c(1L, 51L), "US-born",
      bp_num %in% c(52L, 53L, 54L, 61L, 62L), "US Territory",
      default = "Foreign-born"
    ))
  }
  
  us_states <- nvss_states_lookup()$state_abb
  territories <- nvss_territories()
  
  data.table::fcase(
    is.na(bp) | bp == "" | bp == "ZZ", NA_character_,
    bp %chin% us_states, "US-born",
    bp %chin% territories, "US Territory",
    default = "Foreign-born"
  )
}

#' Harmonize longitudinal US county FIPS codes (1989-present)
#' @keywords internal
harmonize_county_fips <- function(state_FIPS, county_FIPS) {
  xw <- nvss_county_crosswalk()
  raw_fips <- data.table::fifelse(
    !is.na(state_FIPS) & !is.na(county_FIPS),
    paste0(state_FIPS, county_FIPS),
    NA_character_
  )
  idx <- match(raw_fips, xw$raw_FIPS)
  harm_county <- data.table::fifelse(!is.na(idx), xw$harmonized_county_FIPS[idx], county_FIPS)
  harm_county
}

#' Classify underlying ICD-9 cause of death (1989-1998) into project cause groups
#'
#' Handles NCHS tape format where leading 'E' is omitted on external causes
#' (stored as 800-999 / 8000-9999) as well as standard dotted/E-prefixed codes.
#'
#' @keywords internal
clean_cod_icd9 <- function(ucod) {
  # Strip dots, asterisks (used on HIV *042-*044), and optional leading 'E'
  icd4 <- toupper(gsub("[.*]", "", trimws(ucod)))
  icd4 <- sub("^E", "", icd4)
  icd4[icd4 == ""] <- NA_character_
  
  valid_icd <- !is.na(icd4) & grepl("^[0-9]{3,4}$", icd4)
  n3 <- suppressWarnings(as.integer(substr(icd4, 1L, 3L)))
  valid_icd <- valid_icd & !is.na(n3) & n3 >= 1L & n3 <= 999L
  
  data.table::fcase(
    !valid_icd, NA_character_,
    
    # 1. Alcohol-induced (strict priority over all other categories)
    n3 %in% c(291L, 303L, 860L) |
      icd4 %chin% c("3050", "3575", "4255", "5353", "5710", "5711", "5712", "5713", "7903"),
    "Alcohol-induced",
    
    # 2. ADRD (replaces Alzheimer's 331.0; removed from Mental & Nervous system)
    n3 == 290L |
      icd4 %chin% c("2941", "3310", "3311", "3312", "3318"),
    "ADRD",
    
    # 3. Drug overdose (excl. from Suicide, Homicide, Other external)
    n3 %between% c(850L, 858L) |
      icd4 %chin% c("9500", "9501", "9502", "9503", "9504", "9505",
                    "9620",
                    "9800", "9801", "9802", "9803", "9804", "9805"),
    "Drug overdose",
    
    # 4. Homicide (E960-E969, excl. E962.0 already assigned above)
    n3 %between% c(960L, 969L),
    "Homicide",
    
    # 5. Suicide (E950-E959, excl. E950.0-E950.5 already assigned above)
    n3 %between% c(950L, 959L),
    "Suicide",
    
    # 6. Transport accidents (E800-E848, E929.0, E929.1)
    n3 %between% c(800L, 848L) |
      icd4 %chin% c("9290", "9291"),
    "Transport accidents",
    
    # 7. Other external causes (strict residual of E800-E999)
    n3 %between% c(800L, 999L),
    "Other external causes",
    
    # 8. HIV/AIDS (042-044)
    n3 %between% c(42L, 44L),
    "HIV/AIDS",
    
    # 9. Other infectious and parasitic diseases (001-139, excl. 042-044)
    n3 %between% c(1L, 139L),
    "Other infectious and parasitic diseases",
    
    # 10. Liver cancer (155)
    n3 == 155L,
    "Liver cancer",
    
    # 11. Lung cancer (162)
    n3 == 162L,
    "Lung cancer",
    
    # 12. Breast, prostate, colorectal, and cervical cancer (153-154, 174-175, 180, 185)
    n3 %in% c(153L, 154L, 174L, 175L, 180L, 185L),
    "Breast, prostate, colorectal, and cervical cancer",
    
    # 13. All other cancers (140-239, excl. above cancer groups)
    n3 %between% c(140L, 239L),
    "All other cancers",
    
    # 14. Diabetes (250)
    n3 == 250L,
    "Diabetes",
    
    # 15. Endocrine, nutritional, and metabolic diseases excl. Diabetes (240-279, excl. 250)
    n3 %between% c(240L, 279L),
    "Endocrine, nutritional, and metabolic diseases excl. Diabetes",
    
    # 16. Mental disorders (290-319, excl. Alcohol & ADRD already assigned above)
    n3 %between% c(290L, 319L),
    "Mental disorders",
    
    # 17. Nervous system, excl. ADRD (320-389, excl. Alcohol & ADRD already assigned above)
    n3 %between% c(320L, 389L),
    "Nervous system, excl. ADRD",
    
    # 18. Hypertensive heart disease (401-405)
    n3 %between% c(401L, 405L),
    "Hypertensive heart disease",
    
    # 19. Circulatory diseases, excl. Hypertensive heart disease (390-459, excl. 401-405 & 425.5)
    n3 %between% c(390L, 459L),
    "Circulatory diseases, excl. Hypertensive heart disease",
    
    # 20. Influenza and pneumonia (480-487)
    n3 %between% c(480L, 487L),
    "Influenza and pneumonia",
    
    # 21. Respiratory diseases excl. Influenza and pneumonia (460-519, excl. 480-487)
    n3 %between% c(460L, 519L),
    "Respiratory diseases excl. Influenza and pneumonia",
    
    # 22. Diseases of the digestive system (520-579, excl. Alcohol already assigned above)
    n3 %between% c(520L, 579L),
    "Diseases of the digestive system",
    
    # 23. Diseases of the genitourinary system (580-629)
    n3 %between% c(580L, 629L),
    "Diseases of the genitourinary system",
    
    # 24. Perinatal conditions and congenital anomalies (740-759, 760-779)
    n3 %between% c(740L, 779L),
    "Perinatal conditions and congenital anomalies",
    
    # 25. Symptoms, signs, and ill-defined conditions (780-799, excl. 790.3)
    n3 %between% c(780L, 799L),
    "Symptoms, signs, and ill-defined conditions",
    
    # 26. All other causes (280-289, 630-679, 680-709, 710-739, and residual)
    default = "All other causes"
  )
}

#' Classify underlying ICD-10 cause of death (1999+) into 27 project cause groups
#' @keywords internal
clean_cod_icd10 <- function(ucod) {
  icd4 <- toupper(gsub("\\.", "", trimws(ucod)))
  icd4[icd4 == ""] <- NA_character_
  
  icd3 <- substr(icd4, 1L, 3L)
  L1   <- substr(icd3, 1L, 1L)
  N2   <- suppressWarnings(as.integer(substr(icd3, 2L, 3L)))
  
  valid_icd <- !is.na(icd4) & grepl("^[A-Z][0-9]{2}", icd4)
  
  data.table::fcase(
    !valid_icd, NA_character_,
    
    # 1. Alcohol-induced (strict priority over all other groups, incl. X65)
    icd3 %chin% c("F10", "K70", "X45", "X65", "Y15") |
      icd4 %chin% c("E244", "G312", "G621", "G721", "I426", "K292", "K852", "K860", "R780"),
    "Alcohol-induced",
    
    # 2. COVID-19
    icd4 == "U071",
    "COVID-19",
    
    # 3. ADRD (replaces Alzheimer's; removed from Mental and Nervous system)
    icd3 %chin% c("F01", "F02", "F03", "G30") |
      icd4 %chin% c("G310", "G311", "G318"),
    "ADRD",
    
    # 4. Drug overdose
    (L1 == "X" & N2 %between% c(40L, 44L)) |
      (L1 == "X" & N2 %between% c(60L, 64L)) |
      icd3 == "X85" |
      (L1 == "Y" & N2 %between% c(10L, 14L)),
    "Drug overdose",
    
    # 5. Homicide (excl. drug-related X85)
    (L1 == "X" & N2 %between% c(86L, 99L)) |
      (L1 == "Y" & N2 %between% c(0L, 9L)) |
      icd4 == "Y871",
    "Homicide",
    
    # 6. Suicide (excl. drug-related X60-X64 and alcohol-related X65)
    (L1 == "X" & N2 %between% c(66L, 84L)) |
      icd4 == "Y870",
    "Suicide",
    
    # 7. Transport accidents
    (L1 == "V" & N2 %between% c(1L, 99L)) |
      icd3 == "Y85",
    "Transport accidents",
    
    # 8. Other external causes (strict residual of V00-Y89)
    (L1 %chin% c("V", "W", "X")) |
      (L1 == "Y" & N2 %between% c(0L, 89L)),
    "Other external causes",
    
    # 9. HIV/AIDS
    L1 == "B" & N2 %between% c(20L, 24L),
    "HIV/AIDS",
    
    # 10. Other infectious and parasitic diseases
    L1 %chin% c("A", "B") & N2 %between% c(0L, 99L),
    "Other infectious and parasitic diseases",
    
    # 11. Liver cancer
    icd3 == "C22",
    "Liver cancer",
    
    # 12. Lung cancer
    icd3 %chin% c("C33", "C34"),
    "Lung cancer",
    
    # 13. Breast, prostate, colorectal, and cervical cancer
    (L1 == "C" & N2 %between% c(18L, 21L)) |
      icd3 %chin% c("C50", "C53", "C61"),
    "Breast, prostate, colorectal, and cervical cancer",
    
    # 14. All other cancers
    (L1 == "C" & N2 %between% c(0L, 99L)) |
      (L1 == "D" & N2 %between% c(0L, 48L)),
    "All other cancers",
    
    # 15. Diabetes
    L1 == "E" & N2 %between% c(10L, 14L),
    "Diabetes",
    
    # 16. Endocrine, nutritional, and metabolic diseases excl. Diabetes
    L1 == "E" & N2 %between% c(0L, 88L),
    "Endocrine, nutritional, and metabolic diseases excl. Diabetes",
    
    # 17. Mental disorders
    L1 == "F" & N2 %between% c(0L, 99L),
    "Mental disorders",
    
    # 18. Nervous system, excl. ADRD
    L1 == "G" & N2 %between% c(0L, 98L),
    "Nervous system, excl. ADRD",
    
    # 19. Hypertensive heart disease
    L1 == "I" & N2 %between% c(10L, 15L),
    "Hypertensive heart disease",
    
    # 20. Circulatory diseases, excl. Hypertensive heart disease
    L1 == "I" & N2 %between% c(0L, 99L),
    "Circulatory diseases, excl. Hypertensive heart disease",
    
    # 21. Influenza and pneumonia
    L1 == "J" & N2 %between% c(9L, 18L),
    "Influenza and pneumonia",
    
    # 22. Respiratory diseases excl. Influenza and pneumonia
    L1 == "J" & N2 %between% c(0L, 98L),
    "Respiratory diseases excl. Influenza and pneumonia",
    
    # 23. Diseases of the digestive system
    L1 == "K" & N2 %between% c(0L, 92L),
    "Diseases of the digestive system",
    
    # 24. Diseases of the genitourinary system
    L1 == "N" & N2 %between% c(0L, 98L),
    "Diseases of the genitourinary system",
    
    # 25. Perinatal conditions and congenital anomalies
    (L1 == "P" & N2 %between% c(0L, 96L)) |
      (L1 == "Q" & N2 %between% c(0L, 99L)),
    "Perinatal conditions and congenital anomalies",
    
    # 26. Symptoms, signs, and ill-defined conditions
    L1 == "R" & N2 %between% c(0L, 99L),
    "Symptoms, signs, and ill-defined conditions",
    
    # 27. All other causes
    default = "All other causes"
  )
}

#' Dispatch cause-of-death classification by data year (ICD-9 for 1989-1998, ICD-10 for 1999+)
#' @keywords internal
clean_cod <- function(ucod, year = 2003L) {
  if (year <= 1998L) {
    clean_cod_icd9(ucod)
  } else {
    clean_cod_icd10(ucod)
  }
}