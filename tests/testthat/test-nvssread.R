test_that("clean_cod respects priority rules, ADRD, COVID-19, and external residual", {
  codes <- c(
    "X65",   # Alcohol-related suicide -> Alcohol-induced (not Suicide)
    "K703",  # Alcoholic cirrhosis -> Alcohol-induced (not Digestive)
    "G312",  # Alcohol nervous system -> Alcohol-induced (not Nervous/ADRD)
    "U071",  # COVID-19
    "G309",  # Alzheimer's -> ADRD
    "F019",  # Vascular dementia -> ADRD (not Mental)
    "G318",  # Lewy body / other specified deg -> ADRD (not Nervous)
    "G20",   # Parkinson's -> Nervous system, excl. ADRD
    "X42",   # Accidental drug poisoning -> Drug overdose
    "X60",   # Drug suicide -> Drug overdose (not Suicide)
    "X85",   # Drug homicide -> Drug overdose (not Homicide)
    "X95",   # Firearm homicide -> Homicide
    "X72",   # Firearm suicide -> Suicide
    "V435",  # Transport accident
    "W19",   # Fall -> Other external causes
    "I119",  # Hypertensive heart disease
    "I219",  # Circulatory excl. HHD
    "J189",  # Influenza and pneumonia
    "J449",  # Respiratory excl. flu/pneumonia
    "C349",  # Lung cancer
    "C509",  # Breast cancer
    "U014",  # Terrorism -> All other causes
    "   "    # Missing
  )
  
  res <- nvssread:::clean_cod(codes)
  expected <- c(
    "Alcohol-induced",
    "Alcohol-induced",
    "Alcohol-induced",
    "COVID-19",
    "ADRD",
    "ADRD",
    "ADRD",
    "Nervous system, excl. ADRD",
    "Drug overdose",
    "Drug overdose",
    "Drug overdose",
    "Homicide",
    "Suicide",
    "Transport accidents",
    "Other external causes",
    "Hypertensive heart disease",
    "Circulatory diseases, excl. Hypertensive heart disease",
    "Influenza and pneumonia",
    "Respiratory diseases excl. Influenza and pneumonia",
    "Lung cancer",
    "Breast, prostate, colorectal, and cervical cancer",
    "All other causes",
    NA_character_
  )
  
  expect_equal(res, expected)
})

test_that("clean_age handles infant units, abridged top-coding, and single-year top-coding", {
  # 2/999 = infant not stated months -> 0; 1/999 and 9/999 -> NA
  at <- c(2L, 4L, 1L, 1L, 1L, 1L, 9L)
  av <- c(999L, 5L, 3L, 84L, 92L, 999L, 999L)
  
  expect_equal(
    as.character(nvssread:::clean_age(at, av, mode = "abridged", age_top = 85L)),
    c("00-00", "00-00", "01-04", "80-84", "85+", NA, NA)
  )
  expect_equal(
    as.character(nvssread:::clean_age(at, av, mode = "abridged", age_top = 95L)),
    c("00-00", "00-00", "01-04", "80-84", "90-94", NA, NA)
  )
  expect_equal(
    as.character(nvssread:::clean_age(c(1L, 1L, 1L), c(0L, 99L, 104L), mode = "single", age_top = 100L)),
    c("0", "99", "100+")
  )
})

test_that("clean_race_eth gives Hispanic precedence, maps code 23 to NH API, and flags unknown Hispanic as NA", {
  hisp <- c(210L, 100L, 100L, 100L, 999L)
  rdet <- c("02", "01", "07", "03", "01")
  r5   <- c("2",  "1",  "4",  "3",  "1")
  r40  <- c("02", "23", "15", "03", "01")
  
  res <- nvssread:::clean_race_eth(hisp, rdet, r5, r40, year = 2022L, race_type = "single_multi")
  expect_equal(res, c("Hispanic", "NH API", "NH Multiple/Other", "NH AIAN", NA_character_))
})

test_that("redistribute_deaths strictly conserves deaths even when exact stratum has D == 0", {
  # County 001 has ONLY a missing-age death (D == 0 in county 001);
  # fallback must borrow age weights from state 01 (county 003) while keeping
  # those 10 deaths inside county 001.
  dt <- data.table::data.table(
    year        = 2020L,
    state_FIPS  = c("01", "01", "01"),
    county_FIPS = c("001", "003", "003"),
    sex         = c("Female", "Female", "Female"),
    age         = c(NA_character_, "00-00", "85+"),
    nDx         = c(10, 20, 80)
  )
  
  out <- nvssread:::redistribute_deaths(dt, tab_vars = c("year", "state_FIPS", "county_FIPS", "sex", "age"))
  
  expect_equal(sum(out$nDx), 110)
  expect_equal(sum(out[county_FIPS == "001"]$nDx), 10)
  expect_equal(out[county_FIPS == "001" & age == "00-00"]$nDx, 2)
  expect_equal(out[county_FIPS == "001" & age == "85+"]$nDx, 8)
  expect_false(anyNA(out$age))
})

test_that("clean_race_eth supports 'wbh' and 'bw' historical groupings", {
  # Rows: Hispanic Black, NH White, NH Black, NH Asian+NHOPI (23), NH AIAN, NH Black+White (15), Unknown Hisp White
  hisp <- c(210L, 100L, 100L, 100L, 100L, 100L, 999L)
  rdet <- c("02", "01", "02", "78", "03", "02", "01")
  r5   <- c("2",  "1",  "2",  "4",  "3",  "2",  "1")
  r40  <- c("02", "01", "02", "23", "03", "15", "01")
  
  # wbh: NH White, NH Black, Hispanic, NH Other; unknown Hispanic -> NA
  res_wbh <- nvssread:::clean_race_eth(
    hisp, rdet, r5, r40, year = 2022L,
    race_type = "single_multi", race_groups = "wbh"
  )
  expect_equal(
    res_wbh,
    c("Hispanic", "NH White", "NH Black", "NH Other", "NH Other", "NH Other", NA_character_)
  )
  
  # bw: White, Black, Other (including Hispanics; unknown Hispanic ignored)
  res_bw <- nvssread:::clean_race_eth(
    hispanic_origin = NULL, rdet, r5, r40, year = 2022L,
    race_type = "single_multi", race_groups = "bw"
  )
  expect_equal(
    res_bw,
    c("Black", "White", "Black", "Other", "Other", "Other", "White")
  )
  
  # bw with bridged race (pre-2021): Black+White (15) uses bridged Black ("02")
  res_bw_bridged <- nvssread:::clean_race_eth(
    hispanic_origin = NULL, rdet, r5, r40, year = 2019L,
    race_type = "bridged", race_groups = "bw"
  )
  expect_equal(
    res_bw_bridged,
    c("Black", "White", "Black", "Other", "Other", "Black", "White")
  )
})

test_that("pre-2003 age, sex, nativity, and race/Hispanic cleaning work accurately", {
  # Sex: 1=Male, 2=Female
  expect_equal(nvssread:::clean_sex(c("1", "2", "9")), c("Male", "Female", NA_character_))
  
  # 1989-2002 3-digit Detail Age: 0/84 -> 84, 1/03 -> 103, 3/02 (weeks) -> 0, 9/99 -> NA
  expect_equal(
    as.character(nvssread:::clean_age(c(3L, 0L, 0L, 1L, 9L), c(2L, 4L, 84L, 3L, 99L), year = 1995L, mode = "abridged", age_top = 85L)),
    c("00-00", "01-04", "80-84", "85+", NA)
  )
  expect_equal(
    as.character(nvssread:::clean_age(c(0L, 1L, 1L), c(99L, 0L, 4L), year = 1995L, mode = "single", age_top = 100L)),
    c("99", "100+", "100+")
  )
  
  # Numeric State of Birth (78-79): 01-51 US, 52/61 Territory, 57/59 Foreign, 99 NA
  expect_equal(
    nvssread:::clean_nativity(c("06", "51", "52", "61", "57", "59", "99"), year = 1998L),
    c("US-born", "US-born", "US Territory", "US Territory", "Foreign-born", "Foreign-born", NA_character_)
  )
  
  # Pre-2003 Hispanic (00 NH, 01-05 Hisp, 99 NA) & 1989-1991 Race code 09
  expect_equal(
    nvssread:::clean_race_eth(c(0L, 1L, 0L, 99L), c("01", "02", "09", "01"), year = 1990L, race_groups = "detailed"),
    c("NH White", "Hispanic", "NH Multiple/Other", NA_character_)
  )
})

test_that("clean_cod_icd9 handles omitted-'E' external codes, ADRD, alcohol, and residuals", {
  codes <- c(
    "8600", # Alcohol poisoning (E860.0) -> Alcohol-induced
    "5712", # Alcoholic cirrhosis -> Alcohol-induced (not Digestive)
    "5715", # Non-alcoholic cirrhosis -> Digestive
    "3310", # Alzheimer's -> ADRD
    "2900", # Senile dementia -> ADRD (not Mental)
    "2941", # Dementia in other diseases -> ADRD
    "3320", # Parkinson's -> Nervous system, excl. ADRD
    "8500", # Accidental drug poisoning -> Drug overdose
    "9500", # Drug suicide -> Drug overdose (not Suicide)
    "9506", # Non-drug poisoning suicide -> Suicide
    "9620", # Drug homicide -> Drug overdose (not Homicide)
    "9650", # Firearm homicide -> Homicide
    "8161", # Motor vehicle -> Transport accidents
    "9290", # Late effect MV -> Transport accidents
    "916",  # Struck by falling object (3-digit) -> Other external causes
    "0420", # HIV/AIDS
    "1629", # Lung cancer
    "1550", # Liver cancer
    "1749", # Breast cancer
    "1509", # Esophageal cancer -> All other cancers
    "4029", # Hypertensive heart disease
    "410",  # Acute MI -> Circulatory excl. HHD
    "486"   # Pneumonia -> Influenza and pneumonia
  )
  
  res <- nvssread:::clean_cod(codes, year = 1996L)
  expected <- c(
    "Alcohol-induced",
    "Alcohol-induced",
    "Diseases of the digestive system",
    "ADRD",
    "ADRD",
    "ADRD",
    "Nervous system, excl. ADRD",
    "Drug overdose",
    "Drug overdose",
    "Suicide",
    "Drug overdose",
    "Homicide",
    "Transport accidents",
    "Transport accidents",
    "Other external causes",
    "HIV/AIDS",
    "Lung cancer",
    "Liver cancer",
    "Breast, prostate, colorectal, and cervical cancer",
    "All other cancers",
    "Hypertensive heart disease",
    "Circulatory diseases, excl. Hypertensive heart disease",
    "Influenza and pneumonia"
  )
  
  expect_equal(res, expected)
})

test_that("harmonize_county_fips collapses CT counties/planning regions and historical changes", {
  st  <- c("09",  "09",  "12",  "51",  "08",  "02",  "06")
  cty <- c("001", "110", "025", "515", "014", "063", "037")
  res <- nvssread:::harmonize_county_fips(st, cty)
  
  expect_equal(res, c("900", "900", "086", "019", "901", "261", "037"))
})