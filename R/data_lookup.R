#' Internal lookup table for US states and DC
#' @return A data.table with state_abb, state_FIPS, and state_name
#' @keywords internal
nvss_states_lookup <- function() {
  data.table::data.table(
    state_abb = c(
      "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
      "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
      "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
      "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
      "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
    ),
    state_FIPS = c(
      "01", "02", "04", "05", "06", "08", "09", "10", "11", "12",
      "13", "15", "16", "17", "18", "19", "20", "21", "22", "23",
      "24", "25", "26", "27", "28", "29", "30", "31", "32", "33",
      "34", "35", "36", "37", "38", "39", "40", "41", "42", "44",
      "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56"
    ),
    state_name = c(
      "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
      "Connecticut", "Delaware", "District of Columbia", "Florida",
      "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",
      "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
      "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska",
      "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York",
      "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
      "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
      "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
      "West Virginia", "Wisconsin", "Wyoming"
    )
  )
}

#' Internal vector of US territory postal codes used in NVSS files
#' @keywords internal
nvss_territories <- function() {
  c("PR", "VI", "GU", "AS", "MP")
}

#' List cause-of-death categories used by nvssread
#'
#' Returns the ordered character vector of the 27 mutually exclusive ICD-10
#' cause-of-death groups implemented in \code{nvssread}.
#'
#' @return Character vector of 27 cause-of-death category labels.
#' @export
nvss_cod_groups <- function() {
  c(
    "HIV/AIDS",
    "Other infectious and parasitic diseases",
    "Liver cancer",
    "Lung cancer",
    "Breast, prostate, colorectal, and cervical cancer",
    "All other cancers",
    "Mental disorders",
    "Nervous system, excl. ADRD",
    "ADRD",
    "Hypertensive heart disease",
    "Circulatory diseases, excl. Hypertensive heart disease",
    "Respiratory diseases excl. Influenza and pneumonia",
    "Influenza and pneumonia",
    "Diseases of the digestive system",
    "Diabetes",
    "Endocrine, nutritional, and metabolic diseases excl. Diabetes",
    "Diseases of the genitourinary system",
    "Perinatal conditions and congenital anomalies",
    "Homicide",
    "Suicide",
    "Drug overdose",
    "Alcohol-induced",
    "Transport accidents",
    "Other external causes",
    "COVID-19",
    "Symptoms, signs, and ill-defined conditions",
    "All other causes"
  )
}

#' Longitudinal US county FIPS crosswalk (1989-present)
#'
#' Returns the internal mapping table used when \code{harmonize_counties = TRUE}
#' in \code{\link{nvss_tabulate}}. Researchers can also merge this table onto
#' Census population denominator files so county numerators and denominators
#' share identical geographic boundaries across 1989-present.
#'
#' Dedicated synthetic county suffixes (\code{"900"}, \code{"901"}) are used for
#' multi-county clusters (Connecticut statewide, Broomfield CO cluster, and
#' Southeast Alaska cluster) so they never collide with real FIPS codes,
#' foreign codes (\code{"000"}), or missing codes (\code{"999"}).
#'
#' @return A \code{data.table} with columns \code{raw_FIPS}, \code{harmonized_FIPS},
#'   \code{harmonized_state_FIPS}, \code{harmonized_county_FIPS}, and \code{reason}.
#' @export
nvss_county_crosswalk <- function() {
  xw <- data.table::rbindlist(list(
    # --- 1-to-1 renames / recodes ---
    data.table::data.table(
      raw_FIPS        = c("12025", "46113", "02270"),
      harmonized_FIPS = c("12086", "46102", "02158"),
      reason          = c(
        "Dade County, FL renamed Miami-Dade (1997)",
        "Shannon County, SD renamed Oglala Lakota (2015)",
        "Wade Hampton Census Area, AK renamed Kusilvak (2015)"
      )
    ),
    
    # --- Defunct counties / VA independent cities absorbed into surrounding county ---
    data.table::data.table(
      raw_FIPS        = c("51780", "51560", "51515", "30113", "15005"),
      harmonized_FIPS = c("51083", "51005", "51019", "30031", "15009"),
      reason          = c(
        "South Boston city, VA reverted to Halifax County (1995)",
        "Clifton Forge city, VA reverted to Alleghany County (2001)",
        "Bedford city, VA reverted to Bedford County (2013)",
        "Yellowstone National Park, MT abolished (1997; mapped to Gallatin)",
        "Kalawao County, HI combined with Maui County"
      )
    ),
    
    # --- Connecticut: combine all 8 legacy counties + all 9 planning regions (2022+) ---
    data.table::data.table(
      raw_FIPS        = c(
        "09001", "09003", "09005", "09007", "09009", "09011", "09013", "09015",
        "09110", "09120", "09130", "09140", "09150", "09160", "09170", "09180", "09190"
      ),
      harmonized_FIPS = "09900",
      reason          = "Connecticut: all pre-2022 counties and 2022+ planning regions combined statewide"
    ),
    
    # --- Colorado: Broomfield (08014, created 2001 from Adams, Boulder, Jefferson, Weld) ---
    data.table::data.table(
      raw_FIPS        = c("08001", "08013", "08014", "08059", "08123"),
      harmonized_FIPS = "08901",
      reason          = "Colorado: Adams, Boulder, Broomfield, Jefferson, Weld combined for 2001 Broomfield split"
    ),
    
    # --- Alaska: Aleutians split (02010 -> 02013 + 02016) ---
    data.table::data.table(
      raw_FIPS        = c("02010", "02013", "02016"),
      harmonized_FIPS = "02010",
      reason          = "Alaska: Aleutians East & West combined with legacy Aleutian Islands"
    ),
    
    # --- Alaska: Valdez-Cordova split (2019: 02261 -> 02063 + 02066) ---
    data.table::data.table(
      raw_FIPS        = c("02063", "02066", "02261"),
      harmonized_FIPS = "02261",
      reason          = "Alaska: Chugach & Copper River combined with legacy Valdez-Cordova"
    ),
    
    # --- Alaska: Denali created from Yukon-Koyukuk (1990) ---
    data.table::data.table(
      raw_FIPS        = c("02068", "02290"),
      harmonized_FIPS = "02290",
      reason          = "Alaska: Denali Borough combined with Yukon-Koyukuk"
    ),
    
    # --- Alaska: Lake and Peninsula created from Dillingham (1989) ---
    data.table::data.table(
      raw_FIPS        = c("02070", "02164"),
      harmonized_FIPS = "02070",
      reason          = "Alaska: Lake and Peninsula Borough combined with Dillingham"
    ),
    
    # --- Alaska: Southeast AK multi-step splits & boundary transfers (1992-2013) ---
    data.table::data.table(
      raw_FIPS        = c(
        "02105", "02130", "02195", "02198", "02201",
        "02230", "02231", "02232", "02275", "02280", "02282"
      ),
      harmonized_FIPS = "02901",
      reason          = "Alaska: Southeast AK reorganization cluster (Skagway/Yakutat/Angoon/Wrangell/Petersburg/POW/Ketchikan)"
    )
  ))
  
  xw[, harmonized_state_FIPS  := substr(harmonized_FIPS, 1L, 2L)]
  xw[, harmonized_county_FIPS := substr(harmonized_FIPS, 3L, 5L)]
  xw[]
}