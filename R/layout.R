#' Retrieve fixed-width tape positions for a given NVSS data year (1989 onwards)
#'
#' @param year Integer data year (>= 1989).
#' @return A named list of field specifications (start, end, type).
#' @keywords internal
get_nvss_layout <- function(year) {
  year <- as.integer(year)
  if (is.na(year) || year < 1989L) {
    stop("Years prior to 1989 are not supported in nvssread.", call. = FALSE)
  }
  
  # Pre-2003 layout (1989-2002):
  # - Official FIPS state/county codes live at 119-128 (occurrence 119-120,
  #   residence state 124-125, residence county 126-128).
  # - Data year is 2-digit at 1-2 in 1989-1995, and 4-digit at 115-118 in 1996-2002.
  if (year <= 2002L) {
    yr_pos <- if (year <= 1995L) {
      list(start = 1L, end = 2L, type = "i")
    } else {
      list(start = 115L, end = 118L, type = "i")
    }
    
    return(list(
      year             = yr_pos,
      month            = list(start =  55L, end =  56L, type = "i"),
      state_occurrence = list(start = 119L, end = 120L, type = "c"),
      state_residence  = list(start = 124L, end = 125L, type = "c"),
      county_residence = list(start = 126L, end = 128L, type = "c"),
      birth_place      = list(start =  78L, end =  79L, type = "c"),
      sex              = list(start =  59L, end =  59L, type = "c"),
      age_type         = list(start =  64L, end =  64L, type = "i"),
      age_val          = list(start =  65L, end =  66L, type = "i"),
      ucod             = list(start = 142L, end = 145L, type = "c"),
      race_detail      = list(start =  60L, end =  61L, type = "c"),
      hispanic_origin  = list(start =  80L, end =  81L, type = "i")
    ))
  }
  
  # 2003 onwards All-County Mortality Layout
  layout <- list(
    year             = list(start = 102L, end = 105L, type = "i"),
    month            = list(start =  65L, end =  66L, type = "i"),
    state_occurrence = list(start =  21L, end =  22L, type = "c"),
    state_residence  = list(start =  29L, end =  30L, type = "c"),
    county_residence = list(start =  35L, end =  37L, type = "c"),
    birth_place      = list(start =  59L, end =  60L, type = "c"),
    sex              = list(start =  69L, end =  69L, type = "c"),
    age_type         = list(start =  70L, end =  70L, type = "i"),
    age_val          = list(start =  71L, end =  73L, type = "i"),
    ucod             = list(start = 146L, end = 149L, type = "c"),
    race_detail      = list(start = 445L, end = 446L, type = "c"),
    race_rec5        = list(start = 450L, end = 450L, type = "c"),
    hispanic_origin  = list(start = 484L, end = 486L, type = "i")
  )
  
  # Race Recode 40 (positions 489-490) only exists starting in 2012
  if (year >= 2012L) {
    layout$race_rec40 <- list(start = 489L, end = 490L, type = "c")
  }
  
  layout
}

#' Determine minimal raw fields needed for requested tabulation variables
#' @keywords internal
required_raw_fields <- function(vars, year, geo_filter = "residence", race_groups = "detailed") {
  fields <- c("year")
  
  if (geo_filter %in% c("residence", "both") || any(c("state", "county") %in% vars)) {
    fields <- c(fields, "state_residence")
  }
  if (geo_filter %in% c("occurrence", "both")) {
    fields <- c(fields, "state_occurrence")
  }
  if ("month" %in% vars) {
    fields <- c(fields, "month")
  }
  if ("county" %in% vars) {
    fields <- c(fields, "county_residence")
  }
  if ("sex" %in% vars) {
    fields <- c(fields, "sex")
  }
  if ("age" %in% vars) {
    fields <- c(fields, "age_type", "age_val")
  }
  if ("nativity" %in% vars) {
    fields <- c(fields, "birth_place")
  }
  if ("cod" %in% vars) {
    fields <- c(fields, "ucod")
  }
  if ("race_eth" %in% vars) {
    fields <- c(fields, "race_detail")
    if (year >= 2003L) {
      fields <- c(fields, "race_rec5")
    }
    if (race_groups != "bw") {
      fields <- c(fields, "hispanic_origin")
    }
    if (year >= 2012L) {
      fields <- c(fields, "race_rec40")
    }
  }
  
  unique(fields)
}