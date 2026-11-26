#' Read, clean, and tabulate NVSS mortality files (1989 onwards)
#'
#' Reads fixed-width NVSS All-County mortality microdata files (1989 onwards),
#' filters to US residents (50 states + DC by default), cleans selected
#' demographic, geographic, and cause-of-death variables (ICD-9 for 1989-1998;
#' ICD-10 for 1999+), optionally harmonizes longitudinal county FIPS boundaries,
#' reports missingness, optionally performs sequential proportional
#' redistribution of missing values with fallback, and saves one CSV file per year.
#'
#' @param data_dir Path to the base directory containing NVSS data files.
#' @param years Integer vector of data years to process (1989 onwards).
#' @param vars Character vector of variables to tabulate by. Any subset of:
#'   \code{"month"}, \code{"state"}, \code{"county"}, \code{"sex"}, \code{"age"},
#'   \code{"race_eth"}, \code{"nativity"}, \code{"cod"}. \code{"year"} is always
#'   included. Selecting \code{"county"} automatically includes \code{"state"} and
#'   adds a 5-digit \code{FIPS_code} column.
#' @param age_type Character scalar: \code{"abridged"} (default) or \code{"single"}.
#' @param age_top Optional integer top-coding threshold for age. Defaults to
#'   \code{85L} for \code{"abridged"} (multiple of 5) and \code{100L} for
#'   \code{"single"} (pass \code{Inf} or \code{NA} to disable top-coding).
#' @param race_type Character scalar: \code{"single_multi"} (default) or
#'   \code{"bridged"}. Ignored if \code{"race_eth"} is not in \code{vars}.
#' @param race_groups Character scalar controlling race/ethnicity categorization
#'   when \code{"race_eth"} is in \code{vars}: \code{"detailed"} (default),
#'   \code{"wbh"} (NH White, NH Black, Hispanic, NH Other), or \code{"bw"}
#'   (White, Black, Other including Hispanics).
#' @param harmonize_counties Logical. If \code{TRUE}, applies
#'   \code{\link{nvss_county_crosswalk}} to harmonize county FIPS codes over
#'   1989-present (including combining all Connecticut counties and 2022+
#'   planning regions into \code{"09900"}). Defaults to \code{FALSE}.
#' @param redistribute Logical. If \code{TRUE} (default), sequentially
#'   redistributes deaths with missing values across all requested variables.
#' @param complete_grid Logical. If \code{TRUE}, expands the output to include
#'   zero-death cells for all combinations of observed levels. Defaults to \code{FALSE}.
#' @param geo_filter Character scalar: \code{"residence"} (default, 50 US states
#'   + DC residents), \code{"occurrence"}, or \code{"both"}.
#' @param output_dir Optional directory path where annual CSV files will be saved.
#' @param output_pattern Character glue pattern for annual output filenames inside
#'   \code{output_dir}. Defaults to \code{"nvss_deaths_{year}.csv"}.
#' @param file_pattern Character glue pattern for locating input files relative
#'   to \code{data_dir}. Defaults to
#'   \code{file.path("mort{year}", "MULT{year}USAllCnty.txt")}.
#' @param return_data Logical. Whether to return the combined \code{data.table}
#'   in memory. Defaults to \code{is.null(output_dir)}.
#'
#' @return If \code{return_data = TRUE}, a \code{data.table} combining all
#'   processed years. Otherwise, invisibly returns a character vector of written
#'   CSV file paths.
#' @export
nvss_tabulate <- function(
    data_dir,
    years,
    vars = c("state", "county", "sex", "age"),
    age_type = c("abridged", "single"),
    age_top = NULL,
    race_type = c("single_multi", "bridged"),
    race_groups = c("detailed", "wbh", "bw"),
    harmonize_counties = FALSE,
    redistribute = TRUE,
    complete_grid = FALSE,
    geo_filter = c("residence", "occurrence", "both"),
    output_dir = NULL,
    output_pattern = "nvss_deaths_{year}.csv",
    file_pattern = file.path("mort{year}", "MULT{year}USAllCnty.txt"),
    return_data = is.null(output_dir)) {
  
  # --- Validate arguments ---
  years <- sort(unique(as.integer(years)))
  if (length(years) == 0L || anyNA(years)) {
    stop("'years' must be a non-empty integer vector.", call. = FALSE)
  }
  if (any(years < 1989L)) {
    stop("Years prior to 1989 are not supported in nvssread.", call. = FALSE)
  }
  
  valid_vars <- c("month", "state", "county", "sex", "age", "race_eth", "nativity", "cod")
  vars <- unique(as.character(vars))
  bad_vars <- setdiff(vars, valid_vars)
  if (length(bad_vars) > 0L) {
    stop(
      sprintf(
        "Unknown variable(s) in 'vars': %s. Allowed values: %s.",
        paste(bad_vars, collapse = ", "),
        paste(valid_vars, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  if ("county" %in% vars && !("state" %in% vars)) {
    message("Note: 'county' requested without 'state'; automatically including 'state' so county FIPS codes are unambiguous.")
    vars <- c("state", vars)
  }
  
  age_type    <- match.arg(age_type)
  race_type   <- match.arg(race_type)
  race_groups <- match.arg(race_groups)
  geo_filter  <- match.arg(geo_filter)
  
  # --- Upfront harmonization warnings ---
  if ("race_eth" %in% vars) {
    if (race_groups != "bw" && any(years < 1997L)) {
      warning(
        "Hispanic origin was not reported by all US states prior to 1997 ",
        "(e.g., Oklahoma through 1996, New Hampshire through 1992, Louisiana through 1990). ",
        "Decedents from non-reporting states will have missing 'race_eth' (NA) when ",
        "race_groups = '", race_groups, "'. Use race_groups = 'bw' for complete pre-1997 coverage.",
        call. = FALSE
      )
    }
    if (race_type == "bridged" && any(years >= 2020L)) {
      warning(
        "race_type = 'bridged' requested for years including 2020 or later. ",
        "NCHS discontinued bridged race after 2020; any blank bridged fields in ",
        "2021+ files will fall back to Race Recode 40.",
        call. = FALSE
      )
    }
    if (race_type == "single_multi" && any(years < 2012L)) {
      warning(
        "race_type = 'single_multi' requested for years prior to 2012. ",
        "Race Recode 40 is only available from 2012 onwards (nationally complete ",
        "from 2018 onwards); pre-2012 years will use single/bridged race.",
        call. = FALSE
      )
    } else if (race_type == "single_multi" && any(years %between% c(2012L, 2017L))) {
      message(
        "Note: Between 2012 and 2017, states were still phasing in the 2003 death ",
        "certificate; multiracial counts are nationally complete from 2018 onwards."
      )
    }
  }
  
  if ("cod" %in% vars && any(years <= 1998L) && any(years >= 1999L)) {
    message(
      "Note: Requested years span the 1998/1999 ICD-9 to ICD-10 transition. ",
      "ICD-9 rules are applied for <=1998 and ICD-10 rules for >=1999; consider ",
      "NCHS comparability ratios when analyzing trends across 1998-1999."
    )
  }
  
  if (!is.null(output_dir) && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  states_dt <- nvss_states_lookup()
  us_fips   <- states_dt$state_FIPS
  
  out_tables <- if (isTRUE(return_data)) vector("list", length(years)) else NULL
  out_paths  <- character(length(years))
  
  for (idx in seq_along(years)) {
    yr <- years[[idx]]
    file_path <- file.path(data_dir, glue::glue(file_pattern, year = yr))
    message(sprintf("\n=== Processing NVSS year %d ===", yr))
    
    dt <- read_nvss_year(
      file = file_path, year = yr, vars = vars,
      geo_filter = geo_filter, race_groups = race_groups
    )
    
    # Normalize 2-digit data year in 1989-1995 files (e.g., 89 -> 1989)
    dt[, year := data.table::fifelse(!is.na(year) & year < 100L, 1900L + year, as.integer(year))]
    
    # Standardize state_residence / state_occurrence to 2-digit character FIPS
    if ("state_residence" %in% names(dt)) {
      if (yr <= 2002L) {
        dt[, state_res_FIPS := sprintf("%02d", suppressWarnings(as.integer(state_residence)))]
        dt[state_res_FIPS == "NA", state_res_FIPS := NA_character_]
      } else {
        m_res <- match(toupper(dt$state_residence), states_dt$state_abb)
        dt[, state_res_FIPS := states_dt$state_FIPS[m_res]]
      }
    }
    
    if ("state_occurrence" %in% names(dt)) {
      if (yr <= 2002L) {
        dt[, state_occ_FIPS := sprintf("%02d", suppressWarnings(as.integer(state_occurrence)))]
        dt[state_occ_FIPS == "NA", state_occ_FIPS := NA_character_]
      } else {
        m_occ <- match(toupper(dt$state_occurrence), states_dt$state_abb)
        dt[, state_occ_FIPS := states_dt$state_FIPS[m_occ]]
      }
    }
    
    # --- Geographic filter (50 US states + DC) ---
    n_raw <- nrow(dt)
    if (geo_filter == "residence") {
      dt <- dt[state_res_FIPS %chin% us_fips]
    } else if (geo_filter == "occurrence") {
      dt <- dt[state_occ_FIPS %chin% us_fips]
    } else if (geo_filter == "both") {
      dt <- dt[state_res_FIPS %chin% us_fips & state_occ_FIPS %chin% us_fips]
    }
    n_filtered <- nrow(dt)
    message(sprintf(
      "Records read: %s | Kept after '%s' (50 states + DC) filter: %s",
      format(n_raw, big.mark = ","),
      geo_filter,
      format(n_filtered, big.mark = ",")
    ))
    
    # --- Clean requested variables ---
    tab_vars <- c("year")
    age_levels <- NULL
    
    if ("month" %in% vars) {
      dt[, month := data.table::fifelse(month %between% c(1L, 12L), as.integer(month), NA_integer_)]
      tab_vars <- c(tab_vars, "month")
    }
    
    if ("state" %in% vars) {
      dt[, state_FIPS := state_res_FIPS]
      tab_vars <- c(tab_vars, "state_FIPS")
    }
    
    if ("county" %in% vars) {
      dt[, county_FIPS := data.table::fifelse(
        is.na(county_residence) | county_residence %chin% c("", "000", "999"),
        NA_character_,
        sprintf("%03d", suppressWarnings(as.integer(county_residence)))
      )]
      dt[county_FIPS == " NA", county_FIPS := NA_character_]
      
      if (isTRUE(harmonize_counties)) {
        dt[, county_FIPS := harmonize_county_fips(state_FIPS, county_FIPS)]
      }
      tab_vars <- c(tab_vars, "county_FIPS")
    }
    
    if ("sex" %in% vars) {
      dt[, sex := clean_sex(sex)]
      tab_vars <- c(tab_vars, "sex")
    }
    
    if ("age" %in% vars) {
      cleaned_age <- clean_age(dt$age_type, dt$age_val, year = yr, mode = age_type, age_top = age_top)
      age_levels <- attr(cleaned_age, "age_levels")
      dt[, age := as.character(cleaned_age)]
      tab_vars <- c(tab_vars, "age")
    }
    
    if ("race_eth" %in% vars) {
      r5_vec   <- if ("race_rec5" %in% names(dt)) dt$race_rec5 else NULL
      r40_vec  <- if ("race_rec40" %in% names(dt)) dt$race_rec40 else NULL
      hisp_vec <- if ("hispanic_origin" %in% names(dt)) dt$hispanic_origin else NULL
      dt[, race_eth := clean_race_eth(
        hispanic_origin = hisp_vec,
        race_detail     = dt$race_detail,
        race_rec5       = r5_vec,
        race_rec40      = r40_vec,
        year            = yr,
        race_type       = race_type,
        race_groups     = race_groups
      )]
      tab_vars <- c(tab_vars, "race_eth")
    }
    
    if ("nativity" %in% vars) {
      dt[, nativity := clean_nativity(birth_place, year = yr)]
      tab_vars <- c(tab_vars, "nativity")
    }
    
    if ("cod" %in% vars) {
      dt[, cod := clean_cod(ucod, year = yr)]
      tab_vars <- c(tab_vars, "cod")
    }
    
    # --- Report missingness for every requested variable ---
    check_cols <- setdiff(tab_vars, "year")
    miss_counts <- vapply(check_cols, function(col) sum(is.na(dt[[col]])), integer(1L))
    if (all(miss_counts == 0L)) {
      message("Missing values across requested variables: none.")
    } else {
      miss_parts <- vapply(names(miss_counts), function(col) {
        cnt <- miss_counts[[col]]
        sprintf("%s = %s (%.3f%%)", col, format(cnt, big.mark = ","), 100 * cnt / max(n_filtered, 1L))
      }, character(1L))
      message("Missing values: ", paste(miss_parts, collapse = " | "))
    }
    
    # --- Aggregate raw counts ---
    tbl <- dt[, .(nDx = as.numeric(.N)), by = tab_vars]
    rm(dt)
    
    # --- Optional sequential redistribution ---
    if (isTRUE(redistribute)) {
      tbl <- redistribute_deaths(tbl, tab_vars = tab_vars)
    }
    
    # --- Optional grid completion ---
    if (isTRUE(complete_grid)) {
      tbl <- expand_table_grid(tbl, tab_vars = tab_vars, age_levels = age_levels)
    }
    
    # --- Add 5-digit FIPS_code if county is present ---
    if ("county_FIPS" %in% tab_vars) {
      tbl[, FIPS_code := data.table::fifelse(
        !is.na(state_FIPS) & !is.na(county_FIPS),
        paste0(state_FIPS, county_FIPS),
        NA_character_
      )]
    }
    
    # --- Sort cleanly ---
    if ("age" %in% tab_vars && !is.null(age_levels)) {
      tbl[, .age_ord := match(age, age_levels)]
      sort_cols <- replace(tab_vars, tab_vars == "age", ".age_ord")
      data.table::setorderv(tbl, sort_cols, na.last = TRUE)
      tbl[, .age_ord := NULL]
    } else {
      data.table::setorderv(tbl, tab_vars, na.last = TRUE)
    }
    
    if ("FIPS_code" %in% names(tbl)) {
      other_after <- setdiff(names(tbl), c(tab_vars, "FIPS_code", "nDx"))
      data.table::setcolorder(tbl, c(tab_vars, "FIPS_code", other_after, "nDx"))
    }
    
    message(sprintf(
      "Tabulated rows: %s | Total deaths: %s",
      format(nrow(tbl), big.mark = ","),
      format(round(sum(tbl$nDx), 2), big.mark = ",")
    ))
    
    # --- Write annual CSV ---
    if (!is.null(output_dir)) {
      out_file <- file.path(output_dir, glue::glue(output_pattern, year = yr))
      data.table::fwrite(tbl, out_file)
      out_paths[[idx]] <- out_file
      message(sprintf("Saved: %s", out_file))
    }
    
    if (isTRUE(return_data)) {
      out_tables[[idx]] <- tbl
    }
  }
  
  if (isTRUE(return_data)) {
    return(data.table::rbindlist(out_tables, use.names = TRUE, fill = TRUE))
  }
  
  invisible(out_paths)
}

#' Expand observed table to full Cartesian grid (preserving observed state-county pairs)
#' @keywords internal
expand_table_grid <- function(tbl, tab_vars, age_levels = NULL) {
  has_geo_pair <- all(c("state_FIPS", "county_FIPS") %in% tab_vars)
  non_geo_vars <- if (has_geo_pair) setdiff(tab_vars, c("state_FIPS", "county_FIPS")) else tab_vars
  
  level_list <- lapply(non_geo_vars, function(v) {
    if (v == "age" && !is.null(age_levels)) {
      has_na <- anyNA(tbl$age)
      if (has_na) c(age_levels, NA_character_) else age_levels
    } else {
      sort(unique(tbl[[v]]), na.last = TRUE)
    }
  })
  names(level_list) <- non_geo_vars
  
  n_geo <- if (has_geo_pair) nrow(unique(tbl[, .(state_FIPS, county_FIPS)])) else 1
  est_rows <- prod(vapply(level_list, length, numeric(1L))) * n_geo
  if (est_rows > 5e7) {
    stop(
      sprintf(
        "complete_grid = TRUE would create %.0f rows (> 50 million). Set complete_grid = FALSE or reduce 'vars'.",
        est_rows
      ),
      call. = FALSE
    )
  }
  
  base_grid <- do.call(data.table::CJ, c(level_list, list(sorted = FALSE)))
  
  if (has_geo_pair) {
    geo_combos <- unique(tbl[, .(state_FIPS, county_FIPS)])
    base_grid[, .dummy := 1L]
    geo_combos[, .dummy := 1L]
    full_grid <- merge(base_grid, geo_combos, by = ".dummy", allow.cartesian = TRUE, sort = FALSE)
    full_grid[, .dummy := NULL]
  } else {
    full_grid <- base_grid
  }
  
  out <- merge(tbl, full_grid, by = tab_vars, all.y = TRUE, sort = FALSE)
  out[is.na(nDx), nDx := 0]
  out
}