#' Read required columns from a single annual NVSS fixed-width file
#'
#' @param file Path to fixed-width mortality text file.
#' @param year Integer data year.
#' @param vars Character vector of requested tabulation variables.
#' @param geo_filter Geographic restriction ("residence", "occurrence", "both").
#' @param race_groups Type of race group ("detailed", "wbh", "wb").
#' @return A data.table of raw extracted fields.
#' @keywords internal
read_nvss_year <- function(file, year, vars, geo_filter = "residence", race_groups = "detailed") {
  if (!file.exists(file)) {
    stop(sprintf("NVSS file not found for year %d: %s", year, file), call. = FALSE)
  }
  
  layout <- get_nvss_layout(year)
  fields <- required_raw_fields(
    vars = vars, year = year, geo_filter = geo_filter, race_groups = race_groups
  )
  spec <- layout[fields]
  
  starts <- vapply(spec, `[[`, integer(1L), "start")
  ends   <- vapply(spec, `[[`, integer(1L), "end")
  types  <- vapply(spec, `[[`, character(1L), "type")
  
  col_spec <- lapply(types, function(tp) {
    if (tp == "i") readr::col_integer() else readr::col_character()
  })
  names(col_spec) <- fields
  
  # Use na = character() so valid 2-letter codes are never accidentally parsed as NA
  dt <- readr::read_fwf(
    file = file,
    col_positions = readr::fwf_positions(start = starts, end = ends, col_names = fields),
    col_types = do.call(readr::cols, col_spec),
    na = character(),
    trim_ws = TRUE,
    progress = FALSE,
    show_col_types = FALSE
  )
  
  data.table::setDT(dt)
  
  # Convert empty character strings to NA_character_
  char_cols <- fields[types == "c"]
  for (cc in char_cols) {
    data.table::set(dt, i = which(dt[[cc]] == ""), j = cc, value = NA_character_)
  }
  
  dt
}