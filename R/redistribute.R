#' Proportionally redistribute missing values for a single variable with fallback
#'
#' @param dt Aggregated data.table containing tab_vars and numeric nDx.
#' @param target_var Character scalar: name of column to redistribute.
#' @param tab_vars Character vector of all tabulation columns (excluding nDx).
#' @return Aggregated data.table with missing values of target_var redistributed.
#' @keywords internal
redistribute_single_var <- function(dt, target_var, tab_vars) {
  if (!(target_var %in% tab_vars)) return(dt)
  if (!anyNA(dt[[target_var]])) return(dt)
  
  tab_vars_nDx <- c(tab_vars, "nDx")
  miss_dt  <- dt[is.na(get(target_var))]
  known_dt <- dt[!is.na(get(target_var))]
  
  if (nrow(known_dt) == 0L || sum(known_dt$nDx) <= 0) {
    warning(
      sprintf("Cannot redistribute '%s': no non-missing deaths exist in this year.", target_var),
      call. = FALSE
    )
    return(dt)
  }
  
  other_vars <- setdiff(tab_vars, target_var)
  
  # Fallback order: relax finest/sparsest conditioning dimensions first.
  # Always keep 'year'. When redistributing 'county_FIPS', lock 'state_FIPS' so
  # unknown-county deaths stay strictly within their state of residence.
  drop_order <- c("county_FIPS", "month", "cod", "nativity", "race_eth", "age", "sex", "state_FIPS")
  if (target_var == "county_FIPS") {
    drop_order <- setdiff(drop_order, "state_FIPS")
  }
  drop_order <- intersect(drop_order, other_vars)
  
  key_levels <- vector("list", length(drop_order) + 1L)
  key_levels[[1L]] <- other_vars
  curr_keys <- other_vars
  for (k in seq_along(drop_order)) {
    curr_keys <- setdiff(curr_keys, drop_order[[k]])
    key_levels[[k + 1L]] <- curr_keys
  }
  
  miss_remaining <- data.table::copy(miss_dt)
  miss_remaining[, (target_var) := NULL]
  miss_remaining[, .miss_id := .I]
  data.table::setnames(miss_remaining, "nDx", ".nDx_miss")
  
  allocated_list <- list()
  
  for (lvl in seq_along(key_levels)) {
    if (nrow(miss_remaining) == 0L) break
    by_keys <- key_levels[[lvl]]
    
    grp_cols <- c(by_keys, target_var)
    ref_w <- known_dt[, .(.nDx_ref = sum(nDx)), by = grp_cols]
    if (length(by_keys) > 0L) {
      ref_w[, .D := sum(.nDx_ref), by = by_keys]
    } else {
      ref_w[, .D := sum(.nDx_ref)]
    }
    ref_w <- ref_w[.D > 0]
    if (nrow(ref_w) == 0L) next
    
    ref_w[, .w := .nDx_ref / .D]
    ref_w[, c(".nDx_ref", ".D") := NULL]
    
    if (length(by_keys) > 0L) {
      matched_keys <- unique(ref_w[, by_keys, with = FALSE])
      matched_keys[, .has_ref := TRUE]
      
      chk <- merge(miss_remaining, matched_keys, by = by_keys, all.x = TRUE, sort = FALSE)
      to_alloc <- chk[.has_ref == TRUE]
      miss_remaining <- chk[is.na(.has_ref)]
      to_alloc[, .has_ref := NULL]
      miss_remaining[, .has_ref := NULL]
      
      if (nrow(to_alloc) > 0L) {
        alloc <- merge(
          to_alloc, ref_w,
          by = by_keys,
          all.x = TRUE,
          allow.cartesian = TRUE,
          sort = FALSE
        )
        alloc[, nDx := .nDx_miss * .w]
        alloc[, c(".miss_id", ".nDx_miss", ".w") := NULL]
        allocated_list[[length(allocated_list) + 1L]] <- alloc[, tab_vars_nDx, with = FALSE]
      }
    } else {
      # Year-level global fallback
      to_alloc <- data.table::copy(miss_remaining)
      to_alloc[, .dummy := 1L]
      ref_w[, .dummy := 1L]
      alloc <- merge(to_alloc, ref_w, by = ".dummy", allow.cartesian = TRUE, sort = FALSE)
      alloc[, nDx := .nDx_miss * .w]
      alloc[, c(".dummy", ".miss_id", ".nDx_miss", ".w") := NULL]
      allocated_list[[length(allocated_list) + 1L]] <- alloc[, tab_vars_nDx, with = FALSE]
      miss_remaining <- miss_remaining[0L]
    }
  }
  
  if (nrow(miss_remaining) > 0L) {
    warning(
      sprintf(
        "Could not redistribute %.2f deaths with missing '%s' even after fallback; leaving as NA.",
        sum(miss_remaining$.nDx_miss), target_var
      ),
      call. = FALSE
    )
    na_typed <- known_dt[[target_var]][NA_integer_]
    miss_remaining[, (target_var) := na_typed]
    data.table::setnames(miss_remaining, ".nDx_miss", "nDx")
    miss_remaining[, .miss_id := NULL]
    allocated_list[[length(allocated_list) + 1L]] <- miss_remaining[, tab_vars_nDx, with = FALSE]
  }
  
  out <- data.table::rbindlist(c(list(known_dt), allocated_list), use.names = TRUE)
  out[, .(nDx = sum(nDx)), by = tab_vars]
}

#' Sequentially redistribute missing values across all requested tabulation variables
#' @keywords internal
redistribute_deaths <- function(dt, tab_vars) {
  # Order from lowest typical missingness to highest so conditioning strata
  # are as complete as possible for subsequent steps.
  seq_order <- c("month", "sex", "county_FIPS", "cod", "age", "race_eth", "nativity")
  vars_to_run <- intersect(seq_order, tab_vars)
  
  total_before <- sum(dt$nDx)
  for (v in vars_to_run) {
    dt <- redistribute_single_var(dt, target_var = v, tab_vars = tab_vars)
  }
  total_after <- sum(dt$nDx)
  
  if (abs(total_before - total_after) > 1e-6) {
    warning(
      sprintf(
        "Death total changed during redistribution (before: %.2f, after: %.2f).",
        total_before, total_after
      ),
      call. = FALSE
    )
  }
  
  dt
}