# validation/download_wonder_cod.R
# Downloads 1990 (D16, ICD-9) and 2020 (D158, ICD-10) death counts by sex and
# the 27 mutually exclusive nvssread cause-of-death groups from CDC WONDER.

suppressPackageStartupMessages({
  library(httr)
  library(xml2)
  library(data.table)
})

OUT_DIR <- "validation/wonder"
PAUSE_SEC <- 20L
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. Browser-exported XML templates (D16 for 1990, D158 for 2020)
# ------------------------------------------------------------------------------

XML_TEMPLATE_D16 <- '<?xml version="1.0" encoding="UTF-8"?><request-parameters>
	<parameter><name>B_1</name><value>D16.V7</value></parameter>
	<parameter><name>B_2</name><value>*None*</value></parameter>
	<parameter><name>B_3</name><value>*None*</value></parameter>
	<parameter><name>B_4</name><value>*None*</value></parameter>
	<parameter><name>B_5</name><value>*None*</value></parameter>
	<parameter><name>F_D16.V10</name><value>*All*</value></parameter>
	<parameter><name>F_D16.V2</name><value>*All*</value></parameter>
	<parameter><name>F_D16.V4</name><value>*All*</value></parameter>
	<parameter><name>F_D16.V9</name><value>*All*</value></parameter>
	<parameter><name>I_D16.V10</name><value>*All* (The United States)
</value></parameter>
	<parameter><name>I_D16.V2</name><value>*All* (All Causes of Death)
</value></parameter>
	<parameter><name>I_D16.V4</name><value>*All* (All Causes of Death)
</value></parameter>
	<parameter><name>I_D16.V9</name><value>*All* (The United States)
</value></parameter>
	<parameter><name>M_1</name><value>D16.M1</value></parameter>
	<parameter><name>M_2</name><value>D16.M2</value></parameter>
	<parameter><name>M_3</name><value>D16.M3</value></parameter>
	<parameter><name>O_V10_fmode</name><value>freg</value></parameter>
	<parameter><name>O_V2_fmode</name><value>freg</value></parameter>
	<parameter><name>O_V4_fmode</name><value>freg</value></parameter>
	<parameter><name>O_V9_fmode</name><value>freg</value></parameter>
	<parameter><name>O_aar</name><value>aar_none</value></parameter>
	<parameter><name>O_aar_pop</name><value>0000</value></parameter>
	<parameter><name>O_age</name><value>D16.V5</value></parameter>
	<parameter><name>O_export-format</name><value>xls</value></parameter>
	<parameter><name>O_icd</name><value>D16.V2</value></parameter>
	<parameter><name>O_javascript</name><value>on</value></parameter>
	<parameter><name>O_location</name><value>D16.V9</value></parameter>
	<parameter><name>O_oc-sect1-request</name><value>close</value></parameter>
	<parameter><name>O_precision</name><value>1</value></parameter>
	<parameter><name>O_rate_per</name><value>100000</value></parameter>
	<parameter><name>O_show_totals</name><value>true</value></parameter>
	<parameter><name>O_timeout</name><value>600</value></parameter>
	<parameter><name>O_title</name><value/></parameter>
	<parameter><name>VM_D16.M6_D16.V1</name><value>*All*</value></parameter>
	<parameter><name>VM_D16.M6_D16.V10</name><value/></parameter>
	<parameter><name>VM_D16.M6_D16.V7</name><value>*All*</value></parameter>
	<parameter><name>VM_D16.M6_D16.V8</name><value>*All*</value></parameter>
	<parameter><name>V_D16.V1</name><value>1990</value></parameter>
	<parameter><name>V_D16.V10</name><value/></parameter>
	<parameter><name>V_D16.V12</name><value>*All*</value></parameter>
	<parameter><name>V_D16.V13</name><value>*All*</value></parameter>
	<parameter><name>V_D16.V2</name><value/></parameter>
	<parameter><name>V_D16.V4</name><value/></parameter>
	<parameter><name>V_D16.V5</name><value>*All*</value></parameter>
	<parameter><name>V_D16.V6</name><value>00</value></parameter>
	<parameter><name>V_D16.V7</name><value>*All*</value></parameter>
	<parameter><name>V_D16.V8</name><value>*All*</value></parameter>
	<parameter><name>V_D16.V9</name><value/></parameter>
	<parameter><name>action-Send</name><value>Send</value></parameter>
	<parameter><name>dataset_code</name><value>D16</value></parameter>
	<parameter><name>dataset_label</name><value>Compressed Mortality, 1979-1998</value></parameter>
	<parameter><name>finder-stage-D16.V10</name><value>codeset</value></parameter>
	<parameter><name>finder-stage-D16.V2</name><value>codeset</value></parameter>
	<parameter><name>finder-stage-D16.V4</name><value>codeset</value></parameter>
	<parameter><name>finder-stage-D16.V9</name><value>codeset</value></parameter>
	<parameter><name>saved_id</name><value/></parameter>
	<parameter><name>stage</name><value>request</value></parameter>
</request-parameters>'

XML_TEMPLATE_D158 <- '<?xml version="1.0" encoding="UTF-8"?><request-parameters>
	<parameter><name>B_1</name><value>D158.V7</value></parameter>
	<parameter><name>B_2</name><value>*None*</value></parameter>
	<parameter><name>B_3</name><value>*None*</value></parameter>
	<parameter><name>B_4</name><value>*None*</value></parameter>
	<parameter><name>B_5</name><value>*None*</value></parameter>
	<parameter><name>F_D158.V1</name><value>2020</value></parameter>
	<parameter><name>F_D158.V10</name><value>*All*</value></parameter>
	<parameter><name>F_D158.V2</name><value>*All*</value></parameter>
	<parameter><name>F_D158.V25</name><value>*All*</value></parameter>
	<parameter><name>F_D158.V27</name><value>*All*</value></parameter>
	<parameter><name>F_D158.V30</name><value>*All*</value></parameter>
	<parameter><name>F_D158.V31</name><value>*All*</value></parameter>
	<parameter><name>F_D158.V9</name><value>*All*</value></parameter>
	<parameter><name>I_D158.V1</name><value>2020 (2020)
</value></parameter>
	<parameter><name>I_D158.V10</name><value>*All* (The United States)
</value></parameter>
	<parameter><name>I_D158.V2</name><value>*All* (All Causes of Death)
</value></parameter>
	<parameter><name>I_D158.V25</name><value>All Causes of Death
</value></parameter>
	<parameter><name>I_D158.V27</name><value>*All* (The United States)
</value></parameter>
	<parameter><name>I_D158.V30</name><value>*All* (The United States)
</value></parameter>
	<parameter><name>I_D158.V31</name><value>*All* (The United States)
</value></parameter>
	<parameter><name>I_D158.V9</name><value>*All* (The United States)
</value></parameter>
	<parameter><name>M_1</name><value>D158.M1</value></parameter>
	<parameter><name>M_2</name><value>D158.M2</value></parameter>
	<parameter><name>M_3</name><value>D158.M3</value></parameter>
	<parameter><name>O_V10_fmode</name><value>freg</value></parameter>
	<parameter><name>O_V1_fmode</name><value>freg</value></parameter>
	<parameter><name>O_V25_fmode</name><value>freg</value></parameter>
	<parameter><name>O_V27_fmode</name><value>freg</value></parameter>
	<parameter><name>O_V2_fmode</name><value>freg</value></parameter>
	<parameter><name>O_V30_fmode</name><value>freg</value></parameter>
	<parameter><name>O_V31_fmode</name><value>freg</value></parameter>
	<parameter><name>O_V9_fmode</name><value>freg</value></parameter>
	<parameter><name>O_aar</name><value>aar_none</value></parameter>
	<parameter><name>O_aar_pop</name><value>0000</value></parameter>
	<parameter><name>O_age</name><value>D158.V5</value></parameter>
	<parameter><name>O_export-format</name><value>xls</value></parameter>
	<parameter><name>O_javascript</name><value>on</value></parameter>
	<parameter><name>O_location</name><value>D158.V9</value></parameter>
	<parameter><name>O_oc-sect1-request</name><value>close</value></parameter>
	<parameter><name>O_precision</name><value>1</value></parameter>
	<parameter><name>O_race</name><value>D158.V42</value></parameter>
	<parameter><name>O_rate_per</name><value>100000</value></parameter>
	<parameter><name>O_timeout</name><value>600</value></parameter>
	<parameter><name>O_title</name><value/></parameter>
	<parameter><name>O_ucd</name><value>D158.V2</value></parameter>
	<parameter><name>O_urban</name><value>D158.V19</value></parameter>
	<parameter><name>VM_D158.M6_D158.V10</name><value/></parameter>
	<parameter><name>VM_D158.M6_D158.V17</name><value>*All*</value></parameter>
	<parameter><name>VM_D158.M6_D158.V1_S</name><value>*All*</value></parameter>
	<parameter><name>VM_D158.M6_D158.V42</name><value>*All*</value></parameter>
	<parameter><name>VM_D158.M6_D158.V7</name><value>*All*</value></parameter>
	<parameter><name>V_D158.V1</name><value/></parameter>
	<parameter><name>V_D158.V10</name><value/></parameter>
	<parameter><name>V_D158.V11</name><value>*All*</value></parameter>
	<parameter><name>V_D158.V12</name><value>*All*</value></parameter>
	<parameter><name>V_D158.V17</name><value>*All*</value></parameter>
	<parameter><name>V_D158.V18</name><value>*All*</value></parameter>
	<parameter><name>V_D158.V19</name><value>*All*</value></parameter>
	<parameter><name>V_D158.V2</name><value/></parameter>
	<parameter><name>V_D158.V20</name><value>*All*</value></parameter>
	<parameter><name>V_D158.V21</name><value>*All*</value></parameter>
	<parameter><name>V_D158.V22</name><value>*All*</value></parameter>
	<parameter><name>V_D158.V23</name><value>*All*</value></parameter>
	<parameter><name>V_D158.V24</name><value>*All*</value></parameter>
	<parameter><name>V_D158.V25</name><value/></parameter>
	<parameter><name>V_D158.V27</name><value/></parameter>
	<parameter><name>V_D158.V30</name><value/></parameter>
	<parameter><name>V_D158.V31</name><value/></parameter>
	<parameter><name>V_D158.V4</name><value>*All*</value></parameter>
	<parameter><name>V_D158.V42</name><value>*All*</value></parameter>
	<parameter><name>V_D158.V43</name><value>*All*</value></parameter>
	<parameter><name>V_D158.V44</name><value>*All*</value></parameter>
	<parameter><name>V_D158.V45</name><value>*All*</value></parameter>
	<parameter><name>V_D158.V5</name><value>*All*</value></parameter>
	<parameter><name>V_D158.V51</name><value>*All*</value></parameter>
	<parameter><name>V_D158.V52</name><value>*All*</value></parameter>
	<parameter><name>V_D158.V6</name><value>00</value></parameter>
	<parameter><name>V_D158.V7</name><value>*All*</value></parameter>
	<parameter><name>V_D158.V9</name><value/></parameter>
	<parameter><name>action-Send</name><value>Send</value></parameter>
	<parameter><name>dataset_code</name><value>D158</value></parameter>
	<parameter><name>dataset_label</name><value>Underlying Cause of Death, 2018-2024, Single Race</value></parameter>
	<parameter><name>dataset_vintage</name><value>2024</value></parameter>
	<parameter><name>finder-stage-D158.V1</name><value>codeset</value></parameter>
	<parameter><name>finder-stage-D158.V10</name><value>codeset</value></parameter>
	<parameter><name>finder-stage-D158.V2</name><value>codeset</value></parameter>
	<parameter><name>finder-stage-D158.V25</name><value>codeset</value></parameter>
	<parameter><name>finder-stage-D158.V27</name><value>codeset</value></parameter>
	<parameter><name>finder-stage-D158.V30</name><value>codeset</value></parameter>
	<parameter><name>finder-stage-D158.V31</name><value>codeset</value></parameter>
	<parameter><name>finder-stage-D158.V9</name><value>codeset</value></parameter>
	<parameter><name>saved_id</name><value/></parameter>
	<parameter><name>stage</name><value>request</value></parameter>
</request-parameters>'

# ------------------------------------------------------------------------------
# 2. Helper functions: XML builder, API caller, and XML response parser
# ------------------------------------------------------------------------------

build_request_xml <- function(template_str, db, icd_codes) {
  doc <- read_xml(template_str)
  param_name <- paste0("F_", db, ".V2")
  node <- xml_find_first(doc, sprintf("//parameter[name='%s']", param_name))
  if (inherits(node, "xml_missing")) {
    stop("Parameter ", param_name, " not found in XML template.")
  }
  # Replace existing <value> children with the requested ICD codes
  xml_remove(xml_find_all(node, "value"))
  for (code in icd_codes) {
    xml_add_child(node, "value", code)
  }
  as.character(doc)
}

parse_wonder_response <- function(resp_text) {
  doc <- read_xml(resp_text)
  # Check for CDC WONDER API error messages
  err_nodes <- xml_find_all(doc, "//error//message")
  if (length(err_nodes) > 0) {
    stop("CDC WONDER API error: ", paste(xml_text(err_nodes), collapse = " | "))
  }
  
  rows <- xml_find_all(doc, "//data-table/r")
  out <- c(Female = 0, Male = 0)
  for (r in rows) {
    cells <- xml_find_all(r, "c")
    if (length(cells) < 2) next
    sex_label <- xml_attr(cells[[1]], "l")
    if (is.na(sex_label) || !(sex_label %in% c("Female", "Male"))) next
    deaths_raw <- xml_attr(cells[[2]], "v")
    if (is.na(deaths_raw) || deaths_raw == "Suppressed") {
      stop("Suppressed or missing death count returned for ", sex_label)
    }
    out[sex_label] <- as.numeric(gsub(",", "", deaths_raw))
  }
  out
}

query_wonder <- function(db, template_str, icd_codes, label, max_tries = 3L) {
  url <- sprintf("https://wonder.cdc.gov/controller/datarequest/%s", db)
  req_xml <- build_request_xml(template_str, db, icd_codes)
  
  for (attempt in seq_len(max_tries)) {
    resp <- POST(
      url = url,
      body = list(
        request_xml = req_xml,
        accept_datause_restrictions = "true"
      ),
      encode = "form",
      timeout(120)
    )
    status <- status_code(resp)
    raw_bytes <- content(resp, as = "raw")
    txt <- if (length(raw_bytes) > 0L) {
      iconv(rawToChar(raw_bytes), from = "latin1", to = "UTF-8", sub = "")
    } else {
      ""
    }
    
    if (status == 200L && length(txt) == 1L && !is.na(txt) && nzchar(trimws(txt))) {
      return(parse_wonder_response(txt))
    }
    
    reason <- if (is.na(txt) || !nzchar(trimws(txt))) {
      sprintf("empty body (%d raw bytes)", length(raw_bytes))
    } else {
      substr(txt, 1, 300)
    }
    message(sprintf("  [%s] HTTP %d (%s) on attempt %d/%d", label, status, reason, attempt, max_tries))
    if (attempt < max_tries) {
      Sys.sleep(if (status == 429L) 30L else PAUSE_SEC)
    } else {
      stop(sprintf("Failed [%s] after %d attempts (HTTP %d): %s",
                   label, max_tries, status, reason))
    }
  }
}

run_query_plan <- function(year, db, template_str, queries) {
  cache_file <- file.path(OUT_DIR, sprintf("cache_%d.rds", year))
  res <- if (file.exists(cache_file)) readRDS(cache_file) else list()
  
  q_names <- names(queries)
  for (i in seq_along(q_names)) {
    nm <- q_names[i]
    if (!is.null(res[[nm]])) {
      message(sprintf("[%d %2d/%2d] %s (cached)", year, i, length(q_names), nm))
      next
    }
    message(sprintf("[%d %2d/%2d] Querying %s ...", year, i, length(q_names), nm))
    res[[nm]] <- query_wonder(db, template_str, queries[[nm]], label = paste0(year, ":", nm))
    saveRDS(res, cache_file)
    
    if (i < length(q_names)) {
      Sys.sleep(PAUSE_SEC)
    }
  }
  res
}

# ------------------------------------------------------------------------------
# 3. 1990 (D16, ICD-9) query plan & 27-group assembly
# ------------------------------------------------------------------------------

queries_1990 <- list(
  all_causes     = "*All*",
  hiv            = c("042", "043", "044"),
  ch_inf         = "001-139",
  liver_ca       = "155",
  lung_ca        = "162",
  bpcc_ca        = c("153", "154", "174", "175", "180", "185"),
  ch_ca          = "140-239",
  ch_mental      = "290-319",
  mental_excl    = c("290", "291", "303", "305.0"),
  ch_nervous     = "320-389",
  nervous_excl   = c("331.0", "331.1", "331.2", "331.8", "357.5"),
  adrd           = c("290", "331.0", "331.1", "331.2", "331.8"),
  htn            = c("401", "402", "403", "404"),
  ch_circ        = "390-459",
  circ_excl      = c("401", "402", "403", "404", "425.5"),
  flu_pneu       = as.character(c(480:483, 485:487)),
  ch_resp        = "460-519",
  ch_dig         = "520-579",
  dig_excl       = c("535.3", "571.0", "571.1", "571.2", "571.3"),
  diabetes       = "250",
  ch_endo        = "240-279",
  genitourinary  = "580-629",
  perinatal_cong = c("740-759", "760-779"),
  homicide       = c("E960", "E961", "E962.1", "E962.2", "E962.9", paste0("E", 963:969)),
  suicide        = c("E950.6", "E950.7", "E950.8", "E950.9", paste0("E", 951:959)),
  drug_overdose  = c(paste0("E", 850:858), paste0("E950.", 0:5), "E962.0", paste0("E980.", 0:5)),
  alcohol        = c("291", "303", "305.0", "357.5", "425.5", "535.3",
                     "571.0", "571.1", "571.2", "571.3", "790.3", "E860"),
  transport      = c("E800-E807", "E810-E819", "E820-E825", "E826-E829",
                     "E830-E838", "E840-E845", "E846-E848", "E929.0", "E929.1"),
  ch_ext         = "E800-E999",
  ext_alc_plus_d = c("250", "E860"),   # bundled with Diabetes to avoid suppression
  ch_symp        = "780-799",
  symp_alc_plus_d = c("250", "790.3")  # bundled with Diabetes to avoid suppression
)

assemble_1990 <- function(q) {
  e860  <- q$ext_alc_plus_d - q$diabetes
  r7903 <- q$symp_alc_plus_d - q$diabetes
  
  groups <- list(
    "HIV/AIDS"                                                    = q$hiv,
    "Other infectious and parasitic diseases"                     = q$ch_inf - q$hiv,
    "Liver cancer"                                                = q$liver_ca,
    "Lung cancer"                                                 = q$lung_ca,
    "Breast, prostate, colorectal, and cervical cancer"           = q$bpcc_ca,
    "All other cancers"                                           = q$ch_ca - (q$liver_ca + q$lung_ca + q$bpcc_ca),
    "Mental disorders"                                            = q$ch_mental - q$mental_excl,
    "Nervous system, excl. ADRD"                                  = q$ch_nervous - q$nervous_excl,
    "ADRD"                                                        = q$adrd,
    "Hypertensive heart disease"                                  = q$htn,
    "Circulatory diseases, excl. Hypertensive heart disease"      = q$ch_circ - q$circ_excl,
    "Respiratory diseases excl. Influenza and pneumonia"          = q$ch_resp - q$flu_pneu,
    "Influenza and pneumonia"                                     = q$flu_pneu,
    "Diseases of the digestive system"                            = q$ch_dig - q$dig_excl,
    "Diabetes"                                                    = q$diabetes,
    "Endocrine, nutritional, and metabolic diseases excl. Diabetes" = q$ch_endo - q$diabetes,
    "Diseases of the genitourinary system"                        = q$genitourinary,
    "Perinatal conditions and congenital anomalies"               = q$perinatal_cong,
    "Homicide"                                                    = q$homicide,
    "Suicide"                                                     = q$suicide,
    "Drug overdose"                                               = q$drug_overdose,
    "Alcohol-induced"                                             = q$alcohol,
    "Transport accidents"                                         = q$transport,
    "Other external causes"                                       = q$ch_ext - (q$homicide + q$suicide + q$drug_overdose + q$transport + e860),
    "COVID-19"                                                    = c(Female = 0, Male = 0),
    "Symptoms, signs, and ill-defined conditions"                 = q$ch_symp - r7903
  )
  sum_26 <- Reduce(`+`, groups)
  groups[["All other causes"]] <- q$all_causes - sum_26
  groups
}

# ------------------------------------------------------------------------------
# 4. 2020 (D158, ICD-10) query plan & 27-group assembly
# ------------------------------------------------------------------------------

queries_2020 <- list(
  all_causes       = "*All*",
  hiv              = paste0("B", 20:24),
  ch_inf           = "A00-B99",
  liver_ca         = "C22",
  lung_ca          = c("C33", "C34"),
  bpcc_ca          = c("C18", "C19", "C20", "C21", "C50", "C53", "C61"),
  ch_ca            = "C00-D48",
  ch_mental        = "F01-F99",
  mental_excl      = c("F01", "F03", "F10"),
  ch_nervous       = "G00-G98",
  nervous_excl     = c("G30", "G31.0", "G31.1", "G31.2", "G31.8", "G62.1", "G72.1"),
  adrd             = c("F01", "F03", "G30", "G31.0", "G31.1", "G31.8"),
  htn              = c("I10", "I11", "I12", "I13", "I15"),
  ch_circ          = "I00-I99",
  circ_excl        = c("I10", "I11", "I12", "I13", "I15", "I42.6"),
  flu_pneu         = c("J09", paste0("J", 10:16), "J18"),
  ch_resp          = "J00-J98",
  ch_dig           = "K00-K92",
  dig_excl         = c("K29.2", "K70", "K85.2", "K86.0"),
  diabetes         = paste0("E", 10:14),
  ch_endo          = "E00-E88",
  endo_excl        = c(paste0("E", 10:14), "E24.4"),
  genitourinary    = "N00-N98",
  perinatal_cong   = c("P00-P96", "Q00-Q99"),
  homicide         = c(paste0("X", 86:99), sprintf("Y%02d", 0:9), "Y87.1"),
  suicide          = c(paste0("X", 66:84), "Y87.0"),
  drug_overdose    = c(paste0("X", 40:44), paste0("X", 60:64), "X85", paste0("Y", 10:14)),
  alcohol          = c("E24.4", "F10", "G31.2", "G62.1", "G72.1", "I42.6",
                       "K29.2", "K70", "K85.2", "K86.0", "R78.0", "X45", "X65", "Y15"),
  transport        = c("V01-V99", "Y85"),
  ch_ext           = "V01-Y89",
  ext_alc          = c("X45", "X65", "Y15"),
  covid            = "U07.1",
  ch_symp          = "R00-R99",
  r780_plus_covid  = c("U07.1", "R78.0")  # bundled with COVID-19 to avoid suppression
)

assemble_2020 <- function(q) {
  r780 <- q$r780_plus_covid - q$covid
  
  groups <- list(
    "HIV/AIDS"                                                    = q$hiv,
    "Other infectious and parasitic diseases"                     = q$ch_inf - q$hiv,
    "Liver cancer"                                                = q$liver_ca,
    "Lung cancer"                                                 = q$lung_ca,
    "Breast, prostate, colorectal, and cervical cancer"           = q$bpcc_ca,
    "All other cancers"                                           = q$ch_ca - (q$liver_ca + q$lung_ca + q$bpcc_ca),
    "Mental disorders"                                            = q$ch_mental - q$mental_excl,
    "Nervous system, excl. ADRD"                                  = q$ch_nervous - q$nervous_excl,
    "ADRD"                                                        = q$adrd,
    "Hypertensive heart disease"                                  = q$htn,
    "Circulatory diseases, excl. Hypertensive heart disease"      = q$ch_circ - q$circ_excl,
    "Respiratory diseases excl. Influenza and pneumonia"          = q$ch_resp - q$flu_pneu,
    "Influenza and pneumonia"                                     = q$flu_pneu,
    "Diseases of the digestive system"                            = q$ch_dig - q$dig_excl,
    "Diabetes"                                                    = q$diabetes,
    "Endocrine, nutritional, and metabolic diseases excl. Diabetes" = q$ch_endo - q$endo_excl,
    "Diseases of the genitourinary system"                        = q$genitourinary,
    "Perinatal conditions and congenital anomalies"               = q$perinatal_cong,
    "Homicide"                                                    = q$homicide,
    "Suicide"                                                     = q$suicide,
    "Drug overdose"                                               = q$drug_overdose,
    "Alcohol-induced"                                             = q$alcohol,
    "Transport accidents"                                         = q$transport,
    "Other external causes"                                       = q$ch_ext - (q$homicide + q$suicide + q$drug_overdose + q$transport + q$ext_alc),
    "COVID-19"                                                    = q$covid,
    "Symptoms, signs, and ill-defined conditions"                 = q$ch_symp - r780
  )
  sum_26 <- Reduce(`+`, groups)
  groups[["All other causes"]] <- q$all_causes - sum_26
  groups
}

# ------------------------------------------------------------------------------
# 5. Run downloads, reconcile totals, and write CSVs
# ------------------------------------------------------------------------------

write_year_csv <- function(year, groups, all_causes) {
  dt <- rbindlist(lapply(names(groups), function(cod_name) {
    vals <- groups[[cod_name]]
    data.table(
      sex = c("Female", "Male"),
      cod = cod_name,
      wonder_deaths = as.integer(round(vals[c("Female", "Male")]))
    )
  }))
  
  # Reconciliation check
  chk <- dt[, .(sum_27 = sum(wonder_deaths)), by = sex]
  chk[, all_causes := as.integer(round(all_causes[sex]))]
  chk[, diff := sum_27 - all_causes]
  print(chk)
  stopifnot(all(chk$diff == 0L))
  
  out_file <- file.path(OUT_DIR, sprintf("sex_cod_%d.csv", year))
  fwrite(dt, out_file)
  message("Wrote ", out_file)
}

q1990 <- run_query_plan(1990L, "D16", XML_TEMPLATE_D16, queries_1990)
write_year_csv(1990L, assemble_1990(q1990), q1990$all_causes)

q2020 <- run_query_plan(2020L, "D158", XML_TEMPLATE_D158, queries_2020)
write_year_csv(2020L, assemble_2020(q2020), q2020$all_causes)
