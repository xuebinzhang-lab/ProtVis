
#' PD strict ProtVis module final single-file version
#'
#' This file contains:
#' - strict PTM-aware PD spectrum engine helpers
#' - ProtVis-style Shiny module UI/server
#' - a standalone demo app runner
#'
#' It is designed to be placed directly in the `R/` directory of ProtVis.
#'
#' @name pd_strict_protvis_module_final
NULL

# =========================================================
# 1. Config helpers
# =========================================================

#' Create default config for the PD strict engine
#'
#' @return A named list of default engine parameters.
#' @export
pd_default_config <- function() {
  list(
    msf_file = NULL,
    mzxml_file = NULL,

    target_sequence = NULL,
    target_modified_sequence = NA_character_,
    target_raw = NA_character_,

    manual_scan_number = NA_real_,

    tolerance_value = 20,
    tolerance_unit = "ppm",

    max_fragment_charge = 2,
    fragment_rule_set = "ptm_strict",
    activation_type = "HCD",

    min_relative_intensity = 0.5,
    remove_precursor_window = FALSE,
    precursor_mz = NA_real_,
    precursor_exclusion_da = 1.5,

    min_mz = NA_real_,
    max_mz = NA_real_,
    top_n_per_window = NA_integer_,
    window_size = 100,

    label_top_n = 30,
    show_unmatched_peaks = TRUE,
    use_relative_intensity = TRUE,

    export_outputs = FALSE,
    output_pdf = "protvis_pd_strict.pdf",
    output_png = "protvis_pd_strict.png",
    output_csv = "protvis_pd_strict_matches.csv",
    output_rds = "protvis_pd_strict_result.rds",

    verbose = FALSE
  )
}

#' Validate a PD strict engine config
#'
#' @param config A config list produced by [pd_default_config()].
#'
#' @return Invisibly returns `config` if valid.
#' @export
validate_pd_strict_config <- function(config) {
  stopifnot(is.list(config))

  required_fields <- c("msf_file", "mzxml_file", "target_sequence", "tolerance_value", "tolerance_unit")
  miss <- setdiff(required_fields, names(config))
  if (length(miss) > 0) {
    stop("Missing config fields: ", paste(miss, collapse = ", "))
  }

  if (is.null(config$msf_file) || !nzchar(config$msf_file)) {
    stop("config$msf_file is required.")
  }
  if (is.null(config$mzxml_file) || !nzchar(config$mzxml_file)) {
    stop("config$mzxml_file is required.")
  }
  if (is.null(config$target_sequence) || !nzchar(config$target_sequence)) {
    stop("config$target_sequence is required.")
  }
  if (!config$tolerance_unit %in% c("ppm", "Da")) {
    stop("config$tolerance_unit must be 'ppm' or 'Da'.")
  }

  invisible(config)
}

# =========================================================
# 2. Object constructors
# =========================================================

new_pd_psm_candidates <- function(data) {
  structure(list(data = data), class = "pd_psm_candidates")
}

new_pd_psm_selection <- function(best_psm, ranking_table) {
  structure(list(best_psm = best_psm, ranking_table = ranking_table), class = "pd_psm_selection")
}

new_pd_parsed_peptide <- function(sequence, residues, mod_table, nterm_mass_shift = 0, cterm_mass_shift = 0, original_modified_sequence = NA_character_) {
  structure(
    list(
      sequence = sequence,
      residues = residues,
      mod_table = mod_table,
      nterm_mass_shift = nterm_mass_shift,
      cterm_mass_shift = cterm_mass_shift,
      original_modified_sequence = original_modified_sequence
    ),
    class = "pd_parsed_peptide"
  )
}

new_pd_residue_mass_map <- function(data) {
  structure(list(data = data), class = "pd_residue_mass_map")
}

new_pd_theoretical_fragments <- function(data, metadata = list()) {
  structure(list(data = data, metadata = metadata), class = "pd_theoretical_fragments")
}

new_pd_spectrum <- function(peaks, metadata = list()) {
  structure(list(peaks = peaks, metadata = metadata), class = "pd_spectrum")
}

new_pd_candidate_matches <- function(data) {
  structure(list(data = data), class = "pd_candidate_matches")
}

new_pd_final_matches <- function(data) {
  structure(list(data = data), class = "pd_final_matches")
}

new_pd_localization_summary <- function(data) {
  structure(list(data = data), class = "pd_localization_summary")
}

new_pd_annotation_score <- function(metrics) {
  structure(metrics, class = "pd_annotation_score")
}

new_pd_strict_result <- function(
    config,
    psm_candidates,
    psm_selection,
    parsed_peptide,
    residue_mass_map,
    theoretical_fragments,
    raw_spectrum,
    processed_spectrum,
    candidate_matches,
    final_matches,
    localization_summary,
    annotation_score,
    plots = list()
) {
  structure(
    list(
      config = config,
      psm_candidates = psm_candidates,
      psm_selection = psm_selection,
      parsed_peptide = parsed_peptide,
      residue_mass_map = residue_mass_map,
      theoretical_fragments = theoretical_fragments,
      raw_spectrum = raw_spectrum,
      processed_spectrum = processed_spectrum,
      candidate_matches = candidate_matches,
      final_matches = final_matches,
      localization_summary = localization_summary,
      annotation_score = annotation_score,
      plots = plots
    ),
    class = "pd_strict_result"
  )
}

# =========================================================
# 3. Constants and helpers
# =========================================================

pd_constants <- function() {
  list(
    PROTON = 1.007276466812,
    WATER = 18.010564684,
    AMMONIA = 17.026549101,
    CO = 27.99491462,
    CO2 = 43.98982924,
    H3PO4 = 97.97689557,
    HPO3 = 79.96633089
  )
}

pd_get_aa_masses <- function() {
  c(
    A = 71.037113805,
    R = 156.101111050,
    N = 114.042927470,
    D = 115.026943065,
    C = 103.009184505,
    E = 129.042593135,
    Q = 128.058577540,
    G = 57.021463735,
    H = 137.058911875,
    I = 113.084064015,
    L = 113.084064015,
    K = 128.094963050,
    M = 131.040484645,
    F = 147.068413945,
    P = 97.052763875,
    S = 87.032028435,
    T = 101.047678505,
    W = 186.079312980,
    Y = 163.063328575,
    V = 99.068413945
  )
}

pd_stop_if_missing <- function(path, label) {
  if (!file.exists(path)) {
    stop(label, " not found: ", path)
  }
}

pd_pick_first_existing <- function(df, candidates) {
  hit <- candidates[candidates %in% colnames(df)]
  if (length(hit) == 0) {
    return(rep(NA, nrow(df)))
  }
  df[[hit[1]]]
}

pd_safe_first <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  x[1]
}

pd_calc_mass_error <- function(obs_mz, theo_mz, unit = "Da") {
  if (unit == "Da") {
    abs(obs_mz - theo_mz)
  } else if (unit == "ppm") {
    abs(obs_mz - theo_mz) / theo_mz * 1e6
  } else {
    stop("Unsupported tolerance unit. Use 'Da' or 'ppm'.")
  }
}

pd_within_tolerance <- function(obs_mz, theo_mz, tol, unit = "Da") {
  pd_calc_mass_error(obs_mz, theo_mz, unit) <= tol
}

pd_format_ion_label <- function(ion_series, ion_num, charge, loss_type = "none", mod_flag = FALSE) {
  lbl <- paste0(ion_series, ion_num)
  if (!identical(loss_type, "none")) lbl <- paste0(lbl, "-", loss_type)
  if (isTRUE(mod_flag)) lbl <- paste0(lbl, "*")
  if (!is.na(charge) && charge > 1) lbl <- paste0(lbl, "^", charge)
  lbl
}

# =========================================================
# 4. IO
# =========================================================

pd_read_msf <- function(msf_file) {
  pd_stop_if_missing(msf_file, "MSF file")

  con <- DBI::dbConnect(RSQLite::SQLite(), msf_file)

  required_tables <- c("TargetPsms", "TargetPsmsMSnSpectrumInfo", "MSnSpectrumInfo")
  all_tables <- DBI::dbListTables(con)
  miss <- setdiff(required_tables, all_tables)
  if (length(miss) > 0) {
    DBI::dbDisconnect(con)
    stop("Missing required tables: ", paste(miss, collapse = ", "))
  }

  list(
    con = con,
    psms = DBI::dbReadTable(con, "TargetPsms"),
    psm_spectrum = DBI::dbReadTable(con, "TargetPsmsMSnSpectrumInfo"),
    spec_info = DBI::dbReadTable(con, "MSnSpectrumInfo")
  )
}

pd_close_msf <- function(pd_db) {
  if (!is.null(pd_db$con)) {
    try(DBI::dbDisconnect(pd_db$con), silent = TRUE)
  }
  invisible(NULL)
}

pd_build_spectrum_header_index <- function(mzxml_file) {
  pd_stop_if_missing(mzxml_file, "mzXML file")
  ms_data <- mzR::openMSfile(mzxml_file)
  on.exit(mzR::close(ms_data), add = TRUE)
  mzR::header(ms_data)
}

pd_map_scan_number_to_spectrum_index <- function(header_df, scan_number) {
  if (is.null(header_df) || nrow(header_df) == 0) {
    stop("header_df is empty.")
  }

  candidate_cols <- c("seqNum", "acquisitionNum", "scan", "scanNum")
  for (col in candidate_cols) {
    if (col %in% colnames(header_df)) {
      vals <- suppressWarnings(as.numeric(header_df[[col]]))
      hit <- which(!is.na(vals) & vals == as.numeric(scan_number))
      if (length(hit) >= 1) {
        return(hit[1])
      }
    }
  }

  idx <- suppressWarnings(as.numeric(scan_number))
  if (!is.na(idx) && idx >= 1 && idx <= nrow(header_df)) {
    return(idx)
  }

  stop("Could not map scan number to spectrum index: ", scan_number)
}

pd_read_spectrum_by_index <- function(mzxml_file, spectrum_index) {
  ms_data <- mzR::openMSfile(mzxml_file)
  on.exit(mzR::close(ms_data), add = TRUE)

  sp <- mzR::peaks(ms_data, as.numeric(spectrum_index))
  header_df <- mzR::header(ms_data)

  if (is.null(sp) || nrow(sp) == 0) {
    stop("No peaks found for spectrum index: ", spectrum_index)
  }

  new_pd_spectrum(
    peaks = data.frame(
      peak_id = seq_len(nrow(sp)),
      mz = sp[, 1],
      intensity = sp[, 2],
      stringsAsFactors = FALSE
    ),
    metadata = list(
      spectrum_index = spectrum_index,
      header = header_df[spectrum_index, , drop = FALSE]
    )
  )
}

pd_read_spectrum_by_psm <- function(mzxml_file, psm_row) {
  header_df <- pd_build_spectrum_header_index(mzxml_file)
  scan_num <- suppressWarnings(as.numeric(psm_row$ScanNumbers[1]))
  if (is.na(scan_num)) {
    stop("Cannot determine scan number from selected PSM.")
  }
  spectrum_index <- pd_map_scan_number_to_spectrum_index(header_df, scan_num)
  pd_read_spectrum_by_index(mzxml_file, spectrum_index)
}

# =========================================================
# 5. PSM extraction and scoring
# =========================================================

pd_extract_psm_candidates <- function(pd_db, peptide_sequence, modified_sequence = NA_character_, raw_file = NA_character_) {
  psms <- pd_db$psms
  psm_spectrum <- pd_db$psm_spectrum
  spec_info <- pd_db$spec_info

  seq_col <- intersect(colnames(psms), c("Sequence", "AnnotatedSequence", "PeptideSequence"))
  if (length(seq_col) == 0) {
    stop("Cannot find peptide sequence column in TargetPsms.")
  }
  seq_col <- seq_col[1]

  mod_col <- intersect(colnames(psms), c("ModifiedSequence", "AnnotatedSequence", "SequenceWithModifications"))
  mod_col <- pd_safe_first(mod_col)

  psms_sub <- psms[psms[[seq_col]] == peptide_sequence, , drop = FALSE]
  if (!is.na(modified_sequence) && !is.na(mod_col)) {
    psms_sub <- psms_sub[psms_sub[[mod_col]] == modified_sequence, , drop = FALSE]
  }
  if (nrow(psms_sub) == 0) {
    stop("No PSM found for sequence/filter settings.")
  }

  joined <- dplyr::left_join(
    psms_sub,
    psm_spectrum,
    by = c("WorkflowID" = "TargetPsmsWorkflowID", "PeptideID" = "TargetPsmsPeptideID")
  )
  joined <- dplyr::left_join(
    joined,
    spec_info,
    by = c("MSnSpectrumInfoWorkflowID" = "WorkflowID", "MSnSpectrumInfoSpectrumID" = "SpectrumID"),
    suffix = c(".psm", ".spec")
  )

  out <- data.frame(
    Sequence = pd_pick_first_existing(joined, c(seq_col)),
    ModifiedSequence = if (!is.na(mod_col)) pd_pick_first_existing(joined, c(mod_col)) else NA_character_,
    Charge = pd_pick_first_existing(joined, c("Charge", "Charge.psm", "Charge.spec")),
    RetentionTime = pd_pick_first_existing(joined, c("RetentionTime", "RetentionTime.psm", "RetentionTime.spec")),
    ScanNumbers = pd_pick_first_existing(joined, c("ScanNumbers")),
    MasterScanNumbers = pd_pick_first_existing(joined, c("MasterScanNumbers", "MasterScanNumbers.psm", "MasterScanNumbers.spec")),
    MassOverCharge = pd_pick_first_existing(joined, c("MassOverCharge", "MassOverCharge.psm", "MassOverCharge.spec")),
    SpectrumFileName = pd_pick_first_existing(joined, c("SpectrumFileName", "SpectrumFileName.psm", "SpectrumFileName.spec")),
    SearchEngineRank = pd_pick_first_existing(joined, c("SearchEngineRank", "Rank")),
    QValue = pd_pick_first_existing(joined, c("PercolatorQValue", "QValue", "qValue")),
    DeltaScore = pd_pick_first_existing(joined, c("DeltaScore")),
    XCorr = pd_pick_first_existing(joined, c("XCorr", "Score")),
    PrecursorMassError = pd_pick_first_existing(joined, c("DeltaMassPPM", "PrecursorMassError")),
    stringsAsFactors = FALSE
  )

  if (!is.na(raw_file)) {
    out <- out[is.na(out$SpectrumFileName) | out$SpectrumFileName == raw_file, , drop = FALSE]
  }
  if (nrow(out) == 0) {
    stop("No PSM remained after raw file filtering.")
  }

  new_pd_psm_candidates(out)
}

pd_score_psm_candidates <- function(psm_candidates, strict = TRUE) {
  df <- psm_candidates$data
  if (nrow(df) == 0) return(df)

  num_or_na <- function(x) suppressWarnings(as.numeric(x))

  df$rank_num <- num_or_na(df$SearchEngineRank)
  df$qvalue_num <- num_or_na(df$QValue)
  df$deltascore_num <- num_or_na(df$DeltaScore)
  df$xcorr_num <- num_or_na(df$XCorr)
  df$masserr_num <- abs(num_or_na(df$PrecursorMassError))
  df$charge_num <- num_or_na(df$Charge)

  scale_pos <- function(x) {
    if (all(is.na(x))) return(rep(0, length(x)))
    rng <- range(x, na.rm = TRUE)
    if (diff(rng) == 0) return(ifelse(is.na(x), 0, 1))
    y <- (x - rng[1]) / diff(rng)
    y[is.na(y)] <- 0
    y
  }
  scale_neg <- function(x) {
    if (all(is.na(x))) return(rep(0, length(x)))
    rng <- range(x, na.rm = TRUE)
    if (diff(rng) == 0) return(ifelse(is.na(x), 0, 1))
    y <- 1 - (x - rng[1]) / diff(rng)
    y[is.na(y)] <- 0
    y
  }

  df$score_rank <- scale_neg(df$rank_num)
  df$score_qvalue <- scale_neg(df$qvalue_num)
  df$score_deltascore <- scale_pos(df$deltascore_num)
  df$score_xcorr <- scale_pos(df$xcorr_num)
  df$score_masserr <- scale_neg(df$masserr_num)
  df$score_charge <- ifelse(!is.na(df$charge_num) & df$charge_num >= 2, 1, 0.5)
  df$score_modseq <- ifelse(!is.na(df$ModifiedSequence) & nzchar(df$ModifiedSequence), 1, 0)
  df$score_rawfile <- ifelse(!is.na(df$SpectrumFileName) & nzchar(df$SpectrumFileName), 1, 0)

  if (isTRUE(strict)) {
    df$psm_score <- 0.22 * df$score_rank +
      0.20 * df$score_qvalue +
      0.17 * df$score_deltascore +
      0.14 * df$score_xcorr +
      0.14 * df$score_masserr +
      0.05 * df$score_charge +
      0.04 * df$score_modseq +
      0.04 * df$score_rawfile
  } else {
    df$psm_score <- 0.25 * df$score_rank +
      0.15 * df$score_qvalue +
      0.15 * df$score_deltascore +
      0.15 * df$score_xcorr +
      0.15 * df$score_masserr +
      0.10 * df$score_charge +
      0.05 * df$score_modseq
  }

  df <- df[order(-df$psm_score, df$rank_num, df$qvalue_num, -df$deltascore_num), , drop = FALSE]
  df$psm_priority_rank <- seq_len(nrow(df))
  df
}

pd_select_best_psm <- function(psm_candidates, strict = TRUE) {
  ranking_table <- pd_score_psm_candidates(psm_candidates, strict = strict)
  best_psm <- ranking_table[1, , drop = FALSE]
  new_pd_psm_selection(best_psm = best_psm, ranking_table = ranking_table)
}

# =========================================================
# 6. PTM registry and parsing
# =========================================================

pd_get_ptm_registry <- function() {
  data.frame(
    mod_std = c(
      "Phospho", "GlyGly", "Acetyl", "Methyl", "Dimethyl", "Trimethyl",
      "HexNAc", "Hex", "Oxidation", "Carbamidomethyl"
    ),
    delta_mass = c(
      79.966331, 114.042927, 42.010565, 14.015650, 28.031300, 42.046950,
      203.079373, 162.052824, 15.994915, 57.021464
    ),
    ptm_class = c(
      "Phosphorylation", "Ubiquitination", "Acetylation", "Methylation",
      "Methylation", "Methylation", "Glycosylation", "Glycosylation",
      "Other", "Other"
    ),
    stringsAsFactors = FALSE
  )
}

pd_normalize_mod_name <- function(x) {
  x0 <- x
  x <- trimws(x)
  x <- gsub("^\\[|\\]$", "", x)
  x <- gsub("^\\(|\\)$", "", x)
  x_low <- tolower(x)

  dplyr::case_when(
    stringr::str_detect(x_low, "phospho") ~ "Phospho",
    stringr::str_detect(x_low, "glygly|di-gly|digly|k-epsilon-gg|ubiquitin remnant|^gg$") ~ "GlyGly",
    stringr::str_detect(x_low, "acetyl") ~ "Acetyl",
    stringr::str_detect(x_low, "trimethyl") ~ "Trimethyl",
    stringr::str_detect(x_low, "dimethyl") ~ "Dimethyl",
    stringr::str_detect(x_low, "methyl") ~ "Methyl",
    stringr::str_detect(x_low, "hexnac") ~ "HexNAc",
    stringr::str_detect(x_low, "^hex$|hexose") ~ "Hex",
    stringr::str_detect(x_low, "oxidation|ox") ~ "Oxidation",
    stringr::str_detect(x_low, "carbamidomethyl|cam") ~ "Carbamidomethyl",
    TRUE ~ x0
  )
}

pd_get_mod_mass <- function(mod_std, ptm_registry = pd_get_ptm_registry()) {
  idx <- match(mod_std, ptm_registry$mod_std)
  ifelse(is.na(idx), NA_real_, ptm_registry$delta_mass[idx])
}

pd_get_mod_class <- function(mod_std, ptm_registry = pd_get_ptm_registry()) {
  idx <- match(mod_std, ptm_registry$mod_std)
  ifelse(is.na(idx), NA_character_, ptm_registry$ptm_class[idx])
}

pd_normalize_modified_sequence_string <- function(modified_sequence) {
  x <- modified_sequence
  x <- trimws(x)
  x <- gsub("\\s+", "", x)
  x <- gsub("\\{", "[", x, fixed = TRUE)
  x <- gsub("\\}", "]", x, fixed = TRUE)
  x <- gsub("\\(", "[", x)
  x <- gsub("\\)", "]", x)
  x <- gsub("^_+|_+$", "", x)
  x <- gsub("^.*?\\.([A-Z\\[].*?)\\..*$", "\\1", x)

  if (grepl("^[A-Za-z0-9+.-]+-[A-Z]", x)) {
    x <- sub("^([^\\-]+)\\-", "[\\1]", x)
  }
  if (grepl("[A-Z]-[A-Za-z0-9+.-]+$", x)) {
    x <- sub("\\-([^\\-]+)$", "[\\1]", x)
  }

  x
}

pd_tokenize_modified_sequence <- function(modified_sequence) {
  strsplit(modified_sequence, "")[[1]]
}

pd_parse_mod_tokens <- function(tokens, unmodified_sequence = NA_character_) {
  aa_masses <- pd_get_aa_masses()

  if (length(tokens) == 0) {
    stop("Modified sequence token list is empty.")
  }

  residues <- character(0)
  mods <- list()
  i <- 1
  residue_pos <- 0
  pending_nterm_mods <- character(0)
  saw_first_residue <- FALSE

  parse_bracket_content <- function(tokens, start_idx) {
    if (start_idx > length(tokens) || tokens[start_idx] != "[") {
      stop("parse_bracket_content called on non-bracket token.")
    }
    j <- start_idx + 1
    buf <- character(0)
    while (j <= length(tokens) && tokens[j] != "]") {
      buf <- c(buf, tokens[j])
      j <- j + 1
    }
    if (j > length(tokens)) {
      stop("Unclosed modification bracket in modified sequence.")
    }
    list(text = paste0(buf, collapse = ""), end_idx = j)
  }

  normalize_or_mass_shift <- function(mod_raw) {
    if (grepl("^[+-]?[0-9]+\\.?[0-9]*$", mod_raw)) {
      delta <- as.numeric(mod_raw)
      return(list(mod_std = paste0("MassShift(", mod_raw, ")"), delta_mass = delta, ptm_class = "Other"))
    }
    mod_std <- pd_normalize_mod_name(mod_raw)
    delta_mass <- pd_get_mod_mass(mod_std)
    ptm_class <- pd_get_mod_class(mod_std)
    if (is.na(delta_mass)) {
      stop("Unsupported modification: ", mod_raw)
    }
    list(mod_std = mod_std, delta_mass = delta_mass, ptm_class = ptm_class)
  }

  add_mod_row <- function(site_type, site_index, residue, mod_raw) {
    info <- normalize_or_mass_shift(mod_raw)
    data.frame(
      site_type = site_type,
      site_index = site_index,
      residue = residue,
      mod_raw = mod_raw,
      mod_std = info$mod_std,
      delta_mass = info$delta_mass,
      ptm_class = info$ptm_class,
      stringsAsFactors = FALSE
    )
  }

  while (i <= length(tokens)) {
    tok <- tokens[i]

    if (tok %in% names(aa_masses)) {
      residues <- c(residues, tok)
      residue_pos <- residue_pos + 1
      saw_first_residue <- TRUE

      if (length(pending_nterm_mods) > 0 && residue_pos == 1) {
        for (mod_raw in pending_nterm_mods) {
          mods[[length(mods) + 1]] <- add_mod_row("nterm", 0L, "N-term", mod_raw)
        }
        pending_nterm_mods <- character(0)
      }

      while (i < length(tokens) && tokens[i + 1] == "[") {
        parsed <- parse_bracket_content(tokens, i + 1)
        mods[[length(mods) + 1]] <- add_mod_row("residue", residue_pos, tok, parsed$text)
        i <- parsed$end_idx
      }

    } else if (tok == "[") {
      parsed <- parse_bracket_content(tokens, i)

      if (!saw_first_residue) {
        pending_nterm_mods <- c(pending_nterm_mods, parsed$text)
      } else {
        mods[[length(mods) + 1]] <- add_mod_row("cterm", residue_pos, "C-term", parsed$text)
      }
      i <- parsed$end_idx

    } else if (tok %in% c("-", "_", ".", "{", "}", "(", ")")) {
      NULL
    } else if (!grepl("^\\s$", tok)) {
      stop("Unsupported token in modified sequence: '", tok, "'")
    }

    i <- i + 1
  }

  if (length(residues) == 0) {
    if (!is.na(unmodified_sequence) && nzchar(unmodified_sequence)) {
      residues <- strsplit(unmodified_sequence, "")[[1]]
    } else {
      stop("Failed to parse residues from modified sequence.")
    }
  }

  if (!is.na(unmodified_sequence) && nzchar(unmodified_sequence)) {
    expected <- strsplit(unmodified_sequence, "")[[1]]
    if (!identical(residues, expected)) {
      stop(
        "Parsed residue sequence does not match unmodified_sequence. Parsed='",
        paste0(residues, collapse = ""),
        "' Expected='", unmodified_sequence, "'"
      )
    }
  }

  mod_table <- if (length(mods) > 0) {
    dplyr::bind_rows(mods)
  } else {
    data.frame(
      site_type = character(0),
      site_index = integer(0),
      residue = character(0),
      mod_raw = character(0),
      mod_std = character(0),
      delta_mass = numeric(0),
      ptm_class = character(0),
      stringsAsFactors = FALSE
    )
  }

  nterm_mass_shift <- if (nrow(mod_table) > 0) sum(mod_table$delta_mass[mod_table$site_type == "nterm"], na.rm = TRUE) else 0
  cterm_mass_shift <- if (nrow(mod_table) > 0) sum(mod_table$delta_mass[mod_table$site_type == "cterm"], na.rm = TRUE) else 0

  list(
    residues = residues,
    mod_table = mod_table,
    nterm_mass_shift = nterm_mass_shift,
    cterm_mass_shift = cterm_mass_shift
  )
}

pd_parse_modified_sequence <- function(modified_sequence, unmodified_sequence) {
  if (is.na(modified_sequence) || !nzchar(modified_sequence)) {
    residues <- strsplit(unmodified_sequence, "")[[1]]
    return(new_pd_parsed_peptide(
      sequence = unmodified_sequence,
      residues = residues,
      mod_table = data.frame(
        site_type = character(0),
        site_index = integer(0),
        residue = character(0),
        mod_raw = character(0),
        mod_std = character(0),
        delta_mass = numeric(0),
        ptm_class = character(0),
        stringsAsFactors = FALSE
      ),
      nterm_mass_shift = 0,
      cterm_mass_shift = 0,
      original_modified_sequence = NA_character_
    ))
  }

  normalized <- pd_normalize_modified_sequence_string(modified_sequence)
  tokens <- pd_tokenize_modified_sequence(normalized)
  parsed <- pd_parse_mod_tokens(tokens, unmodified_sequence = unmodified_sequence)

  new_pd_parsed_peptide(
    sequence = unmodified_sequence,
    residues = parsed$residues,
    mod_table = parsed$mod_table,
    nterm_mass_shift = parsed$nterm_mass_shift,
    cterm_mass_shift = parsed$cterm_mass_shift,
    original_modified_sequence = modified_sequence
  )
}

pd_build_residue_mass_map <- function(parsed_peptide) {
  aa_masses <- pd_get_aa_masses()
  residues <- parsed_peptide$residues
  mod_table <- parsed_peptide$mod_table

  df <- data.frame(
    position = seq_along(residues),
    residue = residues,
    base_mass = unname(aa_masses[residues]),
    mod_mass = 0,
    mod_label = NA_character_,
    ptm_class = NA_character_,
    stringsAsFactors = FALSE
  )

  if (nrow(mod_table) > 0) {
    residue_mods <- mod_table[mod_table$site_type == "residue", , drop = FALSE]
    for (i in seq_len(nrow(residue_mods))) {
      idx <- residue_mods$site_index[i]
      df$mod_mass[idx] <- df$mod_mass[idx] + residue_mods$delta_mass[i]
      if (is.na(df$mod_label[idx])) {
        df$mod_label[idx] <- residue_mods$mod_std[i]
        df$ptm_class[idx] <- residue_mods$ptm_class[i]
      } else {
        df$mod_label[idx] <- paste(df$mod_label[idx], residue_mods$mod_std[i], sep = ";")
        df$ptm_class[idx] <- paste(df$ptm_class[idx], residue_mods$ptm_class[i], sep = ";")
      }
    }
  }

  df$total_mass <- df$base_mass + df$mod_mass
  new_pd_residue_mass_map(df)
}

# =========================================================
# 7. Fragment generation
# =========================================================

pd_generate_fragment_rules <- function(fragment_rule_set = "ptm_strict", activation_type = "HCD") {
  if (!fragment_rule_set %in% c("core", "core+common", "ptm_strict", "expanded")) {
    stop("Unsupported fragment_rule_set: ", fragment_rule_set)
  }
  if (!activation_type %in% c("CID", "HCD", "ETD", "EThcD")) {
    stop("Unsupported activation_type: ", activation_type)
  }

  list(
    fragment_rule_set = fragment_rule_set,
    activation_type = activation_type,
    allow_common = fragment_rule_set %in% c("core+common", "ptm_strict", "expanded"),
    allow_combined = fragment_rule_set %in% c("expanded"),
    allow_ptm_losses = fragment_rule_set %in% c("ptm_strict", "expanded") && activation_type %in% c("CID", "HCD", "ETD", "EThcD")
  )
}

pd_generate_base_fragments <- function(residue_mass_map, parsed_peptide, charge_range = 1:2, ion_series = c("b", "y")) {
  const <- pd_constants()
  df <- residue_mass_map$data
  n <- nrow(df)
  if (n < 2) return(data.frame())

  out <- list()

  for (series in ion_series) {
    for (k in 1:(n - 1)) {
      if (series == "b") {
        start_idx <- 1
        end_idx <- k
        neutral_mass <- sum(df$total_mass[start_idx:end_idx]) + parsed_peptide$nterm_mass_shift
      } else {
        start_idx <- n - k + 1
        end_idx <- n
        neutral_mass <- sum(df$total_mass[start_idx:end_idx]) + const$WATER + parsed_peptide$cterm_mass_shift
      }

      covered_mod_rows <- which(df$mod_mass[start_idx:end_idx] > 0)
      covered_mod_sites <- if (length(covered_mod_rows) > 0) {
        paste(df$position[start_idx:end_idx][covered_mod_rows], collapse = ";")
      } else {
        NA_character_
      }

      for (z in charge_range) {
        mz <- (neutral_mass + z * const$PROTON) / z
        out[[length(out) + 1]] <- data.frame(
          ion_series = series,
          ion_num = k,
          charge = z,
          frag_start = start_idx,
          frag_end = end_idx,
          neutral_mass = neutral_mass,
          mz = mz,
          contains_mod = length(covered_mod_rows) > 0,
          covered_mod_sites = covered_mod_sites,
          loss_type = "none",
          ion_class = ifelse(length(covered_mod_rows) > 0, paste0(series, "_mod"), series),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  dplyr::bind_rows(out)
}

pd_fragment_sequence <- function(residue_mass_map, start_idx, end_idx) {
  paste0(residue_mass_map$data$residue[start_idx:end_idx], collapse = "")
}

pd_apply_common_loss_rules <- function(base_fragments, residue_mass_map, rules) {
  if (nrow(base_fragments) == 0 || !isTRUE(rules$allow_common)) {
    return(base_fragments[0, , drop = FALSE])
  }

  const <- pd_constants()
  out <- list()

  add_loss_row <- function(row, loss_name, loss_mass, ion_class_suffix = "_loss") {
    new_row <- row
    new_row$neutral_mass <- row$neutral_mass - loss_mass
    if (new_row$neutral_mass <= 0) return(NULL)
    new_row$mz <- (new_row$neutral_mass + row$charge * const$PROTON) / row$charge
    new_row$loss_type <- loss_name
    new_row$ion_class <- ifelse(
      isTRUE(row$contains_mod),
      paste0(row$ion_series, "_mod", ion_class_suffix),
      paste0(row$ion_series, ion_class_suffix)
    )
    new_row
  }

  for (i in seq_len(nrow(base_fragments))) {
    row <- base_fragments[i, , drop = FALSE]
    frag_seq <- pd_fragment_sequence(residue_mass_map, row$frag_start, row$frag_end)

    can_h2o <- stringr::str_detect(frag_seq, "[STED]")
    can_nh3 <- stringr::str_detect(frag_seq, "[KRNQ]")
    can_co2 <- stringr::str_detect(frag_seq, "[DE]")

    if (can_h2o) {
      tmp <- add_loss_row(row, "H2O", const$WATER)
      if (!is.null(tmp)) out[[length(out) + 1]] <- tmp
    }
    if (can_nh3) {
      tmp <- add_loss_row(row, "NH3", const$AMMONIA)
      if (!is.null(tmp)) out[[length(out) + 1]] <- tmp
    }
    if (row$ion_series == "b") {
      tmp <- add_loss_row(row, "CO", const$CO)
      if (!is.null(tmp)) out[[length(out) + 1]] <- tmp
    }
    if (can_co2) {
      tmp <- add_loss_row(row, "CO2", const$CO2)
      if (!is.null(tmp)) out[[length(out) + 1]] <- tmp
    }
    if (isTRUE(rules$allow_combined) && can_h2o && can_nh3) {
      tmp <- add_loss_row(row, "H2O+NH3", const$WATER + const$AMMONIA)
      if (!is.null(tmp)) out[[length(out) + 1]] <- tmp
    }
  }

  if (length(out) == 0) return(base_fragments[0, , drop = FALSE])
  dplyr::bind_rows(out) |>
    dplyr::filter(mz > 0)
}

pd_apply_ptm_loss_rules <- function(base_fragments, residue_mass_map, parsed_peptide, rules) {
  if (nrow(base_fragments) == 0 || !isTRUE(rules$allow_ptm_losses)) {
    return(base_fragments[0, , drop = FALSE])
  }

  mod_table <- parsed_peptide$mod_table
  if (is.null(mod_table) || nrow(mod_table) == 0) {
    return(base_fragments[0, , drop = FALSE])
  }

  const <- pd_constants()
  activation_type <- if (!is.null(rules$activation_type)) rules$activation_type else "HCD"

  residue_mods <- mod_table |>
    dplyr::filter(site_type == "residue")

  if (nrow(residue_mods) == 0) {
    return(base_fragments[0, , drop = FALSE])
  }

  phospho_sites <- residue_mods |>
    dplyr::filter(mod_std == "Phospho")
  glyco_sites <- residue_mods |>
    dplyr::filter(mod_std %in% c("HexNAc", "Hex"))
  gg_sites <- residue_mods |>
    dplyr::filter(mod_std == "GlyGly")
  acetyl_sites <- residue_mods |>
    dplyr::filter(mod_std == "Acetyl")
  methyl_sites <- residue_mods |>
    dplyr::filter(mod_std %in% c("Methyl", "Dimethyl", "Trimethyl"))

  add_loss_row <- function(row, loss_name, loss_mass, ion_class = NULL) {
    new_row <- row
    new_row$neutral_mass <- row$neutral_mass - loss_mass
    if (new_row$neutral_mass <= 0) return(NULL)
    new_row$mz <- (new_row$neutral_mass + row$charge * const$PROTON) / row$charge
    new_row$loss_type <- loss_name
    new_row$ion_class <- if (is.null(ion_class) || is.na(ion_class) || !nzchar(ion_class)) {
      paste0(row$ion_series, "_ptm_loss")
    } else {
      ion_class
    }
    new_row
  }

  add_unique_loss <- function(loss_list, row, loss_name, loss_mass, ion_class = NULL) {
    tmp <- add_loss_row(row, loss_name, loss_mass, ion_class = ion_class)
    if (!is.null(tmp)) loss_list[[length(loss_list) + 1]] <- tmp
    loss_list
  }

  out <- list()

  for (i in seq_len(nrow(base_fragments))) {
    row <- base_fragments[i, , drop = FALSE]

    frag_mods <- residue_mods |>
      dplyr::filter(site_index >= row$frag_start, site_index <= row$frag_end)
    if (nrow(frag_mods) == 0) next

    frag_phospho <- phospho_sites |>
      dplyr::filter(site_index >= row$frag_start, site_index <= row$frag_end)
    frag_glyco <- glyco_sites |>
      dplyr::filter(site_index >= row$frag_start, site_index <= row$frag_end)
    frag_gg <- gg_sites |>
      dplyr::filter(site_index >= row$frag_start, site_index <= row$frag_end)
    frag_acetyl <- acetyl_sites |>
      dplyr::filter(site_index >= row$frag_start, site_index <= row$frag_end)
    frag_methyl <- methyl_sites |>
      dplyr::filter(site_index >= row$frag_start, site_index <= row$frag_end)

    if (nrow(frag_phospho) > 0) {
      has_pST <- any(frag_phospho$residue %in% c("S", "T"))
      has_pY <- any(frag_phospho$residue %in% c("Y"))
      n_phospho <- nrow(frag_phospho)

      if (activation_type %in% c("CID", "HCD")) {
        if (has_pST) {
          out <- add_unique_loss(out, row, "H3PO4", const$H3PO4, ion_class = paste0(row$ion_series, "_phospho_loss"))
          out <- add_unique_loss(out, row, "HPO3", const$HPO3, ion_class = paste0(row$ion_series, "_phospho_loss"))
          if (isTRUE(rules$fragment_rule_set %in% c("expanded")) && n_phospho >= 2) {
            out <- add_unique_loss(out, row, "2H3PO4", 2 * const$H3PO4, ion_class = paste0(row$ion_series, "_phospho_loss"))
          }
        }
        if (has_pY) {
          out <- add_unique_loss(out, row, "HPO3", const$HPO3, ion_class = paste0(row$ion_series, "_phospho_loss"))
        }
      } else if (activation_type == "EThcD") {
        if (has_pST) {
          out <- add_unique_loss(out, row, "H3PO4", const$H3PO4, ion_class = paste0(row$ion_series, "_phospho_loss"))
        }
        if (has_pY) {
          out <- add_unique_loss(out, row, "HPO3", const$HPO3, ion_class = paste0(row$ion_series, "_phospho_loss"))
        }
      }
    }

    if (nrow(frag_glyco) > 0) {
      n_hexnac <- sum(frag_glyco$mod_std == "HexNAc", na.rm = TRUE)
      n_hex <- sum(frag_glyco$mod_std == "Hex", na.rm = TRUE)

      if (activation_type %in% c("CID", "HCD", "EThcD")) {
        if (n_hexnac > 0) {
          out <- add_unique_loss(
            out, row, "HexNAc",
            sum(frag_glyco$delta_mass[frag_glyco$mod_std == "HexNAc"]),
            ion_class = paste0(row$ion_series, "_glyco_loss")
          )
        }
        if (n_hex > 0) {
          out <- add_unique_loss(
            out, row, "Hex",
            sum(frag_glyco$delta_mass[frag_glyco$mod_std == "Hex"]),
            ion_class = paste0(row$ion_series, "_glyco_loss")
          )
        }
        if (isTRUE(rules$fragment_rule_set %in% c("expanded")) && n_hexnac > 0 && n_hex > 0) {
          out <- add_unique_loss(
            out, row, "HexNAc+Hex",
            sum(frag_glyco$delta_mass[frag_glyco$mod_std %in% c("HexNAc", "Hex")]),
            ion_class = paste0(row$ion_series, "_glyco_loss")
          )
        }
      }
    }

    if (nrow(frag_gg) > 0 && isTRUE(rules$fragment_rule_set %in% c("expanded")) && activation_type %in% c("HCD", "CID")) {
      out <- add_unique_loss(
        out, row, "GlyGly",
        sum(frag_gg$delta_mass, na.rm = TRUE),
        ion_class = paste0(row$ion_series, "_glygly_loss")
      )
    }

    if (isTRUE(rules$fragment_rule_set %in% c("expanded")) && activation_type %in% c("CID", "HCD")) {
      if (nrow(frag_acetyl) > 0) {
        out <- add_unique_loss(
          out, row, "Acetyl",
          sum(frag_acetyl$delta_mass, na.rm = TRUE),
          ion_class = paste0(row$ion_series, "_acetyl_loss")
        )
      }
      if (nrow(frag_methyl) > 0) {
        out <- add_unique_loss(
          out, row, "MethylGroup",
          sum(frag_methyl$delta_mass, na.rm = TRUE),
          ion_class = paste0(row$ion_series, "_methyl_loss")
        )
      }
    }
  }

  if (length(out) == 0) return(base_fragments[0, , drop = FALSE])

  out_df <- dplyr::bind_rows(out) |>
    dplyr::filter(mz > 0) |>
    dplyr::distinct(
      ion_series, ion_num, charge, frag_start, frag_end, loss_type, round(mz, 6),
      .keep_all = TRUE
    )
  out_df
}

pd_assign_fragment_priority <- function(theoretical_df) {
  theoretical_df$priority <- dplyr::case_when(
    theoretical_df$loss_type == "none" & !theoretical_df$contains_mod ~ 1,
    theoretical_df$loss_type == "none" & theoretical_df$contains_mod ~ 2,
    theoretical_df$loss_type %in% c("H3PO4", "HPO3", "2H3PO4") ~ 3,
    theoretical_df$loss_type %in% c("HexNAc", "Hex", "HexNAc+Hex") ~ 3.5,
    theoretical_df$loss_type %in% c("GlyGly", "Acetyl", "MethylGroup") ~ 4,
    TRUE ~ 5
  )
  theoretical_df
}

pd_build_theoretical_fragments <- function(residue_mass_map, parsed_peptide, max_fragment_charge = 2, fragment_rule_set = "ptm_strict", activation_type = "HCD") {
  rules <- pd_generate_fragment_rules(fragment_rule_set, activation_type)

  base_fragments <- pd_generate_base_fragments(
    residue_mass_map = residue_mass_map,
    parsed_peptide = parsed_peptide,
    charge_range = 1:max_fragment_charge,
    ion_series = c("b", "y")
  )

  common_losses <- pd_apply_common_loss_rules(base_fragments, residue_mass_map, rules)
  ptm_losses <- pd_apply_ptm_loss_rules(base_fragments, residue_mass_map, parsed_peptide, rules)

  theoretical_df <- dplyr::bind_rows(base_fragments, common_losses, ptm_losses)
  theoretical_df <- pd_assign_fragment_priority(theoretical_df)
  theoretical_df$ion_id <- seq_len(nrow(theoretical_df))
  theoretical_df$label <- mapply(
    FUN = pd_format_ion_label,
    ion_series = theoretical_df$ion_series,
    ion_num = theoretical_df$ion_num,
    charge = theoretical_df$charge,
    loss_type = theoretical_df$loss_type,
    mod_flag = theoretical_df$contains_mod
  )

  theoretical_df <- dplyr::distinct(
    theoretical_df,
    ion_series, ion_num, charge, round(mz, 6), loss_type,
    .keep_all = TRUE
  )
  theoretical_df <- theoretical_df[order(theoretical_df$mz), , drop = FALSE]

  new_pd_theoretical_fragments(
    data = theoretical_df,
    metadata = list(fragment_rule_set = fragment_rule_set, activation_type = activation_type)
  )
}

# =========================================================
# 8. Spectrum preprocessing
# =========================================================

pd_preprocess_spectrum <- function(
    spectrum,
    min_relative_intensity = 0.5,
    remove_precursor_window = FALSE,
    precursor_mz = NA_real_,
    precursor_exclusion_da = 1.5,
    min_mz = NA_real_,
    max_mz = NA_real_,
    top_n_per_window = NA_integer_,
    window_size = 100
) {
  sp_df <- spectrum$peaks
  if (nrow(sp_df) == 0) return(new_pd_spectrum(sp_df, metadata = spectrum$metadata))

  sp_df$rel_intensity <- sp_df$intensity / max(sp_df$intensity, na.rm = TRUE) * 100
  sp_df <- sp_df[sp_df$rel_intensity >= min_relative_intensity, , drop = FALSE]

  if (!is.na(min_mz)) sp_df <- sp_df[sp_df$mz >= min_mz, , drop = FALSE]
  if (!is.na(max_mz)) sp_df <- sp_df[sp_df$mz <= max_mz, , drop = FALSE]
  if (remove_precursor_window && !is.na(precursor_mz)) {
    sp_df <- sp_df[abs(sp_df$mz - precursor_mz) > precursor_exclusion_da, , drop = FALSE]
  }

  if (!is.na(top_n_per_window) && is.finite(top_n_per_window) && top_n_per_window > 0 && nrow(sp_df) > 0) {
    sp_df$window_id <- floor(sp_df$mz / window_size)
    sp_df <- sp_df |>
      dplyr::group_by(window_id) |>
      dplyr::slice_max(order_by = intensity, n = top_n_per_window, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::select(-window_id)
  }

  new_pd_spectrum(peaks = sp_df, metadata = c(spectrum$metadata, list(preprocessed = TRUE)))
}

# =========================================================
# 9. Matching and global assignment
# =========================================================

pd_match_theoretical_to_observed <- function(spectrum, theoretical_fragments, tolerance = 20, tolerance_unit = "ppm") {
  sp_df <- spectrum$peaks
  theo_df <- theoretical_fragments$data
  cand <- list()

  for (i in seq_len(nrow(theo_df))) {
    idx <- which(pd_within_tolerance(sp_df$mz, theo_df$mz[i], tolerance, tolerance_unit))
    if (length(idx) == 0) next

    sub <- sp_df[idx, , drop = FALSE]
    err_da <- sub$mz - theo_df$mz[i]
    err_ppm <- (sub$mz - theo_df$mz[i]) / theo_df$mz[i] * 1e6

    cand[[length(cand) + 1]] <- data.frame(
      peak_id = sub$peak_id,
      mz_obs = sub$mz,
      intensity = sub$intensity,
      rel_intensity = sub$rel_intensity,
      theo_id = theo_df$ion_id[i],
      mz_theo = theo_df$mz[i],
      delta_da = err_da,
      delta_ppm = err_ppm,
      abs_error = pd_calc_mass_error(sub$mz, theo_df$mz[i], tolerance_unit),
      ion_series = theo_df$ion_series[i],
      ion_num = theo_df$ion_num[i],
      charge = theo_df$charge[i],
      loss_type = theo_df$loss_type[i],
      contains_mod = theo_df$contains_mod[i],
      ion_class = theo_df$ion_class[i],
      priority = theo_df$priority[i],
      label = theo_df$label[i],
      covered_mod_sites = theo_df$covered_mod_sites[i],
      stringsAsFactors = FALSE
    )
  }

  if (length(cand) == 0) return(new_pd_candidate_matches(data.frame()))
  new_pd_candidate_matches(dplyr::bind_rows(cand))
}

pd_score_candidate_matches <- function(candidate_matches) {
  df <- candidate_matches$data
  if (nrow(df) == 0) return(candidate_matches)

  scale_pos <- function(x) {
    if (all(is.na(x))) return(rep(0, length(x)))
    rng <- range(x, na.rm = TRUE)
    if (diff(rng) == 0) return(ifelse(is.na(x), 0, 1))
    y <- (x - rng[1]) / diff(rng)
    y[is.na(y)] <- 0
    y
  }
  scale_neg <- function(x) {
    if (all(is.na(x))) return(rep(0, length(x)))
    rng <- range(x, na.rm = TRUE)
    if (diff(rng) == 0) return(ifelse(is.na(x), 0, 1))
    y <- 1 - (x - rng[1]) / diff(rng)
    y[is.na(y)] <- 0
    y
  }

  loss_score <- dplyr::case_when(
    df$loss_type == "none" ~ 1.00,
    df$loss_type %in% c("H3PO4", "HPO3", "2H3PO4") ~ 0.90,
    df$loss_type %in% c("HexNAc", "Hex", "HexNAc+Hex") ~ 0.82,
    df$loss_type %in% c("H2O", "NH3", "CO", "CO2") ~ 0.72,
    TRUE ~ 0.60
  )

  ion_series_score <- ifelse(df$ion_series %in% c("b", "y"), 1, 0.75)
  priority_score <- scale_neg(df$priority)
  error_score <- scale_neg(df$abs_error)
  intensity_score <- scale_pos(df$rel_intensity)
  mod_score <- ifelse(df$contains_mod, 1, 0.8)

  df$match_score <- 0.34 * error_score +
    0.24 * intensity_score +
    0.16 * priority_score +
    0.12 * loss_score +
    0.08 * ion_series_score +
    0.06 * mod_score

  bonus <- rep(0, nrow(df))
  split_idx <- split(seq_len(nrow(df)), paste(df$peak_id))
  for (ix in split_idx) {
    same_peak <- df[ix, , drop = FALSE]
    for (j in seq_len(nrow(same_peak))) {
      rowj <- same_peak[j, , drop = FALSE]
      neighbor_hit <- any(
        df$ion_series == rowj$ion_series &
          df$loss_type == "none" &
          abs(df$ion_num - rowj$ion_num) == 1
      )
      if (neighbor_hit) bonus[ix[j]] <- bonus[ix[j]] + 0.05
    }
  }
  df$match_score <- df$match_score + bonus

  new_pd_candidate_matches(df)
}

pd_component_search_best <- function(df_comp) {
  if (nrow(df_comp) == 0) return(df_comp[0, , drop = FALSE])

  peak_order <- unique(df_comp$peak_id)
  choices_by_peak <- lapply(peak_order, function(pk) {
    sub <- df_comp[df_comp$peak_id == pk, , drop = FALSE]
    sub <- sub[order(-sub$match_score, sub$abs_error), , drop = FALSE]
    sub <- utils::head(sub, 3)
    split(sub, seq_len(nrow(sub)))
  })
  names(choices_by_peak) <- as.character(peak_order)

  best_score <- -Inf
  best_rows <- NULL

  recurse <- function(idx, used_theo, selected_rows, score_sum) {
    if (idx > length(peak_order)) {
      if (score_sum > best_score) {
        best_score <<- score_sum
        best_rows <<- selected_rows
      }
      return(invisible(NULL))
    }

    pk <- peak_order[idx]
    recurse(idx + 1, used_theo, selected_rows, score_sum)

    for (cand in choices_by_peak[[as.character(pk)]]) {
      theo <- cand$theo_id[1]
      if (theo %in% used_theo) next
      recurse(
        idx + 1,
        c(used_theo, theo),
        rbind(selected_rows, cand),
        score_sum + cand$match_score[1]
      )
    }
  }

  recurse(1, numeric(0), df_comp[0, , drop = FALSE], 0)
  if (is.null(best_rows)) return(df_comp[0, , drop = FALSE])
  best_rows
}

pd_resolve_matches_global <- function(candidate_matches) {
  df <- candidate_matches$data
  if (nrow(df) == 0) return(new_pd_final_matches(df))

  df <- df |>
    dplyr::group_by(peak_id) |>
    dplyr::slice_max(order_by = match_score, n = 3, with_ties = FALSE) |>
    dplyr::ungroup()

  peak_nodes <- paste0("p_", unique(df$peak_id))
  theo_nodes <- paste0("t_", unique(df$theo_id))
  nodes <- c(peak_nodes, theo_nodes)
  adj <- setNames(vector("list", length(nodes)), nodes)

  for (i in seq_len(nrow(df))) {
    pnode <- paste0("p_", df$peak_id[i])
    tnode <- paste0("t_", df$theo_id[i])
    adj[[pnode]] <- unique(c(adj[[pnode]], tnode))
    adj[[tnode]] <- unique(c(adj[[tnode]], pnode))
  }

  visited <- setNames(rep(FALSE, length(nodes)), nodes)
  components <- list()

  for (node in nodes) {
    if (visited[[node]]) next

    queue <- c(node)
    comp <- character(0)
    visited[[node]] <- TRUE

    while (length(queue) > 0) {
      cur <- queue[1]
      queue <- queue[-1]
      comp <- c(comp, cur)
      nbrs <- adj[[cur]]
      if (length(nbrs) > 0) {
        for (nb in nbrs) {
          if (!visited[[nb]]) {
            visited[[nb]] <- TRUE
            queue <- c(queue, nb)
          }
        }
      }
    }
    components[[length(components) + 1]] <- comp
  }

  out <- list()

  for (comp in components) {
    comp_peaks <- as.numeric(sub("^p_", "", comp[grepl("^p_", comp)]))
    comp_theos <- as.numeric(sub("^t_", "", comp[grepl("^t_", comp)]))
    df_comp <- df[df$peak_id %in% comp_peaks & df$theo_id %in% comp_theos, , drop = FALSE]
    if (nrow(df_comp) == 0) next

    n_peak <- length(unique(df_comp$peak_id))
    n_theo <- length(unique(df_comp$theo_id))
    if (n_peak <= 8 && n_theo <= 10) {
      best_comp <- pd_component_search_best(df_comp)
    } else {
      best_comp <- df_comp[order(-df_comp$match_score, df_comp$abs_error), , drop = FALSE]
      best_comp <- best_comp |>
        dplyr::group_by(peak_id) |>
        dplyr::slice(1) |>
        dplyr::ungroup() |>
        dplyr::group_by(theo_id) |>
        dplyr::slice(1) |>
        dplyr::ungroup()
    }
    out[[length(out) + 1]] <- best_comp
  }

  resolved <- dplyr::bind_rows(out)
  resolved <- resolved[order(resolved$mz_obs), , drop = FALSE]
  new_pd_final_matches(resolved)
}

# =========================================================
# 10. Localization and scoring
# =========================================================

pd_classify_fragment_localization_power <- function(final_matches, parsed_peptide, residue_mass_map) {
  df <- final_matches$data
  if (nrow(df) == 0) return(df)

  mod_table <- parsed_peptide$mod_table
  candidate_sites <- mod_table |>
    dplyr::filter(site_type == "residue") |>
    dplyr::pull(site_index)

  if (length(candidate_sites) == 0) {
    df$localization_role <- "none"
    return(df)
  }

  df$localization_role <- "covering"

  for (i in seq_len(nrow(df))) {
    covered <- df$covered_mod_sites[i]
    if (is.na(covered) || covered == "") {
      df$localization_role[i] <- "none"
      next
    }

    covered_sites <- suppressWarnings(as.numeric(strsplit(covered, ";", fixed = TRUE)[[1]]))
    if (length(covered_sites) == 0) {
      df$localization_role[i] <- "none"
      next
    }

    if (df$loss_type[i] %in% c("H3PO4", "HPO3", "2H3PO4")) {
      df$localization_role[i] <- "diagnostic"
    } else {
      localizing <- FALSE
      for (site_i in covered_sites) {
        competitors <- setdiff(candidate_sites, site_i)
        if (length(competitors) == 0) next

        includes_comp <- competitors >= df$frag_start[i] & competitors <= df$frag_end[i]
        if (!all(includes_comp)) {
          localizing <- TRUE
          break
        }
      }
      df$localization_role[i] <- if (localizing) "localizing" else "covering"
    }
  }

  df
}

pd_compute_site_localization_score <- function(localization_evidence, parsed_peptide) {
  mod_table <- parsed_peptide$mod_table
  if (nrow(mod_table) == 0) return(data.frame())

  residue_mods <- mod_table |> dplyr::filter(site_type == "residue")
  if (nrow(residue_mods) == 0) return(data.frame())

  out <- data.frame(
    site_index = residue_mods$site_index,
    residue = residue_mods$residue,
    mod_std = residue_mods$mod_std,
    ptm_class = residue_mods$ptm_class,
    covering_ions = 0L,
    localizing_ions = 0L,
    diagnostic_ions = 0L,
    unique_backbone_cleavages = 0L,
    localization_score = 0,
    localization_confidence = "low",
    stringsAsFactors = FALSE
  )

  if (nrow(localization_evidence) == 0) return(out)

  for (i in seq_len(nrow(out))) {
    site_i <- out$site_index[i]

    covered_mask <- vapply(localization_evidence$covered_mod_sites, function(x) {
      if (is.na(x) || x == "") return(FALSE)
      site_i %in% suppressWarnings(as.numeric(strsplit(x, ";", fixed = TRUE)[[1]]))
    }, logical(1))

    sub <- localization_evidence[covered_mask, , drop = FALSE]
    out$covering_ions[i] <- nrow(sub)
    out$localizing_ions[i] <- sum(sub$localization_role == "localizing", na.rm = TRUE)
    out$diagnostic_ions[i] <- sum(sub$localization_role == "diagnostic", na.rm = TRUE)

    if (nrow(sub) > 0) {
      cleavage_ids <- unique(paste(sub$ion_series, sub$ion_num, sep = "_"))
      out$unique_backbone_cleavages[i] <- length(cleavage_ids)
    }

    out$localization_score[i] <-
      1.0 * out$covering_ions[i] +
      2.5 * out$localizing_ions[i] +
      2.0 * out$diagnostic_ions[i] +
      1.5 * out$unique_backbone_cleavages[i]

    out$localization_confidence[i] <- dplyr::case_when(
      out$localization_score[i] >= 12 ~ "high",
      out$localization_score[i] >= 6 ~ "medium",
      TRUE ~ "low"
    )
  }

  out
}

pd_summarize_localization <- function(final_matches, parsed_peptide, residue_mass_map) {
  evidence <- pd_classify_fragment_localization_power(final_matches, parsed_peptide, residue_mass_map)
  summary_df <- pd_compute_site_localization_score(evidence, parsed_peptide)
  new_pd_localization_summary(summary_df)
}

pd_compute_series_continuity <- function(final_matches) {
  df <- final_matches$data
  if (nrow(df) == 0) return(list(b_continuity = 0, y_continuity = 0))

  calc_cont <- function(series_name) {
    idx <- sort(unique(df$ion_num[df$ion_series == series_name & df$loss_type == "none"]))
    if (length(idx) == 0) return(0)
    runs <- c(1, diff(idx))
    sum(runs == 1)
  }

  list(b_continuity = calc_cont("b"), y_continuity = calc_cont("y"))
}

pd_score_annotation <- function(final_matches, processed_spectrum, localization_summary) {
  match_df <- final_matches$data
  sp_df <- processed_spectrum$peaks
  loc_df <- localization_summary$data

  if (nrow(match_df) == 0 || nrow(sp_df) == 0) {
    return(new_pd_annotation_score(list(
      matched_count = 0,
      explained_intensity_pct = 0,
      mod_support_count = 0,
      diagnostic_support_count = 0,
      localization_score = 0,
      final_annotation_score = 0
    )))
  }

  total_int <- sum(sp_df$intensity, na.rm = TRUE)
  explained_int <- sum(match_df$intensity, na.rm = TRUE)
  continuity <- pd_compute_series_continuity(final_matches)
  localization_score <- if (nrow(loc_df) > 0) sum(loc_df$localization_score, na.rm = TRUE) else 0

  metrics <- list(
    matched_count = nrow(match_df),
    explained_intensity_pct = explained_int / total_int * 100,
    mod_support_count = sum(match_df$contains_mod, na.rm = TRUE),
    diagnostic_support_count = sum(match_df$loss_type %in% c("H3PO4", "HPO3", "2H3PO4"), na.rm = TRUE),
    b_continuity = continuity$b_continuity,
    y_continuity = continuity$y_continuity,
    localization_score = localization_score,
    final_annotation_score = nrow(match_df) +
      0.05 * (explained_int / total_int * 100) +
      continuity$b_continuity + continuity$y_continuity +
      localization_score
  )

  new_pd_annotation_score(metrics)
}

# =========================================================
# 11. Plotting
# =========================================================

pd_build_fragmentation_map <- function(residue_mass_map, final_matches, localization_summary) {
  aa_df <- residue_mass_map$data
  loc_df <- localization_summary$data
  match_df <- final_matches$data

  aa_plot <- aa_df |>
    dplyr::mutate(label = ifelse(is.na(mod_label), residue, paste0(residue, "*")))

  base_matches <- match_df |>
    dplyr::filter(loss_type == "none")

  b_pos <- sort(unique(base_matches$ion_num[base_matches$ion_series == "b"]))
  y_pos <- sort(unique(base_matches$ion_num[base_matches$ion_series == "y"]))

  b_plot <- if (length(b_pos) > 0) data.frame(x = b_pos + 0.5, label = paste0("b", b_pos)) else data.frame(x = numeric(0), label = character(0))
  y_plot <- if (length(y_pos) > 0) data.frame(x = nrow(aa_df) - y_pos + 0.5, label = paste0("y", y_pos)) else data.frame(x = numeric(0), label = character(0))

  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0.5, xend = nrow(aa_df) + 0.5, y = 0, yend = 0),
      linewidth = 8,
      color = "#D9D9D9",
      lineend = "round"
    )

  if (nrow(loc_df) > 0) {
    loc_df <- loc_df |>
      dplyr::mutate(
        xmin = site_index - 0.42,
        xmax = site_index + 0.42,
        ymin = -0.12,
        ymax = 0.12,
        alpha_val = dplyr::case_when(
          localization_confidence == "high" ~ 0.88,
          localization_confidence == "medium" ~ 0.62,
          TRUE ~ 0.34
        ),
        linewidth_val = dplyr::case_when(
          localization_confidence == "high" ~ 1.3,
          localization_confidence == "medium" ~ 0.9,
          TRUE ~ 0.5
        )
      )

    p <- p +
      ggplot2::geom_rect(
        data = loc_df,
        ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = ptm_class, alpha = alpha_val),
        color = "grey30",
        linewidth = loc_df$linewidth_val,
        inherit.aes = FALSE
      ) +
      ggplot2::geom_text(
        data = loc_df,
        ggplot2::aes(x = site_index, y = 0.22, label = paste0("L=", round(localization_score, 1))),
        size = 3.1,
        fontface = "bold"
      )
  }

  if (nrow(b_plot) > 0) {
    p <- p +
      ggplot2::geom_segment(data = b_plot, ggplot2::aes(x = x, xend = x, y = 0.05, yend = 0.27), color = "#2166AC", linewidth = 0.8) +
      ggplot2::geom_text(data = b_plot, ggplot2::aes(x = x, y = 0.34, label = label), color = "#2166AC", size = 4.0, fontface = "bold")
  }

  if (nrow(y_plot) > 0) {
    p <- p +
      ggplot2::geom_segment(data = y_plot, ggplot2::aes(x = x, xend = x, y = -0.05, yend = -0.27), color = "#B2182B", linewidth = 0.8) +
      ggplot2::geom_text(data = y_plot, ggplot2::aes(x = x, y = -0.34, label = label), color = "#B2182B", size = 4.0, fontface = "bold")
  }

  p +
    ggplot2::geom_text(data = aa_plot, ggplot2::aes(x = position, y = 0, label = label), size = 7) +
    ggplot2::scale_fill_manual(
      values = c(
        "Phosphorylation" = "#1f77b4",
        "Ubiquitination" = "#ff7f0e",
        "Acetylation" = "#2ca02c",
        "Methylation" = "#9467bd",
        "Glycosylation" = "#d62728",
        "Other" = "#7f7f7f"
      ),
      drop = FALSE
    ) +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_x_continuous(limits = c(0.5, nrow(aa_df) + 0.5), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(-0.45, 0.45), expand = c(0, 0)) +
    ggplot2::labs(title = "Fragmentation / PTM localization map", fill = "PTM class") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0),
      plot.margin = ggplot2::margin(5, 10, 0, 10)
    )
}

pd_build_ms2_plot <- function(processed_spectrum, final_matches, parsed_peptide, psm_selection, config) {
  sp_df <- processed_spectrum$peaks
  match_df <- final_matches$data
  if (nrow(sp_df) == 0) stop("Processed spectrum is empty.")

  sp_plot <- sp_df
  if (isTRUE(config$use_relative_intensity)) {
    sp_plot$plot_intensity <- sp_plot$intensity / max(sp_plot$intensity, na.rm = TRUE) * 100
    ylab_text <- "Relative intensity (%)"
    y_limits <- c(0, 110)
  } else {
    sp_plot$plot_intensity <- sp_plot$intensity
    ylab_text <- "Intensity"
    y_limits <- c(0, max(sp_plot$plot_intensity, na.rm = TRUE) * 1.12)
  }

  if (nrow(match_df) > 0) {
    match_df$plot_intensity <- if (isTRUE(config$use_relative_intensity)) {
      match_df$intensity / max(sp_df$intensity, na.rm = TRUE) * 100
    } else {
      match_df$intensity
    }
  }

  label_df <- if (nrow(match_df) > 0) {
    match_df |>
      dplyr::arrange(dplyr::desc(plot_intensity), abs(delta_ppm)) |>
      dplyr::slice_head(n = config$label_top_n)
  } else {
    match_df
  }

  class_colors <- c(
    "b" = "#2166AC",
    "y" = "#B2182B",
    "b_mod" = "#004B8D",
    "y_mod" = "#8C1225",
    "b_loss" = "#67A9CF",
    "y_loss" = "#EF8A62",
    "b_mod_loss" = "#4393C3",
    "y_mod_loss" = "#D6604D",
    "b_phospho_loss" = "#542788",
    "y_phospho_loss" = "#7B3294",
    "b_glyco_loss" = "#1B9E77",
    "y_glyco_loss" = "#66A61E",
    "b_glygly_loss" = "#E6AB02",
    "y_glygly_loss" = "#A6761D",
    "b_acetyl_loss" = "#7570B3",
    "y_acetyl_loss" = "#E7298A",
    "b_methyl_loss" = "#A6CEE3",
    "y_methyl_loss" = "#FB9A99"
  )

  peptide_label <- ifelse(
    is.na(parsed_peptide$original_modified_sequence),
    parsed_peptide$sequence,
    parsed_peptide$original_modified_sequence
  )
  scan_num <- psm_selection$best_psm$ScanNumbers[1]

  p <- ggplot2::ggplot()

  if (isTRUE(config$show_unmatched_peaks)) {
    p <- p +
      ggplot2::geom_segment(data = sp_plot, ggplot2::aes(x = mz, xend = mz, y = 0, yend = plot_intensity), linewidth = 0.23, color = "grey78")
  }

  if (nrow(match_df) > 0) {
    p <- p +
      ggplot2::geom_segment(
        data = match_df,
        ggplot2::aes(x = mz_obs, xend = mz_obs, y = 0, yend = plot_intensity, color = ion_class),
        linewidth = 0.45
      )
  }

  if (nrow(label_df) > 0) {
    p <- p +
      ggrepel::geom_text_repel(
        data = label_df,
        ggplot2::aes(
          x = mz_obs,
          y = plot_intensity,
          label = paste0(label, "\n", round(mz_obs, 4), "\n", round(delta_ppm, 1), " ppm"),
          color = ion_class
        ),
        size = 3.2,
        box.padding = 0.25,
        point.padding = 0.15,
        segment.size = 0.2,
        max.overlaps = Inf,
        show.legend = FALSE
      )
  }

  subtitle_text <- paste0(
    "Sequence: ", peptide_label,
    " | Scan: ", scan_num,
    " | Tolerance: ", config$tolerance_value, " ", config$tolerance_unit,
    " | Matched ions: ", nrow(match_df)
  )

  p +
    ggplot2::scale_color_manual(values = class_colors, drop = FALSE) +
    ggplot2::labs(
      title = "Annotated MS2 Spectrum",
      subtitle = subtitle_text,
      x = "m/z",
      y = ylab_text,
      color = "Ion class"
    ) +
    ggplot2::scale_y_continuous(limits = y_limits, expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 20, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 11, color = "grey30"),
      axis.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.text = ggplot2::element_text(size = 11),
      legend.title = ggplot2::element_text(size = 11, face = "bold"),
      legend.text = ggplot2::element_text(size = 9),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "grey90"),
      panel.grid.major.y = ggplot2::element_line(color = "grey90"),
      plot.margin = ggplot2::margin(10, 12, 10, 12)
    )
}

pd_combine_plots <- function(fragmentation_map, ms2_plot) {
  fragmentation_map / ms2_plot + patchwork::plot_layout(heights = c(1, 3.2))
}

# =========================================================
# 12. Exports
# =========================================================

pd_export_results <- function(result) {
  config <- result$config
  if (!isTRUE(config$export_outputs)) return(invisible(result))

  ggplot2::ggsave(
    filename = config$output_pdf,
    plot = result$plots$combined,
    width = 14, height = 8.8, units = "in"
  )

  ggplot2::ggsave(
    filename = config$output_png,
    plot = result$plots$combined,
    width = 14, height = 8.8, units = "in", dpi = 300
  )

  if (nrow(result$final_matches$data) > 0) {
    utils::write.csv(result$final_matches$data, config$output_csv, row.names = FALSE)
  }

  saveRDS(result, file = config$output_rds)
  invisible(result)
}

# =========================================================
# 13. Main engine
# =========================================================

#' Run the strict PTM-aware PD spectrum annotation engine
#'
#' @param config A config list produced by [pd_default_config()].
#'
#' @return A `pd_strict_result` object.
#' @export
run_pd_strict_engine <- function(config = pd_default_config()) {
  validate_pd_strict_config(config)

  pd_db <- pd_read_msf(config$msf_file)
  on.exit(pd_close_msf(pd_db), add = TRUE)

  psm_candidates <- pd_extract_psm_candidates(
    pd_db = pd_db,
    peptide_sequence = config$target_sequence,
    modified_sequence = config$target_modified_sequence,
    raw_file = config$target_raw
  )

  psm_selection <- pd_select_best_psm(psm_candidates, strict = TRUE)
  best_psm <- psm_selection$best_psm

  raw_spectrum <- if (!is.na(config$manual_scan_number)) {
    header_df <- pd_build_spectrum_header_index(config$mzxml_file)
    spectrum_index <- pd_map_scan_number_to_spectrum_index(header_df, config$manual_scan_number)
    pd_read_spectrum_by_index(config$mzxml_file, spectrum_index)
  } else {
    pd_read_spectrum_by_psm(config$mzxml_file, best_psm)
  }

  use_sequence <- best_psm$Sequence[1]
  use_modified_sequence <- best_psm$ModifiedSequence[1]
  if (!is.na(config$target_modified_sequence)) {
    use_modified_sequence <- config$target_modified_sequence
  }

  parsed_peptide <- pd_parse_modified_sequence(
    modified_sequence = use_modified_sequence,
    unmodified_sequence = use_sequence
  )

  residue_mass_map <- pd_build_residue_mass_map(parsed_peptide)

  theoretical_fragments <- pd_build_theoretical_fragments(
    residue_mass_map = residue_mass_map,
    parsed_peptide = parsed_peptide,
    max_fragment_charge = config$max_fragment_charge,
    fragment_rule_set = config$fragment_rule_set,
    activation_type = config$activation_type
  )

  processed_spectrum <- pd_preprocess_spectrum(
    spectrum = raw_spectrum,
    min_relative_intensity = config$min_relative_intensity,
    remove_precursor_window = config$remove_precursor_window,
    precursor_mz = config$precursor_mz,
    precursor_exclusion_da = config$precursor_exclusion_da,
    min_mz = config$min_mz,
    max_mz = config$max_mz,
    top_n_per_window = config$top_n_per_window,
    window_size = config$window_size
  )

  candidate_matches <- pd_match_theoretical_to_observed(
    spectrum = processed_spectrum,
    theoretical_fragments = theoretical_fragments,
    tolerance = config$tolerance_value,
    tolerance_unit = config$tolerance_unit
  )

  candidate_matches <- pd_score_candidate_matches(candidate_matches)
  final_matches <- pd_resolve_matches_global(candidate_matches)

  localization_summary <- pd_summarize_localization(
    final_matches = final_matches,
    parsed_peptide = parsed_peptide,
    residue_mass_map = residue_mass_map
  )

  annotation_score <- pd_score_annotation(
    final_matches = final_matches,
    processed_spectrum = processed_spectrum,
    localization_summary = localization_summary
  )

  fragmentation_map <- pd_build_fragmentation_map(residue_mass_map, final_matches, localization_summary)
  ms2_plot <- pd_build_ms2_plot(processed_spectrum, final_matches, parsed_peptide, psm_selection, config)
  combined_plot <- pd_combine_plots(fragmentation_map, ms2_plot)

  result <- new_pd_strict_result(
    config = config,
    psm_candidates = psm_candidates,
    psm_selection = psm_selection,
    parsed_peptide = parsed_peptide,
    residue_mass_map = residue_mass_map,
    theoretical_fragments = theoretical_fragments,
    raw_spectrum = raw_spectrum,
    processed_spectrum = processed_spectrum,
    candidate_matches = candidate_matches,
    final_matches = final_matches,
    localization_summary = localization_summary,
    annotation_score = annotation_score,
    plots = list(
      fragmentation_map = fragmentation_map,
      ms2_plot = ms2_plot,
      combined = combined_plot
    )
  )

  if (isTRUE(config$export_outputs)) pd_export_results(result)

  if (isTRUE(config$verbose)) {
    message("Done.")
    message("Sequence used: ", use_sequence)
    message("Modified sequence used: ", ifelse(is.na(use_modified_sequence), "NA", use_modified_sequence))
    message("Matched ions: ", annotation_score$matched_count)
    message("Explained intensity (%): ", round(annotation_score$explained_intensity_pct, 2))
    message("Final annotation score: ", round(annotation_score$final_annotation_score, 2))
  }

  result
}

# =========================================================
# 14. ProtVis-style Shiny module
# =========================================================

#' PD strict spectrum annotation UI module
#'
#' @param id Shiny module id.
#'
#' @return A UI definition for the module.
#' @export
pd_strict_module_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::page_fillable(
    padding = 0,
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 360,
        open = "desktop",
        shiny::div(
          style = "padding: 6px 4px 4px 4px;",
          shiny::h4("PD Strict Spectrum Annotation"),
          shiny::p(
            "Choose whether files are provided by path or selected from the server,",
            "then run the strict PTM-aware spectrum annotation engine."
          )
        ),
        bslib::accordion(
          id = ns("accordion"),
          open = c("files", "target", "params", "display"),
          bslib::accordion_panel(
            title = "Files",

            shiny::radioButtons(
              ns("file_source"),
              "File source",
              choices = c(
                "Computer path" = "local_path",
                "Server file" = "server"
              ),
              selected = "server"
            ),

            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'local_path'", ns("file_source")),
              shiny::textInput(
                ns("msf_local_path"),
                "MSF file path",
                value = ""
              ),
              shiny::textInput(
                ns("mzxml_local_path"),
                "mzXML file path",
                value = ""
              ),
              shiny::div(
                style = "font-size: 0.9rem; color: #666;",
                "Note: the entered path must be readable by the machine running this app."
              )
            ),

            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'server'", ns("file_source")),
              shiny::actionButton(ns("choose_msf"), "Choose server MSF file", width = "100%"),
              shiny::tags$div(style = "height:8px;"),
              shiny::verbatimTextOutput(ns("msf_server_path"), placeholder = TRUE),

              shiny::tags$div(style = "height:12px;"),
              shiny::actionButton(ns("choose_mzxml"), "Choose server mzXML file", width = "100%"),
              shiny::tags$div(style = "height:8px;"),
              shiny::verbatimTextOutput(ns("mzxml_server_path"), placeholder = TRUE)
            ),

            shiny::tags$div(style = "height:10px;"),
            shiny::actionButton(ns("check_files"), "Check files", width = "100%"),
            shiny::tags$div(style = "height:10px;"),
            shiny::uiOutput(ns("file_status"))
          ),
          bslib::accordion_panel(
            title = "Target",
            shiny::textInput(ns("target_sequence"), "Target sequence", value = ""),
            shiny::textInput(ns("target_modified_sequence"), "Target modified sequence (optional)", value = ""),
            shiny::textInput(ns("target_raw"), "Target raw file name (optional)", value = ""),
            shiny::numericInput(ns("manual_scan_number"), "Manual scan number (optional)", value = NA, min = 1)
          ),
          bslib::accordion_panel(
            title = "Engine parameters",
            shiny::selectInput(ns("tolerance_unit"), "Tolerance unit", choices = c("ppm", "Da"), selected = "ppm"),
            shiny::numericInput(ns("tolerance_value"), "Tolerance value", value = 20, min = 0),
            shiny::numericInput(ns("max_fragment_charge"), "Max fragment charge", value = 2, min = 1, max = 4),
            shiny::selectInput(ns("fragment_rule_set"), "Fragment rule set", choices = c("ptm_strict", "core", "core+common", "expanded"), selected = "ptm_strict"),
            shiny::selectInput(ns("activation_type"), "Activation type", choices = c("HCD", "CID", "ETD", "EThcD"), selected = "HCD"),
            shiny::numericInput(ns("min_relative_intensity"), "Min relative intensity (%)", value = 0.5, min = 0, max = 100),
            shiny::checkboxInput(ns("remove_precursor_window"), "Remove precursor window", value = FALSE),
            shiny::numericInput(ns("precursor_mz"), "Precursor m/z (optional)", value = NA),
            shiny::numericInput(ns("precursor_exclusion_da"), "Precursor exclusion (Da)", value = 1.5, min = 0),
            shiny::numericInput(ns("top_n_per_window"), "Top N per m/z window (optional)", value = NA, min = 1),
            shiny::numericInput(ns("window_size"), "Window size", value = 100, min = 10)
          ),
          bslib::accordion_panel(
            title = "Display",
            shiny::numericInput(ns("label_top_n"), "Top labels on spectrum", value = 30, min = 1),
            shiny::checkboxInput(ns("show_unmatched_peaks"), "Show unmatched peaks", value = TRUE),
            shiny::checkboxInput(ns("use_relative_intensity"), "Use relative intensity", value = TRUE)
          )
        ),
        shiny::br(),
        shiny::fluidRow(
          shiny::column(6, shiny::actionButton(ns("run"), "Run", class = "btn-primary", width = "100%")),
          shiny::column(6, shiny::actionButton(ns("reset"), "Reset", width = "100%"))
        ),
        shiny::br(),
        shiny::downloadButton(ns("download_pdf"), "Download PDF", width = "100%"),
        shiny::br(),
        shiny::downloadButton(ns("download_png"), "Download PNG", width = "100%"),
        shiny::br(),
        shiny::downloadButton(ns("download_csv"), "Download Matches CSV", width = "100%"),
        shiny::br(),
        shiny::downloadButton(ns("download_rds"), "Download Result RDS", width = "100%")
      ),
      bslib::layout_column_wrap(
        width = 1,
        gap = "16px",
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Annotated spectrum"),
          bslib::card_body(
            shiny::uiOutput(ns("run_status")),
            shiny::plotOutput(ns("combined_plot"), height = "820px")
          )
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Summary"),
          bslib::card_body(DT::DTOutput(ns("summary_table")))
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Final matches"),
          bslib::card_body(DT::DTOutput(ns("matches_table")))
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Localization"),
          bslib::card_body(DT::DTOutput(ns("localization_table")))
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("PSM ranking"),
          bslib::card_body(DT::DTOutput(ns("psm_table")))
        )
      )
    )
  )
}

#' PD strict spectrum annotation server module
#'
#' @param id Shiny module id.
#'
#' @return A list containing a reactive `result`.
#' @export
pd_strict_module_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    result_rv <- shiny::reactiveVal(NULL)

    selected_msf_path <- shiny::reactiveVal(NULL)
    selected_mzxml_path <- shiny::reactiveVal(NULL)

    roots <- c(
      home = normalizePath("~"),
      data = "/home",
      root = "/"
    )

    shinyFiles::shinyFileChoose(
      input,
      "choose_msf",
      roots = roots,
      session = session,
      filetypes = c("msf")
    )

    shinyFiles::shinyFileChoose(
      input,
      "choose_mzxml",
      roots = roots,
      session = session,
      filetypes = c("mzXML", "mzxml")
    )

    shiny::observeEvent(input$choose_msf, {
      req <- input$choose_msf
      if (is.null(req)) return()

      parsed <- shinyFiles::parseFilePaths(roots, req)
      if (nrow(parsed) > 0) {
        selected_msf_path(as.character(parsed$datapath[1]))
      }
    })

    shiny::observeEvent(input$choose_mzxml, {
      req <- input$choose_mzxml
      if (is.null(req)) return()

      parsed <- shinyFiles::parseFilePaths(roots, req)
      if (nrow(parsed) > 0) {
        selected_mzxml_path(as.character(parsed$datapath[1]))
      }
    })

    output$msf_server_path <- shiny::renderText({
      x <- selected_msf_path()
      if (is.null(x) || !nzchar(x)) "No MSF file selected." else x
    })

    output$mzxml_server_path <- shiny::renderText({
      x <- selected_mzxml_path()
      if (is.null(x) || !nzchar(x)) "No mzXML file selected." else x
    })

    normalize_optional_text <- function(x) {
      if (is.null(x) || is.na(x) || !nzchar(trimws(x))) {
        return(NA_character_)
      }
      trimws(x)
    }

    normalize_optional_num <- function(x) {
      if (is.null(x) || length(x) == 0 || is.na(x)) {
        return(NA_real_)
      }
      as.numeric(x)
    }

    build_config_from_input <- function() {
      file_source <- input$file_source

      if (identical(file_source, "local")) {
        if (is.null(input$msf_file) || is.null(input$mzxml_file)) {
          stop("Please upload both MSF and mzXML files.")
        }
        msf_path <- input$msf_file$datapath
        mzxml_path <- input$mzxml_file$datapath

      } else if (identical(file_source, "server")) {
        msf_path <- selected_msf_path()
        mzxml_path <- selected_mzxml_path()

        if (is.null(msf_path) || !nzchar(msf_path) || !file.exists(msf_path)) {
          stop("Please choose a valid server MSF file.")
        }
        if (is.null(mzxml_path) || !nzchar(mzxml_path) || !file.exists(mzxml_path)) {
          stop("Please choose a valid server mzXML file.")
        }

      } else {
        stop("Unknown file source mode.")
      }

      if (!nzchar(trimws(input$target_sequence))) {
        stop("Please enter a target sequence.")
      }

      config <- pd_default_config()
      config$msf_file <- msf_path
      config$mzxml_file <- mzxml_path
      config$target_sequence <- trimws(input$target_sequence)
      config$target_modified_sequence <- normalize_optional_text(input$target_modified_sequence)
      config$target_raw <- normalize_optional_text(input$target_raw)
      config$manual_scan_number <- normalize_optional_num(input$manual_scan_number)

      config$tolerance_unit <- input$tolerance_unit
      config$tolerance_value <- as.numeric(input$tolerance_value)
      config$max_fragment_charge <- as.integer(input$max_fragment_charge)
      config$fragment_rule_set <- input$fragment_rule_set
      config$activation_type <- input$activation_type

      config$min_relative_intensity <- as.numeric(input$min_relative_intensity)
      config$remove_precursor_window <- isTRUE(input$remove_precursor_window)
      config$precursor_mz <- normalize_optional_num(input$precursor_mz)
      config$precursor_exclusion_da <- as.numeric(input$precursor_exclusion_da)

      topn <- normalize_optional_num(input$top_n_per_window)
      config$top_n_per_window <- if (is.na(topn)) NA_integer_ else as.integer(topn)
      config$window_size <- as.numeric(input$window_size)

      config$label_top_n <- as.integer(input$label_top_n)
      config$show_unmatched_peaks <- isTRUE(input$show_unmatched_peaks)
      config$use_relative_intensity <- isTRUE(input$use_relative_intensity)

      config$export_outputs <- FALSE
      config$verbose <- FALSE

      validate_pd_strict_config(config)
      config
    }

    output$file_status <- shiny::renderUI({
      if (identical(input$file_source, "local")) {
        shiny::tagList(
          shiny::div(
            style = "font-size: 0.92rem; color: #555;",
            if (is.null(input$msf_file)) "MSF: not uploaded" else paste("MSF:", input$msf_file$name)
          ),
          shiny::div(
            style = "font-size: 0.92rem; color: #555;",
            if (is.null(input$mzxml_file)) "mzXML: not uploaded" else paste("mzXML:", input$mzxml_file$name)
          )
        )
      } else {
        shiny::tagList(
          shiny::div(
            style = "font-size: 0.92rem; color: #555;",
            paste("MSF:", ifelse(is.null(selected_msf_path()), "not selected", basename(selected_msf_path())))
          ),
          shiny::div(
            style = "font-size: 0.92rem; color: #555;",
            paste("mzXML:", ifelse(is.null(selected_mzxml_path()), "not selected", basename(selected_mzxml_path())))
          )
        )
      }
    })

    output$run_status <- shiny::renderUI({
      res <- result_rv()

      if (is.null(res)) {
        return(
          bslib::card(
            style = "margin-bottom: 10px; background: #fafafa;",
            bslib::card_body(
              shiny::p("Select files, set the target peptide, then click Run.")
            )
          )
        )
      }

      score <- res$annotation_score

      bslib::value_box(
        title = "Run completed",
        value = paste0("Matched ions: ", score$matched_count),
        showcase = bsicons::bs_icon("check-circle"),
        theme_color = "success",
        shiny::p(
          paste0("Explained intensity: ", round(score$explained_intensity_pct, 2), "%"),
          shiny::tags$br(),
          paste0("Final annotation score: ", round(score$final_annotation_score, 2))
        )
      )
    })

    shiny::observeEvent(input$run, {
      tryCatch({
        shiny::showNotification("Running PD strict engine...", type = "message", duration = 2)

        res <- shiny::withProgress(message = "Running strict PTM engine", value = 0, {
          shiny::incProgress(0.1, detail = "Building config")
          config <- build_config_from_input()

          shiny::incProgress(0.7, detail = "Running engine")
          run_pd_strict_engine(config)
        })

        result_rv(res)
        shiny::showNotification("Analysis completed.", type = "message", duration = 3)

      }, error = function(e) {
        result_rv(NULL)
        shiny::showNotification(
          paste("Analysis failed:", conditionMessage(e)),
          type = "error",
          duration = 8
        )
      })
    })

    shiny::observeEvent(input$reset, {
      result_rv(NULL)
      selected_msf_path(NULL)
      selected_mzxml_path(NULL)

      shiny::updateRadioButtons(session, "file_source", selected = "server")
      shiny::updateTextInput(session, "target_sequence", value = "")
      shiny::updateTextInput(session, "target_modified_sequence", value = "")
      shiny::updateTextInput(session, "target_raw", value = "")
      shiny::updateNumericInput(session, "manual_scan_number", value = NA)
      shiny::updateSelectInput(session, "tolerance_unit", selected = "ppm")
      shiny::updateNumericInput(session, "tolerance_value", value = 20)
      shiny::updateNumericInput(session, "max_fragment_charge", value = 2)
      shiny::updateSelectInput(session, "fragment_rule_set", selected = "ptm_strict")
      shiny::updateSelectInput(session, "activation_type", selected = "HCD")
      shiny::updateNumericInput(session, "min_relative_intensity", value = 0.5)
      shiny::updateCheckboxInput(session, "remove_precursor_window", value = FALSE)
      shiny::updateNumericInput(session, "precursor_mz", value = NA)
      shiny::updateNumericInput(session, "precursor_exclusion_da", value = 1.5)
      shiny::updateNumericInput(session, "top_n_per_window", value = NA)
      shiny::updateNumericInput(session, "window_size", value = 100)
      shiny::updateNumericInput(session, "label_top_n", value = 30)
      shiny::updateCheckboxInput(session, "show_unmatched_peaks", value = TRUE)
      shiny::updateCheckboxInput(session, "use_relative_intensity", value = TRUE)
    })

    output$combined_plot <- shiny::renderPlot({
      res <- result_rv()
      shiny::req(res)
      print(res$plots$combined)
    }, res = 120)

    output$summary_table <- DT::renderDT({
      res <- result_rv()
      shiny::req(res)

      score <- res$annotation_score
      df <- data.frame(
        Metric = c(
          "Matched ions",
          "Explained intensity (%)",
          "Mod-support count",
          "Diagnostic support count",
          "b continuity",
          "y continuity",
          "Localization score",
          "Final annotation score"
        ),
        Value = c(
          score$matched_count,
          round(score$explained_intensity_pct, 4),
          score$mod_support_count,
          score$diagnostic_support_count,
          score$b_continuity,
          score$y_continuity,
          score$localization_score,
          round(score$final_annotation_score, 4)
        ),
        stringsAsFactors = FALSE
      )

      DT::datatable(df, rownames = FALSE, options = list(pageLength = 8, dom = "tip"))
    })

    output$matches_table <- DT::renderDT({
      res <- result_rv()
      shiny::req(res)
      DT::datatable(res$final_matches$data, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$localization_table <- DT::renderDT({
      res <- result_rv()
      shiny::req(res)
      DT::datatable(res$localization_summary$data, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$psm_table <- DT::renderDT({
      res <- result_rv()
      shiny::req(res)
      DT::datatable(res$psm_selection$ranking_table, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$download_pdf <- shiny::downloadHandler(
      filename = function() "protvis_pd_strict_plot.pdf",
      content = function(file) {
        res <- result_rv()
        shiny::req(res)
        ggplot2::ggsave(file, plot = res$plots$combined, width = 14, height = 8.8, units = "in")
      }
    )

    output$download_png <- shiny::downloadHandler(
      filename = function() "protvis_pd_strict_plot.png",
      content = function(file) {
        res <- result_rv()
        shiny::req(res)
        ggplot2::ggsave(file, plot = res$plots$combined, width = 14, height = 8.8, units = "in", dpi = 300)
      }
    )

    output$download_csv <- shiny::downloadHandler(
      filename = function() "protvis_pd_strict_matches.csv",
      content = function(file) {
        res <- result_rv()
        shiny::req(res)
        utils::write.csv(res$final_matches$data, file, row.names = FALSE)
      }
    )

    output$download_rds <- shiny::downloadHandler(
      filename = function() "protvis_pd_strict_result.rds",
      content = function(file) {
        res <- result_rv()
        shiny::req(res)
        saveRDS(res, file = file)
      }
    )

    list(
      result = shiny::reactive(result_rv())
    )
  })
}

#' Run the PD strict module as a standalone demo app
#'
#' @return A Shiny app object.
#' @export
run_pd_strict_demo_app <- function() {
  ui <- bslib::page_navbar(
    title = "PD Strict Spectrum",
    theme = bslib::bs_theme(version = 5),
    bslib::nav_panel(
      title = "Module",
      pd_strict_module_ui("pd_strict")
    )
  )

  server <- function(input, output, session) {
    pd_strict_module_server("pd_strict")
  }

  shiny::shinyApp(ui, server)
}
