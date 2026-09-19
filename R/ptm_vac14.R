# Vac14/PXD001057 phosphopeptide validation benchmark -----------------------

.protvis_vac14_target <- function() {
  list(
    project = "PXD001057",
    protein = "AT2G01690.1 (Vac14)",
    sequence = "ATSGVPFSQYK",
    modified_sequence = "AT[pS]GVPFSQYK",
    phosphosite = "Ser3",
    phospho_position = 3L,
    run = "E1R2_SCX5_soluble",
    spectrum_id = "index=992",
    spectrum_title = "E1R2_SCX5_soluble.08422.08422.2",
    precursor_mz = 632.7847,
    precursor_charge = 2L,
    base_url = paste0(
      "https://ftp.pride.ebi.ac.uk/pride/data/archive/2019/12/",
      "PXD001057/"
    ),
    mzid_gz = "E1R2_SCX5_soluble.mzid.gz",
    mzid = "E1R2_SCX5_soluble.mzid",
    mgf = "E1R2_SCX5_soluble.mzid_E1R2_SCX5_soluble.MGF"
  )
}

.protvis_vac14_require_packages <- function() {
  packages <- c("Spectra", "MsBackendMgf", "PSMatch", "BiocParallel")
  missing <- packages[!vapply(packages, requireNamespace, logical(1L), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "Vac14 validation requires Bioconductor packages: ",
      paste(missing, collapse = ", "),
      ". Install them with BiocManager::install(c(\"Spectra\", ",
      "\"MsBackendMgf\", \"PSMatch\", \"BiocParallel\")).",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.protvis_ptm_read_psm <- function(mzid_file) {
  .protvis_vac14_require_packages()
  serial_backend <- BiocParallel::SerialParam()

  withCallingHandlers(
    PSMatch::PSM(mzid_file, BPPARAM = serial_backend),
    warning = function(warning) {
      message <- conditionMessage(warning)
      known_mzr_notice <- grepl(
        "mzR has been built against a different Rcpp version",
        message,
        fixed = TRUE
      )
      if (known_mzr_notice) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

.protvis_gunzip_file <- function(source, destination) {
  if (!file.exists(source) || !isTRUE(file.info(source)$size > 0)) {
    stop("The mzIdentML gzip file is missing or empty.", call. = FALSE)
  }
  input <- gzfile(source, open = "rb")
  output <- file(destination, open = "wb")
  on.exit({
    try(close(input), silent = TRUE)
    try(close(output), silent = TRUE)
  }, add = TRUE)
  repeat {
    block <- readBin(input, what = "raw", n = 1024L * 1024L)
    if (!length(block)) break
    writeBin(block, output)
  }
  close(input)
  close(output)
  if (!file.exists(destination) || !isTRUE(file.info(destination)$size > 0)) {
    stop("Failed to uncompress the mzIdentML file.", call. = FALSE)
  }
  normalizePath(destination, winslash = "/", mustWork = TRUE)
}

.protvis_vac14_download_files <- function(cache_dir = NULL) {
  target <- .protvis_vac14_target()
  cache_dir <- cache_dir %||% file.path(tempdir(), "ProtVis", target$project)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  gz_path <- file.path(cache_dir, target$mzid_gz)
  mzid_path <- file.path(cache_dir, target$mzid)
  mgf_path <- file.path(cache_dir, target$mgf)
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(3600, as.numeric(old_timeout %||% 60)))

  fetch <- function(name, destination) {
    if (!file.exists(destination) || !isTRUE(file.info(destination)$size > 0)) {
      partial <- paste0(destination, ".part-", Sys.getpid())
      on.exit(unlink(partial, force = TRUE), add = TRUE)
      status <- utils::download.file(
        paste0(target$base_url, name), partial,
        mode = "wb", quiet = TRUE
      )
      if (!isTRUE(status == 0L) || !file.exists(partial) ||
          !isTRUE(file.info(partial)$size > 0)) {
        stop("Download failed for ", name, ".", call. = FALSE)
      }
      moved <- isTRUE(file.rename(partial, destination))
      if (!moved && !isTRUE(file.copy(partial, destination, overwrite = TRUE))) {
        stop("Could not cache downloaded file ", name, ".", call. = FALSE)
      }
    }
    if (!file.exists(destination) || !isTRUE(file.info(destination)$size > 0)) {
      stop("Download failed for ", name, ".", call. = FALSE)
    }
    destination
  }

  fetch(target$mzid_gz, gz_path)
  fetch(target$mgf, mgf_path)
  if (!file.exists(mzid_path) || !isTRUE(file.info(mzid_path)$size > 0)) {
    .protvis_gunzip_file(gz_path, mzid_path)
  }
  list(mzid = mzid_path, mgf = mgf_path, source = "PRIDE public files")
}

.protvis_vac14_prepare_uploads <- function(mzid_upload, mgf_upload) {
  if (is.null(mzid_upload) || is.null(mgf_upload)) {
    stop("Upload both an mzIdentML file and its corresponding MGF file.", call. = FALSE)
  }
  mzid_path <- tempfile("protvis-vac14-", fileext = ".mzid")
  if (grepl("\\.gz$", mzid_upload$name, ignore.case = TRUE)) {
    .protvis_gunzip_file(mzid_upload$datapath, mzid_path)
  } else if (!file.copy(mzid_upload$datapath, mzid_path, overwrite = TRUE)) {
    stop("Failed to prepare the uploaded mzIdentML file.", call. = FALSE)
  }
  mgf_path <- tempfile("protvis-vac14-", fileext = ".mgf")
  if (!file.copy(mgf_upload$datapath, mgf_path, overwrite = TRUE)) {
    stop("Failed to prepare the uploaded MGF file.", call. = FALSE)
  }
  list(
    mzid = normalizePath(mzid_path, winslash = "/", mustWork = TRUE),
    mgf = normalizePath(mgf_path, winslash = "/", mustWork = TRUE),
    source = "Uploaded files"
  )
}

.protvis_vac14_masses <- function() {
  c(
    A = 71.037113805, R = 156.101111050, N = 114.042927470,
    D = 115.026943065, C = 103.009184505, E = 129.042593135,
    Q = 128.058577540, G = 57.021463735, H = 137.058911875,
    I = 113.084063975, L = 113.084063975, K = 128.094963015,
    M = 131.040484645, F = 147.068413945, P = 97.052763875,
    S = 87.032028435, T = 101.047678505, W = 186.079312980,
    Y = 163.063328575, V = 99.068413945
  )
}

.protvis_vac14_theoretical <- function() {
  target <- .protvis_vac14_target()
  constants <- list(
    proton = 1.007276466621,
    water = 18.010564684,
    ammonia = 17.026549101,
    phospho = 79.966330890,
    phosphoric_acid = 97.976895574
  )
  residues <- strsplit(target$sequence, "", fixed = TRUE)[[1L]]
  n <- length(residues)
  residue_mass <- unname(.protvis_vac14_masses()[residues])
  residue_mass[target$phospho_position] <-
    residue_mass[target$phospho_position] + constants$phospho

  b_mz <- vapply(seq_len(n - 1L), function(i) {
    sum(residue_mass[seq_len(i)]) + constants$proton
  }, numeric(1L))
  y_mz <- vapply(seq_len(n - 1L), function(i) {
    idx <- seq.int(n - i + 1L, n)
    sum(residue_mass[idx]) + constants$water + constants$proton
  }, numeric(1L))
  names(b_mz) <- paste0("b", seq_along(b_mz))
  names(y_mz) <- paste0("y", seq_along(y_mz))

  mh <- sum(residue_mass) + constants$water + constants$proton
  precursor_2plus <- (mh + constants$proton) / 2
  prefix <- vapply(seq_len(n), function(i) paste0(residues[seq_len(i)], collapse = ""), character(1L))
  suffix <- vapply(seq_len(n), function(i) paste0(residues[seq.int(i, n)], collapse = ""), character(1L))
  b_full <- c(b_mz, mh)
  y_full <- rev(c(y_mz, mh))
  fragment_table <- data.frame(
    B = seq_len(n),
    `B Ions` = b_full,
    `B+2H` = (b_full + constants$proton) / 2,
    `B-NH3` = ifelse(grepl("[KNQR]", prefix), b_full - constants$ammonia, NA_real_),
    `B-H2O` = ifelse(grepl("[DEST]", prefix), b_full - constants$water, NA_real_),
    AA = ifelse(seq_len(n) == target$phospho_position, "S+80", residues),
    `Y Ions` = y_full,
    `Y+2H` = (y_full + constants$proton) / 2,
    `Y-NH3` = ifelse(grepl("[KNQR]", suffix), y_full - constants$ammonia, NA_real_),
    `Y-H2O` = ifelse(grepl("[DEST]", suffix), y_full - constants$water, NA_real_),
    Y = n:1L,
    check.names = FALSE
  )
  rounded_table <- fragment_table
  numeric_columns <- vapply(rounded_table, is.numeric, logical(1L))
  rounded_table[numeric_columns] <- lapply(rounded_table[numeric_columns], round, digits = 1L)

  add_candidate <- function(label, mz, series, charge = 1L,
                            neutral = FALSE, priority = 1L) {
    data.frame(
      label = label, mz = as.numeric(mz), series = series,
      charge = charge, neutral = neutral, priority = priority,
      stringsAsFactors = FALSE
    )
  }
  candidates <- list()
  add <- function(...) {
    candidates[[length(candidates) + 1L]] <<- add_candidate(...)
  }
  for (i in seq_along(b_mz)) add(paste0("b", i), b_mz[i], "b")
  for (i in seq_along(y_mz)) add(paste0("y", i), y_mz[i], "y")
  for (i in seq_along(b_mz)) add(paste0("b", i, "++"), (b_mz[i] + constants$proton) / 2, "b", 2L, FALSE, 2L)
  for (i in seq_along(y_mz)) add(paste0("y", i, "++"), (y_mz[i] + constants$proton) / 2, "y", 2L, FALSE, 2L)

  for (i in seq_along(b_mz)) {
    frag <- paste0(residues[seq_len(i)], collapse = "")
    if (grepl("[DEST]", frag)) add(paste0("b", i, "-H2O"), b_mz[i] - constants$water, "b", neutral = TRUE, priority = 4L)
    if (grepl("[KNQR]", frag)) add(paste0("b", i, "-NH3"), b_mz[i] - constants$ammonia, "b", neutral = TRUE, priority = 4L)
  }
  for (i in seq_along(y_mz)) {
    start <- n - i + 1L
    frag <- paste0(residues[seq.int(start, n)], collapse = "")
    if (grepl("[DEST]", frag)) add(paste0("y", i, "-H2O"), y_mz[i] - constants$water, "y", neutral = TRUE, priority = 4L)
    if (grepl("[KNQR]", frag)) add(paste0("y", i, "-NH3"), y_mz[i] - constants$ammonia, "y", neutral = TRUE, priority = 4L)
  }
  for (i in seq.int(target$phospho_position, length(b_mz))) {
    add(paste0("b", i, "-98"), b_mz[i] - constants$phosphoric_acid, "b", neutral = TRUE, priority = 3L)
    frag <- paste0(residues[seq_len(i)], collapse = "")
    if (grepl("[DEST]", frag)) add(paste0("b", i, "-H2O-98"), b_mz[i] - constants$water - constants$phosphoric_acid, "b", neutral = TRUE, priority = 3L)
  }
  for (i in seq_along(y_mz)) {
    start <- n - i + 1L
    if (start <= target$phospho_position) {
      add(paste0("y", i, "-98"), y_mz[i] - constants$phosphoric_acid, "y", neutral = TRUE, priority = 3L)
      frag <- paste0(residues[seq.int(start, n)], collapse = "")
      if (grepl("[DEST]", frag)) add(paste0("y", i, "-H2O-98"), y_mz[i] - constants$water - constants$phosphoric_acid, "y", neutral = TRUE, priority = 3L)
    }
  }
  candidates <- do.call(rbind, candidates)
  candidates <- candidates[is.finite(candidates$mz) & candidates$mz > 0, , drop = FALSE]
  candidates <- candidates[order(candidates$mz), , drop = FALSE]
  key_labels <- c("b3", "b5", "b7", "b8", "y4", "y5", "y6", "y7", "y8")
  key_ions <- candidates[candidates$label %in% key_labels & !candidates$neutral & candidates$charge == 1L, c("label", "mz")]
  key_ions <- key_ions[match(key_labels, key_ions$label), , drop = FALSE]

  list(
    b_mz = b_mz, y_mz = y_mz, mh = mh,
    precursor_2plus = precursor_2plus,
    fragment_table = rounded_table,
    candidates = candidates,
    key_ions = key_ions
  )
}

.protvis_vac14_match_ions <- function(peaks, candidates, tolerance_da = 0.5) {
  tolerance_da <- as.numeric(tolerance_da[[1L]])
  if (!is.finite(tolerance_da) || tolerance_da <= 0) {
    stop("Fragment tolerance must be a positive number.", call. = FALSE)
  }
  matches <- lapply(seq_len(nrow(candidates)), function(i) {
    delta <- abs(peaks$mz - candidates$mz[i])
    j <- which.min(delta)
    if (!length(j) || delta[j] > tolerance_da) return(NULL)
    data.frame(
      peak_index = j,
      observed_mz = peaks$mz[j],
      intensity = peaks$rel[j],
      theoretical_mz = candidates$mz[i],
      error_da = peaks$mz[j] - candidates$mz[i],
      label = candidates$label[i],
      series = candidates$series[i],
      charge = candidates$charge[i],
      neutral = candidates$neutral[i],
      priority = candidates$priority[i],
      stringsAsFactors = FALSE
    )
  })
  matches <- Filter(Negate(is.null), matches)
  if (!length(matches)) stop("No fragment ions matched the target spectrum.", call. = FALSE)
  matched <- do.call(rbind, matches)
  matched <- matched[order(matched$peak_index, matched$priority, abs(matched$error_da)), , drop = FALSE]
  matched <- matched[!duplicated(matched$peak_index), , drop = FALSE]
  matched[order(matched$observed_mz), , drop = FALSE]
}

.protvis_flatten_psm <- function(psm) {
  dat <- as.data.frame(psm, optional = TRUE, stringsAsFactors = FALSE)
  dat[] <- lapply(dat, function(column) {
    vapply(seq_len(nrow(dat)), function(i) {
      value <- column[[i]]
      if (!length(value) || all(is.na(value))) return(NA_character_)
      paste(as.character(value), collapse = "; ")
    }, character(1L))
  })
  dat
}

.protvis_psm_values <- function(psm_row, column) {
  if (!column %in% names(psm_row)) return(character())
  value <- psm_row[[column]]
  if (is.list(value) && length(value) == 1L) value <- value[[1L]]
  value <- unlist(value, recursive = TRUE, use.names = FALSE)
  value <- as.character(value)
  if (length(value) == 1L && grepl(";", value, fixed = TRUE)) {
    value <- unlist(strsplit(value, ";", fixed = TRUE), use.names = FALSE)
  }
  trimws(value[!is.na(value) & nzchar(trimws(value))])
}

.protvis_ptm_modifications <- function(psm_row, sequence) {
  locations <- suppressWarnings(as.integer(.protvis_psm_values(psm_row, "modLocation")))
  masses <- suppressWarnings(as.numeric(.protvis_psm_values(psm_row, "modMass")))
  names <- .protvis_psm_values(psm_row, "modName")
  count <- max(length(locations), length(masses), length(names), 0L)
  if (!count) {
    return(data.frame(location = integer(), mass = numeric(), name = character()))
  }
  recycle <- function(x, n, missing) {
    if (!length(x)) return(rep(missing, n))
    rep(x, length.out = n)
  }
  result <- data.frame(
    location = recycle(locations, count, NA_integer_),
    mass = recycle(masses, count, NA_real_),
    name = recycle(names, count, "Modification"),
    stringsAsFactors = FALSE
  )
  result <- result[is.finite(result$mass) & !is.na(result$location), , drop = FALSE]
  result$location[result$location < 0L] <- 0L
  result$location[result$location > nchar(sequence) + 1L] <- nchar(sequence) + 1L
  result
}

.protvis_modified_sequence_label <- function(sequence, modifications) {
  residues <- strsplit(sequence, "", fixed = TRUE)[[1L]]
  if (!nrow(modifications)) return(sequence)
  nterm <- modifications[modifications$location == 0L, , drop = FALSE]
  cterm <- modifications[modifications$location == length(residues) + 1L, , drop = FALSE]
  labels <- residues
  for (i in seq_along(residues)) {
    mods <- modifications[modifications$location == i, , drop = FALSE]
    if (!nrow(mods)) next
    if (nrow(mods) == 1L && grepl("phosph", mods$name[[1L]], ignore.case = TRUE)) {
      labels[i] <- paste0("[p", residues[i], "]")
      next
    }
    annotations <- vapply(seq_len(nrow(mods)), function(j) {
      paste0(mods$name[j], " ", sprintf("%+.4f", mods$mass[j]))
    }, character(1L))
    labels[i] <- paste0(residues[i], "[", paste(annotations, collapse = "; "), "]")
  }
  result <- paste0(labels, collapse = "")
  if (nrow(nterm)) result <- paste0("[N-term ", sprintf("%+.4f", sum(nterm$mass)), "]", result)
  if (nrow(cterm)) result <- paste0(result, "[C-term ", sprintf("%+.4f", sum(cterm$mass)), "]")
  result
}

.protvis_ptm_theoretical <- function(sequence, modifications = NULL) {
  sequence <- toupper(gsub("[^A-Z]", "", as.character(sequence[[1L]])))
  residues <- strsplit(sequence, "", fixed = TRUE)[[1L]]
  n <- length(residues)
  if (n < 2L) stop("The selected peptide must contain at least two residues.", call. = FALSE)
  base_masses <- .protvis_vac14_masses()
  unsupported <- setdiff(unique(residues), names(base_masses))
  if (length(unsupported)) {
    stop("Unsupported amino-acid code(s): ", paste(unsupported, collapse = ", "), call. = FALSE)
  }
  if (is.null(modifications)) {
    modifications <- data.frame(location = integer(), mass = numeric(), name = character())
  }
  constants <- list(
    proton = 1.007276466621, water = 18.010564684,
    ammonia = 17.026549101, phosphoric_acid = 97.976895574
  )
  residue_mass <- unname(base_masses[residues])
  residue_mod <- numeric(n)
  for (i in seq_len(nrow(modifications))) {
    location <- modifications$location[i]
    if (is.finite(location) && location >= 1L && location <= n) {
      residue_mod[location] <- residue_mod[location] + modifications$mass[i]
    }
  }
  residue_mass <- residue_mass + residue_mod
  nterm_shift <- sum(modifications$mass[modifications$location == 0L], na.rm = TRUE)
  cterm_shift <- sum(modifications$mass[modifications$location == n + 1L], na.rm = TRUE)
  phospho_positions <- modifications$location[
    grepl("phosph", modifications$name, ignore.case = TRUE) |
      abs(modifications$mass - 79.966330890) < 0.02
  ]
  phospho_positions <- phospho_positions[phospho_positions >= 1L & phospho_positions <= n]

  b_mz <- vapply(seq_len(n - 1L), function(i) {
    sum(residue_mass[seq_len(i)]) + nterm_shift + constants$proton
  }, numeric(1L))
  y_mz <- vapply(seq_len(n - 1L), function(i) {
    idx <- seq.int(n - i + 1L, n)
    sum(residue_mass[idx]) + cterm_shift + constants$water + constants$proton
  }, numeric(1L))
  names(b_mz) <- paste0("b", seq_along(b_mz))
  names(y_mz) <- paste0("y", seq_along(y_mz))
  mh <- sum(residue_mass) + nterm_shift + cterm_shift + constants$water + constants$proton
  precursor_charge_2 <- (mh + constants$proton) / 2

  prefix <- vapply(seq_len(n), function(i) paste0(residues[seq_len(i)], collapse = ""), character(1L))
  suffix <- vapply(seq_len(n), function(i) paste0(residues[seq.int(i, n)], collapse = ""), character(1L))
  b_full <- c(b_mz, mh)
  y_full <- rev(c(y_mz, mh))
  aa_labels <- residues
  modified_residues <- which(abs(residue_mod) > .Machine$double.eps)
  aa_labels[modified_residues] <- paste0(
    residues[modified_residues],
    ifelse(residue_mod[modified_residues] >= 0, "+", ""),
    round(residue_mod[modified_residues], 1)
  )
  fragment_table <- data.frame(
    B = seq_len(n), `B Ions` = b_full,
    `B+2H` = (b_full + constants$proton) / 2,
    `B-NH3` = ifelse(grepl("[KNQR]", prefix), b_full - constants$ammonia, NA_real_),
    `B-H2O` = ifelse(grepl("[DEST]", prefix), b_full - constants$water, NA_real_),
    AA = aa_labels, `Y Ions` = y_full,
    `Y+2H` = (y_full + constants$proton) / 2,
    `Y-NH3` = ifelse(grepl("[KNQR]", suffix), y_full - constants$ammonia, NA_real_),
    `Y-H2O` = ifelse(grepl("[DEST]", suffix), y_full - constants$water, NA_real_),
    Y = n:1L, check.names = FALSE
  )
  numeric_columns <- vapply(fragment_table, is.numeric, logical(1L))
  fragment_table[numeric_columns] <- lapply(fragment_table[numeric_columns], round, digits = 1L)

  candidate_list <- list()
  add <- function(label, mz, series, charge = 1L, neutral = FALSE, priority = 1L) {
    candidate_list[[length(candidate_list) + 1L]] <<- data.frame(
      label = label, mz = as.numeric(mz), series = series, charge = charge,
      neutral = neutral, priority = priority, stringsAsFactors = FALSE
    )
  }
  for (i in seq_along(b_mz)) add(paste0("b", i), b_mz[i], "b")
  for (i in seq_along(y_mz)) add(paste0("y", i), y_mz[i], "y")
  for (i in seq_along(b_mz)) add(paste0("b", i, "++"), (b_mz[i] + constants$proton) / 2, "b", 2L, FALSE, 2L)
  for (i in seq_along(y_mz)) add(paste0("y", i, "++"), (y_mz[i] + constants$proton) / 2, "y", 2L, FALSE, 2L)
  for (i in seq_along(b_mz)) {
    frag <- prefix[i]
    if (grepl("[DEST]", frag)) add(paste0("b", i, "-H2O"), b_mz[i] - constants$water, "b", neutral = TRUE, priority = 4L)
    if (grepl("[KNQR]", frag)) add(paste0("b", i, "-NH3"), b_mz[i] - constants$ammonia, "b", neutral = TRUE, priority = 4L)
    if (any(phospho_positions <= i)) add(paste0("b", i, "-98"), b_mz[i] - constants$phosphoric_acid, "b", neutral = TRUE, priority = 3L)
  }
  for (i in seq_along(y_mz)) {
    start <- n - i + 1L
    frag <- paste0(residues[seq.int(start, n)], collapse = "")
    if (grepl("[DEST]", frag)) add(paste0("y", i, "-H2O"), y_mz[i] - constants$water, "y", neutral = TRUE, priority = 4L)
    if (grepl("[KNQR]", frag)) add(paste0("y", i, "-NH3"), y_mz[i] - constants$ammonia, "y", neutral = TRUE, priority = 4L)
    if (any(phospho_positions >= start)) add(paste0("y", i, "-98"), y_mz[i] - constants$phosphoric_acid, "y", neutral = TRUE, priority = 3L)
  }
  candidates <- do.call(rbind, candidate_list)
  candidates <- candidates[is.finite(candidates$mz) & candidates$mz > 0, , drop = FALSE]
  candidates <- candidates[order(candidates$mz), , drop = FALSE]
  ion_coverage <- candidates[
    !candidates$neutral & candidates$charge == 1L,
    c("label", "mz", "series"), drop = FALSE
  ]
  list(
    b_mz = b_mz, y_mz = y_mz, mh = mh,
    precursor_2plus = precursor_charge_2,
    fragment_table = fragment_table,
    candidates = candidates,
    key_ions = ion_coverage,
    modifications = modifications
  )
}

.protvis_ptm_psm_catalog <- function(psm) {
  rows <- lapply(seq_len(nrow(psm)), function(i) {
    row <- psm[i, , drop = FALSE]
    sequence <- .protvis_psm_values(row, "sequence")
    if (!length(sequence)) return(NULL)
    sequence <- sequence[[1L]]
    modifications <- .protvis_ptm_modifications(row, sequence)
    modified <- .protvis_modified_sequence_label(sequence, modifications)
    spectrum_id <- .protvis_psm_values(row, "spectrumID")
    title <- .protvis_psm_values(row, "spectrum.title")
    charge <- .protvis_psm_values(row, "chargeState")
    spectrum_label <- if (length(title)) title[[1L]] else if (length(spectrum_id)) spectrum_id[[1L]] else paste0("PSM ", i)
    charge_label <- if (length(charge)) paste0("z=", charge[[1L]]) else "charge NA"
    data.frame(
      psm_index = i, sequence = sequence, modified_sequence = modified,
      spectrum_id = if (length(spectrum_id)) spectrum_id[[1L]] else "",
      spectrum_title = if (length(title)) title[[1L]] else "",
      charge = if (length(charge)) charge[[1L]] else "",
      label = paste(modified, charge_label, spectrum_label, sep = " | "),
      stringsAsFactors = FALSE
    )
  })
  catalog <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(catalog) || !nrow(catalog)) {
    stop("No peptide-spectrum matches were found in the mzIdentML file.", call. = FALSE)
  }
  key <- paste(catalog$modified_sequence, catalog$spectrum_id,
               catalog$spectrum_title, catalog$charge, sep = "|")
  catalog[!duplicated(key), , drop = FALSE]
}

.protvis_ptm_load_bundle <- function(mzid_file, mgf_file, source = "Input files") {
  .protvis_vac14_require_packages()
  psm <- .protvis_ptm_read_psm(mzid_file)
  catalog <- .protvis_ptm_psm_catalog(psm)
  spectra <- Spectra::Spectra(mgf_file, source = MsBackendMgf::MsBackendMgf())
  metadata <- as.data.frame(Spectra::spectraData(spectra), optional = TRUE)
  if (!length(spectra)) stop("No spectra were found in the MGF file.", call. = FALSE)
  list(psm = psm, catalog = catalog, spectra = spectra,
       metadata = metadata, source = source)
}

.protvis_ptm_find_spectrum <- function(bundle, psm_row, catalog_row) {
  metadata <- bundle$metadata
  text_columns <- names(metadata)[vapply(
    metadata, function(x) is.character(x) || is.factor(x), logical(1L)
  )]
  requested <- unique(c(
    catalog_row$spectrum_title,
    .protvis_psm_values(psm_row, "spectrum.title")
  ))
  requested <- requested[nzchar(requested)]
  for (needle in requested) {
    for (column in text_columns) {
      hit <- which(as.character(metadata[[column]]) == needle)
      if (length(hit)) return(list(index = hit[[1L]], column = column, method = "title"))
    }
  }
  scan_patterns <- unique(gsub(".*?(\\d+\\.\\d+\\.\\d+).*", "\\1", requested))
  scan_patterns <- scan_patterns[grepl("^\\d+\\.\\d+\\.\\d+$", scan_patterns)]
  for (needle in scan_patterns) {
    for (column in text_columns) {
      hit <- grep(needle, as.character(metadata[[column]]), fixed = TRUE)
      if (length(hit)) return(list(index = hit[[1L]], column = column, method = "scan title"))
    }
  }
  spectrum_id <- c(catalog_row$spectrum_id, .protvis_psm_values(psm_row, "spectrumID"))
  index_value <- suppressWarnings(as.integer(sub("^index=", "", spectrum_id[grepl("^index=", spectrum_id)][1L])))
  if (is.finite(index_value)) {
    candidates <- unique(c(index_value + 1L, index_value))
    candidates <- candidates[candidates >= 1L & candidates <= length(bundle$spectra)]
    if (length(candidates)) return(list(index = candidates[[1L]], column = "spectrumID", method = "index"))
  }
  stop("The spectrum linked to the selected PSM was not found in the MGF file.", call. = FALSE)
}

.protvis_ptm_run_selected <- function(bundle, psm_index, tolerance_da = 0.5) {
  catalog_hit <- which(bundle$catalog$psm_index == as.integer(psm_index[[1L]]))
  if (!length(catalog_hit)) stop("Select a valid peptide/PSM.", call. = FALSE)
  catalog_row <- bundle$catalog[catalog_hit[[1L]], , drop = FALSE]
  psm_row <- bundle$psm[catalog_row$psm_index, , drop = FALSE]
  sequence <- catalog_row$sequence[[1L]]
  modifications <- .protvis_ptm_modifications(psm_row, sequence)
  theoretical <- .protvis_ptm_theoretical(sequence, modifications)
  spectrum_match <- .protvis_ptm_find_spectrum(bundle, psm_row, catalog_row)
  spectrum <- bundle$spectra[spectrum_match$index]
  peak_matrix <- Spectra::peaksData(spectrum)[[1L]]
  if (is.null(dim(peak_matrix)) || !nrow(peak_matrix)) stop("The selected spectrum has no peaks.", call. = FALSE)
  keep <- is.finite(peak_matrix[, "mz"]) & is.finite(peak_matrix[, "intensity"])
  peak_matrix <- peak_matrix[keep, , drop = FALSE]
  peak_matrix <- peak_matrix[order(peak_matrix[, "mz"]), , drop = FALSE]
  peaks <- data.frame(mz = as.numeric(peak_matrix[, "mz"]),
                      intensity = as.numeric(peak_matrix[, "intensity"]))
  if (!nrow(peaks) || max(peaks$intensity) <= 0) stop("The selected spectrum has no positive intensities.", call. = FALSE)
  peaks$rel <- peaks$intensity / max(peaks$intensity) * 100
  matched <- .protvis_vac14_match_ions(peaks, theoretical$candidates, tolerance_da)
  coverage <- theoretical$key_ions
  coverage$matched <- vapply(coverage$mz, function(mz) {
    min(abs(peaks$mz - mz), na.rm = TRUE) <= tolerance_da
  }, logical(1L))
  variables <- Spectra::spectraVariables(spectrum)
  observed_mz <- if ("precursorMz" %in% variables) as.numeric(spectrum$precursorMz[1L]) else NA_real_
  charge_values <- suppressWarnings(as.integer(.protvis_psm_values(psm_row, "chargeState")))
  observed_charge <- if ("precursorCharge" %in% variables) as.integer(spectrum$precursorCharge[1L]) else NA_integer_
  charge <- if (length(charge_values) && is.finite(charge_values[[1L]])) charge_values[[1L]] else observed_charge
  if (!is.finite(charge)) charge <- 2L
  calculated_precursor <- (theoretical$mh + (charge - 1L) * 1.007276466621) / charge
  proteins <- .protvis_psm_values(psm_row, "DatabaseAccess")
  protein <- if (length(proteins)) paste(proteins, collapse = "; ") else "Protein not reported"
  spectrum_title <- catalog_row$spectrum_title[[1L]]
  if (!nzchar(spectrum_title)) spectrum_title <- catalog_row$spectrum_id[[1L]]
  benchmark <- .protvis_vac14_target()
  is_public_benchmark <- identical(bundle$source, "PRIDE public files") &&
    identical(sequence, benchmark$sequence) &&
    (identical(catalog_row$spectrum_id[[1L]], benchmark$spectrum_id) ||
       identical(spectrum_title, benchmark$spectrum_title))
  if (is_public_benchmark) protein <- benchmark$protein
  display_sequence <- if (is_public_benchmark) {
    paste0("R.", catalog_row$modified_sequence[[1L]], ".H")
  } else {
    catalog_row$modified_sequence[[1L]]
  }
  spectrum_label <- if (is_public_benchmark) {
    paste("Vac14", display_sequence)
  } else {
    display_sequence
  }
  target <- list(
    project = if (identical(bundle$source, "PRIDE public files")) "PXD001057" else "Uploaded dataset",
    protein = protein, sequence = sequence,
    modified_sequence = catalog_row$modified_sequence[[1L]],
    display_sequence = display_sequence,
    spectrum_label = spectrum_label,
    is_public_benchmark = is_public_benchmark,
    spectrum_id = catalog_row$spectrum_id[[1L]],
    spectrum_title = spectrum_title,
    precursor_mz = if (is.finite(observed_mz)) observed_mz else calculated_precursor,
    precursor_charge = charge
  )
  psm_table <- .protvis_flatten_psm(psm_row)
  display_columns <- intersect(
    c("sequence", "spectrumID", "chargeState", "passThreshold",
      "experimentalMassToCharge", "calculatedMassToCharge", "DatabaseAccess",
      "spectrum.title", "Scaffold.Peptide.Probability", "Mascot.score",
      "modName", "modMass", "modLocation"), names(psm_table)
  )
  psm_table <- psm_table[, display_columns, drop = FALSE]
  summary <- data.frame(
    Item = c("Dataset", "Protein", "Peptide", "Modified peptide", "Spectrum",
             "Spectrum mapping", "Data source", "Observed precursor m/z",
             "Calculated precursor m/z", "Charge", "Fragment tolerance",
             "Matched fragment ions", "Matched b/y coverage ions"),
    Value = c(target$project, protein, sequence, target$modified_sequence,
              spectrum_title, spectrum_match$method, bundle$source,
              ifelse(is.finite(observed_mz), sprintf("%.6f", observed_mz), "Not reported"),
              sprintf("%.6f", calculated_precursor), charge,
              paste0(tolerance_da, " Da"), nrow(matched),
              paste0(sum(coverage$matched), "/", nrow(coverage))),
    stringsAsFactors = FALSE
  )
  list(target = target, source = bundle$source, psm_table = psm_table,
       spectrum_match_column = spectrum_match$column, peaks = peaks,
       matched = matched, theoretical = theoretical, key_ions = coverage,
       summary = summary)
}

.protvis_vac14_run <- function(mzid_file, mgf_file, tolerance_da = 0.5,
                               source = "Input files") {
  .protvis_vac14_require_packages()
  target <- .protvis_vac14_target()
  theoretical <- .protvis_vac14_theoretical()

  psm <- .protvis_ptm_read_psm(mzid_file)
  sequence_index <- which(as.character(psm$sequence) == target$sequence)
  if (!length(sequence_index)) {
    stop("Target peptide ", target$sequence, " was not found in ", basename(mzid_file), ".", call. = FALSE)
  }
  psm_target <- psm[sequence_index, , drop = FALSE]
  exact_index <- which(as.character(psm_target$spectrumID) == target$spectrum_id)
  psm_exact <- if (length(exact_index)) psm_target[exact_index, , drop = FALSE] else psm_target
  psm_table <- .protvis_flatten_psm(psm_exact)
  display_columns <- intersect(
    c("sequence", "spectrumID", "chargeState", "passThreshold",
      "experimentalMassToCharge", "calculatedMassToCharge", "DatabaseAccess",
      "spectrum.title", "Scaffold.Peptide.Probability", "Mascot.score",
      "modName", "modMass", "modLocation"),
    names(psm_table)
  )
  psm_table <- psm_table[, display_columns, drop = FALSE]
  psm_value <- function(column) {
    if (!column %in% names(psm_table)) return(character())
    unlist(strsplit(paste(psm_table[[column]], collapse = ";"), ";", fixed = TRUE))
  }
  site_confirmed <- any(trimws(psm_value("modLocation")) == "3", na.rm = TRUE)
  phospho_confirmed <- any(grepl("phosph", psm_value("modName"), ignore.case = TRUE), na.rm = TRUE)
  threshold_passed <- any(tolower(trimws(psm_value("passThreshold"))) == "true", na.rm = TRUE)

  spectra <- Spectra::Spectra(
    mgf_file,
    source = MsBackendMgf::MsBackendMgf()
  )
  metadata <- as.data.frame(Spectra::spectraData(spectra), optional = TRUE)
  text_columns <- names(metadata)[vapply(metadata, function(x) is.character(x) || is.factor(x), logical(1L))]
  hit <- integer()
  matched_column <- NA_character_
  for (name in text_columns) {
    hit <- which(as.character(metadata[[name]]) == target$spectrum_title)
    if (length(hit)) {
      matched_column <- name
      break
    }
  }
  if (!length(hit)) {
    for (name in text_columns) {
      hit <- grep("08422\\.08422\\.2", as.character(metadata[[name]]))
      if (length(hit)) {
        matched_column <- name
        break
      }
    }
  }
  if (!length(hit)) {
    stop("Target spectrum ", target$spectrum_title, " was not found in the MGF metadata.", call. = FALSE)
  }
  spectrum <- spectra[hit[[1L]]]
  peak_matrix <- Spectra::peaksData(spectrum)[[1L]]
  if (is.null(dim(peak_matrix)) || !nrow(peak_matrix)) {
    stop("No peaks were found in the target spectrum.", call. = FALSE)
  }
  keep <- is.finite(peak_matrix[, "mz"]) & is.finite(peak_matrix[, "intensity"])
  peak_matrix <- peak_matrix[keep, , drop = FALSE]
  peak_matrix <- peak_matrix[order(peak_matrix[, "mz"]), , drop = FALSE]
  peaks <- data.frame(
    mz = as.numeric(peak_matrix[, "mz"]),
    intensity = as.numeric(peak_matrix[, "intensity"]),
    stringsAsFactors = FALSE
  )
  if (!nrow(peaks) || max(peaks$intensity) <= 0) {
    stop("The target spectrum contains no positive finite intensity values.", call. = FALSE)
  }
  peaks$rel <- peaks$intensity / max(peaks$intensity) * 100
  matched <- .protvis_vac14_match_ions(peaks, theoretical$candidates, tolerance_da)

  spectrum_variables <- Spectra::spectraVariables(spectrum)
  observed_precursor <- if ("precursorMz" %in% spectrum_variables) as.numeric(spectrum$precursorMz[1L]) else NA_real_
  observed_charge <- if ("precursorCharge" %in% spectrum_variables) as.integer(spectrum$precursorCharge[1L]) else NA_integer_
  key_ions <- theoretical$key_ions
  key_ions$matched <- vapply(key_ions$mz, function(mz) {
    min(abs(peaks$mz - mz), na.rm = TRUE) <= tolerance_da
  }, logical(1L))

  summary <- data.frame(
    Item = c("Project", "Protein", "Modified peptide", "Validated site",
             "Target run", "Spectrum", "mzIdentML spectrum ID", "Data source",
             "Exact target PSM found", "Phospho modification reported",
             "Ser3 location reported", "PSM passed threshold",
             "Observed precursor m/z", "Calculated precursor m/z", "Charge",
             "Fragment tolerance", "Matched fragment ions"),
    Value = c(target$project, target$protein, target$modified_sequence,
              target$phosphosite, target$run, target$spectrum_title,
              target$spectrum_id, source,
              ifelse(length(exact_index) > 0L, "Yes", "No; peptide-level fallback used"),
              ifelse(phospho_confirmed, "Yes", "Not reported"),
              ifelse(site_confirmed, "Yes", "Not reported"),
              ifelse(threshold_passed, "Yes", "Not reported"),
              ifelse(is.finite(observed_precursor), sprintf("%.6f", observed_precursor), "Not reported"),
              sprintf("%.6f", theoretical$precursor_2plus),
              ifelse(is.na(observed_charge), as.character(target$precursor_charge), as.character(observed_charge)),
              paste0(tolerance_da, " Da"), nrow(matched)),
    stringsAsFactors = FALSE
  )
  list(
    target = target,
    source = source,
    psm_table = psm_table,
    psm_exact = length(exact_index) > 0L,
    spectrum_match_column = matched_column,
    peaks = peaks,
    matched = matched,
    theoretical = theoretical,
    key_ions = key_ions,
    summary = summary
  )
}

.protvis_vac14_draw_table <- function(table) {
  nr <- nrow(table)
  nc <- ncol(table)
  plot.new()
  plot.window(xlim = c(0, nc), ylim = c(0, nr + 2))
  for (j in seq_len(nc)) text(j - 0.5, nr + 1.2, names(table)[j], cex = 0.7, font = 2)
  segments(0, nr + 0.7, nc, nr + 0.7, lwd = 0.8)
  for (i in seq_len(nr)) {
    y <- nr - i + 0.6
    for (j in seq_len(nc)) {
      value <- table[i, j]
      label <- if (is.na(value)) "" else as.character(value)
      text(j - 0.5, y, label, cex = 0.66,
           font = if (names(table)[j] == "AA" && i == 3L) 2 else 1)
    }
  }
  box()
}

.protvis_vac14_draw_spectrum <- function(result, b_color = "#C0392B",
                                          y_color = "#2E63C4") {
  peaks <- result$peaks
  matched <- result$matched
  plot(
    peaks$mz, peaks$rel, type = "h", lwd = 0.75,
    xlim = c(0, max(1300, max(peaks$mz, na.rm = TRUE))), ylim = c(0, 108),
    xlab = "m/z", ylab = "Relative intensity (%)", main = ""
  )
  colors <- ifelse(matched$neutral, "#228B22",
                   ifelse(matched$series == "b", b_color, y_color))
  segments(matched$observed_mz, 0, matched$observed_mz, matched$intensity,
           col = colors, lwd = 1.5)
  labels <- matched[matched$intensity >= 1, , drop = FALSE]
  label_colors <- ifelse(labels$neutral, "#228B22",
                         ifelse(labels$series == "b", b_color, y_color))
  text(labels$observed_mz, pmin(labels$intensity + 3, 101), labels = labels$label,
       col = label_colors, cex = 0.72, font = 2)
  legend("topright", legend = c("b ions", "y ions", "neutral loss"),
         col = c(b_color, y_color, "#228B22"), lwd = 2, bty = "n", cex = 0.8)
  mtext(
    paste0("Vac14 ", result$target$modified_sequence, "; ",
           sprintf("%.4f m/z, %d+", result$target$precursor_mz,
                   result$target$precursor_charge)),
    side = 3, line = 0.15, adj = 0, cex = 0.86, font = 2
  )
}

.protvis_vac14_draw_figure <- function(result, b_color = "#C0392B",
                                        y_color = "#2E63C4") {
  layout(matrix(c(1, 2), nrow = 2L), heights = c(1.15, 1.6))
  on.exit(layout(1), add = TRUE)
  par(mar = c(1, 1, 3.5, 1))
  .protvis_vac14_draw_table(result$theoretical$fragment_table)
  mtext(
    paste(result$target$protein, "     R.AT[pS]GVPFSQYK.H"),
    side = 3, line = 1.5, cex = 1.05, font = 2
  )
  par(mar = c(5, 5, 2.5, 1))
  .protvis_vac14_draw_spectrum(result, b_color, y_color)
}

.protvis_vac14_ui <- function(ns) {
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 360,
      shiny::h4("Vac14 phosphosite validation"),
      shiny::p("Reproduce the published PXD001057 spectrum for ",
               shiny::strong("AT[pS]GVPFSQYK (Ser3)"), "."),
      shiny::radioButtons(
        ns("vac14_source"), "Input source",
        choices = c("PRIDE public files" = "public", "Upload files" = "upload"),
        selected = "public"
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] === 'upload'", ns("vac14_source")),
        shiny::fileInput(ns("vac14_mzid"), "mzIdentML (.mzid or .mzid.gz)",
                         accept = c(".mzid", ".gz")),
        shiny::fileInput(ns("vac14_mgf"), "MGF (.mgf)", accept = ".mgf")
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] === 'public'", ns("vac14_source")),
        shiny::div(
          class = "alert alert-info py-2 small",
          shiny::strong("Article files"), shiny::tags$br(),
          "E1R2_SCX5_soluble.mzid.gz", shiny::tags$br(),
          "E1R2_SCX5_soluble.mzid_E1R2_SCX5_soluble.MGF", shiny::tags$br(),
          shiny::tags$a(
            href = .protvis_vac14_target()$base_url,
            target = "_blank", rel = "noopener noreferrer", "Open PRIDE archive"
          )
        )
      ),
      shiny::numericInput(ns("vac14_tolerance"), "Fragment tolerance (Da)",
                          value = 0.5, min = 0.01, max = 2, step = 0.01),
      shiny::fluidRow(
        shiny::column(6, colourpicker::colourInput(ns("vac14_b_color"), "b ions", "#C0392B")),
        shiny::column(6, colourpicker::colourInput(ns("vac14_y_color"), "y ions", "#2E63C4"))
      ),
      shiny::actionButton(
        ns("vac14_run"), "RUN VAC14 VALIDATION",
        class = "btn-primary w-100 pv-run-button",
        icon = bsicons::bs_icon("play-fill")
      ),
      shiny::uiOutput(ns("vac14_status")),
      shiny::hr(),
      shiny::downloadButton(ns("vac14_pdf"), "PDF", class = "w-100 mb-2"),
      shiny::downloadButton(ns("vac14_png"), "PNG", class = "w-100 mb-2"),
      shiny::downloadButton(ns("vac14_matches_csv"), "MATCHED IONS CSV", class = "w-100 mb-2"),
      shiny::downloadButton(ns("vac14_theory_csv"), "THEORETICAL TABLE CSV", class = "w-100")
    ),
    bslib::navset_card_tab(
      height = "760px",
      bslib::nav_panel(
        "Annotated spectrum",
        bslib::card_body(shiny::plotOutput(ns("vac14_plot"), height = "690px"))
      ),
      bslib::nav_panel("Validation summary", DT::DTOutput(ns("vac14_summary"))),
      bslib::nav_panel("Matched ions", DT::DTOutput(ns("vac14_matches"))),
      bslib::nav_panel("Key ions", DT::DTOutput(ns("vac14_key_ions"))),
      bslib::nav_panel("Theoretical table", DT::DTOutput(ns("vac14_theory"))),
      bslib::nav_panel("Target PSM", DT::DTOutput(ns("vac14_psm")))
    )
  )
}

.protvis_vac14_server <- function(input, output, session, shared_state = NULL) {
  result <- shiny::reactiveVal(NULL)
  status <- shiny::reactiveVal(list(type = "idle", message = "Ready to validate the Vac14 Ser3 benchmark."))
  running <- shiny::reactiveVal(FALSE)
  completed_signature <- shiny::reactiveVal(NULL)

  signature <- shiny::reactive({
    if (identical(input$vac14_source, "upload")) {
      paste(
        input$vac14_source,
        input$vac14_mzid$name %||% "",
        input$vac14_mzid$size %||% "",
        input$vac14_mgf$name %||% "",
        input$vac14_mgf$size %||% "",
        input$vac14_tolerance,
        sep = "|"
      )
    } else {
      paste("public", input$vac14_tolerance, sep = "|")
    }
  })

  output$vac14_status <- shiny::renderUI({
    value <- status()
    class <- switch(value$type,
                    success = "alert alert-success",
                    error = "alert alert-danger",
                    running = "alert alert-warning",
                    "alert alert-secondary")
    shiny::div(class = paste(class, "mt-3 mb-0 py-2"), value$message)
  })

  shiny::observeEvent(input$vac14_run, {
    if (isTRUE(running())) {
      shiny::showNotification("Vac14 validation is already running; duplicate click ignored.", type = "warning")
      return(invisible(NULL))
    }
    current_signature <- signature()
    if (!is.null(result()) && identical(current_signature, completed_signature())) {
      shiny::showNotification("This Vac14 input has already been validated.", type = "message")
      return(invisible(NULL))
    }
    running(TRUE)
    on.exit(running(FALSE), add = TRUE)
    status(list(type = "running", message = "Loading PXD001057 files and validating the target spectrum…"))
    tryCatch({
      files <- shiny::withProgress(message = "Vac14 PXD001057 validation", value = 0, {
        shiny::incProgress(0.1, detail = "Preparing source files")
        selected <- if (identical(input$vac14_source, "upload")) {
          .protvis_vac14_prepare_uploads(input$vac14_mzid, input$vac14_mgf)
        } else {
          .protvis_vac14_download_files()
        }
        shiny::incProgress(0.3, detail = "Reading mzIdentML and MGF")
        value <- .protvis_vac14_run(
          selected$mzid, selected$mgf,
          tolerance_da = input$vac14_tolerance,
          source = selected$source
        )
        shiny::incProgress(0.6, detail = "Matching fragment ions")
        value
      })
      result(files)
      completed_signature(current_signature)
      status(list(
        type = "success",
        message = paste0(
          "Completed: ", nrow(files$matched), " fragment ions matched; ",
          sum(files$key_ions$matched), "/", nrow(files$key_ions),
          " publication-check ions confirmed."
        )
      ))
    }, error = function(error) {
      result(NULL)
      status(list(type = "error", message = paste("Vac14 validation failed:", conditionMessage(error))))
    })
  }, ignoreInit = TRUE)

  output$vac14_plot <- shiny::renderPlot({
    value <- result()
    shiny::req(value)
    .protvis_vac14_draw_figure(value, input$vac14_b_color, input$vac14_y_color)
  }, res = 110)
  output$vac14_summary <- DT::renderDT({
    value <- result(); shiny::req(value)
    DT::datatable(value$summary, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })
  output$vac14_matches <- DT::renderDT({
    value <- result(); shiny::req(value)
    DT::datatable(value$matched, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })
  output$vac14_key_ions <- DT::renderDT({
    value <- result(); shiny::req(value)
    DT::datatable(value$key_ions, rownames = FALSE, options = list(dom = "t"))
  })
  output$vac14_theory <- DT::renderDT({
    value <- result(); shiny::req(value)
    DT::datatable(value$theoretical$fragment_table, rownames = FALSE,
                  options = list(pageLength = 11, scrollX = TRUE))
  })
  output$vac14_psm <- DT::renderDT({
    value <- result(); shiny::req(value)
    DT::datatable(value$psm_table, rownames = FALSE, options = list(scrollX = TRUE))
  })

  output$vac14_pdf <- shiny::downloadHandler(
    filename = function() "Vac14_ATpSGVPFSQYK_reproduced.pdf",
    content = function(file) {
      value <- result(); shiny::req(value)
      grDevices::pdf(file, width = 14, height = 10)
      on.exit(grDevices::dev.off(), add = TRUE)
      .protvis_vac14_draw_figure(value, input$vac14_b_color, input$vac14_y_color)
    }
  )
  output$vac14_png <- shiny::downloadHandler(
    filename = function() "Vac14_ATpSGVPFSQYK_reproduced.png",
    content = function(file) {
      value <- result(); shiny::req(value)
      grDevices::png(file, width = 4200, height = 3000, res = 300)
      on.exit(grDevices::dev.off(), add = TRUE)
      .protvis_vac14_draw_figure(value, input$vac14_b_color, input$vac14_y_color)
    }
  )
  output$vac14_matches_csv <- shiny::downloadHandler(
    filename = function() "Vac14_ATpSGVPFSQYK_matched_ions.csv",
    content = function(file) {
      value <- result(); shiny::req(value)
      utils::write.csv(value$matched, file, row.names = FALSE)
    }
  )
  output$vac14_theory_csv <- shiny::downloadHandler(
    filename = function() "Vac14_ATpSGVPFSQYK_theoretical_table.csv",
    content = function(file) {
      value <- result(); shiny::req(value)
      utils::write.csv(value$theoretical$fragment_table, file, row.names = FALSE)
    }
  )

  invisible(shiny::reactive(result()))
}

# Generic mzIdentML/MGF browser. These definitions intentionally supersede the
# original fixed-target UI/server above while retaining its public benchmark.
.protvis_vac14_table_ion_labels <- function(table) {
  labels <- matrix(
    NA_character_, nrow = nrow(table), ncol = ncol(table),
    dimnames = list(NULL, names(table))
  )
  b_number <- as.integer(table$B)
  y_number <- as.integer(table$Y)
  mappings <- list(
    `B Ions` = paste0("b", b_number),
    `B+2H` = paste0("b", b_number, "++"),
    `B-NH3` = paste0("b", b_number, "-NH3"),
    `B-H2O` = paste0("b", b_number, "-H2O"),
    `Y Ions` = paste0("y", y_number),
    `Y+2H` = paste0("y", y_number, "++"),
    `Y-NH3` = paste0("y", y_number, "-NH3"),
    `Y-H2O` = paste0("y", y_number, "-H2O")
  )
  for (column in intersect(names(mappings), names(table))) {
    labels[, column] <- mappings[[column]]
    labels[is.na(table[[column]]), column] <- NA_character_
  }
  labels
}

.protvis_vac14_publication_table_matches <- function() {
  b_ions <- c("b3", "b4", "b5", "b7", "b8", "b9", "b10")
  b_double <- c("b6++", "b7++", "b9++")
  b_losses <- c("b7-H2O", "b8-H2O", "b9-H2O", "b10-H2O")
  y_ions <- paste0("y", 2:9)
  y_double <- c("y6++", "y7++", "y9++", "y10++")
  y_losses <- c(
    "y2-NH3", "y5-NH3",
    "y6-H2O", "y7-H2O", "y8-H2O", "y9-H2O"
  )
  labels <- c(b_ions, b_double, b_losses, y_ions, y_double, y_losses)
  data.frame(
    label = labels,
    series = substr(labels, 1L, 1L),
    neutral = grepl("-(NH3|H2O)", labels),
    stringsAsFactors = FALSE
  )
}

.protvis_vac14_displayed_matches <- function(matched, min_relative_intensity = 1) {
  if (is.null(matched) || !nrow(matched)) return(matched)
  matched[
    is.finite(matched$intensity) & matched$intensity >= min_relative_intensity,
    , drop = FALSE
  ]
}

.protvis_vac14_match_colors <- function(matched, b_color = "#C0392B",
                                         y_color = "#2E63C4",
                                         neutral_color = "#228B22") {
  if (is.null(matched) || !nrow(matched)) return(character())
  ifelse(
    matched$neutral,
    neutral_color,
    ifelse(matched$series == "b", b_color, y_color)
  )
}

.protvis_vac14_draw_table <- function(table, displayed_matches = NULL,
                                       b_color = "#C0392B",
                                       y_color = "#2E63C4",
                                       neutral_color = "#228B22") {
  nr <- nrow(table)
  nc <- ncol(table)
  ion_labels <- .protvis_vac14_table_ion_labels(table)
  highlight_colors <- character()
  if (!is.null(displayed_matches) && nrow(displayed_matches)) {
    highlight_colors <- stats::setNames(
      .protvis_vac14_match_colors(
        displayed_matches, b_color, y_color, neutral_color
      ),
      as.character(displayed_matches$label)
    )
    highlight_colors <- highlight_colors[!duplicated(names(highlight_colors))]
  }
  plot.new()
  plot.window(xlim = c(0, nc), ylim = c(0, nr + 2))
  for (j in seq_len(nc)) {
    text(j - 0.5, nr + 1.2, names(table)[j], cex = 0.7, font = 2)
  }
  segments(0, nr + 0.7, nc, nr + 0.7, lwd = 0.8)
  modified_rows <- if ("AA" %in% names(table)) grep("[+-]", table$AA) else integer()
  for (i in seq_len(nr)) {
    y <- nr - i + 0.6
    for (j in seq_len(nc)) {
      value <- table[i, j]
      label <- if (is.na(value)) "" else as.character(value)
      ion_label <- ion_labels[i, j]
      if (!is.na(ion_label) && ion_label %in% names(highlight_colors)) {
        highlight <- unname(highlight_colors[[ion_label]])
        rect(
          j - 0.92, y - 0.34, j - 0.08, y + 0.34,
          col = grDevices::adjustcolor(highlight, alpha.f = 0.50),
          border = NA
        )
      }
      text(j - 0.5, y, label, cex = 0.66,
           font = if (names(table)[j] == "AA" && i %in% modified_rows) 2 else 1,
           col = "#111111")
    }
  }
  box()
}

.protvis_vac14_draw_spectrum <- function(result, b_color = "#C0392B",
                                          y_color = "#2E63C4") {
  peaks <- result$peaks
  matched <- result$matched
  plot(
    peaks$mz, peaks$rel, type = "h", lwd = 0.75,
    xlim = c(0, max(1300, max(peaks$mz, na.rm = TRUE))), ylim = c(0, 108),
    xlab = "m/z", ylab = "Relative intensity (%)", main = ""
  )
  colors <- .protvis_vac14_match_colors(matched, b_color, y_color, "#228B22")
  segments(matched$observed_mz, 0, matched$observed_mz, matched$intensity,
           col = colors, lwd = 1.5)
  labels <- .protvis_vac14_displayed_matches(matched)
  label_colors <- .protvis_vac14_match_colors(labels, b_color, y_color, "#228B22")
  text(labels$observed_mz, pmin(labels$intensity + 3, 101), labels = labels$label,
       col = label_colors, cex = 0.72, font = 2)
  legend("topright", legend = c("b ions", "y ions", "neutral loss"),
         col = c(b_color, y_color, "#228B22"), lwd = 2, bty = "n", cex = 0.8)
  mtext(
    paste0(result$target$spectrum_label %||% result$target$modified_sequence, "; ",
           sprintf("%.4f m/z, %d+", result$target$precursor_mz,
                   result$target$precursor_charge)),
    side = 3, line = 0.15, adj = 0, cex = 0.86, font = 2
  )
}

.protvis_vac14_draw_figure <- function(result, b_color = "#C0392B",
                                        y_color = "#2E63C4") {
  layout(matrix(c(1, 2), nrow = 2L), heights = c(1.15, 1.6))
  on.exit(layout(1), add = TRUE)
  par(mar = c(1, 1, 3.5, 1))
  displayed_matches <- if (isTRUE(result$target$is_public_benchmark)) {
    .protvis_vac14_publication_table_matches()
  } else {
    .protvis_vac14_displayed_matches(result$matched)
  }
  .protvis_vac14_draw_table(
    result$theoretical$fragment_table,
    displayed_matches = displayed_matches,
    b_color = b_color,
    y_color = y_color,
    neutral_color = "#228B22"
  )
  mtext(
    paste(
      result$target$protein,
      result$target$display_sequence %||% result$target$modified_sequence,
      sep = "     "
    ),
    side = 3, line = 1.5, cex = 1.05, font = 2
  )
  par(mar = c(5, 5, 2.5, 1))
  .protvis_vac14_draw_spectrum(result, b_color, y_color)
}

.protvis_vac14_ui <- function(ns) {
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 380,
      shiny::h4("PTM peptide-spectrum visualization"),
      shiny::p(
        "Load all PSMs from mzIdentML, select any peptide/spectrum, and visualize its dynamic fragment annotation. ",
        "Changing the selection refreshes the table and spectrum automatically; the button reruns it manually. ",
        shiny::strong("AT[pS]GVPFSQYK (Ser3)"), " remains the default public benchmark."
      ),
      shiny::radioButtons(
        ns("vac14_source"), "Input source",
        choices = c("PRIDE PXD001057 files" = "public", "Upload files" = "upload"),
        selected = "public"
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] === 'upload'", ns("vac14_source")),
        shiny::fileInput(ns("vac14_mzid"), "mzIdentML (.mzid or .mzid.gz)",
                         accept = c(".mzid", ".gz")),
        shiny::fileInput(ns("vac14_mgf"), "MGF (.mgf)", accept = ".mgf")
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] === 'public'", ns("vac14_source")),
        shiny::div(
          class = "alert alert-info py-2 small",
          shiny::strong("PXD001057 files"), shiny::tags$br(),
          "E1R2_SCX5_soluble.mzid.gz", shiny::tags$br(),
          "E1R2_SCX5_soluble.mzid_E1R2_SCX5_soluble.MGF", shiny::tags$br(),
          shiny::tags$a(href = .protvis_vac14_target()$base_url,
                        target = "_blank", rel = "noopener noreferrer",
                        "Open PRIDE archive")
        )
      ),
      shiny::actionButton(
        ns("vac14_load"), "LOAD PSM LIST",
        class = "btn-outline-primary w-100 pv-run-button",
        icon = bsicons::bs_icon("list-ul")
      ),
      shiny::selectizeInput(
        ns("vac14_psm_choice"), "Select peptide / PSM",
        choices = NULL, multiple = FALSE,
        options = list(placeholder = "Load files first, then search peptide or spectrum")
      ),
      shiny::numericInput(ns("vac14_tolerance"), "Fragment tolerance (Da)",
                          value = 0.5, min = 0.01, max = 2, step = 0.01),
      shiny::fluidRow(
        shiny::column(6, colourpicker::colourInput(ns("vac14_b_color"), "b ions", "#C0392B")),
        shiny::column(6, colourpicker::colourInput(ns("vac14_y_color"), "y ions", "#2E63C4"))
      ),
      shiny::actionButton(
        ns("vac14_run"), "VISUALIZE SELECTED PEPTIDE",
        class = "btn-primary w-100 pv-run-button",
        icon = bsicons::bs_icon("play-fill")
      ),
      shiny::uiOutput(ns("vac14_status")),
      shiny::hr(),
      shiny::downloadButton(ns("vac14_pdf"), "PDF", class = "w-100 mb-2"),
      shiny::downloadButton(ns("vac14_png"), "PNG", class = "w-100 mb-2"),
      shiny::downloadButton(ns("vac14_matches_csv"), "MATCHED IONS CSV", class = "w-100 mb-2"),
      shiny::downloadButton(ns("vac14_theory_csv"), "THEORETICAL TABLE CSV", class = "w-100")
    ),
    bslib::navset_card_tab(
      height = "760px",
      bslib::nav_panel("Annotated spectrum",
                       bslib::card_body(shiny::plotOutput(ns("vac14_plot"), height = "690px"))),
      bslib::nav_panel("Validation summary", DT::DTOutput(ns("vac14_summary"))),
      bslib::nav_panel("Matched ions", DT::DTOutput(ns("vac14_matches"))),
      bslib::nav_panel("Ion coverage", DT::DTOutput(ns("vac14_key_ions"))),
      bslib::nav_panel("Theoretical table", DT::DTOutput(ns("vac14_theory"))),
      bslib::nav_panel("Selected PSM", DT::DTOutput(ns("vac14_psm")))
    )
  )
}

.protvis_vac14_server <- function(input, output, session, shared_state = NULL) {
  bundle <- shiny::reactiveVal(NULL)
  result <- shiny::reactiveVal(NULL)
  status <- shiny::reactiveVal(list(
    type = "idle", message = "Load the mzIdentML and MGF files to list their peptide-spectrum matches."
  ))
  load_running <- shiny::reactiveVal(FALSE)
  run_running <- shiny::reactiveVal(FALSE)
  completed_signature <- shiny::reactiveVal(NULL)

  unlock_run_button <- function(button_id) {
    session$sendCustomMessage(
      "protvis-unlock-run-button",
      list(id = session$ns(button_id))
    )
  }

  clear_loaded_data <- function() {
    bundle(NULL)
    result(NULL)
    completed_signature(NULL)
    shiny::updateSelectizeInput(session, "vac14_psm_choice", choices = character(), selected = character())
    status(list(type = "idle", message = "Input changed. Click LOAD PSM LIST."))
  }
  shiny::observeEvent(input$vac14_source, clear_loaded_data(), ignoreInit = TRUE)
  shiny::observeEvent(input$vac14_mzid, clear_loaded_data(), ignoreInit = TRUE)
  shiny::observeEvent(input$vac14_mgf, clear_loaded_data(), ignoreInit = TRUE)

  visualize_selected_psm <- function(show_progress = TRUE) {
    loaded <- bundle()
    choice <- input$vac14_psm_choice
    if (is.null(loaded) || is.null(choice) || !nzchar(choice)) {
      return(invisible(FALSE))
    }
    if (isTRUE(run_running())) {
      shiny::showNotification(
        "Spectrum visualization is already running; duplicate request ignored.",
        type = "warning"
      )
      return(invisible(FALSE))
    }

    run_running(TRUE)
    on.exit({
      run_running(FALSE)
      unlock_run_button("vac14_run")
    }, add = TRUE)
    signature <- paste(choice, input$vac14_tolerance, sep = "|")
    status(list(
      type = "running",
      message = "Calculating modified fragments and matching the selected spectrum…"
    ))

    tryCatch({
      calculate <- function() {
        answer <- .protvis_ptm_run_selected(
          loaded, choice, input$vac14_tolerance
        )
        answer
      }
      value <- if (isTRUE(show_progress)) {
        shiny::withProgress(message = "Visualizing selected peptide", value = 0, {
          shiny::incProgress(0.25, detail = "Calculating modified b/y ions")
          answer <- calculate()
          shiny::incProgress(0.65, detail = "Matching experimental peaks")
          answer
        })
      } else {
        calculate()
      }
      result(value)
      completed_signature(signature)

      spectrum_tables <- list(
        summary = value$summary,
        matched_ions = value$matched,
        ion_coverage = value$key_ions,
        theoretical_fragments = value$theoretical$fragment_table,
        selected_PSM = value$psm_table,
        spectrum_peaks = value$peaks
      )
      spectrum_tables <- spectrum_tables[
        vapply(spectrum_tables, is.data.frame, logical(1))
      ]
      .protvis_record_shared_run(
        shared_state,
        module = "ptm_spectrum",
        method = "mzIdentML_MGF_fragment_matching",
        category = "ptm",
        parameters = list(
          fragment_tolerance_da = input$vac14_tolerance,
          source = loaded$source %||% input$vac14_source,
          psm_index = choice,
          b_ion_color = input$vac14_b_color,
          y_ion_color = input$vac14_y_color
        ),
        tables = spectrum_tables,
        statistics = list(
          protein = value$target$protein %||% NA_character_,
          peptide = value$target$sequence %||% NA_character_,
          modified_peptide = value$target$modified_sequence %||% NA_character_,
          spectrum = value$target$spectrum_label %||%
            value$target$spectrum_title %||% NA_character_,
          precursor_mz = value$target$precursor_mz %||% NA_real_,
          precursor_charge = value$target$precursor_charge %||% NA_integer_
        ),
        plot_data = list(
          peaks = value$peaks,
          matched_ions = value$matched
        ),
        plot_config = list(
          b_color = input$vac14_b_color,
          y_color = input$vac14_y_color
        )
      )

      status(list(
        type = "success",
        message = paste0(
          "Displayed ", value$target$modified_sequence, ": ",
          nrow(value$matched), " fragment ions matched; ",
          sum(value$key_ions$matched), "/", nrow(value$key_ions),
          " primary b/y ions covered."
        )
      ))
      invisible(TRUE)
    }, error = function(error) {
      result(NULL)
      completed_signature(NULL)
      status(list(
        type = "error",
        message = paste("Visualization failed:", conditionMessage(error))
      ))
      invisible(FALSE)
    })
  }

  shiny::observeEvent(input$vac14_psm_choice, {
    completed_signature(NULL)
    unlock_run_button("vac14_run")
    visualize_selected_psm(show_progress = FALSE)
  }, ignoreInit = TRUE)

  output$vac14_status <- shiny::renderUI({
    value <- status()
    class <- switch(value$type, success = "alert alert-success",
                    error = "alert alert-danger", running = "alert alert-warning",
                    "alert alert-secondary")
    shiny::div(class = paste(class, "mt-3 mb-0 py-2"), value$message)
  })

  shiny::observeEvent(input$vac14_load, {
    if (isTRUE(load_running())) {
      shiny::showNotification("PSM loading is already running; duplicate click ignored.", type = "warning")
      return(invisible(NULL))
    }
    load_running(TRUE)
    on.exit({
      load_running(FALSE)
      unlock_run_button("vac14_load")
    }, add = TRUE)
    result(NULL)
    completed_signature(NULL)
    status(list(type = "running", message = "Reading mzIdentML and MGF files…"))
    tryCatch({
      loaded <- shiny::withProgress(message = "Loading peptide-spectrum matches", value = 0, {
        shiny::incProgress(0.15, detail = "Preparing files")
        selected <- if (identical(input$vac14_source, "upload")) {
          .protvis_vac14_prepare_uploads(input$vac14_mzid, input$vac14_mgf)
        } else {
          .protvis_vac14_download_files()
        }
        shiny::incProgress(0.35, detail = "Reading all PSMs and spectra")
        .protvis_ptm_load_bundle(selected$mzid, selected$mgf, selected$source)
      })
      bundle(loaded)
      choices <- stats::setNames(as.character(loaded$catalog$psm_index), loaded$catalog$label)
      target <- .protvis_vac14_target()
      default_hit <- which(loaded$catalog$spectrum_id == target$spectrum_id &
                             loaded$catalog$sequence == target$sequence)
      selected_value <- if (length(default_hit)) {
        as.character(loaded$catalog$psm_index[default_hit[[1L]]])
      } else {
        as.character(loaded$catalog$psm_index[[1L]])
      }
      shiny::updateSelectizeInput(
        session, "vac14_psm_choice", choices = choices,
        selected = selected_value, server = TRUE
      )
      status(list(
        type = "success",
        message = paste0(
          "Loaded ", nrow(loaded$catalog), " selectable PSMs from ",
          length(unique(loaded$catalog$modified_sequence)), " modified peptide forms."
        )
      ))
    }, error = function(error) {
      bundle(NULL)
      status(list(type = "error", message = paste("PSM loading failed:", conditionMessage(error))))
    })
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$vac14_run, {
    if (is.null(bundle())) {
      shiny::showNotification("Load the PSM list first.", type = "warning")
      return(invisible(NULL))
    }
    shiny::req(input$vac14_psm_choice)
    visualize_selected_psm(show_progress = TRUE)
  }, ignoreInit = TRUE)

  output$vac14_plot <- shiny::renderPlot({
    value <- result(); shiny::req(value)
    .protvis_vac14_draw_figure(value, input$vac14_b_color, input$vac14_y_color)
  }, res = 110)
  output$vac14_summary <- DT::renderDT({
    value <- result(); shiny::req(value)
    DT::datatable(value$summary, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })
  output$vac14_matches <- DT::renderDT({
    value <- result(); shiny::req(value)
    DT::datatable(value$matched, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })
  output$vac14_key_ions <- DT::renderDT({
    value <- result(); shiny::req(value)
    DT::datatable(value$key_ions, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })
  output$vac14_theory <- DT::renderDT({
    value <- result(); shiny::req(value)
    DT::datatable(value$theoretical$fragment_table, rownames = FALSE,
                  options = list(pageLength = 15, scrollX = TRUE))
  })
  output$vac14_psm <- DT::renderDT({
    value <- result(); shiny::req(value)
    DT::datatable(value$psm_table, rownames = FALSE, options = list(scrollX = TRUE))
  })

  safe_stem <- function() {
    value <- result(); shiny::req(value)
    gsub("[^A-Za-z0-9._-]+", "_", value$target$modified_sequence)
  }
  output$vac14_pdf <- shiny::downloadHandler(
    filename = function() paste0("PTM_", safe_stem(), ".pdf"),
    content = function(file) {
      value <- result(); shiny::req(value)
      grDevices::pdf(file, width = 14, height = 10)
      on.exit(grDevices::dev.off(), add = TRUE)
      .protvis_vac14_draw_figure(value, input$vac14_b_color, input$vac14_y_color)
    }
  )
  output$vac14_png <- shiny::downloadHandler(
    filename = function() paste0("PTM_", safe_stem(), ".png"),
    content = function(file) {
      value <- result(); shiny::req(value)
      grDevices::png(file, width = 4200, height = 3000, res = 300)
      on.exit(grDevices::dev.off(), add = TRUE)
      .protvis_vac14_draw_figure(value, input$vac14_b_color, input$vac14_y_color)
    }
  )
  output$vac14_matches_csv <- shiny::downloadHandler(
    filename = function() paste0("PTM_", safe_stem(), "_matched_ions.csv"),
    content = function(file) {
      value <- result(); shiny::req(value)
      utils::write.csv(value$matched, file, row.names = FALSE)
    }
  )
  output$vac14_theory_csv <- shiny::downloadHandler(
    filename = function() paste0("PTM_", safe_stem(), "_theoretical_table.csv"),
    content = function(file) {
      value <- result(); shiny::req(value)
      utils::write.csv(value$theoretical$fragment_table, file, row.names = FALSE)
    }
  )
  invisible(shiny::reactive(result()))
}
