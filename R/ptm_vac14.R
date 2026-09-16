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
  packages <- c("Spectra", "MsBackendMgf", "PSMatch")
  missing <- packages[!vapply(packages, requireNamespace, logical(1L), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "Vac14 validation requires Bioconductor packages: ",
      paste(missing, collapse = ", "),
      ". Install them with BiocManager::install(c(\"Spectra\", ",
      "\"MsBackendMgf\", \"PSMatch\")).",
      call. = FALSE
    )
  }
  invisible(TRUE)
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

.protvis_vac14_run <- function(mzid_file, mgf_file, tolerance_da = 0.5,
                               source = "Input files") {
  .protvis_vac14_require_packages()
  target <- .protvis_vac14_target()
  theoretical <- .protvis_vac14_theoretical()

  psm <- PSMatch::PSM(mzid_file)
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

.protvis_vac14_server <- function(input, output, session) {
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
