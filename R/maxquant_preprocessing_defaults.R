# Source-aware preprocessing defaults used by the Shiny modules and pipeline.
#
# The MaxQuant preset reproduces the archived maize/teosinte workflow:
# replicate correction -> log2(x * 1e7) -> impute::impute.knn(seed 12345) ->
# sample-wise median subtraction -> row-wise positive shift.

.protvis_is_maxquant_source <- function(source) {
  source <- base::tolower(base::trimws(base::as.character(source %||% "")))
  base::identical(source, "maxquant")
}

.protvis_is_maxquant_dataset <- function(dataset) {
  inherits(dataset, "ProtVis_dataset") &&
    .protvis_is_maxquant_source(dataset$metadata$source %||% "")
}

#' Return source-specific preprocessing defaults
#'
#' @param source Proteomics data source.
#' @return A named list of stage parameters.
#' @export
protvis_preprocessing_defaults <- function(source = "MaxQuant") {
  if (.protvis_is_maxquant_source(source)) {
    return(list(
      filtering = list(
        method = "maxquant_flags",
        flags = c("site", "reverse", "contaminant")
      ),
      noise_correction = list(
        method = "replicate_correction",
        minimum_observed_replicates = 2L
      ),
      transformation = list(
        method = "maxquant_log2",
        multiplier = 1e7
      ),
      imputation = list(
        method = "knn",
        engine = "impute",
        seed = 12345L
      ),
      normalization = list(
        method = "maxquant_recommended",
        center = "sample_median",
        row_shift = 5,
        zero_value = 1
      )
    ))
  }

  list(
    noise_correction = list(method = "missingness_filter", max_missing = 0.5),
    transformation = list(method = "log2", pseudocount = 1),
    imputation = list(method = "median"),
    normalization = list(method = "median")
  )
}

.protvis_resolve_preprocessing_method <- function(dataset, stage,
                                                   method = "auto") {
  requested <- base::tolower(base::as.character(method %||% "auto"))
  if (!requested %in% c("", "auto", "default", "recommended")) {
    return(requested)
  }
  source <- if (inherits(dataset, "ProtVis_dataset")) {
    dataset$metadata$source %||% ""
  } else {
    ""
  }
  defaults <- protvis_preprocessing_defaults(source)
  stage_defaults <- defaults[[stage]]
  if (base::is.list(stage_defaults) &&
      base::nzchar(base::as.character(stage_defaults$method %||% ""))) {
    return(base::tolower(base::as.character(stage_defaults$method)))
  }
  switch(
    stage,
    noise_correction = "missingness_filter",
    transformation = "log2",
    imputation = "median",
    normalization = "median",
    requested
  )
}

.protvis_maxquant_log2_matrix <- function(matrix, multiplier = 1e7) {
  matrix <- base::as.matrix(matrix)
  storage.mode(matrix) <- "numeric"
  multiplier <- base::as.numeric(multiplier %||% 1e7)
  if (base::length(multiplier) != 1L || !base::is.finite(multiplier) ||
      multiplier <= 0) {
    base::stop("MaxQuant log2 multiplier must be a positive finite number.",
               call. = FALSE)
  }
  finite <- matrix[base::is.finite(matrix)]
  if (base::length(finite) && base::any(finite <= 0)) {
    base::stop(
      "MaxQuant recommended log2 transformation requires positive observed intensities.",
      call. = FALSE
    )
  }
  base::log2(matrix * multiplier)
}

.protvis_knn_impute_exact <- function(matrix, seed = 12345L) {
  if (!requireNamespace("impute", quietly = TRUE)) {
    base::stop(
      "Package 'impute' is required for the MaxQuant recommended kNN method.",
      call. = FALSE
    )
  }
  matrix <- base::as.matrix(matrix)
  storage.mode(matrix) <- "numeric"
  seed <- base::as.integer(seed %||% 12345L)
  if (base::length(seed) != 1L || base::is.na(seed)) seed <- 12345L
  base::set.seed(seed)
  result <- impute::impute.knn(matrix)$data
  storage.mode(result) <- "numeric"
  result
}

.protvis_maxquant_normalize_matrix <- function(matrix, row_shift = 5,
                                               zero_value = 1) {
  matrix <- base::as.matrix(matrix)
  storage.mode(matrix) <- "numeric"
  if (base::any(!base::is.finite(matrix))) {
    base::stop(
      "MaxQuant recommended normalization requires a fully imputed matrix.",
      call. = FALSE
    )
  }

  centered <- base::apply(
    matrix, 2L,
    function(x) x - stats::median(x, na.rm = TRUE)
  )
  if (base::is.null(base::dim(centered))) {
    centered <- base::matrix(centered, ncol = 1L)
  }
  base::rownames(centered) <- base::rownames(matrix)
  base::colnames(centered) <- base::colnames(matrix)

  row_shift <- base::as.numeric(row_shift %||% 5)
  zero_value <- base::as.numeric(zero_value %||% 1)
  shifted <- centered
  for (i in base::seq_len(base::nrow(centered))) {
    values <- centered[i, ]
    minimum <- base::min(values)
    offset <- base::abs(minimum) + row_shift
    shifted[i, ] <- base::ifelse(
      values == 0,
      zero_value,
      values + offset
    )
  }
  shifted
}
