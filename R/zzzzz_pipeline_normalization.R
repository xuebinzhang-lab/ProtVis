# Keep run_protvis_pipeline() consistent with the Shiny normalization module.
# Existing mean/z-score options remain available while the proteomics-specific
# methods added by zzzz_normalization_methods.R are accepted programmatically.

.protvis_normalization <- function(dataset, params) {
  method <- .protvis_resolve_preprocessing_method(
    dataset, "normalization", params$method %||% "auto"
  )
  if (method %in% c("none", "identity")) return(dataset)

  matrix <- base::as.matrix(dataset$expression_data)
  storage.mode(matrix) <- "numeric"

  if (method %in% c("maxquant_recommended", "maxquant_default")) {
    normalized <- .protvis_maxquant_normalize_matrix(
      matrix,
      row_shift = params$row_shift %||% 5,
      zero_value = params$zero_value %||% 1
    )
    return(.protvis_replace_expression(dataset, normalized))
  }

  aliases <- c(
    median = "median",
    median_subtraction = "median",
    quantile = "quantile",
    quantile_normalization = "quantile",
    vsn = "vsn",
    cyclic_loess = "cyclic_loess",
    cyclicloess = "cyclic_loess",
    loess = "cyclic_loess",
    rlr = "rlr"
  )
  normalized_method <- aliases[[method]]
  if (!base::is.null(normalized_method)) {
    input_scale <- params$input_scale %||% "auto"
    input_scale <- .protvis_norm_resolve_scale(
      input_scale,
      .protvis_norm_transformation_method(dataset)
    )
    normalized <- .protvis_apply_normalization(
      matrix,
      method = normalized_method,
      scale = input_scale
    )
    return(.protvis_replace_expression(dataset, normalized))
  }

  if (method == "mean") {
    for (j in base::seq_len(base::ncol(matrix))) {
      observed <- matrix[, j]
      center <- base::mean(observed, na.rm = TRUE)
      if (!base::is.finite(center)) center <- 0
      matrix[, j] <- observed - center
    }
  } else if (method %in% c("zscore", "z_score", "standardize")) {
    for (j in base::seq_len(base::ncol(matrix))) {
      observed <- matrix[, j]
      center <- base::mean(observed, na.rm = TRUE)
      spread <- stats::sd(observed, na.rm = TRUE)
      if (!base::is.finite(center)) center <- 0
      if (!base::is.finite(spread) || spread == 0) spread <- 1
      matrix[, j] <- (observed - center) / spread
    }
  } else {
    base::stop("Unknown normalization method: ", method, call. = FALSE)
  }
  .protvis_replace_expression(dataset, matrix)
}
