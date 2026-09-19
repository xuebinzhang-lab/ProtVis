# proteiNorm-compatible normalization and diagnostics -----------------------
#
# Adds the normalization family evaluated by Graw et al. (ACS Omega 2020,
# doi:10.1021/acsomega.0c02564) to the existing ProtVis normalization
# workspace. The implementation follows the published/open-source proteiNorm
# workflow semantics while retaining the existing ProtVis methods unchanged.
#
# proteiNorm expects non-log intensities as input. ProtVis can therefore use
# raw data directly, or reconstruct an intensity scale when the upstream
# Transformation step is log2/log10. If the upstream scale cannot be safely
# reconstructed, proteiNorm-specific methods report themselves as unavailable
# instead of silently applying a method to an incompatible scale.

.protvis_norm_methods <- c(
  "Median" = "median",
  "Quantile" = "quantile",
  "VSN" = "vsn",
  "Cyclic Loess" = "cyclic_loess",
  "RLR" = "rlr",
  "proteiNorm · Log2 baseline" = "proteinorm_log2",
  "proteiNorm · Median" = "proteinorm_median",
  "proteiNorm · Mean" = "proteinorm_mean",
  "proteiNorm · VSN" = "proteinorm_vsn",
  "proteiNorm · Quantile" = "proteinorm_quantile",
  "proteiNorm · Cyclic Loess" = "proteinorm_cyclic_loess",
  "proteiNorm · RLR" = "proteinorm_rlr",
  "proteiNorm · Global Intensity" = "proteinorm_global_intensity"
)

.protvis_apply_normalization_before_proteinorm <- .protvis_apply_normalization
.protvis_norm_metrics_before_proteinorm <- .protvis_norm_metrics
.protvis_normalization_before_proteinorm <- .protvis_normalization

.protvis_proteinorm_raw <- function(mat, scale = "raw") {
  mat <- .protvis_norm_matrix(mat)
  scale <- base::tolower(base::as.character(scale %||% "raw"))
  raw <- switch(
    scale,
    raw = mat,
    log2 = 2 ^ mat,
    log10 = 10 ^ mat,
    base::stop(
      "proteiNorm methods require raw intensity data or a reversible log2/log10 scale. ",
      "Set Input scale explicitly or rerun normalization from a compatible transformation.",
      call. = FALSE
    )
  )
  finite <- raw[base::is.finite(raw)]
  if (!base::length(finite)) {
    base::stop("No finite intensities are available for proteiNorm normalization.", call. = FALSE)
  }
  if (base::any(finite < 0)) {
    base::stop("proteiNorm normalization requires non-negative intensities.", call. = FALSE)
  }
  raw[!base::is.finite(raw)] <- NA_real_
  raw
}

.protvis_proteinorm_log2 <- function(raw) {
  out <- suppressWarnings(base::log2(raw))
  out[!base::is.finite(out)] <- NA_real_
  base::dimnames(out) <- base::dimnames(raw)
  out
}

.protvis_proteinorm_center_ratio <- function(log_mat, statistic = c("median", "mean")) {
  statistic <- base::match.arg(statistic)
  centers <- if (identical(statistic, "median")) {
    base::apply(log_mat, 2, stats::median, na.rm = TRUE)
  } else {
    base::colMeans(log_mat, na.rm = TRUE)
  }
  if (base::any(!base::is.finite(centers)) || base::any(base::abs(centers) < 1e-12)) {
    base::stop("proteiNorm ", statistic, " normalization encountered an invalid sample center.", call. = FALSE)
  }
  target <- base::mean(centers, na.rm = TRUE)
  out <- base::sweep(log_mat, 2, centers, FUN = "/")
  out <- out * target
  base::dimnames(out) <- base::dimnames(log_mat)
  out
}

.protvis_apply_proteinorm <- function(mat, method, scale = "raw") {
  raw <- .protvis_proteinorm_raw(mat, scale)
  log_mat <- .protvis_proteinorm_log2(raw)

  out <- switch(
    method,
    proteinorm_log2 = log_mat,
    proteinorm_median = .protvis_proteinorm_center_ratio(log_mat, "median"),
    proteinorm_mean = .protvis_proteinorm_center_ratio(log_mat, "mean"),
    proteinorm_vsn = {
      if (!base::requireNamespace("vsn", quietly = TRUE)) {
        base::stop("Package 'vsn' is required for proteiNorm VSN.", call. = FALSE)
      }
      value <- vsn::justvsn(raw, verbose = FALSE)
      base::dimnames(value) <- base::dimnames(raw)
      value
    },
    proteinorm_quantile = {
      value <- preprocessCore::normalize.quantiles(log_mat, copy = TRUE)
      base::dimnames(value) <- base::dimnames(log_mat)
      value
    },
    proteinorm_cyclic_loess = {
      value <- limma::normalizeCyclicLoess(log_mat, method = "fast")
      base::dimnames(value) <- base::dimnames(log_mat)
      value
    },
    proteinorm_rlr = {
      if (!base::requireNamespace("NormalyzerDE", quietly = TRUE)) {
        base::stop("Package 'NormalyzerDE' is required for proteiNorm RLR.", call. = FALSE)
      }
      value <- NormalyzerDE::performGlobalRLRNormalization(
        log_mat, noLogTransform = TRUE
      )
      base::dimnames(value) <- base::dimnames(log_mat)
      value
    },
    proteinorm_global_intensity = {
      if (!base::requireNamespace("NormalyzerDE", quietly = TRUE)) {
        base::stop("Package 'NormalyzerDE' is required for proteiNorm Global Intensity normalization.", call. = FALSE)
      }
      value <- NormalyzerDE::globalIntensityNormalization(
        log_mat, noLogTransform = TRUE
      )
      base::dimnames(value) <- base::dimnames(log_mat)
      value
    },
    base::stop("Unknown proteiNorm normalization method: ", method, call. = FALSE)
  )

  out <- base::as.matrix(out)
  storage.mode(out) <- "numeric"
  base::dimnames(out) <- base::dimnames(mat)
  out
}

.protvis_apply_normalization <- function(mat, method = "median", scale = "raw") {
  method <- base::tolower(base::as.character(method %||% "median"))
  if (base::startsWith(method, "proteinorm_")) {
    return(.protvis_apply_proteinorm(mat, method = method, scale = scale))
  }
  .protvis_apply_normalization_before_proteinorm(mat, method = method, scale = scale)
}

.protvis_proteinorm_group_metrics <- function(mat, sample_info = NULL) {
  mat <- .protvis_norm_matrix(mat)
  groups <- .protvis_norm_group_vector(sample_info, base::colnames(mat))
  if (base::is.null(groups)) {
    return(base::list(
      PCV = NA_real_, PMAD = NA_real_, PEV = NA_real_,
      intragroup_correlation = NA_real_, log2_ratio_IQR = NA_real_
    ))
  }

  group_levels <- base::unique(groups[!base::is.na(groups) & base::nzchar(groups)])
  pcv <- pmad <- pev <- cor_values <- base::numeric()

  for (group in group_levels) {
    idx <- base::which(groups == group)
    if (base::length(idx) < 2L) next
    sub <- mat[, idx, drop = FALSE]

    means <- base::rowMeans(sub, na.rm = TRUE)
    sds <- base::apply(sub, 1, stats::sd, na.rm = TRUE)
    cv <- sds / base::abs(means)
    cv <- cv[base::is.finite(cv)]
    if (base::length(cv)) pcv <- c(pcv, base::mean(cv, na.rm = TRUE))

    mad_values <- matrixStats::rowMads(sub, na.rm = TRUE)
    mad_values <- mad_values[base::is.finite(mad_values)]
    if (base::length(mad_values)) pmad <- c(pmad, base::mean(mad_values, na.rm = TRUE))

    n_obs <- base::rowSums(base::is.finite(sub))
    vars <- base::apply(sub, 1, stats::var, na.rm = TRUE)
    valid <- n_obs > 1L & base::is.finite(vars)
    if (base::any(valid)) {
      denominator <- base::sum(n_obs[valid] - 1L)
      if (denominator > 0) {
        pev <- c(pev, base::sum((n_obs[valid] - 1L) * vars[valid]) / denominator)
      }
    }

    cors <- suppressWarnings(stats::cor(sub, use = "pairwise.complete.obs", method = "pearson"))
    if (base::is.matrix(cors) && base::ncol(cors) > 1L) {
      values <- cors[base::lower.tri(cors)]
      values <- values[base::is.finite(values)]
      if (base::length(values)) cor_values <- c(cor_values, values)
    }
  }

  ratio_values <- base::numeric()
  if (base::length(group_levels) >= 2L) {
    pairs <- utils::combn(group_levels, 2, simplify = FALSE)
    for (pair in pairs) {
      a <- base::rowMeans(mat[, groups == pair[[1L]], drop = FALSE], na.rm = TRUE)
      b <- base::rowMeans(mat[, groups == pair[[2L]], drop = FALSE], na.rm = TRUE)
      value <- a - b
      ratio_values <- c(ratio_values, value[base::is.finite(value)])
    }
  }

  base::list(
    PCV = if (base::length(pcv)) base::mean(pcv, na.rm = TRUE) else NA_real_,
    PMAD = if (base::length(pmad)) base::mean(pmad, na.rm = TRUE) else NA_real_,
    PEV = if (base::length(pev)) base::mean(pev, na.rm = TRUE) else NA_real_,
    intragroup_correlation = if (base::length(cor_values)) stats::median(cor_values, na.rm = TRUE) else NA_real_,
    log2_ratio_IQR = if (base::length(ratio_values)) stats::IQR(ratio_values, na.rm = TRUE) else NA_real_
  )
}

.protvis_norm_metrics <- function(mat, sample_info = NULL, method = "") {
  base_metrics <- .protvis_norm_metrics_before_proteinorm(
    mat, sample_info = sample_info, method = method
  )
  mat <- .protvis_norm_matrix(mat)
  extra <- .protvis_proteinorm_group_metrics(mat, sample_info)
  totals <- base::colSums(mat, na.rm = TRUE)
  total_cv <- if (base::length(totals) > 1L &&
                  base::is.finite(base::mean(totals)) &&
                  base::abs(base::mean(totals)) > 1e-12) {
    stats::sd(totals) / base::abs(base::mean(totals))
  } else {
    NA_real_
  }

  base_metrics$proteiNorm_PCV <- extra$PCV
  base_metrics$proteiNorm_PMAD <- extra$PMAD
  base_metrics$proteiNorm_PEV <- extra$PEV
  base_metrics$proteiNorm_intragroup_correlation <- extra$intragroup_correlation
  base_metrics$proteiNorm_total_intensity_CV <- total_cv
  base_metrics$proteiNorm_log2_ratio_IQR <- extra$log2_ratio_IQR
  base_metrics
}

# Make the programmatic pipeline accept the proteiNorm family as well.
.protvis_normalization <- function(dataset, params) {
  method <- base::tolower(base::as.character(params$method %||% "median"))
  if (!base::startsWith(method, "proteinorm_")) {
    return(.protvis_normalization_before_proteinorm(dataset, params))
  }
  input_scale <- base::tolower(base::as.character(params$input_scale %||% "raw"))
  matrix <- base::as.matrix(dataset$expression_data)
  storage.mode(matrix) <- "numeric"
  normalized <- .protvis_apply_normalization(
    matrix, method = method, scale = input_scale
  )
  .protvis_replace_expression(dataset, normalized)
}
