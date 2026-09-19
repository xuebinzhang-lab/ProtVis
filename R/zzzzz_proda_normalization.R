# proDA is designed to model missing values without imputation. Normalize the
# pre-imputation log-scale matrix with proDA's missing-value-aware median
# normalization immediately before fitting.
.protvis_dep_run_proda <- function(mat, group1_samples, group2_samples, group1, group2) {
  if (!base::requireNamespace("proDA", quietly = TRUE)) {
    base::stop("Package 'proDA' is not installed.", call. = FALSE)
  }
  samples <- c(group1_samples, group2_samples)
  mat <- mat[, samples, drop = FALSE]
  keep <- base::rowSums(base::is.finite(mat)) >= 2L
  mat <- mat[keep, , drop = FALSE]
  if (base::nrow(mat) < 10L) {
    base::stop("Too few proteins are available for proDA.", call. = FALSE)
  }
  mat <- proDA::median_normalization(mat)
  groups <- base::factor(
    ifelse(samples %in% group1_samples, group1, group2),
    levels = c(group1, group2)
  )
  fit <- proDA::proDA(mat, design = groups, data_is_log_transformed = TRUE)
  rn <- proDA::result_names(fit)
  if (base::length(rn) < 2L) {
    base::stop("proDA could not resolve the two comparison groups.", call. = FALSE)
  }
  contrast <- base::paste0("`", rn[[1L]], "` - `", rn[[2L]], "`")
  result <- proDA::test_diff(fit, contrast = contrast)
  result <- base::as.data.frame(result, stringsAsFactors = FALSE, check.names = FALSE)
  .protvis_dep_standardize(
    result, "proDA", result$name, result$diff, result$pval, result$adj_pval, result
  )
}
