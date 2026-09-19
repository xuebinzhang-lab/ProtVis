test_that("extended normalization methods preserve matrix shape", {
  set.seed(1)
  mat <- matrix(rnorm(120, mean = 15, sd = 2), nrow = 20)
  rownames(mat) <- paste0("P", seq_len(nrow(mat)))
  colnames(mat) <- paste0("S", seq_len(ncol(mat)))

  median_norm <- ProtVis:::.protvis_apply_normalization(mat, "median", "log2")
  expect_equal(dim(median_norm), dim(mat))
  expect_true(all(abs(apply(median_norm, 2, median)) < 1e-8))

  quantile <- ProtVis:::.protvis_apply_normalization(mat, "quantile", "log2")
  expect_equal(dim(quantile), dim(mat))
  sorted <- apply(quantile, 2, sort)
  expect_lt(max(abs(sorted[, 1] - sorted[, ncol(sorted)])), 1e-8)

  cyclic <- ProtVis:::.protvis_apply_normalization(mat, "cyclic_loess", "log2")
  expect_equal(dim(cyclic), dim(mat))

  rlr <- ProtVis:::.protvis_apply_normalization(mat, "rlr", "log2")
  expect_equal(dim(rlr), dim(mat))
  expect_true(any(is.finite(rlr)))
})

test_that("VSN runs on raw or back-transformed log data", {
  skip_if_not_installed("vsn")
  set.seed(2)
  raw <- matrix(rexp(100, rate = 0.01) + 10, nrow = 20)
  rownames(raw) <- paste0("P", seq_len(nrow(raw)))
  colnames(raw) <- paste0("S", seq_len(ncol(raw)))

  vsn_raw <- ProtVis:::.protvis_apply_normalization(raw, "vsn", "raw")
  expect_equal(dim(vsn_raw), dim(raw))
  vsn_log2 <- ProtVis:::.protvis_apply_normalization(log2(raw), "vsn", "log2")
  expect_equal(dim(vsn_log2), dim(raw))
})

test_that("normalization comparison returns diagnostics and per-method status", {
  set.seed(3)
  mat <- matrix(rnorm(180, mean = 10, sd = 1.5), nrow = 30)
  rownames(mat) <- paste0("P", seq_len(nrow(mat)))
  colnames(mat) <- paste0("S", seq_len(ncol(mat)))
  sample_info <- data.frame(
    sample_id = colnames(mat),
    group = rep(c("A", "B"), each = 3),
    stringsAsFactors = FALSE
  )
  result <- ProtVis:::.protvis_compare_normalizations(
    mat, sample_info, scale = "log2",
    methods = c("median", "quantile", "cyclic_loess", "rlr")
  )
  expect_equal(nrow(result$status), 4)
  expect_true(all(result$status$status == "Completed"))
  expect_true(all(c(
    "sample_median_sd", "sample_iqr_sd", "median_pairwise_correlation",
    "median_within_group_sd", "missing_fraction"
  ) %in% names(result$metrics)))
})

test_that("normalization UI exposes requested proteomics methods", {
  html <- as.character(ProtVis::data_normalization_ui("norm_methods_test"))
  expect_match(html, "Median", fixed = TRUE)
  expect_match(html, "Quantile", fixed = TRUE)
  expect_match(html, "VSN", fixed = TRUE)
  expect_match(html, "Cyclic Loess", fixed = TRUE)
  expect_match(html, "RLR", fixed = TRUE)
  expect_match(html, "Compare all methods", fixed = TRUE)
})
