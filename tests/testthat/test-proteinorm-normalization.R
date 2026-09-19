test_that("proteiNorm normalization family is exposed", {
  methods <- ProtVis:::.protvis_norm_methods
  expect_true(all(c(
    "proteinorm_log2", "proteinorm_median", "proteinorm_mean",
    "proteinorm_vsn", "proteinorm_quantile", "proteinorm_cyclic_loess",
    "proteinorm_rlr", "proteinorm_global_intensity"
  ) %in% unname(methods)))
})

test_that("proteiNorm core methods preserve dimensions", {
  set.seed(101)
  raw <- matrix(rexp(240, rate = 0.01) + 50, nrow = 40)
  rownames(raw) <- paste0("P", seq_len(nrow(raw)))
  colnames(raw) <- paste0("S", seq_len(ncol(raw)))

  methods <- c(
    "proteinorm_log2", "proteinorm_median", "proteinorm_mean",
    "proteinorm_quantile", "proteinorm_cyclic_loess"
  )
  for (method in methods) {
    value <- ProtVis:::.protvis_apply_normalization(raw, method, "raw")
    expect_equal(dim(value), dim(raw))
    expect_equal(rownames(value), rownames(raw))
    expect_equal(colnames(value), colnames(raw))
  }
})

test_that("proteiNorm VSN accepts raw and reversible log2 input", {
  skip_if_not_installed("vsn")
  set.seed(102)
  raw <- matrix(rexp(180, rate = 0.02) + 20, nrow = 30)
  rownames(raw) <- paste0("P", seq_len(nrow(raw)))
  colnames(raw) <- paste0("S", seq_len(ncol(raw)))

  direct <- ProtVis:::.protvis_apply_normalization(raw, "proteinorm_vsn", "raw")
  from_log <- ProtVis:::.protvis_apply_normalization(log2(raw), "proteinorm_vsn", "log2")
  expect_equal(dim(direct), dim(raw))
  expect_equal(dim(from_log), dim(raw))
})

test_that("proteiNorm RLR and global intensity use NormalyzerDE", {
  skip_if_not_installed("NormalyzerDE")
  set.seed(103)
  raw <- matrix(rexp(300, rate = 0.01) + 25, nrow = 50)
  rownames(raw) <- paste0("P", seq_len(nrow(raw)))
  colnames(raw) <- paste0("S", seq_len(ncol(raw)))

  rlr <- ProtVis:::.protvis_apply_normalization(raw, "proteinorm_rlr", "raw")
  gi <- ProtVis:::.protvis_apply_normalization(raw, "proteinorm_global_intensity", "raw")
  expect_equal(dim(rlr), dim(raw))
  expect_equal(dim(gi), dim(raw))
})

test_that("proteiNorm evaluation metrics are added to method comparison", {
  set.seed(104)
  mat <- matrix(rnorm(360, mean = 12, sd = 1), nrow = 60)
  rownames(mat) <- paste0("P", seq_len(nrow(mat)))
  colnames(mat) <- paste0("S", seq_len(ncol(mat)))
  sample_info <- data.frame(
    sample_id = colnames(mat),
    group = rep(c("A", "B"), each = 3),
    stringsAsFactors = FALSE
  )

  metrics <- ProtVis:::.protvis_norm_metrics(mat, sample_info, "test")
  expect_true(all(c(
    "proteiNorm_PCV", "proteiNorm_PMAD", "proteiNorm_PEV",
    "proteiNorm_intragroup_correlation", "proteiNorm_total_intensity_CV",
    "proteiNorm_log2_ratio_IQR"
  ) %in% names(metrics)))
  expect_true(is.finite(metrics$proteiNorm_PMAD))
  expect_true(is.finite(metrics$proteiNorm_PEV))
  expect_true(is.finite(metrics$proteiNorm_intragroup_correlation))
})

test_that("programmatic normalization pipeline accepts proteiNorm method", {
  set.seed(105)
  raw <- matrix(rexp(120, rate = 0.02) + 10, nrow = 20)
  rownames(raw) <- paste0("P", seq_len(nrow(raw)))
  colnames(raw) <- paste0("S", seq_len(ncol(raw)))
  info <- data.frame(
    sample_id = colnames(raw),
    class = rep(c("A", "B"), each = 3),
    group = rep(c("A", "B"), each = 3),
    stringsAsFactors = FALSE
  )
  object <- ProtVis::create_protvis_dataset(raw, sample_info = info)
  result <- ProtVis:::.protvis_normalization(
    object, list(method = "proteinorm_quantile", input_scale = "raw")
  )
  expect_s4_class(result, "ProtVis_dataset")
  expect_equal(dim(result$expression_data), dim(object$expression_data))
})
