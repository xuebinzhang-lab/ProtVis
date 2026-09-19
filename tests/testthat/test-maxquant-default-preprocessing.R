testthat::test_that("MaxQuant preprocessing defaults reproduce the archived preset", {
  defaults <- protvis_preprocessing_defaults("MaxQuant")

  testthat::expect_identical(
    defaults$filtering$flags,
    c("site", "reverse", "contaminant")
  )
  testthat::expect_identical(
    defaults$noise_correction$method,
    "replicate_correction"
  )
  testthat::expect_identical(
    defaults$transformation$method,
    "maxquant_log2"
  )
  testthat::expect_equal(defaults$transformation$multiplier, 1e7)
  testthat::expect_identical(defaults$imputation$method, "knn")
  testthat::expect_identical(defaults$imputation$seed, 12345L)
  testthat::expect_identical(
    defaults$normalization$method,
    "maxquant_recommended"
  )
  testthat::expect_equal(defaults$normalization$row_shift, 5)
  testthat::expect_equal(defaults$normalization$zero_value, 1)
})

testthat::test_that("MaxQuant transform and normalization match archived equations", {
  matrix <- matrix(
    c(
      1, 2, 4,
      2, 4, 8,
      3, 6, 12,
      4, 8, 16
    ),
    nrow = 4,
    byrow = FALSE,
    dimnames = list(
      paste0("P", 1:4),
      c("S1", "S2", "S3")
    )
  )

  transformed <- ProtVis:::.protvis_maxquant_log2_matrix(
    matrix, multiplier = 1e7
  )
  testthat::expect_equal(transformed, log2(matrix * 10000000))

  normalized <- ProtVis:::.protvis_maxquant_normalize_matrix(
    transformed, row_shift = 5, zero_value = 1
  )

  legacy_step5 <- apply(
    transformed, 2,
    function(x) x - stats::median(x)
  )
  legacy_step6 <- legacy_step5
  for (i in seq_len(nrow(legacy_step5))) {
    value <- legacy_step5[i, ]
    legacy_step6[i, ] <- ifelse(
      value == 0,
      1,
      value + abs(min(value)) + 5
    )
  }

  testthat::expect_equal(normalized, legacy_step6, tolerance = 1e-12)
})

testthat::test_that("MaxQuant exact kNN uses archived impute engine and seed", {
  testthat::skip_if_not_installed("impute")

  matrix <- matrix(
    c(
      10, 11, NA, 13, 14, 15,
      20, 21, 22, 23, NA, 25,
      30, 31, 32, NA, 34, 35,
      40, 41, 42, 43, 44, NA,
      50, NA, 52, 53, 54, 55,
      60, 61, 62, 63, 64, 65,
      70, 71, 72, 73, 74, 75,
      80, 81, 82, 83, 84, 85,
      90, 91, 92, 93, 94, 95,
      100, 101, 102, 103, 104, 105,
      110, 111, 112, 113, 114, 115,
      120, 121, 122, 123, 124, 125
    ),
    nrow = 12,
    byrow = TRUE
  )

  set.seed(12345)
  expected <- impute::impute.knn(matrix)$data
  observed <- ProtVis:::.protvis_knn_impute_exact(matrix, seed = 12345L)

  testthat::expect_equal(observed, expected, tolerance = 1e-12)
})

testthat::test_that("default MaxQuant pipeline resolves to reproducible methods", {
  testthat::skip_if_not_installed("impute")

  expression <- data.frame(
    A_1 = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120),
    A_2 = c(12, 22, 32, 42, 52, 62, 72, 82, 92, 102, 112, 122),
    A_3 = c(NA, 24, 34, 44, 54, 64, 74, 84, 94, 104, 114, 124),
    B_1 = c(20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130),
    B_2 = c(22, 32, 42, 52, 62, 72, 82, 92, 102, 112, 122, 132),
    B_3 = c(24, 34, 44, 54, 64, 74, 84, 94, 104, 114, 124, 134),
    row.names = paste0("P", 1:12),
    check.names = FALSE
  )
  info <- data.frame(
    sample_id = names(expression),
    group = c(rep("A", 3), rep("B", 3)),
    stringsAsFactors = FALSE
  )
  object <- create_protvis_dataset(
    expression,
    sample_info = info,
    metadata = list(source = "MaxQuant")
  )

  raw <- data.frame(
    ID = rownames(expression),
    expression,
    check.names = FALSE
  )
  legacy_step2 <- correct_values(raw)
  legacy_step3 <- log2(as.matrix(legacy_step2[, -1, drop = FALSE]) * 1e7)
  set.seed(12345)
  legacy_step4 <- impute::impute.knn(legacy_step3)$data
  legacy_step5 <- apply(
    legacy_step4, 2,
    function(x) x - stats::median(x)
  )
  legacy_step6 <- legacy_step5
  for (i in seq_len(nrow(legacy_step5))) {
    value <- legacy_step5[i, ]
    legacy_step6[i, ] <- ifelse(
      value == 0,
      1,
      value + abs(min(value)) + 5
    )
  }

  output <- tempfile("protvis_maxquant_defaults_")
  dir.create(output)
  processed <- run_protvis_pipeline(
    object,
    stages = c(
      "noise_correction",
      "transformation",
      "imputation",
      "normalization"
    ),
    checkpoint_dir = output,
    stop_on_error = TRUE
  )

  testthat::expect_equal(
    as.matrix(processed$expression_data),
    legacy_step6,
    tolerance = 1e-10
  )
  testthat::expect_identical(
    processed$analysis_results$normalization$method,
    "maxquant_recommended"
  )
})


testthat::test_that("replicate correction drops proteins with no observed intensity", {
  raw <- data.frame(
    ID = c("P0", "P1"),
    A_1 = c(0, 10),
    A_2 = c(0, 12),
    A_3 = c(0, 0),
    B_1 = c(0, 20),
    B_2 = c(0, 22),
    B_3 = c(0, 24),
    check.names = FALSE
  )

  corrected <- correct_values(raw)
  testthat::expect_identical(corrected$ID, "P1")
  testthat::expect_equal(corrected$A_3, 11)
})
