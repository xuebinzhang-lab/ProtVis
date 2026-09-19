testthat::test_that("preprocessing analysis results retain result tables", {
  expression <- data.frame(
    S1 = c(1, 2, 3),
    S2 = c(4, 5, 6),
    row.names = c("P1", "P2", "P3"),
    check.names = FALSE
  )
  info <- data.frame(
    sample_id = c("S1", "S2"),
    group = c("A", "B"),
    stringsAsFactors = FALSE
  )
  object <- create_protvis_dataset(expression, sample_info = info)

  for (stage in c(
    "noise_correction", "transformation", "imputation", "normalization"
  )) {
    value <- ProtVis:::.protvis_store_preprocessing_result(
      object, stage = stage, method = paste0(stage, "_method")
    )
    result <- value$analysis_results[[stage]]
    testthat::expect_identical(result$status, "success")
    testthat::expect_identical(result$method, paste0(stage, "_method"))
    testthat::expect_true(is.data.frame(result$result_table))
    testthat::expect_identical(
      names(result$result_table),
      c("protein_id", "S1", "S2")
    )
    testthat::expect_identical(
      result$result_table$protein_id,
      c("P1", "P2", "P3")
    )
    testthat::expect_equal(
      as.matrix(result$result_table[, c("S1", "S2")]),
      as.matrix(expression),
      ignore_attr = TRUE
    )
  }
})

testthat::test_that("pipeline preprocessing nodes use the same result-table contract", {
  expression <- data.frame(
    S1 = c(1, 2, 3),
    S2 = c(4, 5, 6),
    row.names = c("P1", "P2", "P3"),
    check.names = FALSE
  )
  info <- data.frame(
    sample_id = c("S1", "S2"),
    group = c("A", "B"),
    stringsAsFactors = FALSE
  )
  object <- create_protvis_dataset(expression, sample_info = info)

  transformed <- ProtVis:::.protvis_transformation(
    object, list(method = "log2", pseudocount = 1)
  )
  transformed <- ProtVis:::.protvis_store_preprocessing_result(
    transformed, "transformation", "log2"
  )
  testthat::expect_true(
    is.data.frame(
      transformed$analysis_results$transformation$result_table
    )
  )
  testthat::expect_equal(
    nrow(transformed$analysis_results$transformation$result_table),
    nrow(expression)
  )
})
