testthat::test_that("MaxQuant output preparation reports independent flags and unique removal", {
  raw <- data.frame(
    `Protein IDs` = paste0("P", 1:6),
    `Only identified by site` = c("+", "", "+", "", "", ""),
    Reverse = c("", "+", "+", "", "", ""),
    `Potential contaminant` = c("", "", "", "+", "+", ""),
    S1 = 1:6,
    S2 = 11:16,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  report <- .protvis_maxquant_filter_report(
    raw, c("site", "reverse", "contaminant")
  )
  testthat::expect_identical(
    report$Metric,
    c(
      "Before filtering",
      "Only identified by site",
      "Reverse",
      "Potential contaminant",
      "Unique rows removed",
      "After filtering"
    )
  )
  testthat::expect_equal(report$Count, c(6, 2, 2, 2, 5, 1))

  parsed <- .protvis_parse_maxquant(
    raw, c("site", "reverse", "contaminant")
  )
  testthat::expect_equal(nrow(parsed$expression), 1L)
  testthat::expect_identical(parsed$expression$ID, "P6")
})

testthat::test_that("MaxQuant staging preserves the original flag table", {
  raw <- data.frame(
    `Protein IDs` = c("P1", "P2", "P3"),
    `Only identified by site` = c("+", "", ""),
    Reverse = c("", "+", ""),
    `Potential contaminant` = c("", "", "+"),
    S1 = c(10, 20, 30),
    S2 = c(11, 21, 31),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  parsed <- .protvis_parse_maxquant(raw, filters = character())
  object <- create_protvis_dataset(
    parsed$expression,
    metadata = list(source = "MaxQuant")
  )
  object <- .protvis_attach_maxquant_preparation(
    object, raw, filename = "proteinGroups.txt"
  )

  testthat::expect_equal(nrow(object$expression_data), 3L)
  testthat::expect_identical(
    object$other_files$maxquant_preparation$raw_table,
    raw
  )
  testthat::expect_true(object$metadata$maxquant_filter_pending)
  testthat::expect_equal(object$metadata$raw_rows, 3L)
  testthat::expect_equal(object$metadata$removed_rows, 0L)
})

testthat::test_that("bundled MaxQuant output removes 389 unique flagged rows", {
  path <- protvis_builtin_data_path()
  testthat::skip_if_not(file.exists(path))

  raw <- protvis_read_table(
    path, filename = "Raw_reporter_corrected_5groups_verified.csv"
  )
  testthat::expect_equal(nrow(raw), 12689L)
  testthat::expect_true(all(c(
    "Only identified by site", "Reverse", "Potential contaminant"
  ) %in% names(raw)))

  unfiltered <- .protvis_parse_maxquant(raw, filters = character())
  testthat::expect_equal(nrow(unfiltered$expression), 12689L)

  report <- .protvis_maxquant_filter_report(
    raw, c("site", "reverse", "contaminant")
  )
  count <- stats::setNames(report$Count, report$Metric)
  testthat::expect_equal(count[["Only identified by site"]], 228)
  testthat::expect_equal(count[["Reverse"]], 184)
  testthat::expect_equal(count[["Potential contaminant"]], 48)
  testthat::expect_equal(count[["Unique rows removed"]], 389)
  testthat::expect_equal(count[["After filtering"]], 12300)

  filtered <- .protvis_parse_maxquant(
    raw, c("site", "reverse", "contaminant")
  )
  testthat::expect_equal(filtered$removed_rows, 389L)
  testthat::expect_equal(nrow(filtered$expression), 12300L)
})
