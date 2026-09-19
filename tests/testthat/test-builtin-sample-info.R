test_that("built-in fixtures provide real B73 and Y12 metadata", {
  for (source in c("DIA-NN", "FragPipe", "Skyline", "OpenMS")) {
    info <- .protvis_builtin_sample_info(source)
    expect_equal(nrow(info), 2L)
    expect_equal(info$maxquant_id, c("B73_TMT1_1", "Y12_TMT1_1"))
    expect_equal(info$group, c("B73", "Y12"))
    expect_equal(info$condition, info$group)
    expect_true(all(info$organism == "Zea mays"))
    expect_true(all(grepl(
      "ProtVis/blob/dev/inst/extdata/Raw_reporter_corrected_5groups_verified.csv",
                          info$source_url, fixed = TRUE)))
  }
})

test_that("MaxQuant demo metadata matches all 30 biological samples", {
  info <- .protvis_builtin_sample_info("MaxQuant")
  expect_equal(nrow(info), 30L)
  expect_equal(info$sample_id, info$maxquant_id)
  expect_equal(as.integer(table(info$genotype)), c(15L, 15L))
  expect_equal(as.integer(table(info$tissue)), c(12L, 18L))
  expect_equal(as.integer(table(info$replicate)), c(10L, 10L, 10L))
  expect_equal(length(unique(info$group)), 10L)
  expect_true(all(table(info$group) == 3L))
  expect_equal(unique(info$stage),
               c("VE", "V1.V2", "V4", "VE.V1.V2", "V4.V6.V8"))
  expect_true(all(info$condition == info$genotype))
  expect_true(all(info$organism == "Zea mays"))
})

test_that("the Sage sample-information template is downloadable and searchable", {
  template <- file.path(testthat::test_path("..", "..", "inst", "extdata"),
                        "PXD065315_sample_info_template.csv")
  expect_true(file.exists(template))
  info <- utils::read.csv(template, stringsAsFactors = FALSE,
                          check.names = FALSE)
  expect_equal(nrow(info), 6L)
  expect_true(all(c("sample_id", "mzml_file", "group") %in% names(info)))
  expect_true(all(tolower(tools::file_ext(info$mzml_file)) == "mzml"))
})
