test_that("PXD065315 raw template has the complete factorial design", {
  info <- .protvis_raw_sample_template()
  expect_equal(nrow(info), 6L)
  expect_equal(table(info$genotype), c(B73 = 3L, EA2024 = 3L))
  expect_equal(table(info$treatment), c(Control = 6L))
  expect_true(all(tolower(tools::file_ext(info$mzml_file)) == "mzml"))
  expect_equal(length(unique(info$sample_id)), 6L)
})

test_that("mzML validation detects missing and non-mzML files", {
  directory <- tempfile("protvis-mzml-")
  dir.create(directory)
  info <- data.frame(
    sample_id = c("A", "B"),
    mzml_file = c("A.mzML", "B.txt"),
    stringsAsFactors = FALSE
  )
  file.create(file.path(directory, "A.mzML"))
  result <- .protvis_validate_mzml_files(info, directory)
  expect_false(result$valid)
  expect_equal(result$manifest$status, c("Found", "Invalid extension"))
  expect_match(result$message, "Non-mzML files")
})
