test_that("Sage configuration uses the reproducible 0.14 search schema", {
  directory <- tempfile("protvis-sage-")
  dir.create(directory)
  fasta <- file.path(directory, "proteins.fasta")
  mzml <- file.path(directory, "sample.mzML")
  file.create(fasta)
  file.create(mzml)
  config <- protvis_sage_build_config(
    fasta, mzml, directory,
    parameters = list(precursor_ppm = 20, fragment_da = 0.5,
                      missed_cleavages = 2, lfq = TRUE)
  )
  expect_equal(config$database$fasta, normalizePath(fasta, winslash = "/"))
  expect_equal(config$mzml_paths, normalizePath(mzml, winslash = "/"))
  expect_equal(config$precursor_tol$ppm, c(-20, 20))
  expect_equal(config$fragment_tol$da, c(-0.5, 0.5))
  expect_equal(config$database$static_mods$C, 57.021464)
  expect_true(isTRUE(config$quant$lfq))
  expect_equal(config$output_directory, normalizePath(directory, winslash = "/"))
})

test_that("the bundled Sage executable is tracked without the removed gzip copy", {
  sage <- testthat::test_path("..", "..", "inst", "extdata", "sage",
                              "windows", "sage.exe")
  expect_true(file.exists(sage))
  expect_false(file.exists(sub("[.]exe$", ".exe.gz", sage)))
})

test_that("Sage can persist a sample-only staging ProtVis_dataset", {
  directory <- tempfile("protvis-sage-staging-")
  dir.create(directory)
  fasta <- file.path(directory, "proteins.fasta")
  mzml <- file.path(directory, "sample.mzML")
  file.create(fasta)
  file.create(mzml)
  sample_info <- data.frame(
    mzml_file = basename(mzml), sample_id = "sample_1",
    group = "Unassigned", stringsAsFactors = FALSE
  )
  dataset <- ProtVis:::.protvis_create_sage_staging_dataset(
    sample_info, fasta, mzml, file.path(directory, "Sage_search")
  )
  expect_s4_class(dataset, "ProtVis_dataset")
  expect_identical(dataset$metadata$workflow_stage, "Sage_staging")
  expect_equal(nrow(dataset$expression_data), 0L)
  expect_equal(dataset$sample_info$sample_id, "sample_1")
  expect_identical(ProtVis::validate_protvis_dataset(dataset), TRUE)
})

test_that("Sage staging can start with sample information only", {
  directory <- tempfile("protvis-sage-sample-only-")
  dir.create(directory)
  info <- data.frame(
    sample_id = c("sample_1", "sample_2"),
    group = c("A", "B"),
    stringsAsFactors = FALSE
  )
  dataset <- ProtVis:::.protvis_create_sage_staging_dataset(
    info, fasta = "", mzml_paths = character(),
    output_directory = file.path(directory, "Sage_search")
  )
  expect_equal(dataset$sample_info$sample_id, info$sample_id)
  expect_equal(nrow(dataset$expression_data), 0L)
  expect_identical(dataset$metadata$workflow_stage, "Sage_staging")
})
