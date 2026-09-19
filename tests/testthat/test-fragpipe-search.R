testthat::test_that("FragPipe manifest is headless-compatible", {
  td <- tempfile("protvis-fragpipe-")
  dir.create(td, recursive = TRUE)
  spectra <- file.path(td, c("A.mzML", "B.mzML"))
  file.create(spectra)
  info <- data.frame(
    sample_id = c("A", "B"),
    mzml_file = basename(spectra),
    group = c("Control", "Treatment"),
    replicate = c(1L, 1L),
    stringsAsFactors = FALSE
  )

  manifest <- protvis_fragpipe_write_manifest(
    spectra, file.path(td, "test.fp-manifest"),
    sample_info = info, data_type = "DDA"
  )
  lines <- readLines(manifest, warn = FALSE)
  testthat::expect_length(lines, 2L)
  testthat::expect_false(grepl("spectra|experiment|bioreplicate", lines[[1L]],
                               ignore.case = TRUE))
  testthat::expect_true(all(vapply(strsplit(lines, "\t", fixed = TRUE),
                                   length, integer(1)) == 4L))
})

testthat::test_that("FragPipe workflow staging injects FASTA database", {
  td <- tempfile("protvis-fragpipe-workflow-")
  dir.create(td, recursive = TRUE)
  fasta <- file.path(td, "proteins.fasta")
  writeLines(c(">P1", "PEPTIDE"), fasta)
  workflow <- file.path(td, "Basic-Search.workflow")
  writeLines(c(
    "# Workflow: Basic Search",
    "database.decoy-tag=rev_",
    "msfragger.run-msfragger=true"
  ), workflow)

  prepared <- ProtVis:::.protvis_fragpipe_prepare_workflow(
    workflow, fasta, file.path(td, "out")
  )
  lines <- readLines(prepared, warn = FALSE)
  testthat::expect_true(any(grepl("^database[.]db-path=", lines)))
  testthat::expect_true(any(grepl(normalizePath(fasta, winslash = "/",
                                                mustWork = TRUE),
                                 lines, fixed = TRUE)))
})

testthat::test_that("FragPipe public backend entrypoints are available", {
  presets <- protvis_fragpipe_workflow_presets()
  testthat::expect_true("LFQ-MBR.workflow" %in% unname(presets))
  testthat::expect_true(is.list(protvis_fragpipe_status()))
  testthat::expect_true(is.function(run_fragpipe_search))
  testthat::expect_true(is.function(fragpipe_search_ui))
  testthat::expect_true(is.function(fragpipe_search_server))
})
