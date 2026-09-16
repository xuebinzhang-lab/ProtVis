test_that("Vac14 benchmark target uses the article files and Ser3 peptide", {
  target <- ProtVis:::.protvis_vac14_target()

  expect_identical(target$project, "PXD001057")
  expect_identical(target$modified_sequence, "AT[pS]GVPFSQYK")
  expect_identical(target$phospho_position, 3L)
  expect_identical(target$spectrum_id, "index=992")
  expect_identical(target$spectrum_title, "E1R2_SCX5_soluble.08422.08422.2")
  expect_identical(target$mzid_gz, "E1R2_SCX5_soluble.mzid.gz")
  expect_identical(target$mgf, "E1R2_SCX5_soluble.mzid_E1R2_SCX5_soluble.MGF")
})

test_that("Vac14 theoretical ions reproduce the expected precursor and key series", {
  theoretical <- ProtVis:::.protvis_vac14_theoretical()

  expect_equal(theoretical$precursor_2plus, 632.784111, tolerance = 1e-6)
  expect_identical(theoretical$key_ions$label,
                   c("b3", "b5", "b7", "b8", "y4", "y5", "y6", "y7", "y8"))
  expect_equal(nrow(theoretical$fragment_table), 11L)
  expect_identical(theoretical$fragment_table$AA[3], "S+80")
  expect_true(any(theoretical$candidates$label == "b3-98"))
})

test_that("Vac14 ion matching honors the article's absolute tolerance", {
  candidates <- data.frame(
    label = c("b3", "y4"), mz = c(300, 500), series = c("b", "y"),
    charge = 1L, neutral = FALSE, priority = 1L
  )
  peaks <- data.frame(
    mz = c(300.4, 500.6), intensity = c(100, 50), rel = c(100, 50)
  )

  matched <- ProtVis:::.protvis_vac14_match_ions(peaks, candidates, 0.5)
  expect_identical(matched$label, "b3")
  expect_error(
    ProtVis:::.protvis_vac14_match_ions(peaks, candidates, 0.1),
    "No fragment ions matched"
  )
})

test_that("PTM UI exposes the Vac14 benchmark without removing the overview", {
  source_text <- paste(
    readLines(testthat::test_path("..", "..", "R", "PTM.R")),
    collapse = "\n"
  )
  vac14_text <- paste(
    readLines(testthat::test_path("..", "..", "R", "ptm_vac14.R")),
    collapse = "\n"
  )

  expect_true(grepl("PTM data overview", source_text, fixed = TRUE))
  expect_true(grepl("Vac14 / PXD001057", source_text, fixed = TRUE))
  expect_true(grepl("RUN VAC14 VALIDATION", vac14_text, fixed = TRUE))
  expect_true(grepl("vac14_matches_csv", vac14_text, fixed = TRUE))
  expect_true(grepl("completed_signature", vac14_text, fixed = TRUE))
})
