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

test_that("generic PTM calculation supports arbitrary modified peptides", {
  modifications <- data.frame(
    location = c(2L, 4L),
    mass = c(79.966330890, 15.994914620),
    name = c("Phospho", "Oxidation")
  )
  theoretical <- ProtVis:::.protvis_ptm_theoretical("ASMFYK", modifications)
  label <- ProtVis:::.protvis_modified_sequence_label("ATSGVPFSQYK", data.frame(
    location = 3L, mass = 79.966330890, name = "Phospho"
  ))

  expect_equal(nrow(theoretical$fragment_table), 6L)
  expect_identical(label, "AT[pS]GVPFSQYK")
  expect_match(theoretical$fragment_table$AA[2], "S\\+80")
  expect_match(theoretical$fragment_table$AA[4], "F\\+16")
  expect_true(any(theoretical$candidates$label == "b2-98"))
  expect_true(all(c("b1", "y1") %in% theoretical$key_ions$label))
})

test_that("PSM catalog keeps separate spectra selectable", {
  psm <- data.frame(
    sequence = c("PEPTIDE", "PEPTIDE"),
    spectrumID = c("index=1", "index=2"),
    chargeState = c(2L, 3L),
    stringsAsFactors = FALSE
  )
  catalog <- ProtVis:::.protvis_ptm_psm_catalog(psm)

  expect_equal(nrow(catalog), 2L)
  expect_identical(catalog$psm_index, c(1L, 2L))
  expect_true(all(grepl("PEPTIDE", catalog$label, fixed = TRUE)))
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

test_that("PTM UI exposes selectable PSM visualization without removing the overview", {
  source_text <- paste(
    readLines(testthat::test_path("..", "..", "R", "PTM.R")),
    collapse = "\n"
  )
  vac14_text <- paste(
    readLines(testthat::test_path("..", "..", "R", "ptm_vac14.R")),
    collapse = "\n"
  )

  expect_true(grepl("PTM data overview", source_text, fixed = TRUE))
  expect_true(grepl("PTM spectrum", source_text, fixed = TRUE))
  expect_true(grepl("LOAD PSM LIST", vac14_text, fixed = TRUE))
  expect_true(grepl("VISUALIZE SELECTED PEPTIDE", vac14_text, fixed = TRUE))
  expect_true(grepl("vac14_psm_choice", vac14_text, fixed = TRUE))
  expect_true(grepl("vac14_matches_csv", vac14_text, fixed = TRUE))
  expect_true(grepl("completed_signature", vac14_text, fixed = TRUE))
})

test_that("PD Strict Spectrum is removed from navigation and exports", {
  ui_text <- paste(
    readLines(testthat::test_path("..", "..", "R", "app_ui.R")),
    collapse = "\n"
  )
  namespace_text <- paste(
    readLines(testthat::test_path("..", "..", "NAMESPACE")),
    collapse = "\n"
  )

  expect_false(grepl("PD Strict Spectrum", ui_text, fixed = TRUE))
  expect_false(grepl("pd_strict", namespace_text, fixed = TRUE))
  expect_false(file.exists(testthat::test_path("..", "..", "R", "pd_strict_module.R")))
})
