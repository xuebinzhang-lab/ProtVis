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

test_that("only matched theoretical cells use spectrum ion colors", {
  table <- ProtVis:::.protvis_vac14_theoretical()$fragment_table
  labels <- ProtVis:::.protvis_vac14_table_ion_labels(table)
  matched <- data.frame(
    label = c("b3", "y9", "b7-H2O"),
    intensity = c(12, 0.5, 3),
    series = c("b", "y", "b"),
    neutral = c(FALSE, FALSE, TRUE)
  )
  displayed <- ProtVis:::.protvis_vac14_displayed_matches(matched)
  colors <- ProtVis:::.protvis_vac14_match_colors(
    displayed,
    b_color = "red", y_color = "blue", neutral_color = "green"
  )

  expect_identical(labels[3, "B Ions"], "b3")
  expect_identical(labels[3, "Y Ions"], "y9")
  expect_identical(labels[7, "B-H2O"], "b7-H2O")
  expect_identical(displayed$label, c("b3", "b7-H2O"))
  expect_identical(colors, c("red", "green"))
})

test_that("Vac14 benchmark table uses the publication-confirmed annotations", {
  article <- ProtVis:::.protvis_vac14_publication_table_matches()
  expected <- c(
    "b3", "b4", "b5", "b7", "b8", "b9", "b10",
    "b6++", "b7++", "b9++",
    "b7-H2O", "b8-H2O", "b9-H2O", "b10-H2O",
    paste0("y", 2:9),
    "y6++", "y7++", "y9++", "y10++",
    "y2-NH3", "y5-NH3",
    "y6-H2O", "y7-H2O", "y8-H2O", "y9-H2O"
  )
  b_double <- article$label[
    article$series == "b" & grepl("\\+\\+$", article$label)
  ]

  expect_identical(article$label, expected)
  expect_identical(b_double, c("b6++", "b7++", "b9++"))
  expect_false("b5++" %in% article$label)
  expect_true(all(c("b3", "b10", "y2", "y9", "y9-H2O") %in% article$label))
  expect_false("y10" %in% article$label)
  expect_equal(nrow(article), 32L)
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
  app_ui_text <- paste(
    readLines(testthat::test_path("..", "..", "R", "app_ui.R")),
    collapse = "\n"
  )

  expect_true(grepl("PTM data overview", source_text, fixed = TRUE))
  expect_true(grepl("PTM spectrum", source_text, fixed = TRUE))
  expect_true(grepl("LOAD PSM LIST", vac14_text, fixed = TRUE))
  expect_true(grepl("VISUALIZE SELECTED PEPTIDE", vac14_text, fixed = TRUE))
  expect_true(grepl("vac14_psm_choice", vac14_text, fixed = TRUE))
  expect_true(grepl("protvis-unlock-run-button", vac14_text, fixed = TRUE))
  expect_true(grepl("visualize_selected_psm(show_progress = FALSE)", vac14_text, fixed = TRUE))
  expect_true(grepl(".protvis_vac14_table_ion_labels", vac14_text, fixed = TRUE))
  expect_true(grepl("displayed_matches = displayed_matches", vac14_text, fixed = TRUE))
  expect_true(grepl("shiny:idle.protvisRunButtons", app_ui_text, fixed = TRUE))
  expect_true(grepl("protvis-unlock-run-button", app_ui_text, fixed = TRUE))
  expect_true(grepl("vac14_matches_csv", vac14_text, fixed = TRUE))
  expect_true(grepl("completed_signature", vac14_text, fixed = TRUE))
  expect_true(grepl("BiocParallel::SerialParam", vac14_text, fixed = TRUE))
  expect_true(grepl(".protvis_ptm_read_psm", vac14_text, fixed = TRUE))
})

test_that("expression profile uses valid bslib navigation children", {
  source_text <- paste(
    readLines(testthat::test_path("..", "..", "R", "Expression_profile.R")),
    collapse = "\n"
  )

  expect_false(grepl("shiny::tabsetPanel", source_text, fixed = TRUE))
  expect_true(grepl('id = ns("kmeans_tabs")', source_text, fixed = TRUE))
  expect_true(grepl('bslib::nav_panel("Input Data"', source_text, fixed = TRUE))
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
