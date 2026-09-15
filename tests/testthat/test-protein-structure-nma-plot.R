test_that("Protein Structure follows the standard bio3d NMA plot workflow", {
  source_text <- paste(
    readLines(testthat::test_path("..", "..", "R", "protein_structure.R")),
    collapse = "\n"
  )

  expect_true(grepl(".protvis_draw_nma_fluctuations", source_text, fixed = TRUE))
  expect_true(grepl(".protvis_run_normal_modes", source_text, fixed = TRUE))
  expect_true(grepl(".protvis_prepare_bio3d_nma", source_text, fixed = TRUE))
  expect_true(grepl("base::library(package = \"bio3d\"", source_text, fixed = TRUE))
  expect_true(grepl("bio3d::read.pdb(pdb_file)", source_text, fixed = TRUE))
  expect_true(grepl("bio3d::nma(pdb)", source_text, fixed = TRUE))
  expect_true(grepl("bio3d::plot.bio3d", source_text, fixed = TRUE))
  expect_true(grepl("analysis_error", source_text, fixed = TRUE))
})
