test_that("Protein Structure uses a version-compatible bio3d NMA plot method", {
  source_text <- paste(
    readLines(testthat::test_path("..", "..", "R", "protein_structure.R")),
    collapse = "\n"
  )

  expect_true(grepl(".protvis_draw_nma_fluctuations", source_text, fixed = TRUE))
  expect_true(grepl('get("plot.bio3d", envir = asNamespace("bio3d")', source_text, fixed = TRUE))
  expect_false(grepl("bio3d::plot.bio3d", source_text, fixed = TRUE))
  expect_true(grepl("analysis_error", source_text, fixed = TRUE))
})
