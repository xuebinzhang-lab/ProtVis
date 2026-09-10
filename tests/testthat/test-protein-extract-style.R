test_that("protein extraction uses a restrained scientific visual style", {
  source_text <- paste(
    readLines(testthat::test_path("..", "..", "R", "protein_extract.R")),
    collapse = "\n"
  )

  expect_true(grepl("pv-protein-extract", source_text, fixed = TRUE))
  expect_true(grepl("theme = \"light\"", source_text, fixed = TRUE))
  expect_true(grepl("--pv-science-navy", source_text, fixed = TRUE))
})
