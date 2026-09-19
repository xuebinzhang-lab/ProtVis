test_that("protein structure module does not depend on unavailable bio3d data objects", {
  source_text <- paste(
    readLines(testthat::test_path("..", "..", "R", "protein_structure.R")),
    collapse = "\n"
  )

  expect_false(grepl("data(elements", source_text, fixed = TRUE))
  expect_false(grepl("data(atom.index", source_text, fixed = TRUE))
})
