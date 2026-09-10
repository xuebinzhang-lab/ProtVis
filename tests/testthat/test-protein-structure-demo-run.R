test_that("selecting the demo PDB triggers the analysis path", {
  source_text <- paste(
    readLines(testthat::test_path("..", "..", "R", "protein_structure.R")),
    collapse = "\n"
  )

  expect_true(grepl("shinyjs::useShinyjs()", source_text, fixed = TRUE))
  expect_true(grepl("shinyjs::click(ns(\"run\"))", source_text, fixed = TRUE))
  expect_true(grepl("Demo PDB selected. Analysis started.", source_text, fixed = TRUE))
})
