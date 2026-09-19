test_that("protein structure server defines a namespace for demo actions", {
  source_text <- paste(
    readLines(testthat::test_path("..", "..", "R", "protein_structure.R")),
    collapse = "\n"
  )

  expect_true(grepl("ns <- session$ns", source_text, fixed = TRUE))
  expect_true(grepl("shinyjs::click(ns(\"run\"))", source_text, fixed = TRUE))
})
