test_that("correlation chord module includes a reproducible example dataset", {
  source_text <- paste(
    readLines(testthat::test_path("..", "..", "R", "correlation_chord.R")),
    collapse = "\n"
  )

  expect_true(grepl("Use Example Data", source_text, fixed = TRUE))
  expect_true(grepl("make_example_data", source_text, fixed = TRUE))
  expect_true(grepl("Root_length", source_text, fixed = TRUE))
  expect_true(grepl("rv$data <- make_example_data()", source_text, fixed = TRUE))
})
