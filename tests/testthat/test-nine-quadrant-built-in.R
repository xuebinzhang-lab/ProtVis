test_that("Nine-Quadrant module provides built-in data and expanded layout", {
  source_text <- paste(
    readLines(testthat::test_path("..", "..", "R", "nine_quadrant.R")),
    collapse = "\n"
  )

  expect_true(grepl("builtin_data", source_text, fixed = TRUE))
  expect_true(grepl("2800", source_text, fixed = TRUE))
  expect_true(grepl("col_widths = c(4, 8)", source_text, fixed = TRUE))
  expect_true(grepl('height = \"820px\"', source_text, fixed = TRUE))
})
