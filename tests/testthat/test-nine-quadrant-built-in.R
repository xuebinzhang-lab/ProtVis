test_that("Nine-Quadrant module provides explicit built-in data controls", {
  source_text <- paste(
    readLines(testthat::test_path("..", "..", "R", "nine_quadrant.R")),
    collapse = "\n"
  )

  data <- ProtVis:::.protvis_nine_quadrant_builtin_data()
  expect_equal(nrow(data), 2800L)
  expect_equal(ncol(data), 4L)
  expect_true(grepl("load_builtin", source_text, fixed = TRUE))
  expect_true(grepl("download_builtin", source_text, fixed = TRUE))
  expect_true(grepl("req(has_data())", source_text, fixed = TRUE))
  expect_true(grepl("col_widths = c(4, 8)", source_text, fixed = TRUE))
  expect_true(grepl('height = \"820px\"', source_text, fixed = TRUE))
})
