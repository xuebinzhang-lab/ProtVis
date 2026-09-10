test_that("column chart module exposes multiple plotting forms", {
  source_text <- paste(
    readLines(testthat::test_path("..", "..", "R", "stacked_column_chart.R")),
    collapse = "\n"
  )

  expect_true(grepl("Chart type", source_text, fixed = TRUE))
  expect_true(grepl("Grouped columns", source_text, fixed = TRUE))
  expect_true(grepl("100% stacked columns", source_text, fixed = TRUE))
  expect_true(grepl("position = switch", source_text, fixed = TRUE))
  expect_true(grepl("pv-stacked-column", source_text, fixed = TRUE))
})
