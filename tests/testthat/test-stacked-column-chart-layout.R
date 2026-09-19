test_that("Column Chart uses a full-width responsive preview layout", {
  source_text <- paste(
    readLines(testthat::test_path("..", "..", "R", "stacked_column_chart.R")),
    collapse = "\n"
  )

  expect_true(grepl("pv-stacked-column-main", source_text, fixed = TRUE))
  expect_true(grepl("DT::DTOutput(ns(\"data_preview\"))", source_text, fixed = TRUE))
  expect_true(grepl("output$plot_status", source_text, fixed = TRUE))
  expect_true(grepl("RUN CHART", source_text, fixed = TRUE))
  expect_true(grepl("USE DEMO DATA", source_text, fixed = TRUE))
})
