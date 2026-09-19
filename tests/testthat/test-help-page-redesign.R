test_that("Help page uses the structured workflow redesign", {
  source_text <- paste(
    readLines(testthat::test_path("..", "..", "R", "help.R")),
    collapse = "\n"
  )

  expect_true(grepl("pv-help-page", source_text, fixed = TRUE))
  expect_true(grepl("Prepare data", source_text, fixed = TRUE))
  expect_true(grepl("Process and analyze", source_text, fixed = TRUE))
  expect_true(grepl("Common questions", source_text, fixed = TRUE))
})
