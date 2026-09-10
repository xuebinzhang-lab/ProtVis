test_that("Venn example data initializes the displayed state", {
  source_text <- paste(
    readLines(testthat::test_path("..", "..", "R", "venn.R")),
    collapse = "\n"
  )

  expect_true(grepl("set_plot_state(parsed_data())", source_text, fixed = TRUE))
  expect_true(grepl("Example Venn/UpSet data loaded and displayed.", source_text, fixed = TRUE))
  expect_true(grepl("table_data <- parsed_data()$bin_df", source_text, fixed = TRUE))
})
