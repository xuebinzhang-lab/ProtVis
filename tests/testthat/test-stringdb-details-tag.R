test_that("STRINGdb controls use exported shiny HTML tags", {
  source_text <- paste(
    readLines(testthat::test_path("..", "..", "R", "stringdb_ppi.R")),
    collapse = "\n"
  )

  expect_true(grepl("shiny::tags$details", source_text, fixed = TRUE))
  expect_false(grepl("shiny::details(", source_text, fixed = TRUE))
})
