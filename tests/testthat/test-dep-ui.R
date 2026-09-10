test_that("DEP plot parameter sidebars are open by default", {
  ui_text <- paste(readLines(testthat::test_path("..", "..", "R", "DEP_analysis.R")),
                   collapse = "\n")
  expect_equal(length(gregexpr('open = "open"', ui_text, fixed = TRUE)[[1]]), 2L)
  expect_false(grepl('open = FALSE', ui_text, fixed = TRUE))
})
