test_that("top-level navigation panels define icons", {
  source_text <- paste(
    readLines(testthat::test_path("..", "..", "R", "app_ui.R")),
    collapse = "\n"
  )

  expect_true(grepl('nav_panel("Overview", icon =', source_text, fixed = TRUE))
  expect_true(grepl('nav_panel("DEP analysis", icon =', source_text, fixed = TRUE))
  expect_true(grepl('nav_panel("Nine Quadrant", icon =', source_text, fixed = TRUE))
  expect_true(grepl('nav_panel("Correlation chord", icon =', source_text, fixed = TRUE))
  expect_true(grepl('bs_icon("diagram-3-fill")', source_text, fixed = TRUE))
})
