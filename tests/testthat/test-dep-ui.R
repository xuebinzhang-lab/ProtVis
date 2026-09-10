test_that("DEP plot parameter sidebars are open by default", {
  ui_text <- paste(readLines(testthat::test_path("..", "..", "R", "DEP_analysis.R")),
                   collapse = "\n")
  expect_equal(length(gregexpr('open = "open"', ui_text, fixed = TRUE)[[1]]), 2L)
  expect_false(grepl('open = FALSE', ui_text, fixed = TRUE))
})

test_that("DEP heatmap guards clustering for singleton dimensions", {
  ui_text <- paste(readLines(testthat::test_path("..", "..", "R", "DEP_analysis.R")),
                   collapse = "\n")
  expect_true(grepl("cluster_rows = base::nrow\\(heatmap_data\\) >= 2L", ui_text))
  expect_true(grepl("cluster_cols = base::ncol\\(heatmap_data\\) >= 2L", ui_text))
  expect_true(grepl("show_colnames = TRUE", ui_text, fixed = TRUE))
  expect_true(grepl("treeheight_row = if", ui_text, fixed = TRUE))
})
